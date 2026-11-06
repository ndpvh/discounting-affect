################################################################################
# PURPOSE:
#
# Create scatterplots comparing the parameters used to simulate the data with
# the parameters recovered by fitting the same model to those data.
#
# The script reads one realistic-recovery result RDS, makes one panel per
# parameter, draws the identity line, and annotates each panel with the Pearson
# correlation and number of paired points. It does not plot the SSE objective.
#
# Select the model result RDS (e.g., exponential.Rds) when prompted. The PDF is
# saved beside that RDS. This script reads the full result object, not just the
# first six rows shown by head().
################################################################################

plot_recovery_parameters <- function(result_file) {
  
  if (!file.exists(result_file)) {
    stop("Cannot find the recovery result file: ", result_file)
  }
  
  result <- readRDS(result_file)
  required_components <- c("simulate", "fit", "metadata")
  missing_components <- setdiff(required_components, names(result))
  if (length(missing_components) > 0L) {
    stop(
      "This file does not contain the expected realistic-recovery tables: ",
      paste(missing_components, collapse = ", ")
    )
  }
  
  if (nrow(result$simulate) != nrow(result$fit) ||
      nrow(result$simulate) != nrow(result$metadata)) {
    stop("The simulate, fit, and metadata tables do not have matching row counts.")
  }
  
  # Plot only parameters available in both paired tables. 'objective' is a
  # fit statistic, not a generating parameter, so it is excluded.
  parameter_names <- intersect(
    names(result$simulate),
    names(result$fit)
  )
  parameter_names <- setdiff(parameter_names, "objective")
  
  if (length(parameter_names) == 0L) {
    stop("No parameter columns are shared by simulate and fit.")
  }
  
  non_numeric <- parameter_names[
    !vapply(result$simulate[parameter_names], is.numeric, logical(1)) |
      !vapply(result$fit[parameter_names], is.numeric, logical(1))
  ]
  if (length(non_numeric) > 0L) {
    stop(
      "Parameter columns must be numeric: ",
      paste(non_numeric, collapse = ", ")
    )
  }
  
  # Save the PDF beside the selected RDS.
  output_file <- sub(
    "\\.[Rr][Dd][Ss]$",
    "_parameter_recovery.pdf",
    result_file
  )
  if (identical(output_file, result_file)) {
    output_file <- paste0(result_file, "_parameter_recovery.pdf")
  }
  
  n_columns <- min(3L, length(parameter_names))
  n_rows <- ceiling(length(parameter_names) / n_columns)
  
  grDevices::pdf(
    file = output_file,
    width = 5 * n_columns,
    height = 4.5 * n_rows
  )
  
  tryCatch(
    {
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par), add = TRUE)
      graphics::par(
        mfrow = c(n_rows, n_columns),
        mar = c(4.5, 4.5, 1.5, 1),
        oma = c(0, 0, 0, 0)
      )
      
      for (parameter in parameter_names) {
        generating <- result$simulate[[parameter]]
        recovered <- result$fit[[parameter]]
        
        keep <- is.finite(generating) & is.finite(recovered)
        generating <- generating[keep]
        recovered <- recovered[keep]
        n_points <- length(generating)
        
        if (n_points == 0L) {
          plot.new()
          graphics::text(0.5, 0.5, paste("No finite pairs for", parameter))
          next
        }
        
        plot_values <- c(generating, recovered)
        plot_limits <- range(plot_values)
        plot_width <- diff(plot_limits)
        if (plot_width == 0) {
          plot_width <- max(abs(plot_limits), 1) * 0.1
        }
        plot_limits <- plot_limits + c(-0.08, 0.08) * plot_width
        
        correlation <- if (
          n_points >= 2L && stats::sd(generating) > 0 &&
          stats::sd(recovered) > 0
        ) {
          stats::cor(generating, recovered)
        } else {
          NA_real_
        }
        
        graphics::plot(
          generating,
          recovered,
          xlim = plot_limits,
          ylim = plot_limits,
          xlab = paste("True (generating)", parameter),
          ylab = paste("Estimated (recovered)", parameter),
          pch = 21,
          bg = "steelblue",
          col = "white",
          cex = 1.1
        )
        graphics::abline(
          a = 0,
          b = 1,
          lty = 2,
          lwd = 1.5,
          col = "darkorange"
        )
        
        correlation_label <- if (is.finite(correlation)) {
          sprintf("Pearson r = %.2f", correlation)
        } else {
          "Pearson r = NA"
        }
        graphics::legend(
          "topleft",
          legend = c(correlation_label, paste("Paired points =", n_points)),
          bty = "n",
          cex = 0.85
        )
      }
    },
    finally = grDevices::dev.off()
  )
  
  message("Saved parameter-recovery scatterplots to: ", output_file)
  return(invisible(output_file))
}

################################################################################
# RUN

# In RStudio, select the model result file from the timestamped realistic
# recovery output directory, for example exponential.Rds.
result_file <- file.choose()
plot_recovery_parameters(result_file)

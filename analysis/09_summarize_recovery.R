################################################################################
# PARAMETER RECOVERY: SUMMARIES AND FIGURES
#
# Reads results from 03_run_recovery.R; does not simulate or refit models.
# Supports both a priori and a posteriori recovery, full runs and pilots.
#
# Usage (from repository root):
#   Rscript analysis/09_summarize_recovery.R                 # both full studies
#   Rscript analysis/09_summarize_recovery.R pilot           # both pilots
#   Rscript analysis/09_summarize_recovery.R apriori         # only a priori
#   Rscript analysis/09_summarize_recovery.R aposteriori pilot
################################################################################

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 2L || any(!args %in% c("pilot", "apriori", "aposteriori")) ||
    sum(args == "pilot") > 1L ||
    sum(args %in% c("apriori", "aposteriori")) > 1L) {
  stop("Usage: Rscript analysis/09_summarize_recovery.R [pilot] [apriori|aposteriori]")
}
pilot <- "pilot" %in% args
studies <- intersect(args, c("apriori", "aposteriori"))
if (!length(studies)) studies <- c("apriori", "aposteriori")
run_type <- if (pilot) "pilot" else "full"

config_file <- if (file.exists("analysis/_config.R")) {
  "analysis/_config.R"
} else if (file.exists("_config.R")) {
  "_config.R"
} else {
  stop("Run from the repository root or the analysis/ directory.")
}
source(config_file)

models <- c("exponential", "quasi_hyperbolic", "double_exponential")

# Summary utilities -----------------------------------------------------------
finite_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}
finite_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) median(x) else NA_real_
}

summarize_parameter <- function(simulated, estimated) {
  valid <- is.finite(simulated) & is.finite(estimated)
  s <- simulated[valid]
  e <- estimated[valid]
  error <- e - s
  n <- length(s)

  # Correlation describes between-simulation/participant rank-order recovery.
  # For a pilot with 2 observations it is not a stable performance measure.
  r <- if (n > 1L && stats::sd(s) > 0 && stats::sd(e) > 0) {
    stats::cor(s, e)
  } else {
    NA_real_
  }
  regression <- if (n > 1L && stats::sd(s) > 0) {
    stats::coef(stats::lm(e ~ s))
  } else {
    c(NA_real_, NA_real_)
  }

  data.frame(
    n_total = length(simulated), n_valid = n,
    n_failed = length(simulated) - n,
    failure_percentage = 100 * (length(simulated) - n) / length(simulated),
    simulated_mean = finite_mean(s),
    simulated_sd = if (n > 1L) stats::sd(s) else NA_real_,
    estimated_mean = finite_mean(e),
    estimated_sd = if (n > 1L) stats::sd(e) else NA_real_,
    pearson_r = r, r_squared = r^2,
    parameter_bias = finite_mean(error),
    median_error = finite_median(error),
    mae = finite_mean(abs(error)),
    rmse = if (n) sqrt(mean(error^2)) else NA_real_,
    regression_intercept = unname(regression[1]),
    regression_slope = unname(regression[2])
  )
}

# Read study/dimensionality from metadata, not filename conventions.
read_recovery <- function(path, study) {
  result <- readRDS(path)
  if (is.null(result$simulate) || is.null(result$fit)) {
    stop("Missing simulate or fit table in ", path)
  }
  info <- result$settings
  if (is.null(info) || !all(c("model", "d", "k") %in% names(info))) {
    stop("Missing model/d/k in settings: ", path)
  }
  if (!identical(as.character(info$study), study)) {
    stop("Unexpected study in file: ", path)
  }
  if (!(info$model %in% models)) stop("Unexpected model in: ", path)

  simulated <- as.data.frame(result$simulate)
  fitted <- as.data.frame(result$fit)
  if (nrow(simulated) != nrow(fitted)) stop("Unpaired recovery rows: ", path)
  if (!is.null(result$metadata) && nrow(result$metadata) != nrow(fitted)) {
    stop("Unpaired metadata rows: ", path)
  }
  parameters <- intersect(names(simulated), names(fitted))
  parameters <- setdiff(parameters, c("objective", "aic", "bic", "bias", "autocorrelation"))
  if (!length(parameters)) stop("No shared parameters in: ", path)
  for (p in parameters) {
    if (!is.numeric(simulated[[p]]) || !is.numeric(fitted[[p]])) {
      stop("Non-numeric parameter ", p, " in ", path)
    }
  }
  list(info = info, simulate = simulated, fit = fitted,
       metadata = result$metadata, parameters = parameters)
}

plot_recovery <- function(x, file) {
  # One panel per parameter, same x/y limits; dashed identity line.
  points <- do.call(rbind, lapply(x$parameters, function(p) {
    data.frame(parameter = p, simulated = x$simulate[[p]],
               estimated = x$fit[[p]])
  }))
  points <- points[is.finite(points$simulated) & is.finite(points$estimated), ]
  if (!nrow(points)) return(invisible(NULL))
  points$parameter <- factor(points$parameter, levels = x$parameters)

  fig <- ggplot2::ggplot(points, ggplot2::aes(x = simulated, y = estimated)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                         color = "grey40") +
    ggplot2::geom_point(alpha = 0.35, size = 1.3, color = "steelblue") +
    ggplot2::facet_wrap(~ parameter, scales = "free", ncol = 3) +
    ggplot2::labs(x = "Generating parameter", y = "Recovered parameter") +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  ggplot2::ggsave(file, fig, width = 12,
                  height = max(4, 3.5 * ceiling(length(x$parameters) / 3)),
                  units = "in", dpi = 300, limitsize = FALSE)
}

# Process each study ----------------------------------------------------------
for (study in studies) {
  input_dir <- file.path(PATHS$recovery, study, run_type)
  if (!dir.exists(input_dir)) {
    warning("No result directory for ", study, " / ", run_type, ": ", input_dir)
    next
  }
  files <- sort(list.files(input_dir, pattern = "\\.[Rr][Dd][Ss]$",
                           full.names = TRUE))
  # Exclude configuration/status RDS files if present.
  files <- files[!grepl("(run_settings|run_status)\\.[Rr][Dd][Ss]$", files)]
  if (!length(files)) {
    warning("No recovery result files for ", study, " / ", run_type)
    next
  }

  summary_dir <- file.path(input_dir, "summary")
  figure_dir <- file.path(PATHS$figures, "recovery", study, run_type)
  dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

  parameter_rows <- list()
  diagnostic_rows <- list()
  overview_rows <- list()
  clipping_rows <- list()

  for (path in files) {
    x <- read_recovery(path, study)
    info <- x$info
    message(study, " / ", run_type, " / ", info$model,
            " (d=", info$d, ", k=", info$k, ")")
    common <- list(study = study, run_type = run_type, model = info$model,
                   d = info$d, k = info$k)

    for (p in x$parameters) {
      row <- cbind(as.data.frame(common), parameter = p,
                   summarize_parameter(x$simulate[[p]], x$fit[[p]]))
      parameter_rows[[length(parameter_rows) + 1L]] <- row
    }

    # The a priori study requests fit diagnostics; the realistic study does not.
    # Missing diagnostics are marked missing, not incorrectly treated as zero.
    for (stat in c("aic", "bic", "autocorrelation", "bias")) {
      vals <- if (stat %in% names(x$fit)) x$fit[[stat]] else numeric(0)
      vals <- vals[is.finite(vals)]
      diagnostic_rows[[length(diagnostic_rows) + 1L]] <- cbind(
        as.data.frame(common), statistic = stat,
        n = length(vals), mean = finite_mean(vals),
        median = finite_median(vals),
        mean_absolute = finite_mean(abs(vals))
      )
    }

    s <- as.matrix(x$simulate[, x$parameters, drop = FALSE])
    f <- as.matrix(x$fit[, x$parameters, drop = FALSE])
    complete <- rowSums(!is.finite(s) | !is.finite(f)) == 0
    this_params <- do.call(rbind, lapply(x$parameters, function(p) {
      summarize_parameter(x$simulate[[p]], x$fit[[p]])
    }))
    overview_rows[[length(overview_rows) + 1L]] <- cbind(
      as.data.frame(common), n_recoveries = nrow(f),
      n_parameters = length(x$parameters),
      complete_recoveries = sum(complete),
      incomplete_percentage = 100 * mean(!complete),
      median_parameter_r = finite_median(this_params$pearson_r),
      mean_absolute_parameter_bias = finite_mean(abs(this_params$parameter_bias)),
      mean_parameter_rmse = finite_mean(this_params$rmse)
    )

    if (!is.null(x$metadata) &&
        "proportion_clipped" %in% names(x$metadata)) {
      clipping_rows[[length(clipping_rows) + 1L]] <- cbind(
        as.data.frame(common), n_recoveries = nrow(x$metadata),
        mean_proportion_clipped = finite_mean(x$metadata$proportion_clipped),
        pct_any_clipping = 100 * mean(x$metadata$proportion_clipped > 0,
                                      na.rm = TRUE)
      )
    }

    name <- tools::file_path_sans_ext(basename(path))
    plot_recovery(x, file.path(figure_dir, paste0(name, ".jpeg")))
  }

  write.csv(do.call(rbind, parameter_rows),
            file.path(summary_dir, "parameter_recovery_summary.csv"), row.names = FALSE)
  write.csv(do.call(rbind, diagnostic_rows),
            file.path(summary_dir, "recovery_diagnostics_summary.csv"), row.names = FALSE)
  overview <- do.call(rbind, overview_rows)
  write.csv(overview, file.path(summary_dir, "recovery_overview.csv"), row.names = FALSE)
  write.csv(overview[, c("study", "model", "d", "k", "n_recoveries",
                         "incomplete_percentage", "median_parameter_r",
                         "mean_absolute_parameter_bias", "mean_parameter_rmse")],
            file.path(summary_dir, "recovery_paper_summary.csv"), row.names = FALSE)
  if (length(clipping_rows)) {
    write.csv(do.call(rbind, clipping_rows),
              file.path(summary_dir, "recovery_clipping_summary.csv"), row.names = FALSE)
  }

  cat("\n", toupper(study), " (", run_type, "): ", nrow(overview),
      " model/dimension configurations summarized.\n",
      "Tables: ", summary_dir, "\nFigures: ", figure_dir, "\n", sep = "")
}

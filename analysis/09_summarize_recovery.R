################################################################################
# PURPOSE:
#
# Summarize the RDS output produced by the recovery workflow (03_run_recovery.R).
#
# The recovery study generates 12 result files:
#   3 model types x d = 1:2 x k = 1:2
#
# For each file, this script summarizes:
#   1. Parameter recovery
#      - Pearson correlation between simulated and estimated values
#      - parameter-estimation bias (estimated - simulated)
#      - MAE and RMSE
#      - regression intercept and slope (estimated ~ simulated)
#      - number/proportion of non-finite recovery estimates
#
#   2. Recovery-level diagnostics saved by recovery.R
#      - AIC
#      - BIC
#      - residual autocorrelation
#      - residual bias
#
#   3. A compact model/dimension-level overview suitable for reporting.
#
# Output is written to:
#   analysis/results/recovery/summary/   (PATHS$recovery/summary)
#
################################################################################


config_file <- if (file.exists(file.path("analysis", "_config.R"))) {
  file.path("analysis", "_config.R")
} else if (file.exists("_config.R")) {
  "_config.R"
} else {
  stop(
    "Could not find analysis/_config.R. ",
    "Run this script from the repository root or analysis/ directory."
  )
}
source(config_file)
source(file.path(PATHS$analysis, "_helpers.R"))
rm(config_file)


################################################################################
# SETTINGS
################################################################################

input_dir <- PATHS$recovery
output_dir <- file.path(input_dir, "summary")

ensure_dir(output_dir)

# Diagnostics requested in recovery.R
requested_diagnostics <- c("aic", "bic", "autocorrelation", "bias")


################################################################################
# HELPER FUNCTIONS
################################################################################

# Parse filenames produced by recovery.R, e.g.:
#   exponential_11.Rds
#   quasi_hyperbolic_22.Rds
#
# The first digit is d and the second digit is k.
parse_file_name <- function(path) {
  nm <- tools::file_path_sans_ext(basename(path))
  match <- regexec("^(.*)_([12])([12])$", nm)
  parts <- regmatches(nm, match)[[1]]

  if (length(parts) != 4) {
    return(data.frame(
      model = nm,
      d = NA_integer_,
      k = NA_integer_,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    model = parts[2],
    d = as.integer(parts[3]),
    k = as.integer(parts[4]),
    stringsAsFactors = FALSE
  )
}


# Safe summary functions for vectors that may contain NA/NaN/Inf.
safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1) NA_real_ else sd(x)
}

safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else median(x)
}

safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else min(x)
}

safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else max(x)
}


# Recursively search a recovery object for a requested diagnostic.
#
# This is intentionally flexible because the exact container used by recovery()
# for fx output can differ across package versions. It looks for:
#   - a list element named exactly like the statistic
#   - a matrix/data.frame column with that name
#   - a matrix/data.frame row with that name
#
# It skips result$simulate and result$fit so parameter names cannot accidentally
# be mistaken for fit diagnostics.
find_diagnostic_candidates <- function(x,
                                       target,
                                       current_path = "result",
                                       skip_names = c("simulate", "fit")) {
  out <- list()

  # Matrix/data.frame: search columns and rows
  if (is.matrix(x) || is.data.frame(x)) {
    cn <- colnames(x)
    rn <- rownames(x)

    if (!is.null(cn) && target %in% cn) {
      out[[length(out) + 1]] <- list(
        path = paste0(current_path, "[, '", target, "']"),
        values = suppressWarnings(as.numeric(x[, target]))
      )
    }

    if (!is.null(rn) && target %in% rn) {
      out[[length(out) + 1]] <- list(
        path = paste0(current_path, "['", target, "', ]"),
        values = suppressWarnings(as.numeric(x[target, ]))
      )
    }

    return(out)
  }

  # Named list: inspect direct match first, then recurse
  if (is.list(x)) {
    nms <- names(x)

    if (!is.null(nms) && target %in% nms) {
      value <- x[[target]]

      if (is.atomic(value) || is.matrix(value) || is.data.frame(value)) {
        vals <- suppressWarnings(as.numeric(unlist(value, use.names = FALSE)))
        out[[length(out) + 1]] <- list(
          path = paste0(current_path, "$", target),
          values = vals
        )
      }
    }

    if (length(x) > 0) {
      for (i in seq_along(x)) {
        child_name <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else paste0("[[", i, "]]" )

        if (child_name %in% skip_names) {
          next
        }

        child_path <- if (startsWith(child_name, "[[")) {
          paste0(current_path, child_name)
        } else {
          paste0(current_path, "$", child_name)
        }

        child <- find_diagnostic_candidates(
          x[[i]],
          target = target,
          current_path = child_path,
          skip_names = skip_names
        )

        if (length(child) > 0) {
          out <- c(out, child)
        }
      }
    }
  }

  out
}


# Select the most plausible diagnostic vector.
# Preference is given to a vector with one value per recovery iteration.
extract_diagnostic <- function(result, target, n_iterations) {
  candidates <- find_diagnostic_candidates(result, target)

  if (length(candidates) == 0) {
    return(list(values = numeric(0), path = NA_character_))
  }

  lengths <- vapply(candidates, function(z) length(z$values), integer(1))

  exact <- which(lengths == n_iterations)
  if (length(exact) > 0) {
    chosen <- candidates[[exact[1]]]
  } else {
    chosen <- candidates[[which.max(lengths)]]
  }

  chosen
}


# Summarize one simulated/estimated parameter pair.
summarize_parameter <- function(simulated, estimated) {
  ok <- is.finite(simulated) & is.finite(estimated)

  s <- simulated[ok]
  e <- estimated[ok]

  n_total <- length(simulated)
  n_valid <- length(s)
  n_failed <- n_total - n_valid

  if (n_valid == 0) {
    return(data.frame(
      n_total = n_total,
      n_valid = 0L,
      n_failed = n_failed,
      failure_percentage = 100,
      simulated_mean = NA_real_,
      simulated_sd = NA_real_,
      estimated_mean = NA_real_,
      estimated_sd = NA_real_,
      pearson_r = NA_real_,
      r_squared = NA_real_,
      parameter_bias = NA_real_,
      median_error = NA_real_,
      mae = NA_real_,
      rmse = NA_real_,
      regression_intercept = NA_real_,
      regression_slope = NA_real_
    ))
  }

  error <- e - s

  r <- if (n_valid > 1 && sd(s) > 0 && sd(e) > 0) {
    cor(s, e)
  } else {
    NA_real_
  }

  if (n_valid > 1 && sd(s) > 0) {
    regression <- lm(e ~ s)
    intercept <- unname(coef(regression)[1])
    slope <- unname(coef(regression)[2])
  } else {
    intercept <- NA_real_
    slope <- NA_real_
  }

  data.frame(
    n_total = n_total,
    n_valid = n_valid,
    n_failed = n_failed,
    failure_percentage = 100 * n_failed / n_total,
    simulated_mean = mean(s),
    simulated_sd = if (n_valid > 1) sd(s) else NA_real_,
    estimated_mean = mean(e),
    estimated_sd = if (n_valid > 1) sd(e) else NA_real_,
    pearson_r = r,
    r_squared = ifelse(is.na(r), NA_real_, r^2),
    parameter_bias = mean(error),
    median_error = median(error),
    mae = mean(abs(error)),
    rmse = sqrt(mean(error^2)),
    regression_intercept = intercept,
    regression_slope = slope
  )
}


################################################################################
# FIND RECOVERY FILES
################################################################################

recovery_files <- list.files(
  input_dir,
  pattern = "\\.[Rr][Dd][Ss]$",
  full.names = TRUE,
  recursive = FALSE
)

if (length(recovery_files) == 0) {
  stop("No recovery .Rds files found in: ", input_dir)
}

message("Found ", length(recovery_files), " recovery result files.")


################################################################################
# SUMMARIZE EACH FILE
################################################################################

parameter_rows <- list()
diagnostic_rows <- list()
file_rows <- list()

for (file_index in seq_along(recovery_files)) {

  path <- recovery_files[file_index]
  info <- parse_file_name(path)

  message(
    "\n[", file_index, "/", length(recovery_files), "] ",
    basename(path)
  )

  result <- readRDS(path)

  # The original recovery.R explicitly uses these two components when plotting.
  if (is.null(result$simulate) || is.null(result$fit)) {
    warning(
      "Skipping ", basename(path),
      ": result$simulate and/or result$fit is missing."
    )
    next
  }

  simulate <- as.data.frame(result$simulate)
  fit <- as.data.frame(result$fit)

  # Match parameters by name.
  common_parameters <- intersect(colnames(simulate), colnames(fit))

  if (length(common_parameters) == 0) {
    warning("No matching parameter columns in ", basename(path))
    next
  }

  if (!identical(colnames(simulate), colnames(fit))) {
    warning(
      "Simulated and estimated parameter columns differ in ", basename(path),
      ". Only matching columns will be summarized."
    )
  }

  n_iterations <- nrow(simulate)

  # ---------------------------------------------------------------------------
  # 1. PARAMETER RECOVERY
  # ---------------------------------------------------------------------------

  this_parameter_rows <- lapply(
    common_parameters,
    function(parameter) {
      summary <- summarize_parameter(
        simulated = simulate[[parameter]],
        estimated = fit[[parameter]]
      )

      cbind(
        data.frame(
          file = basename(path),
          model = info$model,
          d = info$d,
          k = info$k,
          parameter = parameter,
          stringsAsFactors = FALSE
        ),
        summary
      )
    }
  )

  this_parameter_rows <- do.call(rbind, this_parameter_rows)
  parameter_rows[[length(parameter_rows) + 1]] <- this_parameter_rows

  # Complete recovery iteration = finite simulated and fitted value for every
  # parameter in this file.
  complete_iteration <- complete.cases(simulate[, common_parameters, drop = FALSE]) &
    complete.cases(fit[, common_parameters, drop = FALSE])

  # ---------------------------------------------------------------------------
  # 2. FIT / RESIDUAL DIAGNOSTICS
  # ---------------------------------------------------------------------------

  this_diag <- list()

  for (stat in requested_diagnostics) {
    extracted <- extract_diagnostic(
      result,
      target = stat,
      n_iterations = n_iterations
    )

    values <- extracted$values
    values <- values[is.finite(values)]

    if (length(values) == 0) {
      message(
        "  Diagnostic '", stat,
        "' was not located automatically in this object."
      )

      row <- data.frame(
        file = basename(path),
        model = info$model,
        d = info$d,
        k = info$k,
        statistic = stat,
        source_path = NA_character_,
        n = 0L,
        mean = NA_real_,
        sd = NA_real_,
        median = NA_real_,
        min = NA_real_,
        max = NA_real_,
        mean_absolute = NA_real_,
        stringsAsFactors = FALSE
      )

    } else {
      row <- data.frame(
        file = basename(path),
        model = info$model,
        d = info$d,
        k = info$k,
        statistic = stat,
        source_path = extracted$path,
        n = length(values),
        mean = safe_mean(values),
        sd = safe_sd(values),
        median = safe_median(values),
        min = safe_min(values),
        max = safe_max(values),
        mean_absolute = safe_mean(abs(values)),
        stringsAsFactors = FALSE
      )
    }

    this_diag[[length(this_diag) + 1]] <- row
  }

  this_diag <- do.call(rbind, this_diag)
  diagnostic_rows[[length(diagnostic_rows) + 1]] <- this_diag

  # ---------------------------------------------------------------------------
  # 3. FILE-LEVEL OVERVIEW
  # ---------------------------------------------------------------------------

  valid_r <- this_parameter_rows$pearson_r[is.finite(this_parameter_rows$pearson_r)]
  abs_bias <- abs(this_parameter_rows$parameter_bias)
  rmse <- this_parameter_rows$rmse

  get_diag_value <- function(stat, column = "mean") {
    idx <- this_diag$statistic == stat
    if (!any(idx)) return(NA_real_)
    this_diag[[column]][which(idx)[1]]
  }

  file_rows[[length(file_rows) + 1]] <- data.frame(
    file = basename(path),
    model = info$model,
    d = info$d,
    k = info$k,
    n_iterations = n_iterations,
    n_parameters = length(common_parameters),
    complete_iterations = sum(complete_iteration),
    incomplete_iterations = sum(!complete_iteration),
    incomplete_percentage = 100 * mean(!complete_iteration),
    mean_parameter_r = safe_mean(valid_r),
    median_parameter_r = safe_median(valid_r),
    min_parameter_r = safe_min(valid_r),
    max_parameter_r = safe_max(valid_r),
    mean_absolute_parameter_bias = safe_mean(abs_bias),
    mean_parameter_rmse = safe_mean(rmse),
    mean_aic = get_diag_value("aic", "mean"),
    mean_bic = get_diag_value("bic", "mean"),
    mean_residual_autocorrelation = get_diag_value("autocorrelation", "mean"),
    mean_absolute_residual_autocorrelation = get_diag_value("autocorrelation", "mean_absolute"),
    mean_residual_bias = get_diag_value("bias", "mean"),
    mean_absolute_residual_bias = get_diag_value("bias", "mean_absolute"),
    stringsAsFactors = FALSE
  )
}


################################################################################
# COMBINE RESULTS
################################################################################

if (length(parameter_rows) == 0) {
  stop("No valid recovery files could be summarized.")
}

parameter_summary <- do.call(rbind, parameter_rows)
diagnostic_summary <- do.call(rbind, diagnostic_rows)
file_summary <- do.call(rbind, file_rows)

# Order results consistently
model_order <- c("exponential", "quasi_hyperbolic", "double_exponential")

parameter_summary$model <- factor(parameter_summary$model, levels = model_order)
diagnostic_summary$model <- factor(diagnostic_summary$model, levels = model_order)
file_summary$model <- factor(file_summary$model, levels = model_order)

parameter_summary <- parameter_summary[
  order(parameter_summary$model, parameter_summary$d, parameter_summary$k, parameter_summary$parameter),
]

diagnostic_summary <- diagnostic_summary[
  order(diagnostic_summary$model, diagnostic_summary$d, diagnostic_summary$k, diagnostic_summary$statistic),
]

file_summary <- file_summary[
  order(file_summary$model, file_summary$d, file_summary$k),
]

# Return model names to plain character before writing CSV.
parameter_summary$model <- as.character(parameter_summary$model)
diagnostic_summary$model <- as.character(diagnostic_summary$model)
file_summary$model <- as.character(file_summary$model)


################################################################################
# SAVE TABLES
################################################################################

write.csv(
  parameter_summary,
  file.path(output_dir, "parameter_recovery_summary.csv"),
  row.names = FALSE
)

write.csv(
  diagnostic_summary,
  file.path(output_dir, "recovery_diagnostics_summary.csv"),
  row.names = FALSE
)

write.csv(
  file_summary,
  file.path(output_dir, "recovery_overview.csv"),
  row.names = FALSE
)


################################################################################
# CREATE A SMALL PAPER-FOCUSED TABLE
################################################################################

# This table keeps only the statistics that are likely to be useful in the
# written recovery-study results. Parameter-level details remain available in
# parameter_recovery_summary.csv.
paper_summary <- file_summary[, c(
  "model",
  "d",
  "k",
  "n_iterations",
  "n_parameters",
  "incomplete_percentage",
  "median_parameter_r",
  "min_parameter_r",
  "max_parameter_r",
  "mean_absolute_parameter_bias",
  "mean_parameter_rmse",
  "mean_absolute_residual_autocorrelation",
  "mean_absolute_residual_bias"
)]

write.csv(
  paper_summary,
  file.path(output_dir, "recovery_paper_summary.csv"),
  row.names = FALSE
)


################################################################################
# CONSOLE REPORT
################################################################################

cat("\n")
cat("===============================================================================\n")
cat("RECOVERY STUDY SUMMARY\n")
cat("===============================================================================\n\n")
cat("Recovery files processed:", nrow(file_summary), "\n")
cat("Parameter-level rows:     ", nrow(parameter_summary), "\n")
cat("\n")

for (i in seq_len(nrow(file_summary))) {
  x <- file_summary[i, ]

  # Find the weakest recovered parameter for this model/d/k combination
  sub <- parameter_summary[
    parameter_summary$model == x$model &
      parameter_summary$d == x$d &
      parameter_summary$k == x$k,
  ]

  finite_idx <- which(is.finite(sub$pearson_r))
  if (length(finite_idx) > 0) {
    weakest_idx <- finite_idx[which.min(sub$pearson_r[finite_idx])]
    weakest_parameter <- sub$parameter[weakest_idx]
    weakest_r <- sub$pearson_r[weakest_idx]
  } else {
    weakest_parameter <- NA_character_
    weakest_r <- NA_real_
  }

  cat(
    sprintf(
      "%s (d=%d, k=%d)\n",
      x$model,
      x$d,
      x$k
    )
  )
  cat(
    sprintf(
      "  Complete recovery iterations: %d / %d (%.1f%%)\n",
      x$complete_iterations,
      x$n_iterations,
      100 - x$incomplete_percentage
    )
  )
  cat(
    sprintf(
      "  Parameter recovery r: median = %.3f, range = %.3f to %.3f\n",
      x$median_parameter_r,
      x$min_parameter_r,
      x$max_parameter_r
    )
  )
  cat(
    sprintf(
      "  Weakest parameter: %s (r = %.3f)\n",
      weakest_parameter,
      weakest_r
    )
  )
  cat(
    sprintf(
      "  Mean |parameter bias| = %.4f; mean RMSE = %.4f\n",
      x$mean_absolute_parameter_bias,
      x$mean_parameter_rmse
    )
  )
  cat(
    sprintf(
      "  Mean |residual autocorrelation| = %.4f; mean |residual bias| = %.4f\n\n",
      x$mean_absolute_residual_autocorrelation,
      x$mean_absolute_residual_bias
    )
  )
}

cat("Detailed parameter recovery:\n\n")
print(
  parameter_summary[, c(
    "model", "d", "k", "parameter", "n_valid",
    "pearson_r", "parameter_bias", "mae", "rmse",
    "regression_intercept", "regression_slope"
  )],
  row.names = FALSE,
  digits = 4
)

cat("\n")
cat("Saved summary files to:\n  ", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("\nFiles created:\n")
cat("  - parameter_recovery_summary.csv\n")
cat("  - recovery_diagnostics_summary.csv\n")
cat("  - recovery_overview.csv\n")
cat("  - recovery_paper_summary.csv\n")
cat("===============================================================================\n")

################################################################################
# PURPOSE:
# Summarize the distributions of ALL estimated model parameters across
# participants for every dataset x discounting-model combination.
#
# This replaces the earlier forgetting-factor-only summary. It includes:
#   - alpha: intercept / baseline-affect parameters
#   - beta: predictor-effect parameters
#   - gamma, nu, kappa, omega: temporal-weighting parameters
#   - sigma: residual variance/covariance parameters
#
# For every individual parameter the script calculates:
#   n, mean, SD, median, min, 2.5th/25th/75th/97.5th percentiles, max,
#   and the percentage of estimates lying near the parameter bounds.
#
# OUTPUTS:
#   1. parameter_summary_detailed.csv
#      Complete long-format summary of every parameter.
#   2. <DATASET>_parameter_summary.csv
#      The detailed summary split into one file per dataset.
#   3. parameter_summary_supplement.csv
#      Compact wide-format table source: one row per parameter, with the three
#      models side by side. Each model cell contains M (SD) [2.5%, 97.5%].
#   4. parameter_summary_boundaries.csv
#      Only parameters for which >=10% of estimates fall within 1% of either
#      estimation bound. This is a diagnostic aid for interpreting estimates
#      close to parameter constraints; it is not itself an inferential test.
#
# NOTE ON PARAMETER INDICES:
#   alpha_i          : response dimension i
#   beta_ij          : response dimension i, predictor j
#   gamma_ii etc.    : temporal parameter for response dimension i under the
#                      isotropic dynamics used in estimation
#   sigma_ii         : residual variance for response dimension i
#   sigma_ij, i != j : residual covariance between response dimensions i and j
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

input_dir <- PATHS$estimation

# This script was renamed from 08_forgetting_factor_spread.R and therefore
# requires the corresponding canonical output path in _config.R. Give a clear
# error if an older _config.R is still being used.
if (is.null(PATHS$parameter_summary) ||
    !is.character(PATHS$parameter_summary) ||
    length(PATHS$parameter_summary) != 1L ||
    is.na(PATHS$parameter_summary) ||
    !nzchar(PATHS$parameter_summary)) {
  stop(
    "PATHS$parameter_summary is not defined correctly in analysis/_config.R.\n",
    "Add: parameter_summary = file.path(PROJECT_ROOT, \"analysis\", \"results\", \"parameter_summary\")"
  )
}

output_dir <- PATHS$parameter_summary
ensure_dir(output_dir)


################################################################################
# METADATA USED ONLY FOR READABLE OUTPUT LABELS
################################################################################

# Response dimensions and predictors are listed in the same order used when
# the dataset objects are created in 01_process_data.R.
DATASET_METADATA <- list(
  VANHASBROECK_2021 = list(
    responses  = c("Happiness"),
    predictors = c("CR", "EV", "RPE", "Total")
  ),
  VANHASBROECK_2022 = list(
    responses  = c("Positive affect", "Negative affect"),
    predictors = c("Outcome", "Total")
  ),
  VANHASBROECK_2024_1 = list(
    responses  = c("Valence"),
    predictors = c("Outcome")
  ),
  VANHASBROECK_2024_2 = list(
    responses  = c("Positive affect", "Negative affect"),
    predictors = c("Outcome")
  ),
  NIEMEIJER_2022 = list(
    responses  = c("Positive affect", "Negative affect"),
    predictors = c("Positive context", "Negative context")
  )
)

PARAMETER_GROUPS <- c(
  alpha = "Intercept",
  beta  = "Predictor effect",
  gamma = "Temporal weighting",
  nu    = "Temporal weighting",
  kappa = "Temporal weighting",
  omega = "Temporal weighting",
  sigma = "Residual variance/covariance"
)

# Bounds used by the model-estimation functions. They match the package defaults
# used in the current estimation workflow.
PARAMETER_BOUNDS <- list(
  alpha = c(-1, 1),
  beta  = c(-5, 5),
  gamma = c(0, 1),
  nu    = c(0, 1),
  kappa = c(0, 1),
  omega = c(0, 0.5),
  sigma = c(1e-5, 1)
)

# "Near a bound" means within 1% of that parameter's allowable range.
BOUNDARY_TOLERANCE_PROPORTION <- 0.01
BOUNDARY_REPORT_THRESHOLD_PCT <- 10

# Columns produced by 02_estimate_models.R that are diagnostics rather than
# estimated parameters. They are deliberately excluded from this script.
STAT_COLS <- c(
  "participant_id",
  "aic",
  "bic",
  "autocorrelation",
  "bias",
  "objective_sse"
)

# Parameters to be reported in the new supplementary "other parameters" section.
# Temporal-weighting parameters are reported separately elsewhere.
SUPPLEMENT_PARAMETER_FAMILIES <- c("alpha", "beta", "sigma")


################################################################################
# HELPERS
################################################################################

parameter_prefix <- function(parameter) {
  sub("_.*$", "", parameter)
}

# Extract matrix/vector indices from the current parameter naming convention.
# The current models use d <= 2 and k <= 4, so beta_14, beta_21, etc. are
# unambiguous: first digit = response dimension, second = predictor dimension.
parameter_indices <- function(parameter, prefix) {
  if (prefix == "omega") {
    return(c(NA_integer_, NA_integer_))
  }

  suffix <- sub(paste0("^", prefix, "_"), "", parameter)

  if (prefix == "alpha") {
    return(c(as.integer(suffix), NA_integer_))
  }

  if (!grepl("^[0-9]{2}$", suffix)) {
    return(c(NA_integer_, NA_integer_))
  }

  c(
    as.integer(substr(suffix, 1, 1)),
    as.integer(substr(suffix, 2, 2))
  )
}

safe_lookup <- function(x, index, fallback) {
  if (is.na(index) || index < 1L || index > length(x)) return(fallback)
  x[[index]]
}

parameter_label <- function(parameter, dataset_name) {
  prefix <- parameter_prefix(parameter)
  idx <- parameter_indices(parameter, prefix)
  metadata <- DATASET_METADATA[[dataset_name]]

  response_i <- safe_lookup(
    metadata$responses,
    idx[1],
    ifelse(is.na(idx[1]), "", paste0("dimension ", idx[1]))
  )

  response_j <- safe_lookup(
    metadata$responses,
    idx[2],
    ifelse(is.na(idx[2]), "", paste0("dimension ", idx[2]))
  )

  predictor_j <- safe_lookup(
    metadata$predictors,
    idx[2],
    ifelse(is.na(idx[2]), "", paste0("predictor ", idx[2]))
  )

  if (prefix == "alpha") {
    return(paste0("alpha (", response_i, ")"))
  }

  if (prefix == "beta") {
    return(paste0("beta (", response_i, " <- ", predictor_j, ")"))
  }

  if (prefix %in% c("gamma", "nu", "kappa")) {
    return(paste0(prefix, " (", response_i, ")"))
  }

  if (prefix == "omega") {
    return("omega")
  }

  if (prefix == "sigma") {
    if (!is.na(idx[1]) && !is.na(idx[2]) && idx[1] == idx[2]) {
      return(paste0("sigma (residual variance: ", response_i, ")"))
    }

    return(
      paste0(
        "sigma (residual covariance: ", response_i, " / ", response_j, ")"
      )
    )
  }

  parameter
}

parameter_order <- function(parameter) {
  prefix <- parameter_prefix(parameter)
  base_order <- c(
    alpha = 100,
    beta  = 200,
    gamma = 300,
    nu    = 400,
    kappa = 500,
    omega = 600,
    sigma = 700
  )

  suffix <- sub(paste0("^", prefix, "_?"), "", parameter)
  suffix_num <- suppressWarnings(as.integer(suffix))
  if (is.na(suffix_num)) suffix_num <- 0L

  unname(base_order[[prefix]]) + suffix_num
}

identify_parameter_columns <- function(df) {
  candidates <- setdiff(names(df), STAT_COLS)
  candidates[
    grepl("^(alpha|beta|gamma|nu|kappa|omega|sigma)(_|$)", candidates)
  ]
}

summarize_parameter <- function(values,
                                dataset_name,
                                model_name,
                                parameter) {

  prefix <- parameter_prefix(parameter)
  bounds <- PARAMETER_BOUNDS[[prefix]]

  # Off-diagonal sigma terms are residual covariances. Unlike residual
  # variances, their feasible values are not described by the simple scalar
  # sigma bounds above, so boundary diagnostics are not computed for them.
  idx <- parameter_indices(parameter, prefix)
  is_off_diagonal_sigma <- (
    prefix == "sigma" &&
      !is.na(idx[1]) &&
      !is.na(idx[2]) &&
      idx[1] != idx[2]
  )
  if (is_off_diagonal_sigma) {
    bounds <- c(NA_real_, NA_real_)
  }

  finite <- is.finite(values)
  x <- values[finite]

  if (length(x) == 0L) {
    return(data.frame(
      dataset = dataset_name,
      model = model_name,
      parameter_group = unname(PARAMETER_GROUPS[[prefix]]),
      parameter_family = prefix,
      parameter = parameter,
      parameter_label = parameter_label(parameter, dataset_name),
      n_total = length(values),
      n_valid = 0L,
      n_missing = length(values),
      mean = NA_real_,
      sd = NA_real_,
      median = NA_real_,
      min = NA_real_,
      q2.5 = NA_real_,
      q25 = NA_real_,
      q75 = NA_real_,
      q97.5 = NA_real_,
      max = NA_real_,
      lower_bound = bounds[1],
      upper_bound = bounds[2],
      pct_near_lower_bound = NA_real_,
      pct_near_upper_bound = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  has_scalar_bounds <- all(is.finite(bounds))
  tolerance <- if (has_scalar_bounds) {
    (bounds[2] - bounds[1]) * BOUNDARY_TOLERANCE_PROPORTION
  } else {
    NA_real_
  }

  data.frame(
    dataset = dataset_name,
    model = model_name,
    parameter_group = unname(PARAMETER_GROUPS[[prefix]]),
    parameter_family = prefix,
    parameter = parameter,
    parameter_label = parameter_label(parameter, dataset_name),
    n_total = length(values),
    n_valid = length(x),
    n_missing = sum(!finite),
    mean = mean(x),
    sd = stats::sd(x),
    median = stats::median(x),
    min = min(x),
    q2.5 = stats::quantile(x, 0.025, names = FALSE),
    q25 = stats::quantile(x, 0.25, names = FALSE),
    q75 = stats::quantile(x, 0.75, names = FALSE),
    q97.5 = stats::quantile(x, 0.975, names = FALSE),
    max = max(x),
    lower_bound = bounds[1],
    upper_bound = bounds[2],
    pct_near_lower_bound = if (has_scalar_bounds) {
      100 * mean(x <= bounds[1] + tolerance)
    } else {
      NA_real_
    },
    pct_near_upper_bound = if (has_scalar_bounds) {
      100 * mean(x >= bounds[2] - tolerance)
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
}

summarize_model_parameters <- function(df, model_name, dataset_name) {
  parameter_cols <- identify_parameter_columns(df)

  if (length(parameter_cols) == 0L) {
    warning("No parameter columns found for ", dataset_name, " / ", model_name)
    return(NULL)
  }

  do.call(
    rbind,
    lapply(
      parameter_cols,
      function(parameter) {
        summarize_parameter(
          df[[parameter]],
          dataset_name = dataset_name,
          model_name = model_name,
          parameter = parameter
        )
      }
    )
  )
}

format_supplement_cell <- function(mean, sd, q2.5, q97.5) {
  if (!all(is.finite(c(mean, sd, q2.5, q97.5)))) return(NA_character_)
  sprintf("%.3f (%.3f) [%.3f, %.3f]", mean, sd, q2.5, q97.5)
}


################################################################################
# LOAD ESTIMATION RESULTS AND SUMMARIZE ALL PARAMETERS
################################################################################

estimation_data <- load_estimation_data(input_dir, MODEL_TYPES)

all_summaries <- list()
summary_index <- 1L

for (dataset_name in ESTIMATION_DATASETS) {

  dataset_models <- estimation_data[[dataset_name]]
  if (is.null(dataset_models)) next

  for (model_name in MODEL_TYPES) {

    df <- dataset_models[[model_name]]
    if (is.null(df)) next

    summary_df <- summarize_model_parameters(df, model_name, dataset_name)
    if (is.null(summary_df)) next

    all_summaries[[summary_index]] <- summary_df
    summary_index <- summary_index + 1L
  }
}

if (length(all_summaries) == 0L) {
  stop("No parameter summaries could be produced from: ", input_dir)
}

detailed_summary <- do.call(rbind, all_summaries)
rownames(detailed_summary) <- NULL

# Stable manuscript-friendly ordering.
detailed_summary$dataset_order <- match(
  detailed_summary$dataset,
  ESTIMATION_DATASETS
)
detailed_summary$model_order <- match(
  detailed_summary$model,
  c("exponential", "quasi_hyperbolic", "double_exponential")
)
detailed_summary$parameter_order <- vapply(
  detailed_summary$parameter,
  parameter_order,
  numeric(1)
)

detailed_summary <- detailed_summary[
  order(
    detailed_summary$dataset_order,
    detailed_summary$model_order,
    detailed_summary$parameter_order
  ),
]

detailed_summary$dataset_order <- NULL
detailed_summary$model_order <- NULL
detailed_summary$parameter_order <- NULL


################################################################################
# SAVE DETAILED OUTPUT
################################################################################

detailed_path <- file.path(output_dir, "parameter_summary_detailed.csv")
write.csv(detailed_summary, detailed_path, row.names = FALSE)
cat("Saved detailed parameter summary to:\n  ", detailed_path, "\n\n", sep = "")

# Also keep one detailed file per dataset for easier inspection.
for (dataset_name in ESTIMATION_DATASETS) {
  df <- detailed_summary[detailed_summary$dataset == dataset_name, , drop = FALSE]
  if (nrow(df) == 0L) next

  path <- file.path(output_dir, paste0(dataset_name, "_parameter_summary.csv"))
  write.csv(df, path, row.names = FALSE)
}


################################################################################
# CREATE COMPACT SUPPLEMENT-TABLE SOURCE
#
# Instead of placing model on separate rows, put the three models side by side.
# This reduces 147 model-specific rows to one row per unique parameter within a
# dataset while preserving each individual alpha/beta/etc. coefficient.
################################################################################

# Keep a dedicated detailed file for the non-temporal parameters that belong in
# this supplementary section.
other_parameter_summary <- detailed_summary[
  detailed_summary$parameter_family %in% SUPPLEMENT_PARAMETER_FAMILIES,
  ,
  drop = FALSE
]

other_detailed_path <- file.path(
  output_dir,
  "other_parameter_summary_detailed.csv"
)
write.csv(other_parameter_summary, other_detailed_path, row.names = FALSE)
cat(
  "Saved non-temporal detailed summary to:\n  ",
  other_detailed_path,
  "\n\n",
  sep = ""
)

supplement_long <- other_parameter_summary
supplement_long$summary <- mapply(
  format_supplement_cell,
  supplement_long$mean,
  supplement_long$sd,
  supplement_long$q2.5,
  supplement_long$q97.5,
  USE.NAMES = FALSE
)

# Create one row per dataset x parameter. Parameter labels are identical across
# models within a dataset, so take the first one as the row label.
key_df <- unique(
  supplement_long[, c(
    "dataset",
    "parameter_group",
    "parameter_family",
    "parameter",
    "parameter_label"
  )]
)

key_df$dataset_order <- match(key_df$dataset, ESTIMATION_DATASETS)
key_df$parameter_order <- vapply(key_df$parameter, parameter_order, numeric(1))
key_df <- key_df[order(key_df$dataset_order, key_df$parameter_order), ]
key_df$dataset_order <- NULL
key_df$parameter_order <- NULL

for (model_name in c("exponential", "quasi_hyperbolic", "double_exponential")) {
  model_values <- supplement_long[
    supplement_long$model == model_name,
    c("dataset", "parameter", "summary"),
    drop = FALSE
  ]
  names(model_values)[3] <- model_name

  key_df <- merge(
    key_df,
    model_values,
    by = c("dataset", "parameter"),
    all.x = TRUE,
    sort = FALSE
  )
}

# merge() does not preserve our preferred row ordering, so restore it.
key_df$dataset_order <- match(key_df$dataset, ESTIMATION_DATASETS)
key_df$parameter_order <- vapply(key_df$parameter, parameter_order, numeric(1))
key_df <- key_df[order(key_df$dataset_order, key_df$parameter_order), ]
key_df$dataset_order <- NULL
key_df$parameter_order <- NULL
rownames(key_df) <- NULL

supplement_path <- file.path(output_dir, "other_parameter_summary_supplement.csv")
write.csv(key_df, supplement_path, row.names = FALSE, na = "")
cat("Saved compact supplement-table source to:\n  ", supplement_path, "\n\n", sep = "")


################################################################################
# BOUNDARY DIAGNOSTIC
################################################################################

boundary_candidates <- other_parameter_summary[
  is.finite(other_parameter_summary$lower_bound) &
    is.finite(other_parameter_summary$upper_bound),
  ,
  drop = FALSE
]

boundary_summary <- boundary_candidates[
  boundary_candidates$pct_near_lower_bound >= BOUNDARY_REPORT_THRESHOLD_PCT |
    boundary_candidates$pct_near_upper_bound >= BOUNDARY_REPORT_THRESHOLD_PCT,
  ,
  drop = FALSE
]

boundary_path <- file.path(
  output_dir,
  "other_parameter_summary_boundaries.csv"
)
write.csv(boundary_summary, boundary_path, row.names = FALSE)
cat(
  "Saved boundary diagnostic (>=",
  BOUNDARY_REPORT_THRESHOLD_PCT,
  "% near either bound) to:\n  ",
  boundary_path,
  "\n\n",
  sep = ""
)


################################################################################
# CONSOLE REPORT
################################################################################

cat(strrep("=", 79), "\n", sep = "")
cat("PARAMETER SUMMARY COMPLETE\n")
cat("Supplement outputs: alpha, beta, sigma only (temporal parameters excluded)\n")
cat(strrep("=", 79), "\n", sep = "")
cat("Total parameter summaries: ", nrow(detailed_summary), "\n", sep = "")
cat("Datasets:                  ", length(unique(detailed_summary$dataset)), "\n", sep = "")
cat("Models:                    ", length(unique(detailed_summary$model)), "\n", sep = "")
cat(
  "Boundary flags:            ",
  nrow(boundary_summary),
  " parameter/model combinations\n\n",
  sep = ""
)

for (dataset_name in ESTIMATION_DATASETS) {
  df <- key_df[key_df$dataset == dataset_name, , drop = FALSE]
  if (nrow(df) == 0L) next

  cat("--- ", dataset_name, " ---\n", sep = "")
  print(
    df[, c(
      "parameter_label",
      "exponential",
      "quasi_hyperbolic",
      "double_exponential"
    )],
    row.names = FALSE
  )
  cat("\n")
}

if (nrow(boundary_summary) > 0L) {
  cat("Parameters with notable concentration near estimation bounds:\n")
  print(
    boundary_summary[, c(
      "dataset",
      "model",
      "parameter",
      "pct_near_lower_bound",
      "pct_near_upper_bound"
    )],
    row.names = FALSE
  )
  cat("\n")
} else {
  cat("No parameter/model combination had >=",
      BOUNDARY_REPORT_THRESHOLD_PCT,
      "% of estimates near either bound.\n\n", sep = "")
}

cat("All outputs saved inside:\n  ", output_dir, "\n", sep = "")

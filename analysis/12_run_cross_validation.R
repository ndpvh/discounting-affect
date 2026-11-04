################################################################################
# PURPOSE:
#
# Run blocked k-fold cross-validation for the exponential, quasi-hyperbolic,
# and double-exponential discounting models on participant-level data.
#
# Model fitting excludes each held-out fold of affect ratings. Predictions
# are evaluated using RMSE, with participant-level results saved in
# analysis/results/cross_validation/. The optimizer and settings match those
# used in 02_estimate_models.R.
#
# This is a computationally expensive step. Run 13_summarize_cross_validation.R
# afterward to compare the models without repeating estimation.
################################################################################


config_file <- if (file.exists(file.path("analysis", "_config.R"))) {
  file.path("analysis", "_config.R")
} else if (file.exists("_config.R")) {
  "_config.R"
} else {
  stop("Could not find analysis/_config.R. Run from the repository root.")
}
source(config_file)
source(file.path(PATHS$analysis, "_helpers.R"))
rm(config_file)

devtools::load_all(PROJECT_ROOT)

# Same global-search and local-refinement optimizer as 02_estimate_models.R.
optimizer <- function(obj, lower, upper,
                      algorithm = "NLOPT_LN_BOBYQA", maxeval = 1e3,
                      ftol_abs = 1e-15, xtol_abs = 1e-15,
                      print_level = 0, ...) {
  result_de <- DEoptim::DEoptim(
    obj,
    lower,
    upper,
    control = DEoptim::DEoptim.control(...)
  )

  result_nl <- nloptr::nloptr(
    result_de$optim$bestmem,
    obj,
    lb = lower,
    ub = upper,
    opts = list(
      algorithm = algorithm,
      maxeval = maxeval,
      ftol_abs = ftol_abs,
      xtol_abs = xtol_abs,
      print_level = print_level
    )
  )

  list(
    parameters = result_nl$solution,
    objective = result_nl$objective
  )
}

optim_settings <- list(
  dynamics = "isotropic",
  covariance = "symmetric",
  itermax = 1e3,
  NP = 150,
  CR = 0.75,
  strategy = 6,
  p = 0.8,
  reltol = 1e-15,
  steptol = 100,
  trace = FALSE,
  maxeval = 1e5,
  xtol_abs = 1e-20,
  ftol_abs = 1e-20
)

models <- list(
  exponential = exponential,
  quasi_hyperbolic = quasi_hyperbolic,
  double_exponential = double_exponential
)

# Niemeijer folds contain whole days; the 2021 fold count is provisional.
fold_counts <- c(NIEMEIJER_2022 = 7L, VANHASBROECK_2021 = 3L)

datasets <- commandArgs(trailingOnly = TRUE)
if (length(datasets) == 0L) {
  datasets <- RAW_DATASETS
}

unknown <- setdiff(datasets, RAW_DATASETS)
if (length(unknown) > 0L) {
  stop("Unknown dataset(s): ", paste(unknown, collapse = ", "))
}

output_dir <- file.path(PATHS$results, "cross_validation")

for (dataset_name in datasets) {
  message("\nCross-validation: ", dataset_name)

  k <- if (dataset_name %in% names(fold_counts)) {
    fold_counts[[dataset_name]]
  } else {
    5L
  }

  do.call(
    cv_dataset,
    c(
      list(
        folder = file.path(PATHS$processed_data, dataset_name),
        base_name = dataset_name,
        models = models,
        optimizer = optimizer,
        output_dir = output_dir,
        k = k,
        folds_function = get_folds_function(dataset_name, k)
      ),
      optim_settings
    )
  )
}

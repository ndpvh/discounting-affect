################################################################################
# PURPOSE:
#
# Summarize the participant-level results saved by 12_run_cross_validation.R.
# For each dataset and affect variable, report model RMSE and win percentages,
# and compare model pairs with participant-bootstrap confidence intervals.
#
# Save one overview and one pairwise-comparison CSV per dataset in
# analysis/results/cross_validation/summary/.
#
# This script uses existing results and does not refit any models.
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

datasets <- commandArgs(trailingOnly = TRUE)
if (length(datasets) == 0L) {
  datasets <- ESTIMATION_DATASETS
}

unknown <- setdiff(datasets, ESTIMATION_DATASETS)
if (length(unknown) > 0L) {
  stop("Unknown dataset(s): ", paste(unknown, collapse = ", "))
}

input_dir <- file.path(PATHS$results, "cross_validation")
output_dir <- file.path(input_dir, "summary")

require_paths(input_dir, path_type = "dir", context = "cross-validation")
ensure_dir(output_dir)

for (dataset_name in datasets) {
  message("\nSummarizing cross-validation: ", dataset_name)

  summaries <- summarize_cv_dataset(
    output_dir = input_dir,
    base_name = dataset_name,
    model_names = MODEL_TYPES
  )

  overview <- do.call(rbind, lapply(names(summaries), function(outcome) {
    data.frame(
      dataset = dataset_name,
      outcome = sub("^rmse_", "", outcome),
      summaries[[outcome]]$overview
    )
  }))

  pairwise <- do.call(rbind, lapply(names(summaries), function(outcome) {
    data.frame(
      dataset = dataset_name,
      outcome = sub("^rmse_", "", outcome),
      summaries[[outcome]]$pairwise
    )
  }))

  write.csv(
    overview,
    file.path(output_dir, paste0(dataset_name, "_overview.csv")),
    row.names = FALSE
  )

  write.csv(
    pairwise,
    file.path(output_dir, paste0(dataset_name, "_pairwise.csv")),
    row.names = FALSE
  )
}

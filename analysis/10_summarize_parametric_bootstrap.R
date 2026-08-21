################################################################################
# PURPOSE:
#
# Analyze the already-existing parametric-bootstrap results (the per-dataset /
# per-model CSV files produced by 04_run_parametric_bootstrap.R).
#
# This script is the CHEAP counterpart of the bootstrap workflow: it only reads
# saved bootstrap outputs and performs the summary/coverage analysis on them.
# It does NOT simulate new datasets, fit models, or rerun participant-level
# estimation, so a reviewer can (re)analyze existing results without triggering
# the expensive bootstrap generation.
#
# It depends on the raw bootstrap CSVs (and thus on 04 having been run at
# least once) and writes its summary back into the same results directory:
#
#   input : analysis/results/parametric_bootstrap/<dataset>_<model>.csv
#   output: analysis/results/parametric_bootstrap/bootstrap_summary.Rds
#
################################################################################

# Source shared infrastructure
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
# SUMMARY
################################################################################

# Define all datasets and models
datasets <- c(
    "VANHASBROECK_2021",
    "VANHASBROECK_2022",
    "VANHASBROECK_2024_1",
    "VANHASBROECK_2024_2",
    "NIEMEIJER_2022"
)
models <- c(
    "exponential",
    "quasi_hyperbolic",
    "double_exponential"
)

# Loop over the models and create a list of data.frame's summarizing how often
# the model was able to cover the statistics. Furthermore include between-subject 
# aggregated statistics for the phenomena-of-interest
result <- lapply(
    models, 
    function(x) {
        # Loop over the datasets
        result <- lapply(
            datasets, 
            function(y) {
                # Load the results of the parametric bootstrap
                result <- read.csv(
                    file.path(
                        PATHS$parametric_bootstrap,
                        paste0(y, "_", x, ".csv")
                    )
                )

                # Summarize the data in a meaningful way, extracting the 
                # coverage percentage and some between-subject aggregated values
                # for the statistics
                result <- result |>
                    dplyr::group_by(phenomenon, variables) |>
                    dplyr::summarize(
                        dataset = dataset[1],
                        true_sd = sd(true, na.rm = TRUE),
                        true_min = min(true, na.rm = TRUE),
                        true_q025 = quantile(true, prob = 0.025, na.rm = TRUE),
                        true_q25 = quantile(true, prob = 0.25, na.rm = TRUE),
                        true_q50 = quantile(true, prob = 0.50, na.rm = TRUE),
                        true_q75 = quantile(true, prob = 0.75, na.rm = TRUE),
                        true_q975 = quantile(true, prob = 0.975, na.rm = TRUE),
                        true_max = max(true, na.rm = TRUE),
                        dplyr::across(
                            mean:covered,
                            mean,
                            na.rm = TRUE
                        )
                    ) |>
                    dplyr::ungroup() |>
                    dplyr::rename(
                        true_mean = true
                    ) |>
                    dplyr::relocate(
                        true_mean, 
                        .after = dataset
                    )

                # Return
                return(result)
            }
        )

        # Bind together, order in a meaningful way and return
        result <- do.call("rbind", result) |>
            dplyr::arrange(phenomenon, dataset)

        return(result)
    }
) |>
    `names<-` (models)

# Save these results
saveRDS(
    result, 
    file.path(
        PATHS$parametric_bootstrap,
        "bootstrap_summary.Rds"
    )
)

# Examining the coverage itself in a bit more detail across datasets
coverage <- lapply(
    result,
    function(x) x |>
        dplyr::group_by(phenomenon) |>
        dplyr::summarize(
            mean = mean(covered),
            min = min(covered),
            max = max(covered)
        )
)

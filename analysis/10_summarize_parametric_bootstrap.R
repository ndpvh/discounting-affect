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


################################################################################
# EXTENDED ABSOLUTE GOODNESS-OF-FIT REPORTING
#
# Reuse the existing participant-level bootstrap CSVs. The original
# bootstrap_summary.Rds object above is retained unchanged for compatibility.
# Here each CSV row describes one participant / statistic / outcome pair.
# No model is refitted and no bootstrap is rerun.
################################################################################

summary_dir <- file.path(PATHS$parametric_bootstrap, "absolute_gof_summary")
ensure_dir(summary_dir)

# Return NA rather than NaN when no finite observations are available.
safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else mean(x)
}

safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else median(x)
}

# Use category-level summaries for readability, but also retain each lag,
# variable and outcome in the detailed output for interpretation.
phenomenon_family <- function(x) {
  sub("_[0-9]+$", "", x)
}

summarize_group <- function(df) {
  # We need finite empirical and simulated values and a proper interval to
  # assess coverage. Failed/undefined statistics are counted, not discarded
  # silently from the denominator.
  valid <- is.finite(df$true) & is.finite(df$mean) &
    is.finite(df$q025) & is.finite(df$q975) &
    df$q025 <= df$q975
  observed <- df$true[valid]
  predicted <- df$mean[valid]
  lower <- df$q025[valid]
  upper <- df$q975[valid]

  n_total <- nrow(df)
  n_valid <- sum(valid)
  n_covered <- sum(observed >= lower & observed <= upper)
  n_below <- sum(observed < lower)
  n_above <- sum(observed > upper)
  residual <- observed - predicted

  data.frame(
    n_total = n_total,
    n_evaluable = n_valid,
    n_missing = n_total - n_valid,
    n_covered = n_covered,
    n_below = n_below,
    n_above = n_above,
    coverage_pct = if (n_valid > 0L) 100 * n_covered / n_valid else NA_real_,
    below_pct = if (n_valid > 0L) 100 * n_below / n_valid else NA_real_,
    above_pct = if (n_valid > 0L) 100 * n_above / n_valid else NA_real_,
    empirical_mean = safe_mean(df$true),
    simulated_mean = safe_mean(df$mean),
    mean_empirical_minus_simulated = safe_mean(residual),
    median_absolute_discrepancy = safe_median(abs(residual)),
    stringsAsFactors = FALSE
  )
}

# Each row of the detailed output is an individual phenomenon × variable
# within a dataset and model. Metrics such as absolute discrepancies should
# not be averaged across variables with different measurement scales.
detailed <- list()
for (dataset_name in datasets) {
  for (model_name in models) {
    input <- file.path(
      PATHS$parametric_bootstrap,
      paste0(dataset_name, "_", model_name, ".csv")
    )
    if (!file.exists(input)) stop("Missing bootstrap CSV: ", input)
    df <- read.csv(input)
    required <- c("phenomenon", "variables", "true", "mean", "q025", "q975")
    missing_cols <- setdiff(required, names(df))
    if (length(missing_cols) > 0L) {
      stop("Missing columns in ", input, ": ", paste(missing_cols, collapse = ", "))
    }

    groups <- split(df, interaction(df$phenomenon, df$variables, drop = TRUE))
    for (group in groups) {
      detailed[[length(detailed) + 1L]] <- cbind(
        data.frame(
          dataset = dataset_name,
          model = model_name,
          family = phenomenon_family(group$phenomenon[1]),
          phenomenon = group$phenomenon[1],
          variable = group$variables[1],
          stringsAsFactors = FALSE
        ),
        summarize_group(group)
      )
    }
  }
}

detailed <- do.call(rbind, detailed)
rownames(detailed) <- NULL
write.csv(
  detailed,
  file.path(summary_dir, "01_absolute_gof_detailed.csv"),
  row.names = FALSE
)

# Group within dataset × model × family. These are pooled percentages across
# participant/statistic pairs (not averages of lag percentages).
family_groups <- split(
  detailed,
  interaction(detailed$dataset, detailed$model, detailed$family, drop = TRUE)
)
family_summary <- do.call(rbind, lapply(family_groups, function(x) {
  n <- sum(x$n_evaluable)
  data.frame(
    dataset = x$dataset[1],
    model = x$model[1],
    family = x$family[1],
    n_statistics = nrow(x),
    n_total = sum(x$n_total),
    n_evaluable = n,
    n_missing = sum(x$n_missing),
    coverage_pct = if (n > 0L) 100 * sum(x$n_covered) / n else NA_real_,
    below_pct = if (n > 0L) 100 * sum(x$n_below) / n else NA_real_,
    above_pct = if (n > 0L) 100 * sum(x$n_above) / n else NA_real_
  )
}))
rownames(family_summary) <- NULL
write.csv(
  family_summary,
  file.path(summary_dir, "02_absolute_gof_by_family.csv"),
  row.names = FALSE
)

# A readable table for the manuscript: each model as a column, each
# dataset × phenomenon family as a row. NA means no evaluable cases.
coverage_wide <- reshape(
  family_summary[, c("dataset", "family", "model", "coverage_pct")],
  idvar = c("dataset", "family"),
  timevar = "model",
  direction = "wide"
)
write.csv(
  coverage_wide,
  file.path(summary_dir, "03_coverage_comparison.csv"),
  row.names = FALSE
)

cat("\nAbsolute goodness-of-fit summaries saved under:\n", summary_dir, "\n")
print(coverage_wide, row.names = FALSE)
cat("\nCoverage is the proportion of evaluable empirical statistics inside the\n")
cat("model's participant-specific simulated 95% interval, not CV accuracy.\n")
cat("Examine detailed results before inferring that any lag or process is misfit.\n")

################################################################################
# PUBLICATION-ORIENTED SUMMARIES AND FIGURES
#
# This is descriptive bootstrap coverage, not nominally calibrated CI coverage.
# All percentages use evaluable participant–statistic pairs as denominators.
# Moments are shown separately, and missing/undefined statistics are counted.
################################################################################

figure_dir <- file.path(PATHS$figures, "absolute_gof")
ensure_dir(figure_dir)

model_order <- c("exponential", "quasi_hyperbolic", "double_exponential")
model_labels <- c(
  exponential = "Exponential",
  quasi_hyperbolic = "Quasi-hyperbolic",
  double_exponential = "Double-exponential"
)
category_order <- c(
  "moment_1", "moment_2", "outcome_correlation", "moment_4",
  "autocorrelation", "residual_autocorrelation", "moment_3",
  "bimodality_coefficient"
)
category_labels <- c(
  moment_1 = "Mean",
  moment_2 = "Variance",
  outcome_correlation = "Predictor–affect correlation",
  moment_4 = "Fourth central moment",
  autocorrelation = "Affect autocorrelation",
  residual_autocorrelation = "Residual autocorrelation",
  moment_3 = "Third central moment",
  bimodality_coefficient = "Bimodality coefficient"
)
dataset_order <- datasets
dataset_labels <- c(
  VANHASBROECK_2021 = "Vanhasbroeck 2021",
  VANHASBROECK_2022 = "Vanhasbroeck 2022",
  VANHASBROECK_2024_1 = "Vanhasbroeck 2024: valence",
  VANHASBROECK_2024_2 = "Vanhasbroeck 2024: PA/NA",
  NIEMEIJER_2022 = "Niemeijer 2022"
)

# An individual descriptive statistic, not each dataset, is the pooling unit.
# Preserve the underlying integer counts and use them for all aggregations.
publication <- detailed
publication$category <- ifelse(
  publication$family == "moment", publication$phenomenon, publication$family
)

sum_count_cols <- c(
  "n_total", "n_evaluable", "n_missing", "n_covered", "n_below", "n_above"
)
summarize_counts <- function(df, by) {
  x <- stats::aggregate(df[sum_count_cols], df[by], sum)
  x$coverage_pct <- ifelse(
    x$n_evaluable > 0, 100 * x$n_covered / x$n_evaluable, NA_real_
  )
  x$below_pct <- ifelse(
    x$n_evaluable > 0, 100 * x$n_below / x$n_evaluable, NA_real_
  )
  x$above_pct <- ifelse(
    x$n_evaluable > 0, 100 * x$n_above / x$n_evaluable, NA_real_
  )
  x
}

publication_coverage <- summarize_counts(publication, c("model", "category"))
publication_direction <- publication_coverage
publication_lags <- publication[
  publication$family %in% c(
    "autocorrelation", "residual_autocorrelation", "outcome_correlation"
  ),
]
publication_lags$lag <- as.integer(sub("^.*_([0-9]+)$", "\\1", publication_lags$phenomenon))
publication_lags <- summarize_counts(
  publication_lags, c("family", "model", "lag")
)
publication_datasets <- summarize_counts(
  publication, c("dataset", "model", "family")
)

write.csv(publication_coverage,
  file.path(summary_dir, "04_publication_coverage.csv"), row.names = FALSE)
write.csv(publication_direction,
  file.path(summary_dir, "05_publication_direction.csv"), row.names = FALSE)
write.csv(publication_lags,
  file.path(summary_dir, "06_publication_lags.csv"), row.names = FALSE)
write.csv(publication_datasets,
  file.path(summary_dir, "07_publication_datasets.csv"), row.names = FALSE)

# Reusable labels and a restrained style for figures intended for manuscripts.
plot_theme <- ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title.position = "plot"
  )

# Figure 1: overall phenomenon coverage, keeping moments separate.
plot_overall <- publication_coverage
plot_overall$category <- factor(
  plot_overall$category, levels = rev(category_order),
  labels = unname(category_labels[rev(category_order)])
)
plot_overall$model <- factor(
  plot_overall$model, levels = model_order, labels = unname(model_labels[model_order])
)
p1 <- ggplot2::ggplot(
  plot_overall,
  ggplot2::aes(x = coverage_pct, y = category, shape = model, group = model)
) +
  ggplot2::geom_point(
    size = 2.7, position = ggplot2::position_dodge(width = 0.55)
  ) +
  ggplot2::scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  ggplot2::labs(
    title = "Absolute goodness-of-fit across empirical phenomena",
    x = "Within simulated 95% interval (%)", y = NULL, shape = "Model"
  ) + plot_theme

# Figure 2: direction of mismatches, one facet per model.
direction_long <- rbind(
  data.frame(publication_direction[c("model", "category")],
             location = "Below interval", percent = publication_direction$below_pct),
  data.frame(publication_direction[c("model", "category")],
             location = "Inside interval", percent = publication_direction$coverage_pct),
  data.frame(publication_direction[c("model", "category")],
             location = "Above interval", percent = publication_direction$above_pct)
)
direction_long$category <- factor(
  direction_long$category, levels = rev(category_order),
  labels = unname(category_labels[rev(category_order)])
)
direction_long$model <- factor(
  direction_long$model, levels = model_order, labels = unname(model_labels[model_order])
)
direction_long$location <- factor(
  direction_long$location,
  levels = c("Below interval", "Inside interval", "Above interval")
)
p2 <- ggplot2::ggplot(
  direction_long,
  ggplot2::aes(x = percent, y = category, fill = location)
) +
  ggplot2::geom_col(width = 0.8) +
  ggplot2::facet_wrap(~ model, ncol = 1) +
  ggplot2::scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  ggplot2::labs(
    title = "Where observed statistics fall relative to simulations",
    x = "Evaluable participant–statistic pairs (%)", y = NULL, fill = NULL
  ) + plot_theme

# Figure 3: dataset × phenomenon coverage, with all models shown.
plot_datasets <- publication_datasets
plot_datasets$dataset <- factor(
  plot_datasets$dataset, levels = rev(dataset_order),
  labels = unname(dataset_labels[rev(dataset_order)])
)
family_order <- c(
  "outcome_correlation", "autocorrelation", "residual_autocorrelation",
  "moment", "bimodality_coefficient"
)
family_labels <- c(
  "Predictor–affect", "Affect AC", "Residual AC", "Moments", "Bimodality"
)
plot_datasets$family <- factor(
  plot_datasets$family, levels = family_order, labels = family_labels
)
plot_datasets$model <- factor(
  plot_datasets$model, levels = model_order, labels = unname(model_labels[model_order])
)
p3 <- ggplot2::ggplot(
  plot_datasets,
  ggplot2::aes(x = family, y = dataset, fill = coverage_pct)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.6) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", coverage_pct)), size = 2.8) +
  ggplot2::facet_wrap(~ model, ncol = 1) +
  ggplot2::scale_fill_gradient(low = "#eceff3", high = "#5a83a8", limits = c(0, 100)) +
  ggplot2::labs(
    title = "Goodness-of-fit differs across datasets",
    x = NULL, y = NULL, fill = "Coverage (%)"
  ) + plot_theme +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))

# Figure 4: pooling across participants/outcomes but not mixing lag families.
plot_lags <- publication_lags
plot_lags$family <- factor(
  plot_lags$family,
  levels = c("autocorrelation", "residual_autocorrelation", "outcome_correlation"),
  labels = c("Affect autocorrelation", "Residual autocorrelation", "Predictor–affect")
)
plot_lags$model <- factor(
  plot_lags$model, levels = model_order, labels = unname(model_labels[model_order])
)
p4 <- ggplot2::ggplot(
  plot_lags,
  ggplot2::aes(x = lag, y = coverage_pct, shape = model, linetype = model)
) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ family, nrow = 1) +
  ggplot2::scale_x_continuous(breaks = 0:5) +
  ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  ggplot2::labs(
    title = "Coverage of temporal statistics across lags",
    x = "Lag (observations)", y = "Coverage (%)", shape = "Model", linetype = "Model"
  ) + plot_theme

figures <- list(
  "01_overall_coverage.jpeg" = list(plot = p1, width = 10, height = 6),
  "02_direction_of_mismatch.jpeg" = list(plot = p2, width = 10, height = 10),
  "03_dataset_heatmap.jpeg" = list(plot = p3, width = 11, height = 11),
  "04_lag_coverage.jpeg" = list(plot = p4, width = 13, height = 5)
)
for (name in names(figures)) {
  fig <- figures[[name]]
  ggplot2::ggsave(
    filename = file.path(figure_dir, name), plot = fig$plot,
    width = fig$width, height = fig$height, units = "in", dpi = 300
  )
}

notes <- c(
  "Absolute goodness-of-fit: reporting notes",
  "=========================================",
  "All figures and publication tables are derived from the existing bootstrap CSVs.",
  "No simulation or model estimation is performed by this summary script.",
  "Coverage = observed participant statistic within its simulated 2.5%–97.5% interval.",
  "Below/above refer to the OBSERVED statistic relative to the simulated interval.",
  "Denominators count evaluable participant–statistic combinations, not independent people.",
  "Models are fitted to the same empirical data used in the checks; nominal 95%",
  "intervals are descriptive model checks, not automatically calibrated tests.",
  "All categories pool across datasets/outcomes; more numerous datasets and",
  "statistic combinations contribute more. Individual moments are kept separate.",
  "Use 01_absolute_gof_detailed.csv to examine individual outcomes and lags.",
  "A low bimodality coefficient fit does not, alone, demonstrate two distinct modes.",
  "No uncertainty intervals for the aggregate coverage percentages are estimated."
)
writeLines(notes, file.path(summary_dir, "08_reporting_notes.txt"))
cat("\nPublication figures written to: ", figure_dir, "\n", sep = "")

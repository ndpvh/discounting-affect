################################################################################
# PURPOSE:
#
# Conduct non-parametric bootstrap analyses of pairwise differences in the
# proportion of participants best described by the three discounting models:
#   - exponential
#   - quasi-hyperbolic
#   - double-exponential
#
# The analysis is performed separately for:
#   - each dataset
#   - AIC and BIC
#   - each of the three possible model pairs
#
# For each pairwise comparison, the script:
#   1. Identifies which model has the lower AIC/BIC for each participant.
#   2. Calculates the observed difference in winning proportions.
#   3. Non-parametrically bootstraps participants 10,000 times.
#   4. Calculates a 95% percentile bootstrap confidence interval.
#   5. Calculates a two-sided bootstrap p-value for H0: difference = 0.
#   6. Applies Holm correction across the three model comparisons within each
#      dataset × information-criterion combination.
#   7. Saves one summary CSV containing all results.
#
# IMPORTANT:
# This script works directly from the estimation CSV files. Please remember to
# Pull those files or rerun the analysis before this
################################################################################


# SETTINGS

# Models included in the comparison
models <- c(
  "exponential",
  "quasi_hyperbolic",
  "double_exponential"
)

# Datasets included in the comparison
datasets <- c(
  "VANHASBROECK_2021",
  "VANHASBROECK_2022",
  "VANHASBROECK_2024_1",
  "VANHASBROECK_2024_2",
  "NIEMEIJER_2022"
)

# Information criteria
metrics <- c("aic", "bic")

# All pairwise model comparisons
model_pairs <- list(
  c("exponential",      "quasi_hyperbolic"),
  c("exponential",      "double_exponential"),
  c("quasi_hyperbolic", "double_exponential")
)

# Number of bootstrap resamples
n_boot <- 10000

# Confidence level
conf_level <- 0.95

# Seed for reproducibility
set.seed(1234)

# Input/output locations
input_dir <- file.path("scripts", "results", "estimation")
output_dir <- file.path("scripts", "results", "non_parametric_bootstrap")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


# LOAD ESTIMATION RESULTS

# Load the three model-estimation files for one dataset and combine them into
# long format.
load_dataset_results <- function(dataset_name) {

  model_dfs <- lapply(
    models,
    function(model_name) {

      path <- file.path(
        input_dir,
        paste0(dataset_name, "_", model_name, ".csv")
      )

      if (!file.exists(path)) {
        stop("Required estimation file not found: ", path)
      }

      df <- read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      required_columns <- c("participant_id", metrics)
      missing_columns <- setdiff(required_columns, names(df))

      if (length(missing_columns) > 0) {
        stop(
          "File ", path, " is missing required column(s): ",
          paste(missing_columns, collapse = ", ")
        )
      }

      if (anyDuplicated(df$participant_id)) {
        stop(
          "Duplicate participant_id values found in: ", path,
          ". Each participant should occur once per model."
        )
      }

      df$model <- model_name

      df[, c("participant_id", "model", metrics)]
    }
  )

  do.call(rbind, model_dfs)
}


# Load all datasets
long <- lapply(datasets, load_dataset_results)
names(long) <- datasets


# PREPARE ONE PAIRWISE COMPARISON

# Convert one dataset to participant-level pairwise results for two models.
#
# Lower AIC/BIC indicates better fit.
#
# Participants with a missing criterion value for either model are excluded
# from that specific comparison.
#
# Exact equality is retained as a tie rather than assigning the participant
# arbitrarily to one model.
prepare_pairwise <- function(long_df, model_a, model_b, metric) {

  sub_df <- long_df[
    long_df$model %in% c(model_a, model_b),
    c("participant_id", "model", metric)
  ]

  wide_df <- reshape(
    sub_df,
    idvar     = "participant_id",
    timevar   = "model",
    direction = "wide"
  )

  # reshape() produces names such as "aic.exponential"
  names(wide_df) <- sub(
    paste0("^", metric, "\\."),
    "",
    names(wide_df)
  )

  if (!(model_a %in% names(wide_df)) || !(model_b %in% names(wide_df))) {
    stop(
      "Could not find both model columns after reshaping: ",
      model_a, " and ", model_b
    )
  }

  n_total <- nrow(wide_df)

  # Retain only participants with an observed AIC/BIC for both models
  complete <- !is.na(wide_df[[model_a]]) & !is.na(wide_df[[model_b]])
  wide_df <- wide_df[complete, , drop = FALSE]

  n_complete <- nrow(wide_df)

  if (n_complete == 0) {
    stop(
      "No participants have complete ", metric,
      " values for ", model_a, " vs ", model_b, "."
    )
  }

  # Participant-level outcome
  wide_df$winner <- ifelse(
    wide_df[[model_a]] < wide_df[[model_b]],
    model_a,
    ifelse(
      wide_df[[model_b]] < wide_df[[model_a]],
      model_b,
      "tie"
    )
  )

  list(
    data               = wide_df,
    n_total            = n_total,
    n_complete         = n_complete,
    n_excluded_missing = n_total - n_complete
  )
}


# NON-PARAMETRIC BOOTSTRAP

# Bootstrap the difference in the proportion of participants won by model A
# versus model B:
#
#   difference = proportion(model A wins) - proportion(model B wins)
#
# Positive values therefore favor model A; negative values favor model B.
#
# The bootstrap unit is the PARTICIPANT, not the trial.
#
# Because the statistic depends only on whether each participant is classified
# as an A win, B win, or tie, sampling these three observed categories using
# rmultinom() is exactly equivalent to repeatedly sampling participant rows
# with replacement, but is much faster for 10,000 resamples.
#
# The confidence interval is a percentile bootstrap CI.
#
# For the two-sided bootstrap p-value, the bootstrap distribution is centered
# on zero by subtracting the observed difference. The p-value is the proportion
# of centered bootstrap statistics at least as extreme as the observed
# difference. A +1 correction prevents a p-value of exactly zero.
bootstrap_pairwise <- function(pairwise_object,
                               model_a,
                               model_b,
                               n_boot = 10000,
                               conf_level = 0.95) {

  df <- pairwise_object$data
  n  <- nrow(df)

  n_a   <- sum(df$winner == model_a)
  n_b   <- sum(df$winner == model_b)
  n_tie <- sum(df$winner == "tie")

  prop_a   <- n_a / n
  prop_b   <- n_b / n
  prop_tie <- n_tie / n

  observed_difference <- prop_a - prop_b

  # Non-parametric participant bootstrap.
  #
  # The empirical distribution has three possible participant outcomes:
  # A win, B win, or tie.
  boot_counts <- rmultinom(
    n        = n_boot,
    size     = n,
    prob     = c(prop_a, prop_b, prop_tie)
  )

  # Difference in proportions for every bootstrap resample
  boot_difference <- (
    boot_counts[1, ] - boot_counts[2, ]
  ) / n

  # Bootstrap standard error
  bootstrap_se <- sd(boot_difference)

  # Percentile bootstrap confidence interval
  alpha <- 1 - conf_level

  ci <- quantile(
    boot_difference,
    probs = c(alpha / 2, 1 - alpha / 2),
    names = FALSE,
    type = 6
  )

  # Two-sided centered-bootstrap p-value for H0: difference = 0
  centered_boot <- boot_difference - observed_difference

  p_boot <- (
    sum(abs(centered_boot) >= abs(observed_difference)) + 1
  ) / (n_boot + 1)

  # Which model is favored descriptively?
  favored_model <- if (
    observed_difference > 0
  ) {
    model_a
  } else if (
    observed_difference < 0
  ) {
    model_b
  } else {
    "tie"
  }

  data.frame(
    model_a             = model_a,
    model_b             = model_b,

    n_total             = pairwise_object$n_total,
    n_complete          = pairwise_object$n_complete,
    n_excluded_missing  = pairwise_object$n_excluded_missing,

    n_best_a            = n_a,
    n_best_b            = n_b,
    n_ties              = n_tie,

    proportion_best_a   = prop_a,
    proportion_best_b   = prop_b,
    proportion_ties     = prop_tie,

    percentage_best_a   = 100 * prop_a,
    percentage_best_b   = 100 * prop_b,
    percentage_ties     = 100 * prop_tie,

    difference_prop     = observed_difference,
    difference_pp       = 100 * observed_difference,

    bootstrap_se        = bootstrap_se,
    ci_lower_prop       = ci[1],
    ci_upper_prop       = ci[2],
    ci_lower_pp         = 100 * ci[1],
    ci_upper_pp         = 100 * ci[2],

    p_boot              = p_boot,
    favored_model       = favored_model,

    stringsAsFactors = FALSE
  )
}


# RUN ALL DATASETS × METRICS × MODEL PAIRS 

results_list <- list()
result_index <- 1

for (dataset_name in datasets) {

  message("\n========== ", dataset_name, " ==========")

  for (metric in metrics) {

    message("  Metric: ", toupper(metric))

    for (pair in model_pairs) {

      model_a <- pair[1]
      model_b <- pair[2]

      message("    ", model_a, " vs ", model_b)

      pairwise_object <- prepare_pairwise(
        long_df = long[[dataset_name]],
        model_a = model_a,
        model_b = model_b,
        metric  = metric
      )

      result <- bootstrap_pairwise(
        pairwise_object = pairwise_object,
        model_a         = model_a,
        model_b         = model_b,
        n_boot          = n_boot,
        conf_level      = conf_level
      )

      result$dataset <- dataset_name
      result$metric  <- metric

      results_list[[result_index]] <- result
      result_index <- result_index + 1
    }
  }
}

results <- do.call(rbind, results_list)
rownames(results) <- NULL


# Put identifying columns first
results <- results[
  ,
  c(
    "dataset",
    "metric",
    "model_a",
    "model_b",
    "favored_model",

    "n_total",
    "n_complete",
    "n_excluded_missing",
    "n_best_a",
    "n_best_b",
    "n_ties",

    "proportion_best_a",
    "proportion_best_b",
    "proportion_ties",
    "percentage_best_a",
    "percentage_best_b",
    "percentage_ties",

    "difference_prop",
    "difference_pp",
    "bootstrap_se",
    "ci_lower_prop",
    "ci_upper_prop",
    "ci_lower_pp",
    "ci_upper_pp",

    "p_boot"
  )
]


# MULTIPLE-COMPARISON CORRECTION

# There are three pairwise model comparisons within every dataset × metric.
# Treat these three tests as one family and apply Holm's correction separately
# within each family.
results$p_holm <- NA_real_

families <- interaction(
  results$dataset,
  results$metric,
  drop = TRUE
)

for (family in levels(families)) {

  idx <- which(families == family)

  results$p_holm[idx] <- p.adjust(
    results$p_boot[idx],
    method = "holm"
  )
}

results$significant_holm_05 <- results$p_holm < 0.05


# SAVE RESULTS

output_file <- file.path(
  output_dir,
  "pairwise_bootstrap_model_comparison.csv"
)

write.csv(
  results,
  output_file,
  row.names = FALSE
)


# TERMINAL SUMMARY

# The full results remain available in the saved CSV.
# For the console, print one small table for each dataset and metric.

print_summary <- function(results) {

  model_labels <- c(
    exponential        = "EXP",
    quasi_hyperbolic   = "QH",
    double_exponential = "DE"
  )

  format_p <- function(p) {
    if (is.na(p)) {
      return("NA")
    }

    if (p < .001) {
      return("<.001")
    }

    sub("^0", "", sprintf("%.3f", p))
  }

  for (dataset_name in datasets) {

    for (metric_name in metrics) {

      sub_results <- results[
        results$dataset == dataset_name &
          results$metric == metric_name,
        ,
        drop = FALSE
      ]

      # Skip only if there genuinely are no results for this combination.
      if (nrow(sub_results) == 0) {
        cat(
          "\n",
          dataset_name,
          " - ",
          toupper(metric_name),
          ": no results found\n",
          sep = ""
        )
        next
      }

      summary_table <- data.frame(
        Comparison = paste0(
          model_labels[sub_results$model_a],
          " vs ",
          model_labels[sub_results$model_b]
        ),

        `Wins (%)` = paste0(
          sprintf("%.1f", sub_results$percentage_best_a),
          " vs ",
          sprintf("%.1f", sub_results$percentage_best_b)
        ),

        `Diff. (pp)` = sprintf(
          "%+.1f",
          sub_results$difference_pp
        ),

        `95% CI` = paste0(
          "[",
          sprintf("%.1f", sub_results$ci_lower_pp),
          ", ",
          sprintf("%.1f", sub_results$ci_upper_pp),
          "]"
        ),

        `Holm p` = vapply(
          sub_results$p_holm,
          format_p,
          character(1)
        ),

        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      cat(
        "\n",
        dataset_name,
        " - ",
        toupper(metric_name),
        "\n",
        sep = ""
      )

      # capture.output() + writeLines() forces the formatted table to be written
      # to the interactive console as text and avoids wide data-frame splitting.
      table_text <- capture.output(
        print(
          summary_table,
          row.names = FALSE,
          right = FALSE
        )
      )

      writeLines(table_text)
    }
  }

  cat("\n")
  cat("Model abbreviations: EXP = exponential; QH = quasi-hyperbolic; DE = double-exponential.\n")
  cat("Diff. (pp): positive values favor the first model; negative values favor the second.\n")

  invisible(NULL)
}

print_summary(results)

flush.console()

cat(
  "\nBootstrap analysis complete.\n",
  "Results saved to: ",
  output_file,
  "\n",
  sep = ""
)

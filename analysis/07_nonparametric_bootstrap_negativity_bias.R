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


# SETTINGS

# Models included in the comparison (same set as MODEL_TYPES in _config.R;
# kept local here because this script's model order and naming are used in its
# own output columns).
models <- c(
  "exponential",
  "quasi_hyperbolic",
  "double_exponential"
)

# Datasets included in the comparison (same set as ESTIMATION_DATASETS in _config.R)
datasets <- ESTIMATION_DATASETS

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
input_dir <- PATHS$estimation
output_dir <- PATHS$non_parametric_bootstrap

ensure_dir(output_dir)


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



# ==============================================================================
# NEGATIVITY-BIAS ANALYSIS: PA vs NA FORGETTING STEPS
# ==============================================================================
#
# Conceptual hypothesis:
#   Negativity bias predicts that negative affect is discounted more slowly than
#   positive affect, meaning that negative affect should require MORE forgetting
#   steps before its modeled influence falls below the 5% threshold.
#
# Operationalization from forgetting_steps.R:
#   - dimension 11 = Positive Affect (PA)
#   - dimension 22 = Negative Affect (NA)
#   - forgetting steps = number of time steps until the modeled effect is < 0.05
#
# Therefore, for every participant:
#
#   negativity_bias_difference = NA forgetting steps - PA forgetting steps
#
# Positive values support the predicted direction: NA lingers longer than PA.
#
# Only participants for whom BOTH PA and NA forgetting-step estimates are valid
# are included. Files without both dimensions are skipped automatically, which
# excludes VANHASBROECK_2021 and the VANHASBROECK_2024 valence-only estimates.
#
# Two analyses are reported for each eligible dataset × model:
#   1. Conventional paired-samples t-test on the observed forgetting steps.
#   2. Non-parametric participant bootstrap (10,000 resamples).
#
# The substantive prediction is NA > PA. Both one-sided and two-sided p-values
# are saved for transparency, but the primary inferential reporting uses
# two-sided tests, with the observed sign of NA - PA used to determine whether a
# significant difference is in the predicted negativity-bias direction.
# ==============================================================================

FORGETTING_RESULTS_DIR <- PATHS$forgetting_steps

# Use longest model names first so that, for example, a double-exponential file
# is not accidentally identified as an exponential file.
NEGATIVITY_MODELS <- c(
  "double_exponential",
  "quasi_hyperbolic",
  "exponential"
)

# Give this analysis its own reproducible bootstrap stream.
set.seed(5678)


# dentify dataset/model from a forgetting-steps filename

identify_forgetting_file <- function(path) {

  file_name <- basename(path)

  matched_model <- NEGATIVITY_MODELS[
    vapply(
      NEGATIVITY_MODELS,
      function(model_name) {
        endsWith(
          file_name,
          paste0("_", model_name, "_forgetting_steps.csv")
        )
      },
      logical(1)
    )
  ][1]

  if (is.na(matched_model)) {
    return(NULL)
  }

  suffix <- paste0(
    "_",
    matched_model,
    "_forgetting_steps.csv"
  )

  dataset_name <- substr(
    file_name,
    1,
    nchar(file_name) - nchar(suffix)
  )

  list(
    dataset = dataset_name,
    model = matched_model
  )
}


# Prepare paired PA/NA forgetting steps

prepare_negativity_bias_data <- function(path) {

  file_info <- identify_forgetting_file(path)

  if (is.null(file_info)) {
    return(NULL)
  }

  df <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # The forgetting-steps implementation labels PA as dimension 11 and NA as 22.
  # Column names differ slightly across models:
  #   exponential:        n_forget_gamma_11 / n_forget_gamma_22
  #   quasi-hyperbolic:   n_forget_11       / n_forget_22
  #   double-exponential: n_forget_11       / n_forget_22
  # Matching by the dimension suffix therefore works for all three models.
  pa_cols <- grep(
    "^n_forget_.*11$",
    names(df),
    value = TRUE
  )

  na_cols <- grep(
    "^n_forget_.*22$",
    names(df),
    value = TRUE
  )

  # A file without both PA and NA dimensions is not eligible for the negativity
  # bias comparison (e.g. happiness-only or valence-only datasets).
  if (length(pa_cols) == 0 || length(na_cols) == 0) {
    return(NULL)
  }

  if (length(pa_cols) != 1 || length(na_cols) != 1) {
    stop(
      "Expected exactly one PA and one NA forgetting-step column in: ",
      path
    )
  }

  pa_col <- pa_cols[1]
  na_col <- na_cols[1]

  # Reconstruct the matching validity-column names using the same naming rule
  # used by forgetting_steps.R.
  pa_valid_col <- paste0(
    "is_valid_",
    sub("^n_forget_", "", pa_col)
  )

  na_valid_col <- paste0(
    "is_valid_",
    sub("^n_forget_", "", na_col)
  )

  if (!(pa_valid_col %in% names(df)) || !(na_valid_col %in% names(df))) {
    stop(
      "Missing PA/NA validity columns in: ",
      path,
      ". Run forgetting_steps.R first."
    )
  }

  pa_valid <- as.logical(df[[pa_valid_col]])
  na_valid <- as.logical(df[[na_valid_col]])

  pa_steps <- df[[pa_col]]
  na_steps <- df[[na_col]]

  keep <- (
    !is.na(pa_valid) &
      !is.na(na_valid) &
      pa_valid &
      na_valid &
      !is.na(pa_steps) &
      !is.na(na_steps) &
      is.finite(pa_steps) &
      is.finite(na_steps)
  )

  if (sum(keep) < 2) {
    warning(
      "Fewer than two valid paired participants for ",
      file_info$dataset,
      " / ",
      file_info$model,
      "; skipping."
    )
    return(NULL)
  }

  data.frame(
    dataset = file_info$dataset,
    model = file_info$model,
    participant_id = as.character(df$participant_id[keep]),
    pa_forgetting_steps = pa_steps[keep],
    na_forgetting_steps = na_steps[keep],
    difference_na_minus_pa = na_steps[keep] - pa_steps[keep],
    stringsAsFactors = FALSE
  )
}


# Paired t-test + non-parametric bootstrap

analyze_negativity_bias <- function(paired_df,
                                    n_boot = 10000,
                                    conf_level = 0.95) {

  pa <- paired_df$pa_forgetting_steps
  na <- paired_df$na_forgetting_steps
  difference <- paired_df$difference_na_minus_pa

  n <- length(difference)
  observed_difference <- mean(difference)

  # Conventional paired-samples t-test.
  # Directional version directly tests the negativity-bias hypothesis:
  # H1: mean NA forgetting steps > mean PA forgetting steps.
  t_directional <- t.test(
    x = na,
    y = pa,
    paired = TRUE,
    alternative = "greater",
    conf.level = conf_level
  )

  # Also retain the standard two-sided paired t-test for comparison/reporting.
  t_two_sided <- t.test(
    x = na,
    y = pa,
    paired = TRUE,
    alternative = "two.sided",
    conf.level = conf_level
  )

  # Paired standardized mean difference (Cohen's dz).
  sd_difference <- sd(difference)

  cohen_dz <- if (
    is.finite(sd_difference) && sd_difference > 0
  ) {
    observed_difference / sd_difference
  } else {
    NA_real_
  }

  # Non-parametric bootstrap: resample PARTICIPANTS with replacement while
  # keeping each participant's PA/NA pair together.
  boot_mean_difference <- replicate(
    n_boot,
    {
      sampled_idx <- sample.int(
        n = n,
        size = n,
        replace = TRUE
      )

      mean(difference[sampled_idx])
    }
  )

  bootstrap_se <- sd(boot_mean_difference)

  alpha <- 1 - conf_level

  # Percentile 95% bootstrap CI around the observed mean difference.
  boot_ci <- quantile(
    boot_mean_difference,
    probs = c(alpha / 2, 1 - alpha / 2),
    names = FALSE,
    type = 6
  )

  # Center the empirical bootstrap distribution at the null value (0) to obtain
  # bootstrap p-values for the observed mean paired difference.
  boot_under_null <- boot_mean_difference - observed_difference

  # Directional bootstrap p-value for H1: NA - PA > 0.
  p_boot_directional <- (
    sum(boot_under_null >= observed_difference) + 1
  ) / (n_boot + 1)

  # Two-sided bootstrap p-value retained for comparison.
  p_boot_two_sided <- (
    sum(abs(boot_under_null) >= abs(observed_difference)) + 1
  ) / (n_boot + 1)

  data.frame(
    dataset = paired_df$dataset[1],
    model = paired_df$model[1],
    n_participants = n,

    mean_pa_steps = mean(pa),
    sd_pa_steps = sd(pa),
    mean_na_steps = mean(na),
    sd_na_steps = sd(na),

    mean_difference_na_minus_pa = observed_difference,
    sd_difference = sd_difference,
    cohen_dz = cohen_dz,

    t_value = unname(t_two_sided$statistic),
    df = unname(t_two_sided$parameter),
    t_ci_lower = unname(t_two_sided$conf.int[1]),
    t_ci_upper = unname(t_two_sided$conf.int[2]),

    p_t_directional = t_directional$p.value,
    p_t_two_sided = t_two_sided$p.value,

    bootstrap_se = bootstrap_se,
    bootstrap_ci_lower = boot_ci[1],
    bootstrap_ci_upper = boot_ci[2],
    p_boot_directional = p_boot_directional,
    p_boot_two_sided = p_boot_two_sided,

    stringsAsFactors = FALSE
  )
}


# Run all eligible forgetting-step results files

forgetting_files <- list.files(
  FORGETTING_RESULTS_DIR,
  pattern = "_forgetting_steps\\.csv$",
  full.names = TRUE
)

if (length(forgetting_files) == 0) {
  stop(
    "No forgetting-step result files found in: ",
    FORGETTING_RESULTS_DIR,
    ". Run forgetting_steps.R first."
  )
}

negativity_paired_list <- lapply(
  forgetting_files,
  prepare_negativity_bias_data
)

negativity_paired_list <- Filter(
  Negate(is.null),
  negativity_paired_list
)

if (length(negativity_paired_list) == 0) {
  stop(
    "No eligible PA/NA forgetting-step files were found. ",
    "Expected files containing both dimensions 11 (PA) and 22 (NA)."
  )
}

negativity_bias_results <- do.call(
  rbind,
  lapply(
    negativity_paired_list,
    analyze_negativity_bias,
    n_boot = n_boot,
    conf_level = conf_level
  )
)

rownames(negativity_bias_results) <- NULL


# Multiple-comparison correction
#
# Three model forms are tested within each dataset. Apply Holm correction across
# those three model tests within each dataset. Two-sided tests are the primary
# inferential results used for reporting, while directional tests are retained
# as supplementary information for transparency.

negativity_bias_results$p_t_directional_holm <- NA_real_
negativity_bias_results$p_boot_directional_holm <- NA_real_
negativity_bias_results$p_t_two_sided_holm <- NA_real_
negativity_bias_results$p_boot_two_sided_holm <- NA_real_

for (dataset_name in unique(negativity_bias_results$dataset)) {

  idx <- which(
    negativity_bias_results$dataset == dataset_name
  )

  negativity_bias_results$p_t_directional_holm[idx] <- p.adjust(
    negativity_bias_results$p_t_directional[idx],
    method = "holm"
  )

  negativity_bias_results$p_boot_directional_holm[idx] <- p.adjust(
    negativity_bias_results$p_boot_directional[idx],
    method = "holm"
  )

  negativity_bias_results$p_t_two_sided_holm[idx] <- p.adjust(
    negativity_bias_results$p_t_two_sided[idx],
    method = "holm"
  )

  negativity_bias_results$p_boot_two_sided_holm[idx] <- p.adjust(
    negativity_bias_results$p_boot_two_sided[idx],
    method = "holm"
  )
}

# Direction of the observed effect, for reader-friendly reporting.
negativity_bias_results$direction <- ifelse(
  negativity_bias_results$mean_difference_na_minus_pa > 0,
  "NA slower",
  ifelse(
    negativity_bias_results$mean_difference_na_minus_pa < 0,
    "PA slower",
    "equal"
  )
)

# A concise inferential interpretation based on the two-sided bootstrap result
# after Holm correction. Statistical significance is determined two-sided; the
# sign of NA - PA then determines whether the difference is in the predicted
# negativity-bias direction. This is for console readability only.
negativity_bias_results$bootstrap_conclusion <- ifelse(
  negativity_bias_results$p_boot_two_sided_holm < 0.05 &
    negativity_bias_results$mean_difference_na_minus_pa > 0,
  "Supports bias",
  ifelse(
    negativity_bias_results$p_boot_two_sided_holm < 0.05 &
      negativity_bias_results$mean_difference_na_minus_pa < 0,
    "Opposite direction",
    "No clear bias"
  )
)


# Save negativity-bias results

negativity_output_file <- file.path(
  output_dir,
  "negativity_bias_forgetting_steps.csv"
)

write.csv(
  negativity_bias_results,
  negativity_output_file,
  row.names = FALSE
)

# Save the exact participant-level pairs used in the tests as an audit trail.
negativity_paired_data <- do.call(
  rbind,
  negativity_paired_list
)

rownames(negativity_paired_data) <- NULL

negativity_paired_output_file <- file.path(
  output_dir,
  "negativity_bias_participant_pairs.csv"
)

write.csv(
  negativity_paired_data,
  negativity_paired_output_file,
  row.names = FALSE
)


# -Negativity-bias console report

format_negativity_p <- function(p) {

  if (is.na(p)) {
    return("NA")
  }

  if (p < .001) {
    return("<.001")
  }

  sub("^0", "", sprintf("%.3f", p))
}

negativity_model_labels <- c(
  exponential = "EXP",
  quasi_hyperbolic = "QH",
  double_exponential = "DE"
)

cat("\n")
cat("======================================================================\n")
cat("NEGATIVITY BIAS: DO NEGATIVE EFFECTS LINGER LONGER THAN POSITIVE ONES?\n")
cat("======================================================================\n")
cat("Outcome: forgetting steps until the modeled effect falls below 5%.\n")
cat("Prediction: NA > PA. Positive Delta (NA - PA) supports negativity bias.\n")
cat("p-values below are two-sided and Holm-adjusted within dataset.\n")
cat("A significant positive Delta (NA - PA) is in the predicted negativity-bias direction.\n")

for (dataset_name in unique(negativity_bias_results$dataset)) {

  sub_results <- negativity_bias_results[
    negativity_bias_results$dataset == dataset_name,
    ,
    drop = FALSE
  ]

  # Keep model order consistent across datasets.
  model_order <- match(
    sub_results$model,
    c(
      "exponential",
      "quasi_hyperbolic",
      "double_exponential"
    )
  )

  sub_results <- sub_results[
    order(model_order),
    ,
    drop = FALSE
  ]

  summary_table <- data.frame(
    Model = unname(
      negativity_model_labels[sub_results$model]
    ),

    N = sub_results$n_participants,

    `PA mean` = sprintf(
      "%.2f",
      sub_results$mean_pa_steps
    ),

    `NA mean` = sprintf(
      "%.2f",
      sub_results$mean_na_steps
    ),

    `Delta` = sprintf(
      "%+.2f",
      sub_results$mean_difference_na_minus_pa
    ),

    `t(df)` = paste0(
      sprintf("%.2f", sub_results$t_value),
      "(",
      sub_results$df,
      ")"
    ),

    `p t` = vapply(
      sub_results$p_t_two_sided_holm,
      format_negativity_p,
      character(1)
    ),

    `Boot 95% CI` = paste0(
      "[",
      sprintf("%.2f", sub_results$bootstrap_ci_lower),
      ", ",
      sprintf("%.2f", sub_results$bootstrap_ci_upper),
      "]"
    ),

    `p boot` = vapply(
      sub_results$p_boot_two_sided_holm,
      format_negativity_p,
      character(1)
    ),

    Result = sub_results$bootstrap_conclusion,

    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  cat("\n", dataset_name, "\n", sep = "")

  table_text <- capture.output(
    print(
      summary_table,
      row.names = FALSE,
      right = FALSE
    )
  )

  writeLines(table_text)
}

cat("\nModel abbreviations: EXP = exponential; QH = quasi-hyperbolic; DE = double-exponential.\n")
cat("Delta = mean NA forgetting steps - mean PA forgetting steps.\n")
cat("'Supports bias' = Delta > 0 and two-sided Holm-adjusted bootstrap p < .05.\n")
cat("'Opposite direction' = Delta < 0 and two-sided Holm-adjusted bootstrap p < .05.\n")

flush.console()


# ==============================================================================
# FINAL OUTPUT LOCATIONS
# ==============================================================================

cat(
  "\nAll non-parametric bootstrap analyses complete.\n",
  "Model-comparison results saved to: ",
  output_file,
  "\n",
  "Negativity-bias results saved to: ",
  negativity_output_file,
  "\n",
  "Negativity-bias participant pairs saved to: ",
  negativity_paired_output_file,
  "\n",
  sep = ""
)
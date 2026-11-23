################################################################################
# 16_inspect_cv_zero_rmse.R
# PURPOSE: Diagnose zero/near-zero participant-level cross-validation RMSEs.
# Reads existing CV results, full-data model estimates, and processed RDS data.
# Includes model-ranking sensitivity with/without constant-response outcomes.
# Does NOT refit models or alter source results. Base R only.
# Run from repository root: Rscript analysis/16_inspect_cv_zero_rmse.R
# Output: analysis/results/cross_validation/diagnostics/zero_rmse/
################################################################################

config_file <- if (file.exists("analysis/_config.R")) {
  "analysis/_config.R"
} else if (file.exists("_config.R")) {
  "_config.R"
} else {
  stop("Run from the repository root or analysis/ (missing _config.R).")
}
source(config_file)

# Numerical cutoffs are for diagnostics, not inferential decisions.
near_zero_rmse <- 1e-8
near_zero_parameter <- 1e-8
near_zero_sd <- 1e-8
min_valid_folds <- 3L

cv_dir <- file.path(PATHS$results, "cross_validation")
estimation_dir <- PATHS$estimation
out_dir <- file.path(cv_dir, "diagnostics", "zero_rmse")
if (!dir.exists(cv_dir)) stop("Missing CV directory: ", cv_dir)
if (!dir.exists(estimation_dir)) stop("Missing estimation directory: ", estimation_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

models <- c("exponential", "quasi_hyperbolic", "double_exponential")
cv_files <- list.files(cv_dir, pattern = "_cv\\.csv$", full.names = TRUE)
if (!length(cv_files)) stop("No model-level *_cv.csv files in ", cv_dir)

# Parse model and dataset from filenames, including the 2024 d=1/d=2 split.
parse_name <- function(path) {
  stem <- sub("_cv\\.csv$", "", basename(path))
  # Match longest model names first: "double_exponential" also ends in
  # "_exponential", so a simple vectorized suffix match is ambiguous.
  matching <- models[order(nchar(models), decreasing = TRUE)]
  matching <- matching[endsWith(stem, paste0("_", matching))]
  if (!length(matching)) return(NULL)
  model <- matching[1]
  dataset <- substr(stem, 1L, nchar(stem) - nchar(model) - 1L)
  list(dataset = dataset, model = model)
}

rows <- list()
for (path in cv_files) {
  info <- parse_name(path)
  if (is.null(info)) next
  dat <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!"participant_id" %in% names(dat)) stop("Missing participant_id: ", path)
  rmse_cols <- grep("^rmse_", names(dat), value = TRUE)
  for (column in rmse_cols) {
    outcome <- sub("^rmse_", "", column)
    folds_col <- paste0("n_valid_folds_", outcome)
    if (!folds_col %in% names(dat)) stop("Missing ", folds_col, " in ", path)
    rows[[length(rows) + 1L]] <- data.frame(
      dataset = info$dataset,
      participant_id = as.character(dat$participant_id),
      outcome = outcome,
      model = info$model,
      rmse = as.numeric(dat[[column]]),
      n_valid_folds = as.numeric(dat[[folds_col]]),
      stringsAsFactors = FALSE
    )
  }
}
if (!length(rows)) stop("No recognized model/outcome RMSE results.")
cv <- do.call(rbind, rows)
rownames(cv) <- NULL
cv$valid_cv <- is.finite(cv$rmse) & is.finite(cv$n_valid_folds) &
  cv$n_valid_folds >= min_valid_folds
cv$exact_zero <- cv$valid_cv & cv$rmse == 0
cv$near_zero <- cv$valid_cv & cv$rmse >= 0 & cv$rmse <= near_zero_rmse

# Classify each dataset + participant + outcome using complete model coverage.
key <- paste(cv$dataset, cv$participant_id, cv$outcome, sep = "||")
groups <- split(seq_len(nrow(cv)), key)
classification <- do.call(rbind, lapply(groups, function(idx) {
  x <- cv[idx, ]
  full_set <- setequal(x$model, models) && nrow(x) == length(models)
  all_valid <- full_set && all(x$valid_cv)
  data.frame(
    dataset = x$dataset[1], participant_id = x$participant_id[1],
    outcome = x$outcome[1], models_present = length(unique(x$model)),
    all_models_valid = all_valid,
    n_exact_models = sum(x$exact_zero),
    n_near_models = sum(x$near_zero),
    all_exact = all_valid && all(x$exact_zero),
    all_near = all_valid && all(x$near_zero),
    any_exact = any(x$exact_zero),
    any_near = any(x$near_zero),
    stringsAsFactors = FALSE
  )
}))
rownames(classification) <- NULL
# Restrict detailed evaluation to any flagged participant/outcome.
flagged <- subset(classification, any_near)
if (!nrow(flagged)) {
  warning("No valid zero/near-zero RMSE values found.")
}

# Locate estimation result for a given CV dataset, including optional dimension
# suffix used only if the corresponding estimation file exists.
read_estimates <- function(dataset, model, ids) {
  candidates <- dataset
  if (dataset == "VANHASBROECK_2024") {
    candidates <- c("VANHASBROECK_2024_1", "VANHASBROECK_2024_2")
  }
  pieces <- list()
  for (dset in candidates) {
    path <- file.path(estimation_dir, paste0(dset, "_", model, ".csv"))
    if (!file.exists(path)) next
    x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    if (!"participant_id" %in% names(x)) next
    x <- x[x$participant_id %in% ids, , drop = FALSE]
    if (nrow(x)) pieces[[length(pieces) + 1L]] <- x
  }
  pieces
}

# Model parameter estimates: separate predictor coefficients (beta), residual
# covariance entries (sigma), intercepts (alpha), and remaining parameters.
parameter_rows <- list()
for (i in seq_len(nrow(flagged))) {
  f <- flagged[i, ]
  for (model in models) {
    estimates <- read_estimates(f$dataset, model, f$participant_id)
    if (!length(estimates)) {
      parameter_rows[[length(parameter_rows) + 1L]] <- data.frame(
        dataset = f$dataset, participant_id = f$participant_id,
        outcome = f$outcome, model = model, parameter = NA_character_,
        estimate = NA_real_, group = NA_character_,
        near_zero_parameter = NA, stringsAsFactors = FALSE
      )
      next
    }
    x <- estimates[[1]][1, , drop = FALSE]
    parameter_names <- grep("^(alpha_|beta_|sigma_|gamma_|nu_|kappa_|omega$)",
                            names(x), value = TRUE)
    for (p in parameter_names) {
      value <- as.numeric(x[[p]])
      group <- if (startsWith(p, "beta_")) "predictor_coefficient" else if (
        startsWith(p, "sigma_")) "residual_covariance" else if (
          startsWith(p, "alpha_")) "intercept" else "other_model_parameter"
      parameter_rows[[length(parameter_rows) + 1L]] <- data.frame(
        dataset = f$dataset, participant_id = f$participant_id,
        outcome = f$outcome, model = model, parameter = p, estimate = value,
        group = group,
        near_zero_parameter = is.finite(value) && abs(value) <= near_zero_parameter,
        stringsAsFactors = FALSE
      )
    }
    for (p in intersect(c("objective_sse", "aic", "bic"), names(x))) {
      parameter_rows[[length(parameter_rows) + 1L]] <- data.frame(
        dataset = f$dataset, participant_id = f$participant_id,
        outcome = f$outcome, model = model, parameter = p,
        estimate = as.numeric(x[[p]]), group = "fit_statistic",
        near_zero_parameter = NA, stringsAsFactors = FALSE
      )
    }
  }
}
params <- if (length(parameter_rows)) do.call(rbind, parameter_rows) else
  data.frame(dataset = character(), participant_id = character(),
             outcome = character(), model = character(), parameter = character(),
             estimate = numeric(), group = character(), near_zero_parameter = logical())

# Read raw observed Y columns from the processed RDS datasets. For 2024 the
# same folder contains both response dimensionalities. Do not infer variability
# from estimated intercepts: compute it from the actual ratings.
observed_rows <- list()
for (i in seq_len(nrow(flagged))) {
  f <- flagged[i, ]
  raw_dataset <- sub("_[12]$", "", f$dataset)
  path <- file.path(PATHS$processed_data, raw_dataset,
                    paste0(f$participant_id, ".rds"))
  if (!file.exists(path)) {
    warning("Missing processed participant data: ", path)
    next
  }
  data <- readRDS(path)
  y <- as.matrix(data@Y)
  outcome_names <- colnames(y)
  if (is.null(outcome_names)) outcome_names <- paste0("y", seq_len(ncol(y)))
  outcome_index <- match(f$outcome, outcome_names)
  if (is.na(outcome_index)) {
    warning("Outcome not found for ", f$participant_id, ": ", f$outcome,
            " (available: ", paste(outcome_names, collapse = ", "), ")")
    next
  }
  values <- y[, outcome_index]
  values <- values[is.finite(values)]
  observed_rows[[length(observed_rows) + 1L]] <- data.frame(
    dataset = f$dataset, participant_id = f$participant_id,
    outcome = f$outcome, n_observed = length(values),
    n_unique = length(unique(values)),
    observed_mean = if (length(values)) mean(values) else NA_real_,
    observed_sd = if (length(values) >= 2L) sd(values) else NA_real_,
    observed_range = if (length(values)) diff(range(values)) else NA_real_,
    constant_observed = length(values) >= 2L && length(unique(values)) == 1L,
    near_constant_observed = length(values) >= 2L &&
      diff(range(values)) <= near_zero_sd,
    stringsAsFactors = FALSE
  )
}
observed <- if (length(observed_rows)) do.call(rbind, observed_rows) else
  data.frame(dataset = character(), participant_id = character(),
             outcome = character(), n_observed = integer(), n_unique = integer(),
             observed_mean = numeric(), observed_sd = numeric(),
             observed_range = numeric(), constant_observed = logical(),
             near_constant_observed = logical())

# Produce one row per flagged dataset/participant/outcome, with model-specific
# RMSE and observed-variability columns. The full parameter values remain in a
# separate long-format CSV for transparency and easy inspection.
main <- merge(flagged, observed, by = c("dataset", "participant_id", "outcome"),
              all.x = TRUE, sort = FALSE)
for (model in models) {
  x <- cv[cv$model == model,
          c("dataset", "participant_id", "outcome", "rmse", "n_valid_folds")]
  names(x)[4:5] <- paste0(c("rmse_", "folds_"), model)
  main <- merge(main, x, by = c("dataset", "participant_id", "outcome"),
                all.x = TRUE, sort = FALSE)
}
# Aggregate estimation evidence at the participant level, but report actual
# parameter values in the separate long table (not merely a yes/no flag).
if (nrow(params)) {
  param_key <- paste(params$dataset, params$participant_id, params$outcome, sep = "||")
  evidence <- do.call(rbind, lapply(split(seq_len(nrow(params)), param_key), function(idx) {
    x <- params[idx, ]
    beta <- x[x$group == "predictor_coefficient", , drop = FALSE]
    sigma <- x[x$group == "residual_covariance", , drop = FALSE]
    sse <- x[x$parameter == "objective_sse", , drop = FALSE]
    data.frame(dataset = x$dataset[1], participant_id = x$participant_id[1],
               outcome = x$outcome[1],
               all_beta_near_zero = nrow(beta) > 0L &&
                 all(is.finite(beta$estimate)) &&
                 all(abs(beta$estimate) <= near_zero_parameter),
               all_sigma_near_zero = nrow(sigma) > 0L &&
                 all(is.finite(sigma$estimate)) &&
                 all(abs(sigma$estimate) <= near_zero_parameter),
               all_sse_zero = nrow(sse) == length(models) &&
                 all(is.finite(sse$estimate)) && all(sse$estimate == 0),
               stringsAsFactors = FALSE)
  }))
  main <- merge(main, evidence,
                by = c("dataset", "participant_id", "outcome"),
                all.x = TRUE, sort = FALSE)
}
main <- main[order(main$dataset, main$participant_id, main$outcome), ]
rownames(main) <- NULL

# Counts refer to outcome-specific cases, not unique people. Model counts are
# treated separately, particularly for participants with two affect outcomes.
summary <- do.call(rbind, lapply(split(classification, classification$dataset),
                                  function(x) {
  flagged_set <- subset(x, any_near)
  data.frame(dataset = x$dataset[1],
             n_participant_outcomes = nrow(x),
             n_any_exact = sum(x$any_exact),
             n_all_exact = sum(x$all_exact),
             n_any_near = sum(x$any_near),
             n_all_near = sum(x$all_near),
             n_unique_flagged_participants = length(unique(flagged_set$participant_id)),
             stringsAsFactors = FALSE)
}))

write.csv(cv, file.path(out_dir, "01_all_model_rmse.csv"), row.names = FALSE)
write.csv(classification, file.path(out_dir, "02_all_outcome_classifications.csv"), row.names = FALSE)
write.csv(main, file.path(out_dir, "03_flagged_participant_diagnostics.csv"), row.names = FALSE)
write.csv(params, file.path(out_dir, "04_flagged_parameter_estimates.csv"), row.names = FALSE)
write.csv(summary, file.path(out_dir, "05_dataset_summary.csv"), row.names = FALSE)

# Sensitivity analysis --------------------------------------------------------
# The original summary compares participants with a finite RMSE and at least
# min_valid_folds successful folds in EACH of the three models, independently
# for each affect outcome. Apply the exact same inclusion rule here.
#
# Two exclusions are reported:
#   - exclude_constant_outcomes: remove only flagged constant outcomes.
#   - exclude_constant_participants: remove all outcomes of any participant
#     with a flagged constant outcome (the literal participant exclusion).
# No models are re-estimated. The original participant-level RMSEs are reused.

# Avoid using low RMSE as a proxy for constant ratings: exclude ONLY confirmed
# constant outcomes in the processed observed data.
constant_cases <- main[
  !is.na(main$constant_observed) & main$constant_observed,
  c("dataset", "participant_id", "outcome"), drop = FALSE
]
constant_cases <- unique(constant_cases)
case_key <- function(dataset, id, outcome) {
  paste(dataset, id, outcome, sep = "||")
}
constant_keys <- case_key(constant_cases$dataset,
                          constant_cases$participant_id,
                          constant_cases$outcome)
constant_ids <- unique(paste(constant_cases$dataset,
                             constant_cases$participant_id, sep = "||"))

# Incomplete observations in processed files must not silently be treated as
# variable outcomes. Signal them when flagged cases cannot be classified.
if (any(is.na(main$constant_observed))) {
  warning("Some flagged outcomes lack observed data; exclusions are based only ",
          "on confirmed constant outcomes.")
}

# Assemble matrices with the same model order and eligible participant set
# that summarize_cv_dataset() / build_rmse_matrix() use.
make_matched_rmse <- function(dataset, outcome) {
  subset_cv <- cv[cv$dataset == dataset & cv$outcome == outcome, ]
  ids <- unique(subset_cv$participant_id)
  wide <- matrix(NA_real_, nrow = length(ids), ncol = length(models),
                 dimnames = list(ids, models))
  for (model in models) {
    values <- subset_cv[subset_cv$model == model, ]
    pos <- match(ids, values$participant_id)
    valid <- !is.na(pos)
    valid[valid] <- values$valid_cv[pos[valid]]
    wide[valid, model] <- values$rmse[pos[valid]]
  }
  wide[rowSums(is.finite(wide)) == length(models), , drop = FALSE]
}

# Each row contains an RMSE summary and unique-win/tie percentages for one
# dataset, outcome, sample, and model. Ties are counted separately, as in the
# existing summarize_cv() function.
summarize_sample <- function(mat, dataset, outcome, sample) {
  if (!nrow(mat)) {
    return(data.frame(dataset = dataset, outcome = outcome, sample = sample,
                      model = models, n = 0L, mean_rmse = NA_real_,
                      sd_rmse = NA_real_, win_pct = NA_real_,
                      tie_pct = NA_real_))
  }
  min_rmse <- apply(mat, 1, min)
  wins <- sweep(mat, 1, min_rmse, "==")
  ties <- rowSums(wins) > 1L
  wins[ties, ] <- FALSE
  data.frame(dataset = dataset, outcome = outcome, sample = sample,
             model = models, n = nrow(mat),
             mean_rmse = colMeans(mat),
             sd_rmse = apply(mat, 2, sd),
             win_pct = 100 * colMeans(wins),
             tie_pct = 100 * mean(ties), row.names = NULL)
}

# Pairwise rates complement the three-way unique-win summary; with an exact
# tie, neither model receives a win. Keep these purely descriptive: no new
# bootstrap is needed to check ranking sensitivity.
pairwise_sample <- function(mat, dataset, outcome, sample) {
  pairs <- combn(models, 2, simplify = FALSE)
  do.call(rbind, lapply(pairs, function(pair) {
    a <- mat[, pair[1]]
    b <- mat[, pair[2]]
    data.frame(dataset = dataset, outcome = outcome, sample = sample,
               model_1 = pair[1], model_2 = pair[2], n = length(a),
               wins_1 = if (length(a)) 100 * mean(a < b) else NA_real_,
               wins_2 = if (length(a)) 100 * mean(b < a) else NA_real_,
               tie_pct = if (length(a)) 100 * mean(a == b) else NA_real_,
               mean_diff = if (length(a)) mean(a - b) else NA_real_)
  }))
}

samples <- c("original", "exclude_constant_outcomes",
             "exclude_constant_participants")
comparison_rows <- list()
pairwise_rows <- list()
exclusion_rows <- list()
for (dataset in unique(cv$dataset)) {
  outcomes <- unique(cv$outcome[cv$dataset == dataset])
  for (outcome in outcomes) {
    original <- make_matched_rmse(dataset, outcome)
    ids <- rownames(original)
    keep_outcomes <- !case_key(dataset, ids, outcome) %in% constant_keys
    keep_participants <- !paste(dataset, ids, sep = "||") %in% constant_ids
    mats <- list(original, original[keep_outcomes, , drop = FALSE],
                 original[keep_participants, , drop = FALSE])
    names(mats) <- samples
    exclusion_rows[[length(exclusion_rows) + 1L]] <- data.frame(
      dataset = dataset, outcome = outcome,
      n_original = nrow(original),
      n_exclude_constant_outcomes = nrow(mats[[2]]),
      n_exclude_constant_participants = nrow(mats[[3]]),
      n_removed_outcomes = sum(!keep_outcomes),
      n_removed_participants = sum(!keep_participants)
    )
    for (sample in samples) {
      comparison_rows[[length(comparison_rows) + 1L]] <-
        summarize_sample(mats[[sample]], dataset, outcome, sample)
      pairwise_rows[[length(pairwise_rows) + 1L]] <-
        pairwise_sample(mats[[sample]], dataset, outcome, sample)
    }
  }
}
comparison <- do.call(rbind, comparison_rows)
pairwise_comparison <- do.call(rbind, pairwise_rows)
exclusions <- do.call(rbind, exclusion_rows)

# Calculate actual changes relative to the original analysis. They are in
# raw RMSE units / percentage points, and not significance tests.
original_summary <- comparison[comparison$sample == "original",
                               c("dataset", "outcome", "model",
                                 "mean_rmse", "win_pct")]
for (metric in c("mean_rmse", "win_pct")) {
  names(original_summary)[names(original_summary) == metric] <-
    paste0(metric, "_original")
}
changes <- merge(comparison[comparison$sample != "original", ],
                 original_summary, by = c("dataset", "outcome", "model"),
                 sort = FALSE)
changes$delta_mean_rmse <- changes$mean_rmse - changes$mean_rmse_original
changes$delta_win_pp <- changes$win_pct - changes$win_pct_original
changes <- changes[order(changes$dataset, changes$outcome,
                         changes$sample, changes$model), ]

write.csv(exclusions, file.path(out_dir, "07_sensitivity_sample_sizes.csv"),
          row.names = FALSE)
write.csv(comparison, file.path(out_dir, "08_sensitivity_model_summary.csv"),
          row.names = FALSE)
write.csv(pairwise_comparison,
          file.path(out_dir, "09_sensitivity_pairwise.csv"), row.names = FALSE)
write.csv(changes, file.path(out_dir, "10_sensitivity_changes.csv"),
          row.names = FALSE)

# Write a compact text comparison for each dataset/outcome with the model order
# explicit, so results can be inspected without opening multiple CSV files.
sensitivity_report <- c("", "SENSITIVITY: EXCLUDING CONSTANT RESPONSES",
                        "=========================================")
for (i in seq_len(nrow(exclusions))) {
  e <- exclusions[i, ]
  x <- comparison[comparison$dataset == e$dataset &
                  comparison$outcome == e$outcome, ]
  sensitivity_report <- c(
    sensitivity_report, "",
    paste("Dataset/outcome:", e$dataset, "/", e$outcome),
    paste("Eligible original participants:", e$n_original),
    paste("After outcome exclusion:", e$n_exclude_constant_outcomes),
    paste("After participant exclusion:", e$n_exclude_constant_participants)
  )
  for (sample in samples) {
    z <- x[x$sample == sample, ]
    sensitivity_report <- c(sensitivity_report, paste("  ", sample, ":"))
    for (j in seq_len(nrow(z))) {
      sensitivity_report <- c(sensitivity_report, sprintf(
        "    %-20s mean RMSE = %.9g; unique wins = %.3f%%; ties = %.3f%%",
        z$model[j], z$mean_rmse[j], z$win_pct[j], z$tie_pct[j]
      ))
    }
  }
}

# A plain-text, readable report that does not require R Markdown or Pandoc.
report <- c(
  "CROSS-VALIDATION ZERO-RMSE DIAGNOSTICS",
  "======================================",
  paste("Near-zero RMSE cutoff:", format(near_zero_rmse, scientific = TRUE)),
  paste("Near-zero parameter cutoff:", format(near_zero_parameter, scientific = TRUE)),
  paste("Minimum successful folds per model:", min_valid_folds),
  "",
  "Dataset-level summary (counts are participant/outcome pairs):",
  capture.output(print(summary, row.names = FALSE)),
  "",
  paste("Total unique flagged participants:", length(unique(flagged$participant_id))),
  paste("Exact-zero in every model (participant/outcome pairs):", sum(classification$all_exact)),
  paste("Near-zero in every model (participant/outcome pairs):", sum(classification$all_near)),
  "",
  "Interpretation guide:",
  "- Inspect 03_flagged_participant_diagnostics.csv for each flagged outcome.",
  "- constant_observed=TRUE means identical observed responses on that outcome.",
  "- all_beta_near_zero checks estimated input coefficients across all 3 models.",
  "- all_sigma_near_zero checks residual covariance entries, not an independent", 
  "  estimate of the variance of the observed responses.",
  "- all_sse_zero checks full-data estimation SSE, not cross-validation SSE.",
  "- Missing RDS/estimation files remain NA; do not interpret NA as FALSE.",
  "- Near-zero numerical values do not automatically establish a software bug.",
  "- Fold-specific RMSE and masked-fit parameters are not persisted by the", 
  "  current CV CSV export; they cannot be independently inspected here.",
  "- The pooled CV RMSE implementation should separately be reviewed for", 
  "  failed or non-finite folds and its pooling denominator.",
  "",
  "These diagnostics are descriptive and do not refit or change any models."
)
report <- c(report, sensitivity_report)
writeLines(report, file.path(out_dir, "06_readme_results.txt"))
cat(paste(report, collapse = "\n"), "\n")
cat("\nSaved diagnostic outputs in: ", out_dir, "\n", sep = "")

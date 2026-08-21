################################################################################
# PURPOSE:
# Read the parameter-estimation CSV files (one per dataset x model) and, for
# each dataset, compute the spread of the forgetting-factor (decay parameter)
# estimates: mean, SD, min, max, and a 95% quantile interval.
#
# One summary CSV is saved per dataset, and each summary is also printed to
# the console.
#
# NOTE ON DIMENSION CODES (the "_11" / "_22" suffixes on parameter columns,
# e.g. gamma_11, nu_22):
#   - VANHASBROECK_2021:      single dimension = valence
#   - VANHASBROECK_2022:      11 = positive affect, 22 = negative affect
#   - VANHASBROECK_2024_1:    single dimension = valence
#   - VANHASBROECK_2024_2:    11 = positive affect, 22 = negative affect
#   - NIEMEIJER_2022:         11 = positive affect, 22 = negative affect
################################################################################

input_dir  <- file.path("scripts", "results", "estimation")
output_dir <- file.path("scripts", "results", "forgetting_factor_spread")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Which columns count as "forgetting factors" (decay parameters) per model.
# Matched by column-name prefix.
DECAY_PARAM_PREFIXES <- list(
  double_exponential  = c("gamma", "nu", "omega"),
  quasi_hyperbolic    = c("nu", "kappa"),
  exponential         = c("gamma")
)

models <- c("double_exponential", "quasi_hyperbolic", "exponential")

# ---- Identify dataset/model from filename -----------------------------------

# Assumes filenames follow the pattern "<dataset>_<model_type>.csv"
identify_estimation_file <- function(path, models) {

  file_name_no_csv <- sub("\\.csv$", "", basename(path))

  matched_model <- models[
    vapply(models, function(m) endsWith(file_name_no_csv, m), logical(1))
  ][1]

  if (is.na(matched_model)) {
    return(NULL)
  }

  dataset_name <- sub(paste0("_", matched_model, "$"), "", file_name_no_csv)

  list(dataset = dataset_name, model = matched_model)
}


# ---- Compute spread statistics for one model's decay-parameter columns ------

summarize_forgetting_factors <- function(df, model_type, dataset_name) {

  prefixes <- DECAY_PARAM_PREFIXES[[model_type]]
  decay_cols <- unlist(lapply(
    prefixes,
    function(p) grep(paste0("^", p), names(df), value = TRUE)
  ))

  if (length(decay_cols) == 0) {
    return(NULL)
  }

  do.call(rbind, lapply(decay_cols, function(col) {

    values <- df[[col]]

    data.frame(
      dataset   = dataset_name,
      model     = model_type,
      parameter = col,
      mean      = mean(values, na.rm = TRUE),
      sd        = sd(values, na.rm = TRUE),
      min       = min(values, na.rm = TRUE),
      q2.5      = quantile(values, 0.025, na.rm = TRUE, names = FALSE),
      q97.5     = quantile(values, 0.975, na.rm = TRUE, names = FALSE),
      max       = max(values, na.rm = TRUE)
    )
  }))
}


# ---- Run: load every estimation file, summarize, save per dataset -----------

files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

all_summaries <- list()

for (f in files) {

  file_info <- identify_estimation_file(f, models)
  if (is.null(file_info)) {
    warning("Could not identify dataset/model for file: ", f)
    next
  }

  df <- read.csv(f, stringsAsFactors = FALSE)

  summary_df <- summarize_forgetting_factors(df, file_info$model, file_info$dataset)
  if (is.null(summary_df)) next

  dataset_name <- file_info$dataset

  if (is.null(all_summaries[[dataset_name]])) {
    all_summaries[[dataset_name]] <- summary_df
  } else {
    all_summaries[[dataset_name]] <- rbind(all_summaries[[dataset_name]], summary_df)
  }
}

# ---- Save one CSV per dataset, and print each to the console ----------------

for (dataset_name in names(all_summaries)) {

  summary_df <- all_summaries[[dataset_name]]
  rownames(summary_df) <- NULL

  cat("=== ", dataset_name, " ===\n", sep = "")
  print(summary_df)
  cat("\n")

  out_path <- file.path(output_dir, paste0(dataset_name, "_forgetting_factor_spread.csv"))
  write.csv(summary_df, out_path, row.names = FALSE)
  cat("Saved to:", out_path, "\n\n")
}

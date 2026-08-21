# ==============================================================================
# Model comparison: best model per participant by AIC and BIC, per dataset
# ==============================================================================

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

# The shared MODEL_TYPES set in _config.R is intentionally UNORDERED.
# This script's comparison logic resolves exact AIC/BIC ties with which.min(),
# so the model sequence passed to the comparison must preserve the historical
# processing order (which came from the old forgetting_steps workflow).
MODEL_COMPARISON_ORDER <- c(
  "double_exponential",
  "quasi_hyperbolic",
  "exponential"
)

data <- load_estimation_data(PATHS$estimation, MODEL_TYPES)

# The 2024 estimation is split by response dimensionality:
#   _1 = d = 1 = valence-only participants
#   _2 = d = 2 = positive/negative affect participants (no valence)
#
# Rename these technical dimensionality labels immediately after loading so
# all downstream model-comparison output uses meaningful dataset names.
dataset_name_map <- c(
  "VANHASBROECK_2024_1" = "VANHASBROECK_2024_valence",
  "VANHASBROECK_2024_2" = "VANHASBROECK_2024_no_valence"
)

names(data) <- ifelse(
  names(data) %in% names(dataset_name_map),
  unname(dataset_name_map[names(data)]),
  names(data)
)


# Create a dedicated output directory for model-comparison results.
comparison_output_dir <- PATHS$model_comparison

ensure_dir(comparison_output_dir)

cat("\nModel-comparison results will be saved to:\n",
    normalizePath(comparison_output_dir), "\n")

# Remove obsolete comparison outputs that used the technical _1/_2 labels.
# This only removes old model-comparison CSVs; the estimation CSVs are untouched.
old_2024_outputs <- file.path(
  comparison_output_dir,
  c(
    "VANHASBROECK_2024_1_model_comparison.csv",
    "VANHASBROECK_2024_2_model_comparison.csv"
  )
)

unlink(old_2024_outputs[file.exists(old_2024_outputs)])

compare_models_for_dataset <- function(dataset_name, data, model_types) {
  model_dfs <- lapply(model_types, function(m) {
    df <- data[[dataset_name]][[m]]
    if (is.null(df)) return(NULL)
    data.frame(
      participant_id = df[["participant_id"]],
      aic = df[["aic"]],
      bic = df[["bic"]]
    )
  })
  names(model_dfs) <- model_types
  model_dfs <- model_dfs[!sapply(model_dfs, is.null)]

  if (length(model_dfs) == 0) return(NULL)

  merged <- Reduce(function(x, y) merge(x, y, by = "participant_id", all = TRUE),
                    lapply(names(model_dfs), function(m) {
                      df <- model_dfs[[m]]
                      names(df)[names(df) == "aic"] <- paste0("aic_", m)
                      names(df)[names(df) == "bic"] <- paste0("bic_", m)
                      df
                    }))

  aic_cols <- grep("^aic_", names(merged), value = TRUE)
  bic_cols <- grep("^bic_", names(merged), value = TRUE)

  merged$best_aic_model <- sub("^aic_", "", aic_cols[apply(merged[aic_cols], 1, which.min)])
  merged$best_bic_model <- sub("^bic_", "", bic_cols[apply(merged[bic_cols], 1, which.min)])

  merged
}

# ---- run per dataset, keep results and win-counts separate ----

dataset_names <- names(data)
comparisons_by_dataset <- list()

for (dataset_name in dataset_names) {
  comparison <- compare_models_for_dataset(
    dataset_name,
    data,
    MODEL_COMPARISON_ORDER
  )
  if (is.null(comparison)) next

  comparisons_by_dataset[[dataset_name]] <- comparison

  write.csv(
    comparison,
    file.path(
      comparison_output_dir,
      paste0(dataset_name, "_model_comparison.csv")
    ),
    row.names = FALSE
  )
  
  aic_counts <- table(comparison$best_aic_model)
  bic_counts <- table(comparison$best_bic_model)
  
  aic_pct <- round(100 * prop.table(aic_counts), 0)
  bic_pct <- round(100 * prop.table(bic_counts), 0)

  cat("\n===", dataset_name, "===\n")
  #cat("Best model by AIC:\n")
  #print(table(comparison$best_aic_model))
  #cat("Best model by BIC:\n")
  #print(table(comparison$best_bic_model))
  #cat("\n")
  
  cat("Best model by AIC (%):\n")
  print(aic_pct)
  cat("\n")
  cat("Best model by BIC (%):\n")
  print(bic_pct)
}



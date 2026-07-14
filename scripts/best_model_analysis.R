# ==============================================================================
# Model comparison: best model per participant by AIC and BIC, per dataset
# ==============================================================================

data <- load_estimation_data(ESTIMATE_DIR, MODEL_TYPES)

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
  comparison <- compare_models_for_dataset(dataset_name, data, MODEL_TYPES)
  if (is.null(comparison)) next
  
  comparisons_by_dataset[[dataset_name]] <- comparison
  
  write.csv(comparison,
            file.path("scripts/results", paste0(dataset_name, "_model_comparison.csv")),
            row.names = FALSE)
  
  cat("\n===", dataset_name, "===\n")
  cat("Best model by AIC:\n")
  print(table(comparison$best_aic_model))
  cat("\n")
  cat("Best model by BIC:\n")
  print(table(comparison$best_bic_model))
}

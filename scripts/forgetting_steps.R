# ==============================================================================
# Forgetting Steps Analysis: Temporal Discounting Models
# ------------------------------------------------------------------------------
# Computes the number of "forgetting steps" (time steps until a stimulus's
# effect decays below a chosen threshold) from parameter estimates of
# exponential, quasi-hyperbolic, and double-exponential discounting models.
# Validity of a forgetting-steps estimate is judged post-hoc by comparing it
# against the number of trials observed for that participant/dataset, rather
# than by artificially capping the computed value.
# ==============================================================================

# ---- Configuration ----------------------------------------------------------

ESTIMATE_DIR     <- "scripts/results/estimation"
RAW_DATA_DIR     <- "scripts/data"
MODEL_TYPES      <- c("double_exponential", "quasi_hyperbolic", "exponential")
FORGET_THRESHOLD <- 0.05   # effect considered "negligible" below this proportion
SEARCH_LIMIT     <- 1000   # upper bound for numeric search (double-exponential only)

DECAY_PARAM_PREFIXES <- list(
  exponential         = c("gamma"),
  double_exponential  = c("gamma", "nu", "omega"),
  quasi_hyperbolic    = c("nu", "kappa")
)

# ---- Data loading -------------------------------------------------------------

#' Read all parameter estimation CSVs into a nested list: data[[dataset]][[model_type]]
#'
#' Assumes filenames follow the pattern "<dataset>_<model_type>.csv", where
#' model_type is one of MODEL_TYPES.
load_estimation_data <- function(dir, model_types) {
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  data <- list()
  
  for (f in files) {
    file_name_no_csv <- sub("\\.csv$", "", basename(f))
    
    model_type <- model_types[sapply(model_types, function(m) endsWith(file_name_no_csv, m))][1]
    if (is.na(model_type)) {
      warning(paste("Could not identify model type for file:", f))
      next
    }
    dataset <- sub(paste0("_", model_type, "$"), "", file_name_no_csv)
    
    df <- read.csv(f, stringsAsFactors = FALSE)
    
    if (is.null(data[[dataset]])) {
      data[[dataset]] <- list()
    }
    data[[dataset]][[model_type]] <- df
  }
  
  data
}

# ---- Column identification ---------------------------------------------------

#' Get the decay-parameter column names for a given model type's data frame
#' (e.g. "gamma_11", "gamma_22" for a 2D exponential fit).
get_decay_columns <- function(df, model_type) {
  if (is.null(df) || length(df) == 0) {
    stop("DataFrame does not exist for this dataset/model_type combination.")
  }
  if (!(model_type %in% names(DECAY_PARAM_PREFIXES))) {
    stop("model_type must be one of: ", paste(names(DECAY_PARAM_PREFIXES), collapse = ", "))
  }
  
  prefixes <- DECAY_PARAM_PREFIXES[[model_type]]
  unlist(lapply(prefixes, function(p) grep(paste0("^", p), names(df), value = TRUE)))
}

# ---- Trial counts -------------------------------------------------------------

# Maps split dataset names to the raw data file that actually contains them.
# Only datasets that were split during parameter estimation need an entry here;
# unsplit datasets (e.g. "VANHASBROECK_2021") are looked up by their own name.
RAW_DATASET_MAP <- list(
  VANHASBROECK_2024_1 = "VANHASBROECK_2024",
  VANHASBROECK_2024_2 = "VANHASBROECK_2024"
)

get_raw_dataset_name <- function(dataset_name) {
  if (dataset_name %in% names(RAW_DATASET_MAP)) {
    return(RAW_DATASET_MAP[[dataset_name]])
  }
  dataset_name
}

#' Load number of trials per participant from the raw data file matching
#' `dataset_name`. Returns a data frame with columns: participant_id, n_trials.
load_trial_counts <- function(dataset_name, raw_dir) {
  raw_dataset_name <- get_raw_dataset_name(dataset_name)
  
  files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)
  
  matching_file <- files[grepl(raw_dataset_name, basename(files))]
  if (length(matching_file) == 0) {
    stop("No raw data file found for dataset: ", raw_dataset_name)
  }
  if (length(matching_file) > 1) {
    stop("Multiple raw data files matched dataset: ", raw_dataset_name)
  }
  
  df <- read.csv(matching_file)
  
  max_trial_per_ppt <- sapply(split(df$trial, df$id), max)
  
  data.frame(
    participant_id = names(max_trial_per_ppt),
    n_trials = as.numeric(max_trial_per_ppt)
  )
}

# ---- Validity filtering --------------------------------------------------------

#' Flag forgetting-steps results as invalid if n_forgetting_steps exceeds
#' the number of trials observed for that participant. Adds one is_valid_*
#' column per n_forget_* column found in `results`.

strip_id_prefix <- function(id) {
  sub("^.*_", "", id)
}

flag_invalid_forgetting_steps <- function(results, trial_counts) {
  results$participant_id <- strip_id_prefix(as.character(results$participant_id))
  trial_counts$participant_id <- as.character(trial_counts$participant_id)
  
  df_merged <- merge(results, trial_counts, by = "participant_id", all.x = TRUE)
  
  if (any(is.na(df_merged$n_trials))) {
    warning("Some participants had no matching trial count and will have NA validity.")
  }
  
  forget_cols <- grep("^n_forget_", names(df_merged), value = TRUE)
  
  for (col in forget_cols) {
    suffix <- sub("^n_forget_", "", col)
    valid_col <- paste0("is_valid_", suffix)
    df_merged[[valid_col]] <- df_merged[[col]] <= df_merged[["n_trials"]]
  }
  
  df_merged
}

# ---- Forgetting-steps computation: Exponential --------------------------------

#' Number of time steps until an exponentially-decaying effect drops below
#' `threshold`, given decay rate `gamma`. Returns Inf if |gamma| >= 1, since
#' the effect never decays (validity is judged later against trial counts,
#' not capped here).
n_forgetting_steps_exponential <- function(gamma, threshold = FORGET_THRESHOLD) {
  if (abs(gamma) >= 1) {
    return(Inf)
  }
  ceiling(log(threshold) / log(abs(gamma)))
}

#' Compute forgetting steps for every decay-parameter column in an exponential
#' model's data frame, returning a results data frame with one pair of columns
#' (raw value, forgetting steps) per decay parameter.
compute_forgetting_steps_exponential <- function(df, threshold = FORGET_THRESHOLD) {
  decay_cols <- get_decay_columns(df, "exponential")
  
  results <- data.frame(participant_id = df[["participant_id"]])
  for (col in decay_cols) {
    gamma_values <- df[[col]]
    results[[col]] <- gamma_values
    results[[paste0("n_forget_", col)]] <- sapply(
      gamma_values, n_forgetting_steps_exponential, threshold = threshold
    )
  }
  results
}

# ---- Forgetting-steps computation: Quasi-hyperbolic ----------------------------

#' Number of time steps until a quasi-hyperbolic-decaying effect drops below
#' `threshold`. Effect is 1 at j=0 (present, undiscounted), and nu^j * kappa
#' for j >= 1. Returns Inf if |nu| >= 1 (effect never decays).
n_forgetting_steps_quasi_hyperbolic <- function(nu, kappa, threshold = FORGET_THRESHOLD) {
  if (abs(nu) >= 1) {
    return(Inf)
  }
  max(ceiling(log(threshold / kappa) / log(abs(nu))), 1)
}

#' Compute forgetting steps for the quasi-hyperbolic model. nu and kappa are
#' paired by matching dimension suffix (e.g. nu_11 with kappa_11), since the
#' equation combines both parameters jointly for a given dimension.
compute_forgetting_steps_quasi_hyperbolic <- function(df, threshold = FORGET_THRESHOLD) {
  nu_cols <- grep("^nu_", names(df), value = TRUE)
  
  results <- data.frame(participant_id = df[["participant_id"]])
  for (nu_col in nu_cols) {
    suffix <- sub("^nu_", "", nu_col)
    kappa_col <- paste0("kappa_", suffix)
    
    if (!(kappa_col %in% names(df))) {
      stop("No matching kappa column found for ", nu_col)
    }
    
    nu_values <- df[[nu_col]]
    kappa_values <- df[[kappa_col]]
    
    results[[nu_col]] <- nu_values
    results[[kappa_col]] <- kappa_values
    results[[paste0("n_forget_", suffix)]] <- mapply(
      n_forgetting_steps_quasi_hyperbolic, nu_values, kappa_values,
      MoreArgs = list(threshold = threshold)
    )
  }
  results
}

# ---- Forgetting-steps computation: Double-exponential --------------------------

#' Number of time steps until a double-exponential-decaying effect drops below
#' `threshold`. Effect is a weighted mixture: omega*gamma^j + (1-omega)*nu^j.
#' No closed-form solution exists for j, so this searches step by step from
#' j=1 up to search_limit. Returns Inf if the threshold is never reached
#' within that search range (validity is judged later against trial counts).
n_forgetting_steps_double_exponential <- function(gamma, nu, omega,
                                                  threshold = FORGET_THRESHOLD,
                                                  search_limit = SEARCH_LIMIT) {
  for (j in 1:search_limit) {
    effect <- omega * gamma^j + (1 - omega) * nu^j
    if (abs(effect) < threshold) {
      return(j)
    }
  }
  Inf  # threshold never reached within search_limit steps
}

#' Compute forgetting steps for the double-exponential model. gamma and nu are
#' paired by matching dimension suffix (e.g. gamma_11 with nu_11), since the
#' equation combines both for a given dimension. omega is a single shared
#' column (not per-dimension) and is used for every pairing.
compute_forgetting_steps_double_exponential <- function(df, threshold = FORGET_THRESHOLD,
                                                        search_limit = SEARCH_LIMIT) {
  gamma_cols <- grep("^gamma_", names(df), value = TRUE)
  omega_values <- df[["omega"]]
  
  results <- data.frame(participant_id = df[["participant_id"]], omega = omega_values)
  
  for (gamma_col in gamma_cols) {
    suffix <- sub("^gamma_", "", gamma_col)
    nu_col <- paste0("nu_", suffix)
    
    if (!(nu_col %in% names(df))) {
      stop("No matching nu column found for ", gamma_col)
    }
    
    gamma_values <- df[[gamma_col]]
    nu_values <- df[[nu_col]]
    
    results[[gamma_col]] <- gamma_values
    results[[nu_col]] <- nu_values
    results[[paste0("n_forget_", suffix)]] <- mapply(
      n_forgetting_steps_double_exponential, gamma_values, nu_values, omega_values,
      MoreArgs = list(threshold = threshold, search_limit = search_limit)
    )
  }
  results
}

# ==============================================================================
# Validity summary report: valid/invalid counts per results CSV
# ==============================================================================

results_dir <- "scripts/results/forgetting_steps"
files <- list.files(results_dir, pattern = "_forgetting_steps\\.csv$", full.names = TRUE)

validity_summary <- do.call(rbind, lapply(files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  valid_cols <- grep("^is_valid_", names(df), value = TRUE)
  
  do.call(rbind, lapply(valid_cols, function(col) {
    is_valid <- as.logical(df[[col]])
    v_count   <- sum(is_valid, na.rm = TRUE)
    inv_count <- sum(!is_valid, na.rm = TRUE)
    
    data.frame(
      file = basename(f),
      parameter = sub("^is_valid_", "", col),
      n_valid = v_count,
      n_invalid = inv_count,
      # Make sure digits = 0 is safely inside the round function:
      percentage_valid = round((v_count / (v_count + inv_count)) * 100, digits = 0)
    )
  }))
}))

print(validity_summary)

# ==============================================================================
# Run: VANHASBROECK datasets only, all model types
# ==============================================================================

#' Run the full forgetting-steps pipeline for one dataset/model_type:
#' compute forgetting steps, flag validity, print summaries (all participants
#' and valid-only), and save the results as a CSV.
#' Returns the results data frame (with validity columns) invisibly.
run_forgetting_steps <- function(dataset_name, model_type, estimation_data, raw_dir,
                                 output_dir = "scripts/results/forgetting_steps") {
  
  df <- estimation_data[[dataset_name]][[model_type]]
  if (is.null(df)) {
    stop("No estimation data found for ", dataset_name, " / ", model_type)
  }
  
  compute_fn <- COMPUTE_FUNCTIONS[[model_type]]
  results <- compute_fn(df)
  
  trial_counts <- load_trial_counts(dataset_name, raw_dir)
  results <- flag_invalid_forgetting_steps(results, trial_counts)
  
  n_forget_cols <- grep("^n_forget_", names(results), value = TRUE)
  valid_cols <- grep("^is_valid_", names(results), value = TRUE)
  
  cat("=== ", dataset_name, " / ", model_type, " (all participants) ===\n", sep = "")
  print(summary(results[n_forget_cols]))
  
  # keep only rows valid across every decay parameter (all is_valid_* columns TRUE)
  is_fully_valid <- Reduce(`&`, results[valid_cols])
  only_valid_ppt <- results[is_fully_valid, ]
  
  cat("=== ", dataset_name, " / ", model_type, " (valid participants only) ===\n", sep = "")
  print(summary(only_valid_ppt[n_forget_cols]))
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  out_path <- file.path(output_dir, paste0(dataset_name, "_", model_type, "_forgetting_steps.csv"))
  write.csv(results, out_path, row.names = FALSE)
  cat("Saved to:", out_path, "\n")
  
  invisible(results)
}

# ==============================================================================
# PLOTTING
# ==============================================================================
library(ggplot2)

#' Given the number of decay dimensions found for this dataset/model_type
#' and a dimension code (e.g. "11", "22"), return a human-readable label.
#' Single-dimension datasets are labeled "Valence"; two-dimension datasets
#' are labeled by affect valence (positive/negative), based on dimension code.
get_parameter_label <- function(dimension_code, n_dimensions) {
  if (n_dimensions == 1) {
    return("Valence")
  }
  
  two_dim_labels <- list("11" = "Positive affect", "22" = "Negative affect")
  if (dimension_code %in% names(two_dim_labels)) {
    return(two_dim_labels[[dimension_code]])
  }
  
  dimension_code  # fallback if an unexpected code shows up
}

#' Load previously computed forgetting-steps results from CSV, then create
#' and save a boxplot (valid participants only) for one dataset/model_type.
plot_forgetting_steps <- function(dataset_name, model_type,
                                  results_dir = "scripts/results/forgetting_steps",
                                  figures_dir = "scripts/results/figures") {
  
  # ---- load ----
  file_path <- file.path(results_dir, paste0(dataset_name, "_", model_type, "_forgetting_steps.csv"))
  if (!file.exists(file_path)) {
    stop("No saved results file found: ", file_path)
  }
  results <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # ---- prepare data ----
  n_forget_cols <- grep("^n_forget_", names(results), value = TRUE)
  n_dimensions <- length(n_forget_cols)
  
  valid_cols <- grep("^is_valid_", names(results), value = TRUE)
  
  # read.csv writes/reads logicals as "TRUE"/"FALSE" strings in some edge cases;
  # force back to logical to guarantee Reduce(`&`, ...) works correctly
  for (col in valid_cols) {
    results[[col]] <- as.logical(results[[col]])
  }
  
  is_fully_valid <- Reduce(`&`, results[valid_cols])
  valid_results <- results[is_fully_valid, ]
  n_valid <- nrow(valid_results)
  
  long_data <- do.call(rbind, lapply(n_forget_cols, function(col) {
    raw_suffix <- sub("^n_forget_", "", col)
    dimension_code <- sub("^[a-z]+_", "", raw_suffix)
    label <- get_parameter_label(dimension_code, n_dimensions)
    
    data.frame(
      participant_id = valid_results$participant_id,
      parameter = label,
      n_forgetting_steps = valid_results[[col]]
    )
  }))
  
  # ---- plot ----
  plot_title <- paste0(dataset_name, " — ", model_type)
  plot_subtitle <- paste0("n = ", n_valid)
  
  p <- ggplot(long_data, aes(x = parameter, y = n_forgetting_steps)) +
    geom_boxplot(fill = "#4C72B0", alpha = 0.6, outlier.shape = 1) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Decay parameter",
      y = "Forgetting steps"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
  
  # ---- save ----
  if (!dir.exists(figures_dir)) {
    dir.create(figures_dir, recursive = TRUE)
  }
  out_path <- file.path(figures_dir, paste0(dataset_name, "_", model_type, "_n", n_valid, "_forgetting_steps.png"))
  ggsave(out_path, plot = p, width = 7, height = 5, dpi = 300)
  cat("Saved plot to:", out_path, "\n")
  
  invisible(p)
}

# ==============================================================================
# Forgetting Steps Analysis: Temporal Discounting Models
# ------------------------------------------------------------------------------
# Computes the number of "forgetting steps" (time steps until a stimulus's
# effect decays below a chosen threshold) from parameter estimates of
# exponential, quasi-hyperbolic, and double-exponential discounting models.
# ==============================================================================

# ---- Configuration ----------------------------------------------------------

ESTIMATE_DIR      <- "scripts/results/estimation"
MODEL_TYPES       <- c("double_exponential", "quasi_hyperbolic", "exponential")
FORGET_THRESHOLD  <- 0.05   # effect considered "negligible" below this proportion
MAX_T             <- 100    # cap on forgetting steps (e.g. max experiment timepoints)
NEAR_UNITY_CUTOFF <- 0.95   # |decay rate| at/above this treated as effectively non-decaying

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

# ---- Forgetting-steps computation: Exponential --------------------------------

#' Number of time steps until an exponentially-decaying effect drops below
#' `threshold`, given decay rate `gamma`. Capped at `max_t` for gammas so
#' close to 1 that decay is negligible within the observed experiment horizon.
n_forgetting_steps_exponential <- function(gamma, threshold = FORGET_THRESHOLD,
                                           max_t = MAX_T,
                                           near_unity_cutoff = NEAR_UNITY_CUTOFF) {
  if (abs(gamma) >= near_unity_cutoff) {
    return(max_t)
  }
  ceiling(log(threshold) / log(abs(gamma)))
}

#' Compute forgetting steps for every decay-parameter column in an exponential
#' model's data frame, returning a results data frame with one pair of columns
#' (raw value, forgetting steps) per decay parameter.
compute_forgetting_steps_exponential <- function(df, threshold = FORGET_THRESHOLD,
                                                 max_t = MAX_T) {
  decay_cols <- get_decay_columns(df, "exponential")
  
  results <- data.frame(participant_id = df[["participant_id"]])
  for (col in decay_cols) {
    gamma_values <- df[[col]]
    results[[col]] <- gamma_values
    results[[paste0("n_forget_", col)]] <- sapply(
      gamma_values, n_forgetting_steps_exponential,
      threshold = threshold, max_t = max_t
    )
  }
  results
}

# ---- Forgetting-steps computation: Quasi-hyperbolic ----------------------------

#' Number of time steps until a quasi-hyperbolic-decaying effect drops below
#' `threshold`. Effect is 1 at j=0 (present, undiscounted), and nu^j * kappa
#' for j >= 1. Capped at `max_t` for nu so close to 1 that decay is negligible
#' within the observed experiment horizon.
n_forgetting_steps_quasi_hyperbolic <- function(nu, kappa, threshold = FORGET_THRESHOLD,
                                                max_t = MAX_T,
                                                near_unity_cutoff = NEAR_UNITY_CUTOFF) {
  if (abs(nu) >= near_unity_cutoff) {
    return(max_t)
  }
  
  n_steps <- ceiling(log(threshold / kappa) / log(abs(nu)))
  min(max(n_steps, 1), max_t)
}

#' Compute forgetting steps for the quasi-hyperbolic model. nu and kappa are
#' paired by matching dimension suffix (e.g. nu_11 with kappa_11), since the
#' equation combines both parameters jointly for a given dimension.
compute_forgetting_steps_quasi_hyperbolic <- function(df, threshold = FORGET_THRESHOLD,
                                                      max_t = MAX_T) {
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
      MoreArgs = list(threshold = threshold, max_t = max_t)
    )
  }
  results
}

# ---- Forgetting-steps computation: Double-exponential --------------------------

#' Number of time steps until a double-exponential-decaying effect drops below
#' `threshold`. Effect is a weighted mixture: omega*gamma^j + (1-omega)*nu^j.
#' No closed-form solution exists for j, so this searches step by step from
#' j=1 up to max_t, relying on the effect being monotonically decreasing
#' (true as long as |gamma| < 1 and |nu| < 1).
n_forgetting_steps_double_exponential <- function(gamma, nu, omega,
                                                  threshold = FORGET_THRESHOLD,
                                                  max_t = MAX_T) {
  for (j in 1:max_t) {
    effect <- omega * gamma^j + (1 - omega) * nu^j
    if (abs(effect) < threshold) {
      return(j)
    }
  }
  max_t  # threshold never reached within max_t steps
}

#' Compute forgetting steps for the double-exponential model. gamma and nu are
#' paired by matching dimension suffix (e.g. gamma_11 with nu_11), since the
#' equation combines both for a given dimension. omega is a single shared
#' column (not per-dimension) and is used for every pairing.
compute_forgetting_steps_double_exponential <- function(df, threshold = FORGET_THRESHOLD,
                                                        max_t = MAX_T) {
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
      MoreArgs = list(threshold = threshold, max_t = max_t)
    )
  }
  results
}


# ==============================================================================
# Run: Exponential model, single dataset
# ==============================================================================

data <- load_estimation_data(ESTIMATE_DIR, MODEL_TYPES)

df_exp <- data[["VANHASBROECK_2022"]][["exponential"]]
results_exp <- compute_forgetting_steps_exponential(df_exp)

n_forget_cols_exp <- grep("^n_forget_", names(results_exp), value = TRUE)
summary(results_exp[n_forget_cols_exp])
#boxplot(results_exp[n_forget_cols_exp])
#View(results_exp)

# ==============================================================================
# Run: Quasi-hyperbolic model, single dataset
# ==============================================================================

df_qh <- data[["VANHASBROECK_2022"]][["quasi_hyperbolic"]]
results_qh <- compute_forgetting_steps_quasi_hyperbolic(df_qh)

n_forget_cols_qh <- grep("^n_forget_", names(results_qh), value = TRUE)
summary(results_qh[n_forget_cols_qh])
#boxplot(results_qh[n_forget_cols_qh])
#View(results_qh)

# ==============================================================================
# Run: Double-exponential model, single dataset
# ==============================================================================

df_dexp <- data[["VANHASBROECK_2022"]][["double_exponential"]]
results_dexp <- compute_forgetting_steps_double_exponential(df_dexp)

n_forget_cols_dexp <- grep("^n_forget_", names(results_dexp), value = TRUE)
summary(results_dexp[n_forget_cols_dexp])
#boxplot(results_dexp[n_forget_cols_dexp])
#View(results_dexp)
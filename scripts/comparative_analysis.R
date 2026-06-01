################################################################################
# PURPOSE:
# Compare exponential, quasi-hyperbolic, and double-exponential discounting
# models using AIC/BIC from estimation output.
#
# Steps:
#   1. Load data (mock or real)
#   2. Find best model per participant
#   3. Summarize % of participants each model won
#   4. Pairwise comparisons between specific models
################################################################################

# ── 1. DATA ───────────────────────────────────────────────────────────────────

# Mock data simulating what the estimation output will look like
# TO DELETE ONCE WE HAVE REAL PARAMETERS
set.seed(123)
n_participants <- 10
models <- c("exponential", "quasi_hyperbolic", "double_exponential")

long_2021 <- do.call(rbind, lapply(models, function(m) {
  data.frame(
    participant_id = paste0("ppt_", 1:n_participants),
    model          = m,
    aic            = rnorm(n_participants, mean = -100, sd = 20),
    bic            = rnorm(n_participants, mean = -95,  sd = 20)
  )
}))

# REAL DATA - uncomment once parameters are estimated
#input_dir <- file.path("scripts", "results", "estimation")
#year <- "2021"
#long_2021 <- do.call(rbind, lapply(models, function(m) {
#  path <- file.path(input_dir, paste0("VANHASBROECK_", year, "_", m, ".rds"))
#  df <- readRDS(path)
#  df$model <- m
#  df[, c("participant_id", "model", "aic", "bic")]
#}))


# ── 2. BEST MODEL PER PARTICIPANT ─────────────────────────────────────────────

# For each participant, find the model with the lowest AIC (or BIC)
# Returns one row per participant showing their best model
metric      <- "aic"   # switch to "bic" to repeat analysis with BIC
best_models <- do.call(rbind, lapply(split(long_2021, long_2021$participant_id), function(df) {
  df[which.min(df[[metric]]), ]
}))


# ── 3. SUMMARY: % OF PARTICIPANTS EACH MODEL WON ─────────────────────────────

# Counts how many (and what %) of participants were best described by each model
counts <- table(best_models$model)
model_summary <- data.frame(
  model          = names(counts),
  n_best         = as.numeric(counts),
  percentage_best = round(100 * as.numeric(counts) / nrow(best_models), 1)
)
print(model_summary)


# ── 4. PAIRWISE COMPARISONS ───────────────────────────────────────────────────

# Head-to-head comparison between two specific models, per participant
# Lower AIC/BIC = better fitting model
run_pairwise <- function(long_df, model_a, model_b, metric = "aic") {
  
  # Keep only the two models being compared
  sub_df <- long_df[long_df$model %in% c(model_a, model_b), ]
  
  # Reshape to wide: one row per participant, one column per model
  wide_df <- reshape(sub_df[, c("participant_id", "model", metric)],
                     idvar     = "participant_id",
                     timevar   = "model",
                     direction = "wide")
  
  # Clean up column names (e.g. "aic.exponential" -> "exponential")
  names(wide_df) <- gsub(paste0(metric, "."), "", names(wide_df), fixed = TRUE)
  
  # Determine winner per participant (lower AIC/BIC wins)
  wide_df$winner <- ifelse(wide_df[[model_a]] < wide_df[[model_b]], model_a, model_b)
  
  # Summary: % of participants each model won
  counts  <- table(wide_df$winner)
  summary <- data.frame(
    model      = names(counts),
    n_best     = as.numeric(counts),
    percentage = round(100 * as.numeric(counts) / nrow(wide_df), 1)
  )
  
  list(per_person = wide_df, summary = summary)
}

# Comparison 1: quasi-hyperbolic vs double-exponential
pw_qh_de  <- run_pairwise(long_2021, "quasi_hyperbolic", "double_exponential", metric)
print(pw_qh_de$summary)

# Comparison 2: exponential vs double-exponential
pw_exp_de <- run_pairwise(long_2021, "exponential", "double_exponential", metric)
print(pw_exp_de$summary)
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

# Define the models to be compared to each other
models <- c("exponential", "quasi_hyperbolic", "double_exponential")

# Define the datasets to compare the results for
datasets <- c(
  "VANHASBROECK_2021",
  "VANHASBROECK_2022",
  "VANHASBROECK_2024_1",
  "VANHASBROECK_2024_2",
  "NIEMEIJER_2022"
)

# Define the metrics to use for the comparison
metrics <- c("aic", "bic")

# Load the results for each of the datasets
input_dir <- file.path("scripts", "results", "estimation")
long <- lapply(
  datasets,
  function(x) do.call(
    rbind, 
    lapply(
      models, 
      function(m) {
        path <- file.path(input_dir, paste0(x, "_", m, ".csv"))
        df <- read.csv(path)
        df$model <- m
        return(df[, c("participant_id", "model", metrics)])
      }
    )
  )
) |>
  `names<-` (datasets) 


# ── 2. BEST MODEL PER PARTICIPANT ─────────────────────────────────────────────

# For each participant, find the model with the lowest AIC (or BIC)
# Returns one row per participant showing their best model.
#
# Not the most beautiful result (three implicit loops in one), but works just 
# fine for our purposes. List structure is as follows:
#   - Metric
#       - Dataset
#           - Best model per participant
best <- lapply(
  metrics,
  function(x) lapply(
    names(long),
    function(y) do.call(
      rbind,
      lapply(
        split(long[[y]], long[[y]]$participant_id), 
        function(df) df[which.min(df[[x]]), ]
      )
    )
  ) |>
    `names<-` (names(long))
) |>
  `names<-` (metrics)


# ── 3. SUMMARY: % OF PARTICIPANTS EACH MODEL WON ─────────────────────────────

# Counts how many (and what %) of participants were best described by each model
#
# List structure is the same as before, specifically
#   - Metric
#       - Dataset
#           - Number/Percentage of time a model performed best
# The results will be merged in one dataset, however, somewhat easing interpreting
# the results
counts <- lapply(
  metrics, 
  function(x) do.call(
    rbind, 
    lapply(
      names(long), 
      function(y) {
        count <- table(best[[x]][[y]]$model)
        
        return(
          data.frame(
            metric = x, 
            dataset = y,
            model = names(count),
            n_best = as.numeric(count),
            percentage_best = round(100 * as.numeric(count) / sum(count), 1)
          )
        )
      }   
    )   
  )
) 
counts <- do.call(rbind, counts)



# ── 4. PAIRWISE COMPARISONS ───────────────────────────────────────────────────

# Head-to-head comparison between two specific models, per participant
# Lower AIC/BIC = better fitting model
#
# Importantly, this pairwise comparison is done for each dataset separately and
# the function is used in this way
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
pw_qh_de_aic  <- run_pairwise(long_2021, "quasi_hyperbolic", "double_exponential", metric)
pw_qh_de_bic  <- run_pairwise(long_2021, "quasi_hyperbolic", "double_exponential", metric = "bic")
print(pw_qh_de_aic$summary)
print(pw_qh_de_bic$summary)

# Comparison 2: exponential vs double-exponential
pw_exp_de_aic <- run_pairwise(long_2021, "exponential", "double_exponential", metric)
pw_exp_de_bic <- run_pairwise(long_2021, "exponential", "double_exponential", metric = "bic")
print(pw_exp_de_aic$summary)
print(pw_exp_de_bic$summary)

# Comparison 3: quasi-hyperbolic vs exponential
pw_exp_qu_aic <- run_pairwise(long_2021, "quasi_hyperbolic", "exponential", metric)
pw_exp_qu_bic <- run_pairwise(long_2021, "quasi_hyperbolic", "exponential", metric = "bic")
print(pw_exp_qu_aic$summary)
print(pw_exp_qu_bic$summary)


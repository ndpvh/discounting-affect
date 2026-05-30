################################################################################
# PURPOSE:
#
# Estimate the three discounting models (exponential, quasi-hyperbolic, and
# double-exponential) on each participant's data from the three datasets:
#   - VANHASBROECK_2021  (d = 1, k = 3)
#   - VANHASBROECK_2022  (d = 3, k = 2)
#   - VANHASBROECK_2024  (d = 3, k = 1)
#
# For each dataset, three data.frames are produced (one per model) where every
# row is a participant and every column is an estimated parameter (plus AIC,
# BIC, autocorrelation, bias, and the raw SSE objective value).
#
# The optimisation strategy mirrors the one used in scripts/recovery.R:
#   Step 1 – DEoptim  (global search, finds a good region)
#   Step 2 – nloptr / BOBYQA  (local refinement from DEoptim's best solution)
#
################################################################################

devtools::load_all()

# ── packages needed beyond the discounting package ───────────────────────────
library(DEoptim)
library(nloptr)


################################################################################
# SHARED OPTIMIZER FUNCTION
#
# This is copied directly from recovery.R. The function:
#   1. Runs DEoptim.
#   2. Takes DEoptim's best solution as the starting point for nloptr/BOBYQA,
#      which then fine-tunes the result.
#
################################################################################

optimizer <- function(obj,
                      lower,
                      upper,
                      algorithm  = "NLOPT_LN_BOBYQA",
                      maxeval    = 1e3,
                      ftol_abs   = 1e-15,
                      xtol_abs   = 1e-15,
                      print_level = 0,
                      ...) {

  # Step 1: DEoptim global search
  result_de <- DEoptim::DEoptim(
    obj,
    lower,
    upper,
    control = DEoptim::DEoptim.control(...)
  )

  # Extract the best parameter vector found by DEoptim
  x0 <- result_de$optim$bestmem

  # Step 2: nloptr local refinement
  result_nl <- nloptr::nloptr(
    x0,
    obj,
    lb   = lower,
    ub   = upper,
    opts = list(
      algorithm   = algorithm,
      maxeval     = maxeval,
      ftol_abs    = ftol_abs,
      xtol_abs    = xtol_abs,
      print_level = print_level
    )
  )

  # Return in the format expected by discounting::fit()
  list(
    parameters = result_nl$solution,
    objective  = result_nl$objective
  )
}

################################################################################
# NA-AWARE OBJECTIVE FUNCTION
#
# Why this is needed:
#   In the 2021 dataset, happiness (Y) is only measured on some trials.
#   Rows with no happiness rating have Y = NA. The standard objective_function()
#   computes sum((Y - Y_hat)^2) over all rows. When Y is NA it returns
#   NaN, and sum(NaN) = NaN, so the optimizer receives NaN for every
#   parameter set it tries and cannot make progress.
#
# What this function does:
#   1. Runs predict() over ALL rows.
#   2. Computes SSE only on rows where Y is actually observed (not NA).
#
################################################################################
 
na_aware_objective_function <- function(model, data, parameters, dynamics) {
 
  # Bounds check - return a large penalty if parameters are out of range
  bounds <- get_bounds(model, dynamics = dynamics, parameters_only = TRUE)
  if (any(parameters < bounds$lower | parameters > bounds$upper)) {
    return(Inf)
  }
 
  # Fill the model with the candidate parameters
  model <- fill(model, parameters, dynamics = dynamics, parameters_only = TRUE)
 
  # Predict over the FULL time series (all rows, including NA ones)
  prediction <- predict(model, data)
 
  # Identify rows where Y is fully observed (complete.cases returns TRUE
  # for rows that have no NA in any column)
  observed <- complete.cases(data@Y)
 
  # SSE on observed rows only
  residuals <- data@Y[observed, , drop = FALSE] -
               prediction@Y[observed, , drop = FALSE]
 
  sum(residuals^2)
}



################################################################################
# ESTIMATE ONE PARTICIPANT
#
# estimate_participant() wraps a single call to fit() and assembles the results
# into a named numeric vector that can later become one row of a data.frame.
# The optimizer the optimizer argument passed to fit() wraps
# na_aware_objective_function() instead of letting fit() use its internal
# objective_function() to deal with the 2021 dataset.
#
# Arguments:
#   ds          – a dataset object (loaded from an RDS file)
#   model_empty – an empty model of the right dimensionality, e.g.
#                 exponential(d = 1, k = 3)
#   dynamics    – structure of the forgetting matrices ("isotropic" here)
#   covariance  – structure of the residual covariance ("symmetric" here)
#   ...         – extra arguments forwarded to optimizer() via fit()
#
################################################################################

estimate_participant <- function(ds,
                                 model_empty,
                                 dynamics   = "isotropic",
                                 covariance = "symmetric",
                                 ...) {
  tryCatch({
 
    fitobj <- fit(
      model_empty,
      ds,
      dynamics   = dynamics,
      covariance = covariance,
 
      # fit() accepts a function here instead of a string like "DEoptim".
      # fit() calls it as: optimizer(obj, lower, upper, ...)
      # We ignore the obj that fit() would pass (which uses the standard SSE)
      # and substitute the NA-aware version instead.
      optimizer = function(obj, lower, upper, ...) {
        na_obj <- function(x) {
          na_aware_objective_function(model_empty, ds, x, dynamics)
        }
        optimizer(na_obj, lower, upper, ...)
      },
      ...
    )
 
    params <- fitobj$parameters
 
    stats <- c(
      aic             = aic(fitobj),
      bic             = bic(fitobj),
      autocorrelation = autocorrelation(fitobj),
      bias            = bias(fitobj),
      objective_sse   = fitobj$objective
    )
 
    c(params, stats)
 
  }, error = function(e) {
    message("  Estimation failed: ", conditionMessage(e))
    NULL
  })
}
 


################################################################################
# PROCESS ONE ENTIRE DATASET FOLDER
#
# run_estimation() does the following for ONE dataset folder:
#   1. Lists all .rds files in the folder.
#   2. For each model type (exponential, quasi-hyperbolic, double-exponential)
#      loops over every participant and calls estimate_participant().
#   3. Stacks the per-participant rows into a data.frame.
#   4. Returns a named list with three data.frames (one per model).
#
# Arguments:
#   folder     – path to the folder that holds the per-participant RDS files
#   d          – number of dependent variables (outcome dimensions)
#   k          – number of independent variables (predictors)
#   ...        – extra arguments forwarded to optimizer via estimate_participant
################################################################################

run_estimation <- function(folder, d, k, ...) {

  # Find all RDS files
  rds_files <- list.files(folder, pattern = "\\.rds$", full.names = TRUE,
                          ignore.case = TRUE)

  if (length(rds_files) == 0) {
    stop("No RDS files found in: ", folder)
  }

  message("Found ", length(rds_files), " participants in ", basename(folder))

  # Define the three empty models for this dataset's dimensionality
  # fit() will fill them in during estimation.
  models <- list(
    exponential       = exponential(d = d, k = k),
    quasi_hyperbolic  = quasi_hyperbolic(d = d, k = k),
    double_exponential = double_exponential(d = d, k = k)
  )

  # Loop over model types
  results <- lapply(names(models), function(model_name) {

    message("\n  Fitting model: ", model_name)

    model_empty <- models[[model_name]]

    # Loop over participants
    rows <- lapply(seq_along(rds_files), function(idx) {

      rds_path       <- rds_files[idx]
      participant_id <- tools::file_path_sans_ext(basename(rds_path))

      message("    Participant ", idx, " / ", length(rds_files),
              "  (", participant_id, ")")

      # Load the pre-saved dataset object
      ds <- readRDS(rds_path)

      # Run estimation
      row_values <- estimate_participant(
        ds          = ds,
        model_empty = model_empty,
        ...
      )

      if (is.null(row_values)) {
        # Estimation failed – build an all-NA row so the data.frame stays
        # rectangular. We don't know column names yet, so we signal with NULL
        # and handle it after the loop.
        return(list(id = participant_id, values = NULL))
      }

      list(id = participant_id, values = row_values)
    })

    # Determine column names from the first successful row
    # (needed to build NA rows for failed participants)
    first_ok <- Filter(function(r) !is.null(r$values), rows)

    if (length(first_ok) == 0) {
      warning("All participants failed for model ", model_name,
              " in folder ", basename(folder))
      return(NULL)
    }

    col_names <- names(first_ok[[1]]$values)
    na_row    <- setNames(rep(NA_real_, length(col_names)), col_names)

    # Stack rows into a data.frame
    df_rows <- lapply(rows, function(r) {
      vals <- if (is.null(r$values)) na_row else r$values
      data.frame(
        participant_id = r$id,
        t(vals),           # transpose so one participant = one row
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })

    df <- do.call(rbind, df_rows)
    rownames(df) <- NULL
    df
  })

  names(results) <- names(models)
  results
}


################################################################################
# RUN ESTIMATION FOR EACH DATASET
#
# Here we call run_estimation() three times, once per dataset.
# The DEoptim / nloptr settings are passed as ... arguments and mirror
# exactly those used in recovery.R.
#
# Key settings (same as recovery.R):
#   itermax  = 1e3   – DEoptim runs 1000 generations
#   NP       = 150   – 150 candidate solutions per generation
#   CR       = 0.75  – crossover probability
#   strategy = 6     – DEoptim mutation/crossover strategy
#   p        = 0.8   – parameter for strategy 6
#   reltol   = 1e-15 – relative convergence tolerance for DEoptim
#   steptol  = 100   – stop if best value unchanged for 100 generations
#   trace    = FALSE – suppress DEoptim's iteration-by-iteration output
#   maxeval  = 1e5   – nloptr maximum evaluations
#   xtol_abs = 1e-20 – nloptr absolute tolerance on parameters
#   ftol_abs = 1e-20 – nloptr absolute tolerance on objective value
################################################################################

# Common optimizer settings
optim_settings <- list(
  # DEoptim control arguments
  itermax  = 1e3,
  NP       = 150,
  CR       = 0.75,
  strategy = 6,
  p        = 0.8,
  reltol   = 1e-15,
  steptol  = 100,
  trace    = FALSE,
  # nloptr arguments
  maxeval  = 1e5,
  xtol_abs = 1e-20,
  ftol_abs = 1e-20
)

# Dataset paths
# The standard layout assumed here:
#   scripts/data/VANHASBROECK_2021_per_participant/
#   scripts/data/VANHASBROECK_2022_per_participant/
#   scripts/data/VANHASBROECK_2024_per_participant/

path_2021 <- file.path("scripts", "data", "VANHASBROECK_2021_per_participant")
path_2022 <- file.path("scripts", "data", "VANHASBROECK_2022_per_participant")
path_2024 <- file.path("scripts", "data", "VANHASBROECK_2024_per_participant")

# VANHASBROECK_2021  (d = 1, k = 3)
# One dependent variable (happiness) and three predictors (cr, ev, rpe).
# Because d = 1 the covariance "matrix" is just a single variance, so
# symmetric and isotropic covariance are identical here.
message("\n========== VANHASBROECK 2021 (d=1, k=3) ==========")
results_2021 <- do.call(
  run_estimation,
  c(list(folder = path_2021, d = 1, k = 3), optim_settings)
)

#  VANHASBROECK_2022  (d = 3, k = 2)
# Three dependent variables (PA, NA, valence) and two predictors
# (outcome, total).
message("\n========== VANHASBROECK 2022 (d=3, k=2) ==========")
results_2022 <- do.call(
  run_estimation,
  c(list(folder = path_2022, d = 3, k = 2), optim_settings)
)

# VANHASBROECK_2024  (d = 3, k = 1)
# Three dependent variables (PA, NA, valence) and one predictor (outcome).
message("\n========== VANHASBROECK 2024 (d=3, k=1) ==========")
results_2024 <- do.call(
  run_estimation,
  c(list(folder = path_2024, d = 3, k = 1), optim_settings)
)


################################################################################
# SAVE RESULTS
#
# Each results_* object is a named list with three data.frames:
#   $exponential
#   $quasi_hyperbolic
#   $double_exponential
#
# We save them both as RDS and as CSV.
################################################################################

output_dir <- file.path("scripts", "results", "estimation")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

datasets <- list(
  "2021" = results_2021,
  "2022" = results_2022,
  "2024" = results_2024
)

for (year in names(datasets)) {
  dataset_results <- datasets[[year]]

  for (model_name in names(dataset_results)) {
    df <- dataset_results[[model_name]]

    if (is.null(df)) next   # skip if all participants failed

    base_name <- paste0("VANHASBROECK_", year, "_", model_name)

    # Save as RDS (preserves column types perfectly)
    saveRDS(df, file.path(output_dir, paste0(base_name, ".rds")))

    # Save as CSV (human-readable)
    write.csv(df, file.path(output_dir, paste0(base_name, ".csv")),
              row.names = FALSE)

    message("Saved: ", base_name)
  }
}

message("\nAll done! ", output_dir)
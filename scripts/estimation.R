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

fit_participant <- function(data,
                            model,
                            dynamics   = "isotropic",
                            covariance = "symmetric",
                            ...) {

  # Wrapped in a tryCatch so that potential problems are caught and dealt with
  tryCatch(
    {
      # Use the dataset "data" and an empty model object "model" to peform the 
      # estimation, using the optimizer defined above.
      fitobj <- fit(
        model,
        data,
        dynamics   = dynamics,
        covariance = covariance,
        optimizer = optimizer,
        ...
      )

      # Extract the parameters and the statistics computed based on the 
      # fitobj. 
      params <- fitobj$parameters  
      stats <- c(
        aic             = aic(fitobj),
        bic             = bic(fitobj),
        autocorrelation = autocorrelation(fitobj),
        bias            = bias(fitobj),
        objective_sse   = fitobj$objective
      )
  
      return(c(params, stats))
    }, 
    error = function(e) {
      message("  Estimation failed: ", conditionMessage(e))
      return(NULL)
    }
  )
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
#   ...        – extra arguments forwarded to optimizer via estimate_participant
################################################################################

run_estimation <- function(folder, 
                           base_name, 
                           models,
                           ...) {

  # Find all RDS files
  rds_files <- list.files(
    folder, pattern = "\\.rds$", 
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(rds_files) == 0) {
    stop("No RDS files found in: ", folder)
  }
  message("Found ", length(rds_files), " participants in ", basename(folder))

  # Loop over the different model types and perform the estimation for these.
  # Note that looping across models is done in sequence while the estimation per
  # participant is done in parallel
  results <- lapply(
    names(models), 
    function(model_name) {
      message("\n  Fitting model: ", model_name)

      # Loop over participants
      rows <- parallel::mclapply(
        seq_along(rds_files), 
        function(idx) {
          # Load the dataset object with the data for this participant
          rds_path       <- rds_files[idx]
          ds <- readRDS(rds_path)

          # Provide some feedback to the user
          participant_id <- tools::file_path_sans_ext(basename(rds_path))
          message(
            "    Participant ", 
            idx, 
            " / ", 
            length(rds_files),
            "  (", 
            participant_id, 
            ")"
          )

          # Define an empty version of the model to be estimated with the
          # dimensionality of this dataset: fit() will fill this empty model 
          # during estimation. Here, we infer the dimensionality from the data 
          # directly.
          d <- ncol(ds@Y)
          k <- ncol(ds@X)
          model_empty <- models[[model_name]](d = d, k = k)        

          # Run estimation
          row_values <- fit_participant(
            data  = ds,
            model = model_empty,
            ...
          )

          if (is.null(row_values)) {
            # Estimation failed – build an all-NA row so the data.frame stays
            # rectangular. We don't know column names yet, so we signal with NULL
            # and handle it after the loop.
            return(list(id = participant_id, values = NULL))
          }

          return(list(id = participant_id, values = row_values))
        },
        mc.cores = ifelse(
          Sys.info()["sysname"] == "Windows",
          1,
          round(parallel::detectCores() / 2) - 1  # Optimized for Niels' Mac/Linux system
        )
      )

      # Determine column names from the first successful row (needed to build NA 
      # rows for failed participants)
      first_ok <- Filter(
        function(r) !is.null(r$values), 
        rows
      )

      if (length(first_ok) == 0) {
        warning(
          "All participants failed for model ", 
          model_name,
          " in folder ", 
          basename(folder)
        )

        return(NULL)
      }

      col_names <- names(first_ok[[1]]$values)
      na_row    <- setNames(rep(NA_real_, length(col_names)), col_names)

      # Stack rows into a data.frame
      df_rows <- lapply(
        rows, 
        function(r) {
          vals <- if (is.null(r$values)) na_row else r$values
          
          return(
            data.frame(
              participant_id = r$id,
              t(vals),           # transpose so one participant = one row
              check.names = FALSE,
              stringsAsFactors = FALSE
            )
          )
        }
      )
      df <- do.call(rbind, df_rows) |>
        `rownames<-` (NULL)

      # Save the results
      write.csv(
        df, 
        file.path(
          "scripts", 
          "results",
          "estimation", 
          paste0(base_name, "_", model_name, ".csv")
        ),
        row.names = FALSE
      )

      # Return nothing
      return(NULL)
    }
  )

  return(NULL)
}



################################################################################
# RUN ESTIMATION FOR EACH DATASET
#
# PLEASE REMEMBER TO RUN THE "processing_data.R" script
# BEFORE THE FOLLOWING ESTIMATION!
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
  # General settings
  dynamics   = "isotropic",
  covariance = "symmetric",

  # DEoptim control arguments
  itermax    = 1e3,
  NP         = 150,
  CR         = 0.75,
  strategy   = 6,
  p          = 0.8,
  reltol     = 1e-15,
  steptol    = 100,
  trace      = FALSE,

  # nloptr arguments
  maxeval    = 1e5,
  xtol_abs   = 1e-20,
  ftol_abs   = 1e-20
)

# Define the names of the datasets considered in our analysis. 
datasets <- c(
  "VANHASBROECK_2021",
  "VANHASBROECK_2022",
  "VANHASBROECK_2024",
  "NIEMEIJER_2022"
)

# Define the functions that contain the models of choice. Importantly, we don't
# provide it with a dimensionality yet: This is automatically done under the 
# hood.
models <- list(
  exponential        = exponential,
  quasi_hyperbolic   = quasi_hyperbolic,
  double_exponential = double_exponential
)

# Loop over these datasets and perform the estimation. Note that we asssume a 
# same path structure as the one created in "processing_data.R"
for(i in seq_along(datasets)) {
  message(paste0("\n========== ", datasets[i], " =========="))

  empty <- do.call(
    run_estimation, 
    c(
      append(
        list(
          folder = file.path(
            "scripts", 
            "data", 
            paste0(datasets[i], "_per_participant")
          ),
          base_name = datasets[i], 
          models = models
        ),
        optim_settings
      )
    )
  )
}

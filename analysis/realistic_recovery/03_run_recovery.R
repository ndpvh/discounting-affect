################################################################################
# PURPOSE:
#
# Run participant-based parameter recovery for VANHASBROECK_2024 only.
#
# Each participant's previously estimated parameters generate new data using
# that participant's original stimulus sequence. Responses are clipped to
# [0, 1], and the original missing-response pattern is retained for refitting.
#
# The optimization strategy is unchanged: DEoptim followed by nloptr/BOBYQA.
#
# This script performs recovery GENERATION ONLY. It saves paired generating
# and recovered parameter tables for later scatterplots and correlations.
# No absolute goodness-of-fit statistics are requested.
#
# Save this script and the new recovery.R in analysis/realistic_recovery/.
# Run from the repository root or the analysis/ directory. The original
# recovery function, run script, and output files are left unchanged.
################################################################################

################################################################################
# SOURCE SHARED INFRASTRUCTURE

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

devtools::load_all(PROJECT_ROOT)

# Source the new analysis function AFTER loading the original package.
recovery_function_file <- file.path(
  PATHS$analysis, "realistic_recovery", "recovery.R"
)
if (!file.exists(recovery_function_file)) {
  stop("Could not find the new recovery function: ", recovery_function_file)
}
source(recovery_function_file)
rm(recovery_function_file)

for (package_name in c("DEoptim", "nloptr")) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop("Install the required package before running recovery: ", package_name)
  }
}
rm(package_name)

################################################################################
# PRELIMINARIES

# The dataset is intentionally fixed for this first implementation.
dataset_name <- "VANHASBROECK_2024"
expected_d <- 1L  # Change to 2L 
expected_k <- 1L

# Number of simulated datasets generated from EACH participant's parameter set.
# Change this to 10L if multiple repetitions are wanted.
iterations <- 1L

# NULL uses all participants. Set to 1L for a one-participant pilot.
participant_limit <- NULL

# Keep all three models, or use "exponential" for the initial pilot.
model_names <- c(
  "exponential",
  "quasi_hyperbolic",
  "double_exponential"
)

# Generate reproducible seeds for all participant/model tasks before starting
# workers.
seed <- 20261105L

models <- list(
  exponential = exponential,
  quasi_hyperbolic = quasi_hyperbolic,
  double_exponential = double_exponential
)
if (length(model_names) == 0L || anyDuplicated(model_names) ||
    any(!model_names %in% names(models))) {
  stop("Select one or more unique model names from the models list.")
}
models <- models[model_names]

if (!is.numeric(iterations) || length(iterations) != 1L ||
    !is.finite(iterations) || iterations < 1 || iterations != floor(iterations) ||
    iterations > .Machine$integer.max) {
  stop("\"iterations\" must be a positive integer.")
}
iterations <- as.integer(iterations)

# Original processed participant datasets and previously estimated parameters.
participant_folder <- file.path(PATHS$processed_data, dataset_name)
rds_files <- sort(list.files(
  participant_folder,
  pattern = "\\.rds$",
  full.names = TRUE,
  ignore.case = TRUE
))
if (length(rds_files) == 0L) {
  raw_file <- file.path(PATHS$raw_data, paste0(dataset_name, ".csv"))
  stop(
    "No processed participant RDS files found in: ", participant_folder,
    "\nThe CSVs in PATHS$estimation contain fitted parameter estimates; they do not ",
    "contain the participant stimulus sequences needed for recovery.",
    "\nRun the VANHASBROECK_2021 section of the project's data-processing script first.",
    "\nThat section should read the raw file at: ", raw_file,
    "\nand create one participant RDS file per person in: ", participant_folder,
    "\nIf the raw file is stored elsewhere, check PATHS$raw_data and PROJECT_ROOT."
  )
}

# Keep only participant files with dimensions matching this run.
rds_dims <- vapply(
  rds_files,
  function(file) {
    ds <- readRDS(file)
    c(d = ncol(ds@Y), k = ncol(ds@X))
  },
  FUN.VALUE = c(d = 0L, k = 0L)
)

keep_dimensions <-
  rds_dims["d", ] == expected_d &
  rds_dims["k", ] == expected_k

message(
  "Participant files matching d = ", expected_d,
  " and k = ", expected_k, ": ",
  sum(keep_dimensions), " of ", length(rds_files)
)

rds_files <- rds_files[keep_dimensions]

if (length(rds_files) == 0L) {
  stop("No participant RDS files match the requested dimensions.")
}

if (!is.null(participant_limit)) {
  if (!is.numeric(participant_limit) || length(participant_limit) != 1L ||
      !is.finite(participant_limit) || participant_limit < 1 ||
      participant_limit != floor(participant_limit)) {
    stop("\"participant_limit\" must be NULL or a positive integer.")
  }
  rds_files <- head(rds_files, participant_limit)
}

participant_ids <- tools::file_path_sans_ext(basename(rds_files))

# Load and check all required estimation tables before starting any fits.
estimation_data <- lapply(
  names(models),
  function(model_name) {
    estimation_file <- file.path(
      PATHS$estimation,
      paste0(dataset_name, "_", expected_d, "_", model_name, ".csv")
    )
    if (!file.exists(estimation_file)) {
      stop("Missing participant-estimation file: ", estimation_file)
    }
    
    df <- read.csv(
      estimation_file,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    if (!"participant_id" %in% names(df)) {
      stop("No participant_id column in: ", estimation_file)
    }
    if (anyNA(df$participant_id) || anyDuplicated(df$participant_id)) {
      stop("Missing or duplicate participant IDs in: ", estimation_file)
    }
    
    missing_ids <- setdiff(participant_ids, df$participant_id)
    
    if (length(missing_ids) > 0L) {
      message(
        length(missing_ids), " participant(s) have no estimate row in ",
        basename(estimation_file),
        "; they will be marked as skipped."
      )
    }
    
    model_empty <- models[[model_name]](d = expected_d, k = expected_k)
    required_parameters <- parameter_names(
      model_empty,
      dynamics = "isotropic",
      covariance = "symmetric",
      parameters_only = FALSE
    )
    missing_columns <- setdiff(required_parameters, names(df))
    if (length(missing_columns) > 0L) {
      stop(
        "Missing parameter columns in ", estimation_file, ": ",
        paste(missing_columns, collapse = ", ")
      )
    }
    
    return(df)
  }
) |>
  `names<-`(names(models))

################################################################################
# SHARED OPTIMIZER FUNCTION

optimizer <- function(obj,
                      lower,
                      upper,
                      algorithm = "NLOPT_LN_BOBYQA",
                      maxeval = 1e3,
                      ftol_abs = 1e-15,
                      xtol_abs = 1e-15,
                      print_level = 0,
                      ...) {
  
  # Global search.
  result_de <- DEoptim::DEoptim(
    obj,
    lower,
    upper,
    control = DEoptim::DEoptim.control(...)
  )
  x0 <- result_de$optim$bestmem
  
  # Local refinement from the best global solution.
  result_nl <- nloptr::nloptr(
    x0,
    obj,
    lb = lower,
    ub = upper,
    opts = list(
      algorithm = algorithm,
      maxeval = maxeval,
      ftol_abs = ftol_abs,
      xtol_abs = xtol_abs,
      print_level = print_level
    )
  )
  
  return(list(
    parameters = result_nl$solution,
    objective = result_nl$objective
  ))
}

# Same fitting settings as the original recovery and participant estimation.
optim_settings <- list(
  dynamics = "isotropic",
  covariance = "symmetric",
  itermax = 1e3,
  NP = 150,
  CR = 0.75,
  strategy = 6,
  p = 0.8,
  reltol = 1e-15,
  steptol = 100,
  trace = FALSE,
  maxeval = 1e5,
  xtol_abs = 1e-20,
  ftol_abs = 1e-20,
  print_level = 0
)

################################################################################
# OUTPUT AND PARALLEL SETTINGS

# A separate timestamped directory prevents overwriting original recovery
# results or mixing the output of a pilot with that of a later full run.
run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_dir <- file.path(
  PATHS$recovery, "realistic",
  paste0(dataset_name, "_d", expected_d),
  run_id
)
if (dir.exists(output_dir)) {
  stop("This run directory already exists. Start again with a new run_id: ", output_dir)
}
ensure_dir(output_dir)

set.seed(seed)
task_seeds <- matrix(
  sample.int(.Machine$integer.max, length(rds_files) * length(models)),
  nrow = length(rds_files),
  ncol = length(models),
  dimnames = list(participant_ids, names(models))
)

run_settings <- list(
  dataset = dataset_name,
  run_id = run_id,
  models = names(models),
  iterations = iterations,
  participant_files = rds_files,
  estimation_directory = PATHS$estimation,
  seed = seed,
  task_seeds = task_seeds,
  optimizer_settings = optim_settings
)
saveRDS(run_settings, file.path(output_dir, "run_settings.Rds"))

# If execution stops early, the run remains explicitly marked incomplete.
saveRDS(
  list(complete = FALSE, models = names(models)),
  file.path(output_dir, "run_status.Rds")
)

detected_cores <- parallel::detectCores()
if (is.na(detected_cores)) detected_cores <- 1L
recovery_cores <- if (Sys.info()["sysname"] == "Windows") {
  1L
} else {
  min(length(rds_files), max(1L, as.integer(detected_cores - 1L)))
}

message("Participants: ", length(rds_files))
message("Repetitions per participant and model: ", iterations)
message("Results will be saved to: ", normalizePath(output_dir))

################################################################################
# PERFORM THE RECOVERY

# Models are processed sequentially; participants are processed in parallel.
for (j in seq_along(models)) {
  model_name <- names(models)[j]
  estimates <- estimation_data[[model_name]]
  message("\n========== ", dataset_name, " / ", model_name, " ==========")
  
  rows <- parallel::mclapply(
    seq_along(rds_files),
    function(idx) {
      participant_id <- participant_ids[idx]
      
      # Estimation failures stored as NA rows are explicitly skipped.
      # Other errors are logged and cause the run to stop in the parent.
      tryCatch(
        {
          ds <- readRDS(rds_files[idx])
          d <- ncol(ds@Y)
          k <- ncol(ds@X)
          if (d != expected_d || k != expected_k) {
            stop(
              dataset_name, " requires d = ", expected_d,
              " and k = ", expected_k, "."
            )
          }
          
          model_empty <- models[[j]](d = d, k = k)
          required_parameters <- parameter_names(
            model_empty,
            dynamics = optim_settings$dynamics,
            covariance = optim_settings$covariance,
            parameters_only = FALSE
          )
          
          # Select only parameter columns, in the model's required
          # order. AIC, BIC, bias, etc. are not generating parameters.
          row_index <- match(participant_id, estimates$participant_id)
          if (is.na(row_index)) {
            return(list(
              participant_id = participant_id,
              status = "skipped",
              message = "No matching participant estimate in the estimation CSV.",
              result = NULL
            ))
          }
          parameter_values <- as.numeric(unlist(
            estimates[row_index, required_parameters, drop = FALSE],
            use.names = FALSE
          ))
          names(parameter_values) <- required_parameters
          
          if (any(!is.finite(parameter_values))) {
            return(list(
              participant_id = participant_id,
              status = "skipped",
              message = "Original parameter estimates contain missing or non-finite values.",
              result = NULL
            ))
          }
          
          # A cheap consistency check: the saved estimates should
          # reproduce their original SSE on this processed dataset.
          # This can catch mismatched or subsequently changed inputs.
          if ("objective_sse" %in% names(estimates)) {
            saved_sse <- estimates$objective_sse[row_index]
            if (is.finite(saved_sse)) {
              mean_parameter_names <- parameter_names(
                model_empty,
                dynamics = optim_settings$dynamics,
                covariance = optim_settings$covariance,
                parameters_only = TRUE
              )
              original_sse <- objective_function(
                model_empty,
                ds,
                parameter_values[mean_parameter_names],
                dynamics = optim_settings$dynamics
              )
              if (!isTRUE(all.equal(
                original_sse, saved_sse,
                tolerance = 1e-6,
                check.attributes = FALSE
              ))) {
                stop(
                  "Saved estimates do not reproduce the original SSE. ",
                  "Check that estimates and processed data belong to the same analysis."
                )
              }
            }
          }
          
          result <- do.call(
            recovery_realistic,
            c(
              list(
                sim_model = model_empty,
                data = ds,
                estimated_parameters = parameter_values,
                participant_id = participant_id,
                iterations = iterations,
                fx = list(),
                seed = task_seeds[idx, j],
                print_iteration = TRUE,
                print_content = paste(dataset_name, model_name, sep = " / "),
                optimizer = optimizer
              ),
              optim_settings
            )
          )
          
          return(list(
            participant_id = participant_id,
            status = "ok",
            message = "",
            result = result
          ))
        },
        error = function(e) {
          error_message <- conditionMessage(e)
          
          if (grepl(
            "The generating covariance must be positive definite",
            error_message,
            fixed = TRUE
          )) {
            return(list(
              participant_id = participant_id,
              status = "skipped",
              message = error_message,
              result = NULL
            ))
          }
          
          return(list(
            participant_id = participant_id,
            status = "failed",
            message = error_message,
            result = NULL
          ))
        }
      )
    },
    mc.cores = recovery_cores,
    mc.set.seed = FALSE
  )
  
  worker_failures <- which(vapply(
    rows, function(x) inherits(x, "try-error"), logical(1)
  ))
  if (length(worker_failures) > 0L) {
    stop(
      "Parallel worker failure(s) for participant index/indices: ",
      paste(worker_failures, collapse = ", "),
      ". This run is incomplete."
    )
  }
  
  # Retain a readable record of successful, skipped, and failed participants.
  participant_status <- data.frame(
    participant_id = vapply(rows, function(x) x$participant_id, character(1)),
    status = vapply(rows, function(x) x$status, character(1)),
    message = vapply(rows, function(x) x$message, character(1)),
    stringsAsFactors = FALSE
  )
  status_file <- file.path(output_dir, paste0(model_name, "_participant_status.csv"))
  write.csv(participant_status, status_file, row.names = FALSE)
  
  if (any(participant_status$status == "failed")) {
    stop(
      "Recovery failed for ", model_name, ". See: ", status_file,
      ". No raw result file was saved for this model; the run is incomplete."
    )
  }
  
  successful_rows <- Filter(function(x) x$status == "ok", rows)
  if (length(successful_rows) == 0L) {
    stop("No participants had usable estimates for ", model_name, ". See: ", status_file)
  }
  
  # The same row order is used in all three tables, preserving the pairing.
  result <- list(
    simulate = do.call(rbind, lapply(successful_rows, function(x) x$result$simulate)),
    fit = do.call(rbind, lapply(successful_rows, function(x) x$result$fit)),
    metadata = do.call(rbind, lapply(successful_rows, function(x) x$result$metadata)),
    participant_status = participant_status,
    settings = list(
      dataset = dataset_name,
      model = model_name,
      d = expected_d,
      k = expected_k,
      iterations = iterations,
      optimizer_settings = optim_settings
    )
  )
  rownames(result$simulate) <- NULL
  rownames(result$fit) <- NULL
  rownames(result$metadata) <- NULL
  result$metadata$dataset <- dataset_name
  result$metadata$model <- model_name
  
  saveRDS(result, file.path(output_dir, paste0(model_name, ".Rds")))
  message(
    "Saved ", model_name, ": ", length(successful_rows), " participants; ",
    nrow(result$fit), " recoveries; ",
    sum(participant_status$status == "skipped"), " skipped."
  )
}

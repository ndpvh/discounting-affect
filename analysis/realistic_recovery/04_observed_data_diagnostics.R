################################################################################
# PURPOSE:
#
# Summarize parameter recovery and absolute fit for the simulated datasets in a
# saved realistic-recovery result. The generating parameters are in
# result$simulate; result$fit contains the parameters recovered from each
# simulated dataset.
#
# Run from the repository root:
#   Rscript analysis/realistic_recovery/04_observed_data_diagnostics.R \
#     <model-result.Rds> [replicates] [seed]
#
# In an interactive R session, source this file and call
# run_recovery_diagnostics(file.choose()).
################################################################################

config_file <- if (file.exists(file.path("analysis", "_config.R"))) {
  file.path("analysis", "_config.R")
} else if (file.exists("_config.R")) {
  "_config.R"
} else {
  stop(
    "Could not find analysis/_config.R. ",
    "Run this script from the repository root or the analysis/ directory."
  )
}
source(config_file)
source(file.path(PATHS$analysis, "_helpers.R"))
required_packages <- c("devtools", "MASS", "DEoptim", "nloptr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Install the required R package(s) before running diagnostics: ",
    paste(missing_packages, collapse = ", ")
  )
}
devtools::load_all(PROJECT_ROOT)
rm(config_file, required_packages, missing_packages)

recovery_models <- list(
  exponential = exponential,
  quasi_hyperbolic = quasi_hyperbolic,
  double_exponential = double_exponential
)

fit_statistics <- c(
  "mean",
  "sd",
  "range",
  "skewness",
  "lag2_correlation"
)

series_statistics <- function(y, trial) {
  if (!is.numeric(y) || !is.numeric(trial) ||
      length(y) != length(trial)) {
    stop("Responses and trial indices must be numeric vectors of equal length.")
  }
  if (any(!is.finite(y[!is.na(y)])) || any(!is.finite(trial))) {
    stop("Responses and trial indices must be finite where present.")
  }
  
  values <- y[!is.na(y)]
  n_observed <- length(values)
  lagged_index <- match(trial - 2, trial)
  current_index <- which(!is.na(y) & !is.na(lagged_index))
  previous_index <- lagged_index[current_index]
  paired <- !is.na(y[current_index]) & !is.na(y[previous_index])
  current_index <- current_index[paired]
  previous_index <- previous_index[paired]
  n_lag2_pairs <- length(current_index)
  
  second_moment <- if (n_observed > 0L) {
    mean((values - mean(values))^2)
  } else {
    NA_real_
  }
  lag2_valid <- n_lag2_pairs >= 3L &&
    stats::sd(y[current_index]) > 0 &&
    stats::sd(y[previous_index]) > 0
  
  statistic_values <- c(
    mean = if (n_observed >= 1L) mean(values) else NA_real_,
    sd = if (n_observed >= 2L) stats::sd(values) else NA_real_,
    range = if (n_observed >= 2L) diff(range(values)) else NA_real_,
    skewness = if (n_observed >= 3L && is.finite(second_moment) &&
                   second_moment > 0) {
      mean((values - mean(values))^3) / second_moment^(3 / 2)
    } else {
      NA_real_
    },
    lag2_correlation = if (lag2_valid) {
      stats::cor(y[current_index], y[previous_index])
    } else {
      NA_real_
    }
  )
  reasons <- c(
    mean = if (n_observed >= 1L) "" else "fewer_than_1_observation",
    sd = if (n_observed >= 2L) "" else "fewer_than_2_observations",
    range = if (n_observed >= 2L) "" else "fewer_than_2_observations",
    skewness = if (n_observed < 3L) {
      "fewer_than_3_observations"
    } else if (!is.finite(second_moment) || second_moment <= 0) {
      "zero_or_invalid_variance"
    } else {
      ""
    },
    lag2_correlation = if (n_lag2_pairs < 3L) {
      "fewer_than_3_observed_lag2_pairs"
    } else if (!lag2_valid) {
      "zero_variance_in_lag2_pairs"
    } else {
      ""
    }
  )
  
  data.frame(
    statistic = fit_statistics,
    value = unname(statistic_values[fit_statistics]),
    valid = is.finite(unname(statistic_values[fit_statistics])),
    reason = unname(reasons[fit_statistics]),
    n_observed = n_observed,
    n_lag2_pairs = n_lag2_pairs,
    stringsAsFactors = FALSE
  )
}

check_equal_numeric <- function(actual, expected, context, tolerance = 1e-12) {
  if (!isTRUE(all.equal(
    as.numeric(actual),
    as.numeric(expected),
    tolerance = tolerance,
    check.attributes = FALSE
  ))) {
    stop(context, " does not match its expected values.")
  }
  invisible(TRUE)
}

run_recovery_diagnostics <- function(result_file,
                                     replicates = 1000L,
                                     seed = 20261123L,
                                     output_root = NULL) {
  target_dataset <- "VANHASBROECK_2021"
  
  if (!is.character(result_file) || length(result_file) != 1L ||
      is.na(result_file) || !nzchar(result_file) || !file.exists(result_file)) {
    stop("Provide the path to one existing realistic-recovery model RDS.")
  }
  result_file <- normalizePath(result_file, mustWork = TRUE)
  if (!is.numeric(replicates) || length(replicates) != 1L ||
      !is.finite(replicates) || replicates < 2 ||
      replicates != floor(replicates) ||
      replicates > .Machine$integer.max) {
    stop("\"replicates\" must be an integer of at least 2.")
  }
  replicates <- as.integer(replicates)
  if (!is.numeric(seed) || length(seed) != 1L ||
      !is.finite(seed) || seed < 0 ||
      seed != floor(seed) || seed > .Machine$integer.max) {
    stop("\"seed\" must be a non-negative integer.")
  }
  seed <- as.integer(seed)
  
  result <- readRDS(result_file)
  required_components <- c(
    "simulate", "fit", "metadata", "participant_status", "settings"
  )
  missing_components <- setdiff(required_components, names(result))
  if (length(missing_components) > 0L) {
    stop(
      "The selected RDS is missing required component(s): ",
      paste(missing_components, collapse = ", ")
    )
  }
  if (!is.data.frame(result$simulate) || !is.data.frame(result$fit) ||
      !is.data.frame(result$metadata) ||
      !is.data.frame(result$participant_status)) {
    stop("The selected RDS contains an invalid table component.")
  }
  if (!is.list(result$settings)) {
    stop("The selected RDS must include a settings list.")
  }
  if (!identical(result$settings$dataset, target_dataset)) {
    stop(
      "Expected dataset ", target_dataset, " but the selected result declares ",
      paste(result$settings$dataset, collapse = ", "), "."
    )
  }
  
  model_name <- result$settings$model
  if (!is.character(model_name) || length(model_name) != 1L ||
      is.na(model_name) || !model_name %in% names(recovery_models)) {
    stop("The selected result has a missing or unsupported model setting.")
  }
  if (nrow(result$simulate) == 0L ||
      nrow(result$simulate) != nrow(result$fit) ||
      nrow(result$simulate) != nrow(result$metadata)) {
    stop("The simulate, fit, and metadata tables must have matching nonzero rows.")
  }
  if ("dataset" %in% names(result$metadata) &&
      (anyNA(result$metadata$dataset) ||
       any(result$metadata$dataset != target_dataset))) {
    stop("Recovery metadata contains a dataset label that conflicts with settings.")
  }
  if ("model" %in% names(result$metadata) &&
      (anyNA(result$metadata$model) ||
       any(result$metadata$model != model_name))) {
    stop("Recovery metadata contains a model label that conflicts with settings.")
  }
  
  required_metadata <- c(
    "participant_id", "iteration", "seed", "n_observed",
    "n_clipped_lower", "n_clipped_upper"
  )
  missing_metadata <- setdiff(required_metadata, names(result$metadata))
  if (length(missing_metadata) > 0L) {
    stop(
      "Recovery metadata is missing required column(s): ",
      paste(missing_metadata, collapse = ", ")
    )
  }
  metadata <- result$metadata
  metadata$participant_id <- as.character(metadata$participant_id)
  if (anyNA(metadata$participant_id) || any(!nzchar(metadata$participant_id)) ||
      anyDuplicated(data.frame(
        participant_id = metadata$participant_id,
        iteration = metadata$iteration
      ))) {
    stop("Recovery metadata contains missing IDs or duplicate participant/iteration keys.")
  }
  numeric_metadata <- c(
    "iteration", "seed", "n_observed", "n_clipped_lower", "n_clipped_upper"
  )
  if (!all(vapply(metadata[numeric_metadata], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(metadata[numeric_metadata]))) ||
      any(metadata$iteration < 1 |
          metadata$iteration != floor(metadata$iteration) |
          metadata$n_observed < 1 |
          metadata$n_observed != floor(metadata$n_observed) |
          metadata$n_clipped_lower < 0 |
          metadata$n_clipped_lower != floor(metadata$n_clipped_lower) |
          metadata$n_clipped_upper < 0 |
          metadata$n_clipped_upper != floor(metadata$n_clipped_upper) |
          metadata$n_clipped_lower + metadata$n_clipped_upper >
          metadata$n_observed)) {
    stop("Recovery metadata contains invalid iteration, seed, or observation counts.")
  }
  if (any(metadata$seed < 0 |
          metadata$seed > .Machine$integer.max |
          metadata$seed != floor(metadata$seed))) {
    stop("Recovery metadata contains an invalid simulation seed.")
  }
  
  status <- result$participant_status
  if (!all(c("status", "participant_id") %in% names(status))) {
    stop("participant_status must contain status and participant_id columns.")
  }
  status$participant_id <- as.character(status$participant_id)
  if (anyNA(status$participant_id) || anyNA(status$status) ||
      anyDuplicated(status$participant_id)) {
    stop("participant_status contains missing or duplicate participant IDs.")
  }
  status_index <- match(unique(metadata$participant_id), status$participant_id)
  if (anyNA(status_index) || any(status$status[status_index] != "ok")) {
    stop("Every participant with recovery rows must have status 'ok'.")
  }
  
  if (!all(c("d", "k", "optimizer_settings") %in% names(result$settings))) {
    stop("Recovery settings must contain d, k, and optimizer_settings.")
  }
  d <- as.integer(result$settings$d)
  k <- as.integer(result$settings$k)
  if (length(d) != 1L || is.na(d) || d < 1L ||
      length(k) != 1L || is.na(k) || k < 1L) {
    stop("Recovery settings contain invalid model dimensions.")
  }
  dynamics <- result$settings$optimizer_settings$dynamics
  covariance <- result$settings$optimizer_settings$covariance
  if (!is.character(dynamics) || length(dynamics) != 1L ||
      !is.character(covariance) || length(covariance) != 1L) {
    stop("Recovery settings must record dynamics and covariance structures.")
  }
  
  empty_model <- recovery_models[[model_name]](d = d, k = k)
  parameter_columns <- parameter_names(
    empty_model,
    dynamics = dynamics,
    covariance = covariance,
    parameters_only = FALSE
  )
  if (!identical(names(result$simulate), parameter_columns)) {
    stop(
      "Generating parameter columns/order do not match parameter_names() for ",
      model_name, " with d=", d, " and k=", k, ". Expected: ",
      paste(parameter_columns, collapse = ", ")
    )
  }
  if (!all(parameter_columns %in% names(result$fit)) ||
      !"objective" %in% names(result$fit)) {
    stop("The fit table must contain recovered parameters and objective.")
  }
  if (!all(vapply(result$simulate, is.numeric, logical(1))) ||
      !all(vapply(result$fit[parameter_columns], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(result$simulate))) ||
      any(!is.finite(as.matrix(result$fit[parameter_columns]))) ||
      !is.numeric(result$fit$objective) ||
      any(!is.finite(result$fit$objective))) {
    stop("Generating/recovered parameters and fit objectives must be finite numeric values.")
  }
  
  estimation_file <- file.path(
    PATHS$estimation,
    paste0(target_dataset, "_", model_name, ".csv")
  )
  if (!file.exists(estimation_file)) {
    stop("Cannot find the 2021 parameter-provenance CSV: ", estimation_file)
  }
  estimates <- utils::read.csv(
    estimation_file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!"participant_id" %in% names(estimates) ||
      anyNA(estimates$participant_id) ||
      anyDuplicated(as.character(estimates$participant_id))) {
    stop("The 2021 parameter CSV must have unique nonmissing participant_id values.")
  }
  missing_estimate_columns <- setdiff(parameter_columns, names(estimates))
  if (length(missing_estimate_columns) > 0L) {
    stop(
      "The parameter CSV is missing required generating parameter(s): ",
      paste(missing_estimate_columns, collapse = ", ")
    )
  }
  estimate_row <- match(metadata$participant_id, as.character(estimates$participant_id))
  if (anyNA(estimate_row)) {
    stop("Some recovery participant IDs have no matching 2021 estimate row.")
  }
  for (row in seq_len(nrow(metadata))) {
    check_equal_numeric(
      result$simulate[row, parameter_columns, drop = TRUE],
      estimates[estimate_row[row], parameter_columns, drop = TRUE],
      paste0("Generating parameters for ", metadata$participant_id[row])
    )
  }
  
  participant_rows <- split(
    seq_len(nrow(metadata)),
    factor(metadata$participant_id, levels = unique(metadata$participant_id))
  )
  for (participant_id in names(participant_rows)) {
    rows <- participant_rows[[participant_id]]
    reference <- unlist(
      result$simulate[rows[1L], parameter_columns, drop = FALSE],
      use.names = FALSE
    )
    for (row in rows[-1L]) {
      check_equal_numeric(
        unlist(result$simulate[row, parameter_columns, drop = FALSE],
               use.names = FALSE),
        reference,
        paste0("Generating parameter consistency for ", participant_id)
      )
    }
  }
  
  processed_dir <- file.path(PATHS$processed_data, target_dataset)
  if (!dir.exists(processed_dir)) {
    stop(
      "Processed participant RDS directory is missing: ", processed_dir,
      "\nRun the project's 2021 preprocessing before this diagnostics script."
    )
  }
  raw_file <- file.path(PATHS$raw_data, paste0(target_dataset, ".csv"))
  if (!file.exists(raw_file)) {
    stop("Cannot find the raw participant trial table: ", raw_file)
  }
  raw <- utils::read.csv(
    raw_file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  required_raw <- c("id", "trial", "cr", "ev", "rpe", "happiness")
  missing_raw <- setdiff(required_raw, names(raw))
  if (length(missing_raw) > 0L) {
    stop("Raw 2021 data are missing column(s): ", paste(missing_raw, collapse = ", "))
  }
  raw$participant_id <- paste0(target_dataset, "_", as.character(raw$id))
  participant_ids <- sort(unique(metadata$participant_id))
  if (any(!participant_ids %in% raw$participant_id)) {
    stop("Some recovery participants do not match IDs in the raw 2021 data.")
  }
  
  total_simulations <- nrow(metadata) * replicates
  saved_seeds <- unique(as.integer(metadata$seed))
  if (total_simulations > .Machine$integer.max - length(saved_seeds)) {
    stop("The requested recovery-row/replicate count is too large.")
  }
  set.seed(seed)
  sampled_seeds <- sample.int(
    .Machine$integer.max,
    size = total_simulations + length(saved_seeds),
    replace = FALSE
  )
  simulation_seeds <- head(setdiff(sampled_seeds, saved_seeds), total_simulations)
  simulation_seed_matrix <- matrix(
    simulation_seeds,
    nrow = nrow(metadata),
    ncol = replicates,
    dimnames = list(as.character(seq_len(nrow(metadata))), NULL)
  )
  
  fill_recovery_model <- function(parameters, context) {
    fitted <- fill(
      empty_model,
      parameters,
      dynamics = dynamics,
      covariance = covariance,
      parameters_only = FALSE,
      cholesky = FALSE
    )
    if (any(!is.finite(fitted@covariance)) ||
        !isSymmetric(fitted@covariance)) {
      stop("Invalid covariance for ", context)
    }
    tryCatch(
      chol(fitted@covariance),
      error = function(e) {
        stop(
          "Covariance is not positive definite for ", context, ": ",
          conditionMessage(e)
        )
      }
    )
    fitted
  }
  
  parameter_rows_out <- list()
  observed_rows <- list()
  simulation_rows <- list()
  residual_rows <- list()
  pointwise_rows <- list()
  
  for (participant_id in participant_ids) {
    data_file <- file.path(processed_dir, paste0(participant_id, ".rds"))
    if (!file.exists(data_file)) {
      stop("Processed participant dataset is missing: ", data_file)
    }
    data <- readRDS(data_file)
    if (!methods::is(data, "dataset") ||
        !is.matrix(data@X) || !is.matrix(data@Y) ||
        !is.numeric(data@X) || !is.numeric(data@Y)) {
      stop("Invalid processed dataset object for participant ", participant_id)
    }
    if (ncol(data@X) != k || ncol(data@Y) != d ||
        nrow(data@X) != nrow(data@Y)) {
      stop("Processed data dimensions disagree with recovery settings for ",
           participant_id)
    }
    
    participant_raw <- raw[raw$participant_id == participant_id, , drop = FALSE]
    participant_raw <- participant_raw[order(participant_raw$trial), , drop = FALSE]
    if (nrow(participant_raw) != nrow(data@Y) ||
        anyNA(participant_raw$trial) ||
        anyDuplicated(participant_raw$trial)) {
      stop("Raw trial rows do not align with processed data for ", participant_id)
    }
    raw_x <- as.matrix(participant_raw[, c("cr", "ev", "rpe"), drop = FALSE]) / 100
    check_equal_numeric(data@X, raw_x, paste0("Processed predictors for ", participant_id))
    if (ncol(data@Y) == 1L) {
      check_equal_numeric(
        data@Y[, 1L], participant_raw$happiness,
        paste0("Processed responses for ", participant_id)
      )
    }
    if (any(!is.finite(data@Y[!is.na(data@Y)]))) {
      stop("Original responses are non-finite for participant ", participant_id)
    }
    observed_mask <- !is.na(data@Y)
    participant_metadata_rows <- participant_rows[[participant_id]]
    if (any(metadata$n_observed[participant_metadata_rows] != sum(observed_mask))) {
      stop("Observed response count disagrees with recovery metadata for ",
           participant_id)
    }
    trial <- as.numeric(participant_raw$trial)
    response_names <- colnames(data@Y)
    if (is.null(response_names) || anyNA(response_names) ||
        any(!nzchar(response_names))) {
      response_names <- paste0("Y", seq_len(d))
    }
    response_names <- make.unique(response_names)
    
    for (row in participant_metadata_rows) {
      iteration <- metadata$iteration[row]
      context <- paste(participant_id, "iteration", iteration)
      generating_parameters <- unlist(
        result$simulate[row, parameter_columns, drop = FALSE],
        use.names = FALSE
      )
      names(generating_parameters) <- parameter_columns
      generating_model <- fill_recovery_model(generating_parameters, context)
      
      set.seed(as.integer(metadata$seed[row]))
      generated_raw <- simulate(generating_model, X = data@X)@Y
      if (!identical(dim(generated_raw), dim(data@Y)) ||
          any(!is.finite(generated_raw[observed_mask]))) {
        stop("Saved-seed recovery simulation is invalid for ", context)
      }
      clipped_lower <- sum(generated_raw[observed_mask] < 0)
      clipped_upper <- sum(generated_raw[observed_mask] > 1)
      if (clipped_lower != metadata$n_clipped_lower[row] ||
          clipped_upper != metadata$n_clipped_upper[row]) {
        stop("Saved-seed clipping counts do not reproduce for ", context)
      }
      recovery_y <- pmin(pmax(generated_raw, 0), 1)
      recovery_y[!observed_mask] <- NA_real_
      
      recovered_parameters <- unlist(
        result$fit[row, parameter_columns, drop = FALSE],
        use.names = FALSE
      )
      names(recovered_parameters) <- parameter_columns
      recovered_model <- fill_recovery_model(recovered_parameters, context)
      prediction <- predict(recovered_model, data)@Y
      if (!identical(dim(prediction), dim(data@Y)) ||
          any(!is.finite(prediction[observed_mask]))) {
        stop("Recovered-model predictions are invalid at observed trials for ", context)
      }
      
      parameter_error <- recovered_parameters - generating_parameters
      parameter_rows_out[[length(parameter_rows_out) + 1L]] <- data.frame(
        participant_id = participant_id,
        iteration = iteration,
        parameter = parameter_columns,
        generating = unname(generating_parameters),
        recovered = unname(recovered_parameters),
        error = unname(parameter_error),
        absolute_error = abs(unname(parameter_error)),
        squared_error = unname(parameter_error^2),
        relative_error = ifelse(
          generating_parameters == 0,
          NA_real_,
          unname(parameter_error / abs(generating_parameters))
        ),
        fit_objective = result$fit$objective[row],
        stringsAsFactors = FALSE
      )
      
      residual <- recovery_y - prediction
      residual[!observed_mask] <- NA_real_
      for (dimension in seq_len(d)) {
        response_stats <- series_statistics(recovery_y[, dimension], trial)
        response_stats$participant_id <- participant_id
        response_stats$iteration <- iteration
        response_stats$response_dimension <- response_names[dimension]
        response_stats <- response_stats[, c(
          "participant_id", "iteration", "response_dimension",
          "statistic", "value", "valid", "reason",
          "n_observed", "n_lag2_pairs"
        )]
        observed_rows[[length(observed_rows) + 1L]] <- response_stats
        
        residual_values <- residual[, dimension]
        residual_stats <- series_statistics(residual_values, trial)
        finite_residuals <- residual_values[!is.na(residual_values)]
        residual_rows[[length(residual_rows) + 1L]] <- data.frame(
          participant_id = participant_id,
          iteration = iteration,
          response_dimension = response_names[dimension],
          residual_mean = if (length(finite_residuals) > 0L) {
            mean(finite_residuals)
          } else {
            NA_real_
          },
          residual_sd = if (length(finite_residuals) > 1L) {
            stats::sd(finite_residuals)
          } else {
            NA_real_
          },
          rmse = if (length(finite_residuals) > 0L) {
            sqrt(mean(finite_residuals^2))
          } else {
            NA_real_
          },
          mae = if (length(finite_residuals) > 0L) {
            mean(abs(finite_residuals))
          } else {
            NA_real_
          },
          max_absolute_residual = if (length(finite_residuals) > 0L) {
            max(abs(finite_residuals))
          } else {
            NA_real_
          },
          residual_lag2_correlation = residual_stats$value[
            residual_stats$statistic == "lag2_correlation"
          ],
          n_observed = length(finite_residuals),
          stringsAsFactors = FALSE
        )
        
        pointwise_rows[[length(pointwise_rows) + 1L]] <- data.frame(
          participant_id = participant_id,
          iteration = iteration,
          trial = trial,
          response_dimension = response_names[dimension],
          recovery_response = recovery_y[, dimension],
          recovered_prediction = prediction[, dimension],
          residual = residual[, dimension],
          squared_error = residual[, dimension]^2,
          observed_rating = !is.na(recovery_y[, dimension]),
          stringsAsFactors = FALSE
        )
      }
      
      for (replicate in seq_len(replicates)) {
        simulation_seed <- simulation_seed_matrix[as.character(row), replicate]
        set.seed(simulation_seed)
        auxiliary_raw <- simulate(recovered_model, X = data@X)@Y
        if (!identical(dim(auxiliary_raw), dim(data@Y)) ||
            any(!is.finite(auxiliary_raw[observed_mask]))) {
          stop("Recovered-model simulation is invalid for ", context,
               ", replicate ", replicate)
        }
        auxiliary_y <- pmin(pmax(auxiliary_raw, 0), 1)
        auxiliary_y[!observed_mask] <- NA_real_
        for (dimension in seq_len(d)) {
          stats <- series_statistics(auxiliary_y[, dimension], trial)
          simulation_rows[[length(simulation_rows) + 1L]] <- data.frame(
            participant_id = participant_id,
            iteration = iteration,
            response_dimension = response_names[dimension],
            replicate = replicate,
            seed = simulation_seed,
            statistic = stats$statistic,
            value = stats$value,
            valid = stats$valid,
            reason = stats$reason,
            n_observed = stats$n_observed,
            n_lag2_pairs = stats$n_lag2_pairs,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  parameter_recovery <- do.call(rbind, parameter_rows_out)
  recovery_data_statistics <- do.call(rbind, observed_rows)
  simulation_statistics <- do.call(rbind, simulation_rows)
  absolute_fit_statistics <- do.call(rbind, residual_rows)
  pointwise_squared_error <- do.call(rbind, pointwise_rows)
  rownames(parameter_recovery) <- NULL
  rownames(recovery_data_statistics) <- NULL
  rownames(simulation_statistics) <- NULL
  rownames(absolute_fit_statistics) <- NULL
  rownames(pointwise_squared_error) <- NULL
  
  parameter_summary <- lapply(parameter_columns, function(parameter_name) {
    rows <- parameter_recovery$parameter == parameter_name
    generating <- parameter_recovery$generating[rows]
    recovered <- parameter_recovery$recovered[rows]
    errors <- parameter_recovery$error[rows]
    correlation <- if (
      length(generating) >= 2L &&
      stats::sd(generating) > 0 &&
      stats::sd(recovered) > 0
    ) {
      stats::cor(generating, recovered)
    } else {
      NA_real_
    }
    data.frame(
      parameter = parameter_name,
      n = sum(is.finite(errors)),
      mean_bias = mean(errors),
      median_bias = stats::median(errors),
      mae = mean(abs(errors)),
      rmse = sqrt(mean(errors^2)),
      correlation = correlation,
      stringsAsFactors = FALSE
    )
  })
  parameter_recovery_summary <- do.call(rbind, parameter_summary)
  
  interval_rows <- vector("list", nrow(recovery_data_statistics))
  alpha <- 0.05
  for (i in seq_len(nrow(recovery_data_statistics))) {
    observed_row <- recovery_data_statistics[i, ]
    same_series <- simulation_statistics$participant_id ==
      observed_row$participant_id &
      simulation_statistics$iteration == observed_row$iteration &
      simulation_statistics$response_dimension ==
      observed_row$response_dimension &
      simulation_statistics$statistic == observed_row$statistic
    replicate_values <- simulation_statistics$value[same_series]
    valid_count <- sum(is.finite(replicate_values))
    interval_valid <- observed_row$valid && valid_count == replicates
    lower <- upper <- NA_real_
    reason <- observed_row$reason
    if (!observed_row$valid) {
      reason <- paste0("recovery_statistic_invalid:", observed_row$reason)
    } else if (valid_count != replicates) {
      reason <- "one_or_more_recovered_model_statistics_invalid"
    } else {
      bounds <- stats::quantile(
        replicate_values,
        probs = c(alpha / 2, 1 - alpha / 2),
        type = 7,
        names = FALSE
      )
      lower <- bounds[1L]
      upper <- bounds[2L]
      reason <- ""
    }
    covered <- if (interval_valid) {
      lower <= observed_row$value && observed_row$value <= upper
    } else {
      NA
    }
    interval_rows[[i]] <- data.frame(
      participant_id = observed_row$participant_id,
      iteration = observed_row$iteration,
      response_dimension = observed_row$response_dimension,
      statistic = observed_row$statistic,
      recovery_data_value = observed_row$value,
      lower = lower,
      upper = upper,
      covered = covered,
      interval_valid = interval_valid,
      n_valid_replicates = valid_count,
      n_replicates = replicates,
      n_observed = observed_row$n_observed,
      n_lag2_pairs = observed_row$n_lag2_pairs,
      reason = reason,
      stringsAsFactors = FALSE
    )
  }
  intervals <- do.call(rbind, interval_rows)
  rownames(intervals) <- NULL
  
  eligible <- !is.na(intervals$covered)
  overall_eligible <- sum(eligible)
  coverage_rows <- list(data.frame(
    summary = "overall_recovery_row_statistic_cell_weighted",
    statistic = NA_character_,
    participant_id = NA_character_,
    n_covered = sum(intervals$covered[eligible]),
    n_eligible = overall_eligible,
    n_invalid = nrow(intervals) - overall_eligible,
    coverage = if (overall_eligible > 0L) {
      mean(intervals$covered[eligible])
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  ))
  for (statistic in fit_statistics) {
    rows <- intervals$statistic == statistic & !is.na(intervals$covered)
    n_eligible <- sum(rows)
    coverage_rows[[length(coverage_rows) + 1L]] <- data.frame(
      summary = "by_statistic",
      statistic = statistic,
      participant_id = NA_character_,
      n_covered = sum(intervals$covered[rows]),
      n_eligible = n_eligible,
      n_invalid = sum(intervals$statistic == statistic) - n_eligible,
      coverage = if (n_eligible > 0L) {
        mean(intervals$covered[rows])
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  }
  participant_coverage <- lapply(participant_ids, function(participant_id) {
    rows <- intervals$participant_id == participant_id &
      !is.na(intervals$covered)
    n_eligible <- sum(rows)
    data.frame(
      summary = "by_participant",
      statistic = NA_character_,
      participant_id = participant_id,
      n_covered = sum(intervals$covered[rows]),
      n_eligible = n_eligible,
      n_invalid = sum(intervals$participant_id == participant_id) - n_eligible,
      coverage = if (n_eligible > 0L) {
        mean(intervals$covered[rows])
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  })
  participant_coverage <- do.call(rbind, participant_coverage)
  participant_rates <- participant_coverage$coverage[
    is.finite(participant_coverage$coverage)
  ]
  coverage_rows[[length(coverage_rows) + 1L]] <- data.frame(
    summary = "participant_balanced",
    statistic = NA_character_,
    participant_id = NA_character_,
    n_covered = NA_integer_,
    n_eligible = length(participant_rates),
    n_invalid = sum(participant_coverage$n_invalid),
    coverage = if (length(participant_rates) > 0L) {
      mean(participant_rates)
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
  coverage_summary <- do.call(rbind, c(coverage_rows, list(participant_coverage)))
  rownames(coverage_summary) <- NULL
  
  if (is.null(output_root)) {
    output_root <- file.path(PATHS$recovery, "realistic_fit_diagnostics")
  }
  output_dir <- file.path(
    output_root,
    target_dataset,
    model_name,
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )
  if (dir.exists(output_dir)) {
    stop("Diagnostics output directory already exists: ", output_dir)
  }
  ensure_dir(output_dir)
  
  analysis_script <- file.path(
    PATHS$analysis,
    "realistic_recovery",
    "04_observed_data_diagnostics.R"
  )
  run_manifest <- list(
    dataset = target_dataset,
    model = model_name,
    d = d,
    k = k,
    result_file = result_file,
    analysis_script = normalizePath(analysis_script, mustWork = TRUE),
    estimation_file = normalizePath(estimation_file, mustWork = TRUE),
    processed_directory = normalizePath(processed_dir, mustWork = TRUE),
    raw_file = normalizePath(raw_file, mustWork = TRUE),
    participant_ids = participant_ids,
    participant_status_counts = table(status$status),
    recovery_rows = nrow(metadata),
    auxiliary_replicates_per_recovery_row = replicates,
    analysis_seed = seed,
    simulation_seeds = simulation_seed_matrix,
    confidence_level = 1 - alpha,
    quantile_type = 7L,
    statistics = fit_statistics,
    generating_parameters = "result$simulate; validated against the 2021 estimates CSV",
    recovered_parameters = "result$fit",
    recovery_response = paste(
      "Regenerated from result$simulate and metadata$seed;",
      "clip to [0, 1] and restore original Y missingness"
    ),
    predictions = paste(
      "predict() conditional expectation using result$fit parameters;",
      "not clipped"
    ),
    auxiliary_simulations = paste(
      "simulate under result$fit parameters; no refitting;",
      "clip to [0, 1] and restore original Y missingness"
    ),
    coverage_definition = paste(
      "Intervals are conditional on each recovery fit; invalid cells are",
      "excluded and counted in coverage_summary$n_invalid"
    ),
    rng_kind = RNGkind(),
    R_version = R.version.string,
    package_version = read.dcf(
      file.path(PROJECT_ROOT, "DESCRIPTION"),
      fields = "Version"
    )[1, "Version"],
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    file_md5 = tools::md5sum(c(
      result_file,
      analysis_script,
      estimation_file,
      raw_file,
      file.path(processed_dir, paste0(participant_ids, ".rds"))
    ))
  )
  
  utils::write.csv(
    parameter_recovery,
    file.path(output_dir, "parameter_recovery.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    parameter_recovery_summary,
    file.path(output_dir, "parameter_recovery_summary.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    intervals,
    file.path(output_dir, "recovery_fit_intervals.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    simulation_statistics,
    file.path(output_dir, "recovered_model_simulation_statistics.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    absolute_fit_statistics,
    file.path(output_dir, "absolute_fit_statistics.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    pointwise_squared_error,
    file.path(output_dir, "pointwise_squared_error.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    coverage_summary,
    file.path(output_dir, "coverage_summary.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    status,
    file.path(output_dir, "participant_status.csv"),
    row.names = FALSE,
    na = ""
  )
  saveRDS(run_manifest, file.path(output_dir, "run_manifest.Rds"))
  saveRDS(
    list(
      parameter_recovery = parameter_recovery,
      parameter_recovery_summary = parameter_recovery_summary,
      recovery_data_statistics = recovery_data_statistics,
      intervals = intervals,
      simulation_statistics = simulation_statistics,
      absolute_fit_statistics = absolute_fit_statistics,
      pointwise_squared_error = pointwise_squared_error,
      coverage_summary = coverage_summary,
      participant_status = status,
      manifest = run_manifest
    ),
    file.path(output_dir, "recovery_diagnostics.Rds")
  )
  
  grDevices::pdf(
    file.path(output_dir, "pointwise_squared_error.pdf"),
    width = 9,
    height = 4.5,
    onefile = TRUE
  )
  tryCatch(
    {
      for (participant_id in participant_ids) {
        participant_iterations <- unique(
          pointwise_squared_error$iteration[
            pointwise_squared_error$participant_id == participant_id
          ]
        )
        for (iteration in participant_iterations) {
          for (response_dimension in unique(
            pointwise_squared_error$response_dimension[
              pointwise_squared_error$participant_id == participant_id &
              pointwise_squared_error$iteration == iteration
            ]
          )) {
            panel <- pointwise_squared_error[
              pointwise_squared_error$participant_id == participant_id &
                pointwise_squared_error$iteration == iteration &
                pointwise_squared_error$response_dimension ==
                response_dimension,
              ,
              drop = FALSE
            ]
            errors <- panel$squared_error
            finite_errors <- errors[is.finite(errors)]
            y_limits <- if (length(finite_errors) > 0L) {
              range(c(0, finite_errors))
            } else {
              c(0, 1)
            }
            if (diff(y_limits) == 0) {
              y_limits <- y_limits + c(-0.05, 0.05)
              y_limits[1L] <- max(0, y_limits[1L])
            }
            graphics::plot(
              panel$trial,
              errors,
              type = "b",
              pch = 16,
              cex = 0.55,
              col = "steelblue",
              xlab = "Trial",
              ylab = paste(
                "Squared error:",
                participant_id,
                "/ iteration",
                iteration,
                "/",
                response_dimension
              ),
              main = "",
              ylim = y_limits
            )
          }
        }
      }
    },
    finally = grDevices::dev.off()
  )
  
  message("Saved recovery diagnostics to: ", output_dir)
  invisible(output_dir)
}

if (sys.nframe() == 0L) {
  arguments <- commandArgs(trailingOnly = TRUE)
  if (length(arguments) < 1L || length(arguments) > 3L) {
    if (interactive() && length(arguments) == 0L) {
      run_recovery_diagnostics(file.choose())
    } else {
      stop(
        "Usage: Rscript analysis/realistic_recovery/04_observed_data_diagnostics.R ",
        "<model-result.Rds> [replicates] [seed]"
      )
    }
  } else {
    replicates <- if (length(arguments) >= 2L) {
      suppressWarnings(as.numeric(arguments[2L]))
    } else {
      1000L
    }
    seed <- if (length(arguments) >= 3L) {
      suppressWarnings(as.numeric(arguments[3L]))
    } else {
      20261123L
    }
    run_recovery_diagnostics(
      arguments[1L],
      replicates = replicates,
      seed = seed
    )
  }
}

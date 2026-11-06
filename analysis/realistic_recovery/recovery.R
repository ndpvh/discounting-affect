################################################################################
# PURPOSE:
#
# Perform participant-based parameter recovery using estimated parameters and
# the participant's original stimulus sequence.
#
# Unlike the original recovery() function, this function does not generate new
# parameter sets. Each repetition uses the same estimated parameters and X,
# but newly simulated residuals. Simulated responses are clipped to [0, 1],
# and the original response-missingness pattern is restored before fitting.
#
# This file is sourced by analysis/realistic_recovery/03_run_recovery.R.
# The separate function name leaves the original recovery() unchanged.
################################################################################

#' Participant-based parameter recovery
#'
#' @param sim_model Empty model with the dimensionality of the participant's
#'   dataset. The same model is fitted back to the simulated data.
#' @param data Original participant dataset, containing X and the original Y
#'   missingness pattern. The observed Y values are not used as simulated values.
#' @param estimated_parameters Named numeric vector of estimated parameters,
#'   including the residual covariance entries returned by fit(). These entries
#'   are actual covariances, not Cholesky factors.
#' @param participant_id Character identifier for the participant.
#' @param iterations Number of simulated datasets per participant.
#' @param fx Optional named list of functions that each take a fit object and
#'   return one numeric statistic. No statistics are requested by default.
#' @param dynamics Structure of the dynamical matrices.
#' @param covariance Structure of the residual covariance matrix.
#' @param seed Optional seed used to generate a reproducible seed per repetition.
#' @param print_iteration Whether to print progress.
#' @param print_content Additional text in the progress message.
#' @param ... Additional arguments forwarded to fit(), including the optimizer.
#'
#' @return List with paired generating-parameter and recovered-parameter tables
#'   (simulate and fit), and a metadata table identifying each repetition.
setGeneric(
  "recovery_realistic",
  function(sim_model, data, ...) standardGeneric("recovery_realistic")
)

setMethod(
  "recovery_realistic",
  c("model", "dataset"),
  function(sim_model,
           data,
           estimated_parameters,
           participant_id,
           iterations = 1L,
           fx = list(),
           dynamics = "isotropic",
           covariance = "symmetric",
           seed = NULL,
           print_iteration = TRUE,
           print_content = "",
           ...) {
    
    ########################################################################
    # CHECK THE INPUT
    
    if (!is.numeric(iterations) || length(iterations) != 1L ||
        !is.finite(iterations) || iterations < 1 ||
        iterations != floor(iterations) ||
        iterations > .Machine$integer.max) {
      stop("\"iterations\" must be a positive integer.")
    }
    iterations <- as.integer(iterations)
    
    if (!is.character(participant_id) || length(participant_id) != 1L ||
        is.na(participant_id) || !nzchar(participant_id)) {
      stop("Provide one non-empty character participant_id.")
    }
    
    if (!is.matrix(data@X) || !is.numeric(data@X) ||
        !is.matrix(data@Y) || !is.numeric(data@Y)) {
      stop("The original X and Y must be numeric matrices.")
    }
    
    if (nrow(data@X) != nrow(data@Y) ||
        ncol(data@X) != sim_model@k || ncol(data@Y) != sim_model@d) {
      stop("The original dataset and simulation model must have matching dimensions.")
    }
    
    observed <- !is.na(data@Y)
    if (sum(complete.cases(data@Y)) < 2L) {
      stop("At least two complete response rows are needed to estimate covariance.")
    }
    
    if (any(!is.finite(data@Y[observed]))) {
      stop("Observed responses must be finite.")
    }
    
    if (any(data@Y[observed] < 0 | data@Y[observed] > 1)) {
      stop("Original responses are outside [0, 1]. Check their preprocessing and scale.")
    }
    
    # Use the package's parameter order rather than the order in the CSV.
    data_names <- parameter_names(
      sim_model,
      dynamics = dynamics,
      covariance = covariance,
      parameters_only = FALSE
    )
    
    if (!is.numeric(estimated_parameters) ||
        is.null(names(estimated_parameters)) ||
        anyNA(names(estimated_parameters)) ||
        anyDuplicated(names(estimated_parameters))) {
      stop("Estimated parameters must be a named numeric vector with unique names.")
    }
    
    missing_parameters <- setdiff(data_names, names(estimated_parameters))
    if (length(missing_parameters) > 0L) {
      stop("Missing generating parameters: ", paste(missing_parameters, collapse = ", "))
    }
    
    estimated_parameters <- estimated_parameters[data_names]
    if (any(!is.finite(estimated_parameters))) {
      stop("Generating parameters must all be finite.")
    }
    
    # Keep the original template for refitting. No empirical starting values
    # are passed to the optimizer; its search strategy remains unchanged.
    fit_model <- sim_model
    
    # fit() exports actual covariance entries, so do NOT square them through
    # a Cholesky reconstruction.
    sim_model <- fill(
      sim_model,
      estimated_parameters,
      dynamics = dynamics,
      covariance = covariance,
      parameters_only = FALSE,
      cholesky = FALSE
    )
    
    generating_covariance <- sim_model@covariance
    if (any(!is.finite(generating_covariance)) ||
        !isSymmetric(generating_covariance)) {
      stop("The generating covariance must be finite and symmetric.")
    }
    
    covariance_cholesky <- tryCatch(
      chol(generating_covariance),
      error = function(e) NULL
    )
    if (is.null(covariance_cholesky)) {
      stop("The generating covariance must be positive definite.")
    }
    
    if (!is.list(fx) || !all(vapply(fx, is.function, logical(1)))) {
      stop("\"fx\" must be a list of functions.")
    }
    if (length(fx) > 0L) {
      if (is.null(names(fx))) names(fx) <- paste0("fx_", seq_along(fx))
      if (anyNA(names(fx)) || any(!nzchar(names(fx))) ||
          anyDuplicated(names(fx)) ||
          any(names(fx) %in% c(data_names, "objective"))) {
        stop("Functions in fx need unique names that do not duplicate result columns.")
      }
    }
    
    if (!is.null(seed)) {
      if (!is.numeric(seed) || length(seed) != 1L ||
          !is.finite(seed) || seed < 0 || seed != floor(seed) ||
          seed > .Machine$integer.max) {
        stop("\"seed\" must be NULL or a non-negative integer.")
      }
      set.seed(as.integer(seed))
    }
    
    # Save the seed of each repetition so individual recoveries can be rerun.
    iteration_seeds <- sample.int(.Machine$integer.max, iterations)
    
    ########################################################################
    # PREDEFINE THE OUTPUT
    
    result <- list(
      simulate = matrix(
        NA_real_, nrow = iterations, ncol = length(data_names)
      ) |>
        as.data.frame() |>
        `colnames<-`(data_names),
      fit = matrix(
        NA_real_, nrow = iterations,
        ncol = length(data_names) + 1L + length(fx)
      ) |>
        as.data.frame() |>
        `colnames<-`(c(data_names, "objective", names(fx))),
      metadata = data.frame(
        participant_id = rep(participant_id, iterations),
        iteration = seq_len(iterations),
        seed = iteration_seeds,
        n_observed = rep(sum(observed), iterations),
        n_clipped_lower = integer(iterations),
        n_clipped_upper = integer(iterations),
        proportion_clipped = numeric(iterations),
        stringsAsFactors = FALSE
      )
    )
    
    ########################################################################
    # SIMULATE AND REFIT
    
    for (i in seq_len(iterations)) {
      if (print_iteration) {
        message(print_content, " | ", participant_id,
                " | repetition ", i, " / ", iterations)
      }
      set.seed(iteration_seeds[i])
      
      # The generating parameter vector stays fixed across repetitions.
      result$simulate[i, ] <- estimated_parameters
      
      # Use all original stimulus rows, including trials without a rating.
      simulated <- simulate(sim_model, X = data@X)
      
      if (!identical(dim(simulated@Y), dim(data@Y))) {
        stop("Simulation did not return the original response dimensions.")
      }
      if (any(!is.finite(simulated@Y[observed]))) {
        stop("Simulation produced non-finite responses at originally observed positions.")
      }
      
      # Count clipping only at positions used for parameter recovery.
      n_lower <- sum(simulated@Y[observed] < 0)
      n_upper <- sum(simulated@Y[observed] > 1)
      
      # Copy the original dataset so its X, ordering, labels, and other
      # dataset information are retained. Replace only its responses.
      simulated_data <- data
      simulated_data@Y[] <- pmin(pmax(simulated@Y, 0), 1)
      simulated_data@Y[!observed] <- NA_real_
      
      fitobj <- fit(
        fit_model,
        simulated_data,
        dynamics = dynamics,
        covariance = covariance,
        ...
      )
      
      recovered_parameters <- fitobj$parameters[data_names]
      if (any(!is.finite(recovered_parameters)) ||
          length(fitobj$objective) != 1L ||
          !is.finite(fitobj$objective)) {
        stop("Refitting returned missing or non-finite parameters or objective.")
      }
      
      summarized <- vapply(
        fx,
        function(fun) {
          value <- fun(fitobj)
          if (!is.numeric(value) || length(value) != 1L) {
            stop("Each function in fx must return one numeric value.")
          }
          return(as.numeric(value))
        },
        numeric(1)
      )
      
      result$fit[i, ] <- c(
        recovered_parameters, fitobj$objective, summarized
      )
      result$metadata$n_clipped_lower[i] <- n_lower
      result$metadata$n_clipped_upper[i] <- n_upper
      result$metadata$proportion_clipped[i] <-
        (n_lower + n_upper) / sum(observed)
    }
    
    return(result)
  }
)


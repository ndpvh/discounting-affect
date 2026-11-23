#' Simulate and estimate a model
#' 
#' This function takes in one or two models of a same dimensionality and 
#' simulates data with the first and estimates the second model on those data.
#' This allows users to perform recovery studies -- the topic of which is to 
#' find out how accurately the parameters a model can be estimated -- and to 
#' perform distinguishability studies -- the topic of which is to find out how
#' well we can distinguish between one or the other model based on their fit
#' or some statistics.
#' 
#' @param sim_model An instance of the \code{\link[discounting]{model-class}}
#' with which the data will be generated/simulated. Note that this model can 
#' be empty: Parameters will be simulated automatically within this function.
#' @param fit_model An instance of the \code{\link[discounting]{model-class}}
#' that will be estimated on the simulated data. Note that this model can be 
#' empty, but should have a same dimensionality (that is, \code{d} and \code{k})
#' as \code{sim_model}. By default, \code{fit_model} is equal to \code{sim_model}
#' @param iterations Integer denoting the number of iterations to run for the 
#' recovery/distinguishability study. Defaults to \code{100}.
#' @param fx Named list of functions that take the output of the estimation 
#' procedure and summarize it in a way. The function should only take in a 
#' single argument, it being the output of\code{\link[discounting]{fit}}, and 
#' should return a single value. Examples of functions that may be useful to 
#' include are estimates of \eqn{AIC} and \eqn{BIC}. Defaults to an empty list. 
#' @param dynamics,sim_dynamics,fit_dynamics Character denoting the structure of 
#' the dynamic parameters of the models. See \code{\link[discounting]{fill}} for 
#' guidance on their potential values. By default, both the simulation and 
#' fitting model are \code{"isotropic"}.
#' @param covariance,sim_covariance,fit_covariance Character denoting the 
#' structure of the covariance matrix for the models. See 
#' \code{\link[discounting]{fill}} for guidance on their potential values. By
#' default, both the simulation and fitting model are \code{"symmetric"}.
#' @param print_iteration Logical denoting whether to print the iteration of 
#' the recovery at this moment. Defaults to \code{TRUE}.
#' @param print_content Character containing information that you would wish 
#' to print alongside the iteration. Gets printed before the iteration itself.
#' Defaults to an empty string.
#' @param ... Arguments passed on to \code{\link[discounting]{fit}}.
#' @inheritParams fit
#' @inheritParams simulate,model-method
#' 
#' @return Named list containing a \code{data.frame} with simulated parameters
#' (under \code{"simulate"}) and a \code{data.frame} with estimated parameters, 
#' the result of the objective function, and other statistics that users can 
#' compute based on the output of \code{\link[discounting]{fit}}.
#' 
#' @examples 
#' # Create a study in which we simulate from a slightly more complicated model
#' # and estimate a simpler model, both based on the exponential discounting 
#' # model
#' recovery(
#'   exponential(d = 1, k = 1),
#' 
#'   # Specifications of the recovery
#'   iterations = 10,
#'   fx = list(
#'     "aic" = function(x) 
#'       (length(x$residuals) * log(x$objective) / length(x$residuals) + 2 * length(x$parameters))
#'   ),
#' 
#'   # Arguments for simulate
#'   Xfun = \(x) rnorm(x),
#'   N = 25, 
#'   sim_dynamics = "symmetric",
#'   sim_covariance = "symmetric",
#' 
#'   # Arguments for fit
#'   fit_dynamics = "isotropic",
#'   fit_covariance = "isotropic",
#'   itermax = 25,
#'   trace = FALSE
#' )
#' 
#' @rdname recovery
#' 
#' @concept simulation 
#' 
#' @export 
setGeneric(
    "recovery",
    function(sim_model, ...) standardGeneric("recovery")
)

#' @rdname recovery
#' @export
setMethod(
    "recovery",
    "model",
    function(sim_model,
             fit_model = NULL,
             iterations = 100,
             fx = list(),
             X = NULL,
             Xfun = NULL,
             N = NULL,
             dynamics = "isotropic",
             covariance = "symmetric",
             sim_dynamics = dynamics,
             sim_covariance = covariance,
             fit_dynamics = dynamics,
             fit_covariance = covariance,
             print_iteration = TRUE,
             print_content = "",
             ...) {

        # If fit_model is NULL, we will assign it the same model as sim_model
        if(is.null(fit_model)) {
            fit_model <- sim_model
        }
        
        # Ensure that fit_model is an instance of the model-class
        if(!inherits(fit_model, "model")) {
            stop("\"fit_model\" should be an instance of the model class.")
        }

        # Ensure the sim_model and fit_model have a same dimensionality
        if(sim_model@d != fit_model@d | sim_model@k != fit_model@k) {
            stop(
                paste(
                    "The simulation model and the model to be estimated should",
                    "have the same dimensionality."
                )
            )
        }

        # Check whether the list of functions to run on the data have names, 
        # and if not, then create some yourself
        if(is.null(names(fx)) & length(fx) != 0) {
            names(fx) <- paste(
                "fx_",
                1:length(fx),
                sep = ""
            )
        }

        # Predefine the data.frame's that will contain the simulated parameters 
        # and the estimated parameters
        combination <- list(
            list(sim_model, sim_dynamics, sim_covariance),
            list(fit_model, fit_dynamics, fit_covariance)
        )

        result <- lapply(
            seq_along(combination),
            function(i) {
                # Get the dimensionality of the model
                n <- count_parameters(
                    combination[[i]][[1]], 
                    dynamics = combination[[i]][[2]], 
                    covariance = combination[[i]][[3]],
                    parameters_only = FALSE
                )

                # Get the names of the model
                data_names <- parameter_names(
                    combination[[i]][[1]], 
                    dynamics = combination[[i]][[2]], 
                    covariance = combination[[i]][[3]],
                    parameters_only = FALSE
                )

                # Add names to it if the data.frame to creat is for the results
                # of the estimation, in the least having the objective in it
                if(i == 2) {
                    data_names <- c(data_names, "objective", names(fx))
                }

                # Create the actual data.frame itself
                output <- matrix(0, nrow = iterations, ncol = length(data_names)) |>
                    as.data.frame() |>
                    `colnames<-` (data_names)

                return(output)
            }
        ) |>
            `names<-` (c("simulate", "fit"))

        # Loop over each of the iterations and perform the simulation study
        for(i in seq_len(iterations)) {
            # If you need to print content, do so
            if(print_iteration) {
                cat("\r", print_content, i, paste(rep(" ", 10), sep = ""))
            }

            # Generate parameters of the simulation model
            parameters <- generate_parameters(
                sim_model,
                dynamics = sim_dynamics,
                covariance = sim_covariance,
                parameters_only = FALSE
            )
            sim_model <- fill(
                sim_model,
                parameters,
                dynamics = sim_dynamics,
                covariance = sim_covariance,
                parameters_only = FALSE,
                cholesky = TRUE
            )
            result$simulate[i, ] <- parameters(
                sim_model,
                dynamics = sim_dynamics,
                covariance = sim_covariance,
                parameters_only = FALSE
            )

            # Simulate data according to the specifications
            data <- simulate(
                sim_model,
                X = X,
                Xfun = Xfun,
                N = N
            )

            # Estimate the model back using the simulated data
            fitobj <- fit(
                fit_model,
                data, 
                dynamics = fit_dynamics,
                covariance = fit_covariance,
                ...
            )

            # Peform the functions on the fitobj to summarize the output 
            # except for only the parameter values
            summarized <- sapply(
                seq_along(fx),
                \(i) fx[[i]](fitobj)
            )

            # Put everything in the result list
            result$fit[i, ] <- c(
                fitobj$parameters,
                fitobj$objective,
                summarized
            )
        }

        # Skip ahead if the recovery is done
        # if(print_iteration) {
        #     cat("\n")
        # }

        return(result)
    }
)


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
# This function is part of the package alongside the original recovery().
# The separate function name preserves the behavior of recovery().
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
#' @export
setGeneric(
  "recovery_realistic",
  function(sim_model, data, ...) standardGeneric("recovery_realistic")
)


#' @rdname recovery_realistic
#' @export
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
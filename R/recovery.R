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
                cat("\r", print_content, i)
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
            result$simulate[i, ] <- parameters

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
        if(print_iteration) {
            cat("\n")
        }

        return(result)
    }
)
#' Objective function
#' 
#' This method defines how the objective function of the estimation procedure 
#' looks like. The result of this objective function is the sum squared error,
#' meaning the estimation routine makes use of a least-squares procedure.
#' 
#' @details 
#' For the purposes of the package, the objective function defines how the 
#' estimation routine should go about, therefore defining the procedure with 
#' which the fit of the model to the data under a particular set of parameters
#' is evaluated. Within this package, we use the least-squares procedure, 
#' therefore minimizing the sum of squared error (\eqn{SSE}), defined as:
#' 
#' \deqn{SSE = Y - \hat{Y}}
#' 
#' where \eqn{Y} represents the observed data and \eqn{\hat{Y}} the predictions
#' of the model under the proposed parameter set. 
#' 
#' The procedure as used here relies heavily on the implementation of the 
#' \code{\link[discounting]{predict}} method to get the predictions 
#' \eqn{\hat{Y}}. If a recovery does not go well, it may be useful to take a 
#' look at how this method is defined for your particular model.
#' 
#' Note that the objective function is only useful when using a non-analytic 
#' approach to estimation, as the one used in this package.
#' 
#' @param model Instance of the \code{\link[discounting]{model-class}}, 
#' defining the model to evaluate the objective function for.
#' @param data Instance of the \code{\link[discounting]{dataset-class}}
#' containing the data to fit the model to.
#' @param parameters Numeric vector containing the parameters of the model to 
#' compute the \eqn{SSE} for.
#' @param dynamics Character denoting the structure of the dynamical matrices.
#' Can either be \code{"anisotropic"} (completely free), \code{"symmetric"}
#' (symmetric around the diagonal), and \code{"isotropic"} (diagonal). Note that
#' this influences different parameters for different models, namely 
#' \eqn{\Gamma} for the exponential discounting model, \eqn{N} and \eqn{K} for
#' the quasi-hyperbolic discounting model, and \eqn{\Gamma} and \eqn{N} for the
#' double-exponential discounting model. Defaults to \code{"isotropic"}.
#' @param ... Additional arguments passed on to the methods.
#' 
#' @return The sum of squared error or \eqn{SSE}, quantifying misfit of the model
#' to the data.
#' 
#' @examples
#' # Simulate data to use for this example
#' data <- simulate(
#'   quasi_hyperbolic(
#'     parameters = list(
#'       "alpha" = 1, 
#'       "beta" = as.matrix(2),
#'       "nu" = as.matrix(0.75),
#'       "kappa" = as.matrix(0.5)
#'     ),
#'     covariance = as.matrix(1)
#'   ),
#'   X = rnorm(100)
#' )
#' 
#' # Evaluate the objective function for an exponential model with a particular
#' # set of parameters
#' objective_function(
#'   exponential(),
#'   data,
#'   c(1, 2, 0.75)
#' )
#' 
#' # Evaluate the objective function when using exactly the model and the 
#' # parameters that generated the data
#' objective_function(
#'   quasi_hyperbolic(),
#'   data,
#'   c(1, 2, 0.75, 0.5)
#' )
#' 
#' @rdname objective_function
#' @export 
setGeneric(
    "objective_function",
    function(model, data, parameters, ...) standardGeneric("objective_function") 
)

#' @rdname objective_function
#' @export 
setMethod(
    "objective_function",
    c("model", "dataset"),
    function(model, 
             data,
             parameters,
             dynamics = "isotropic") {
        
        # Put the parameters inside the model format
        model <- fill(
            model, 
            parameters, 
            dynamics = dynamics, 
            parameters_only = TRUE
        )

        # Use the model to predict what the data should look like according to 
        # the model
        prediction <- predict(model, data)

        # Compare model predictions to the data
        residuals <- data@Y - prediction@Y 

        # Compute the sum squared error and return
        SSE <- sum(residuals^2)

        return(SSE)
    }
)


#' Fit a model to data
#' 
#' This method enables the user to fit a model (defined in \code{model}) on a
#' particular dataset (defined in \code{data}). The estimation procedure makes
#' use of numerical optimization using either \code{\link[DEoptim]{DEoptim}} or
#' \code{\link[nloptr]{nloptr}}, as specified by the user. Estimation proceeds
#' through an optimization according to the output of the objective function 
#' of the provided model, as defined through 
#' \code{\link[discounting]{objective_function}}, thus using least-squares
#' as optimization standard.
#' 
#' @details 
#' Note that currently, least-squares estimation is assumed, meaning that the 
#' covariance matrix is left out of the objective function. If maximum-likelihood
#' were needed instead, then this function would need to change.
#' 
#' @param model Instance of the \code{\link[discounting]{model-class}}, 
#' defining the model to evaluate the objective function for.
#' @param data Instance of the \code{\link[discounting]{dataset-class}}
#' containing the data to fit the model to.
#' @param dynamics Character denoting the structure of the dynamical matrices.
#' Can either be \code{"anisotropic"} (completely free), \code{"symmetric"}
#' (symmetric around the diagonal), and \code{"isotropic"} (diagonal). Note that
#' this influences different parameters for different models, namely 
#' \eqn{\Gamma} for the exponential discounting model, \eqn{N} and \eqn{K} for
#' the quasi-hyperbolic discounting model, and \eqn{\Gamma} and \eqn{N} for the
#' double-exponential discounting model. Defaults to \code{"isotropic"}.
#' @param covariance Character denoting the structure of covariance matrix.
#' Can either by \code{"symmetric"} (symmetric around the diagonal) and 
#' \code{"isotropic"} (diagonal). Defaults to \code{"symmetric"}.
#' @param optimizer Character denoting the optimizer to use for the estimation.
#' Can either be \code{"DEoptim"} for the differential evolution algorithm in 
#' \code{\link[DEoptim]{DEoptim}} or \code{"nloptr"} for the library implemented
#' in \code{\link[nloptr]{nloptr}}. Defaults to \code{"DEoptim"}.
#' @param ... Arguments passed on to the control parameters of the optimizer, 
#' either to \code{\link[DEoptim]{DEoptim.control}} or the \code{opts} 
#' argument of \code{\link[nloptr]{nloptr}}.
#' 
#' @return An named list containing an instance of the 
#' \code{\link[discounting]{model-class}} with the estimated parameters 
#' (\code{"model"}), the results of the optimization procedure (\code{"fit"}), 
#' the value of the objective after ending the optimization procedure 
#' (\code{"objective"}), the residuals of the model (\code{"residuals"}), and a 
#' named vector containing the values of the estimated parameters linked to a 
#' character vector explaining their content (\code{"parameters"}). 
#' 
#' @examples
#' # Simulate data to use for this example
#' data <- simulate(
#'   quasi_hyperbolic(
#'     parameters = list(
#'       "alpha" = c(1, -1) ,
#'       "beta" = matrix(2, nrow = 2, ncol = 2),
#'       "nu" = diag(2) * 0.75,
#'       "kappa" = diag(2) * 0.5
#'     ),
#'     covariance = matrix(c(1, 0.25, 0.25, 1), nrow = 2, ncol = 2)
#'   ),
#'   X = matrix(rnorm(200), nrow = 100, ncol = 2)
#' )
#' 
#' # Evaluate the objective function for an exponential model with a particular
#' # set of parameters
#' fit(
#'   exponential(d = 2, k = 2),
#'   data,
#'   dynamics = "isotropic",
#'   covariance = "isotropic",
#'   itermax = 50,
#'   trace = FALSE
#' )
#' 
#' @rdname fit
#' @export 
setGeneric(
    "fit",
    function(model, data, ...) standardGeneric("fit") 
)

#' @rdname fit
#' @export  
setMethod(
    "fit",
    c("model", "dataset"),
    function(model, 
             data, 
             dynamics = "isotropic",
             covariance = "symmetric",
             optimizer = "DEoptim",
             ...) {
        
        # Extract the bounds of the model to be optimized
        bounds <- get_bounds(model, dynamics = dynamics)

        # Prepare the objective function for the optimization
        obj <- function(x) objective_function(
            model,
            data,
            x, 
            dynamics = dynamics
        )

        # Perform the estimation procedure according to the optimizer chosen
        # by the user
        if(optimizer == "DEoptim") {
            # Run the optimization
            result <- DEoptim::DEoptim(
                obj,
                lower = bounds$lower,
                upper = bounds$upper,
                control = DEoptim.control(
                    ...
                )
            )

            # Extract those things we definitely need
            parameters <- result$optim$bestmem
            objective <- result$optim$bestval

        } else if(optimizer == "nloptr") {
            # Define the initial condition
            x0 <- runif(
                length(bounds$lower),
                min = bounds$lower, 
                max = bounds$upper
            )

            # Run the optimization
            result <- nloptr::nloptr(
                x0, 
                eval_f = obj,
                lb = bounds$lower,
                ub = bounds$upper,
                opts = list(
                    ...
                )
            )

            # Extract those things we definitely need
            parameters <- result$solution
            objective <- result$objective

        } else {
            stop("Optimizer is not recognized. Please use \"DEoptim\" or \"nloptr\".")
        }

        # Add the parameters to the model that was estimated
        model <- fill(
            model,
            parameters,
            dynamics = dynamics,
            parameters_only = TRUE
        )

        # Estimate the covariance matrix according to the structure of the 
        # model
        residuals <- data@Y - predict(model, data)@Y
        if(covariance == "symmetric") {
            v <- cov(residuals)

            model@covariance <- as.matrix(v)
            parameters <- c(parameters, v[lower.tri(v, diag = TRUE)])

        } else if(covariance == "isotropic") {
            v <- sapply(
                seq_len(ncol(residuals)),
                var
            )

            diag(model@covariance) <- v
            parameters <- c(parameters, v)

        }

        # Define the number of parameters of the model and adjust accordingly
        model@n <- count_parameters(
            model,
            dynamics = dynamics,
            covariance = covariance,
            parameters_only = FALSE
        )

        # Add context to the vector of parameters of the model.
        names(parameters) <- parameter_names(
            model,
            dynamics = dynamics,
            covariance = covariance,
            parameters_only = FALSE
        )

        # Join everything together in a named list and return
        fitobj <- list(
            "model" = model,
            "fit" = result,
            "objective" = objective,
            "residuals" = residuals,
            "parameters" = parameters
        )

        return(fitobj)
    }
)

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
#' @param na.rm Logical denoting whether to remove NAs in the calculation of the 
#' sum squared error. Defaults to \code{TRUE}.
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
#' 
#' @concept estimation
#' 
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
             dynamics = "isotropic",
             na.rm = TRUE) {

        # Check whether the parameters are inside the bounds. If not, return a 
        # penalty
        bounds <- get_bounds(
            model,
            dynamics = dynamics,
            parameters_only = TRUE
        )

        if(any(parameters < bounds$lower | parameters > bounds$upper)) {
            return(Inf)
        }
        
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
        SSE <- sum(residuals^2, na.rm = na.rm)

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
#' By default, the \code{fit} function performs optimizations through the 
#' \code{optim} function in R, alternatively allowing users to make use of the
#' \code{DEoptim} or \code{nloptr} packages instead. For more difficult models,
#' however, it may be useful to have greater control over the optimization 
#' procedure, for example by specifying your own initial conditions, by using 
#' a combination of different optimization algorithms at once, or by using 
#' another library not provided through this package. 
#' 
#' In these cases, one can provide a function to the argument \code{optimizer}
#' instead of a string. This function should at least take in 3 argument: A
#' function representing the objective function, a lower bound for the 
#' parameters, and an upper bound for these same parameters (cf. \code{optim}, 
#' \code{DEoptim}, and \code{nloptr}). This function can furthermore take in 
#' additional argument, for example specifying the maximal number of iterations
#' or controlling other aspects of the estimation routine. These can be specified
#' in the \code{fit} function through the optional arguments \code{...}.
#' 
#' The custom optimization function should, minimally, output a named list with 
#' slots \code{"parameters"} and \code{"objective"}, containing the final 
#' result of the parameter estimation and the value of the objective function 
#' for this final parameter set respectively. These will then be used by the 
#' \code{fit} function as final results of the optimization procedure. 
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
#' \code{\link[DEoptim]{DEoptim}}, \code{"nloptr"} for the library implemented
#' in \code{\link[nloptr]{nloptr}}, or \code{"optim"} for the default 
#' optimization library in R. Defaults to \code{"optim"}. Alternatively, 
#' \code{optimizer} can also be a custom function that defines the estimation 
#' routine. To see what this function should look like, we refer the interested
#' reader to the details.
#' @param lower,upper Numeric vector containing lower and upper bounds for the 
#' parameters in the estimation routine. Uses the same defaults as 
#' \code{\link[discounting]{get_bounds}}.
#' @param method Character denoting the method to be used for estimation. Ignored
#' if \code{optimizer} is not \code{"optim"}, in which case the native arguments
#' of the other optimization packages can be used.
#' @param na.rm Logical denoting whether to remove NAs in the calculation of the 
#' sum squared error. Defaults to \code{TRUE}.
#' @param ... Arguments passed on to the control parameters of the optimizer, 
#' either to \code{\link[DEoptim]{DEoptim.control}}, the \code{opts} 
#' argument of \code{\link[nloptr]{nloptr}}, the \code{control} list of the 
#' \code{\link[stats]{optim}} function, or to the additional arguments of your
#' own \code{optimizer} function (see Details).
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
#'   optimizer = "DEoptim",
#'   itermax = 50,
#'   trace = FALSE
#' )
#' 
#' @rdname fit
#' 
#' @concept estimation
#' 
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
             optimizer = "optim",
             lower = NULL, 
             upper = NULL,
             method = "Nelder-Mead",
             na.rm = TRUE,
             ...) {
        
        # Extract the bounds of the model to be optimized
        bounds <- get_bounds(
            model, 
            dynamics = dynamics,
            lower = lower, 
            upper = upper
        )

        # Prepare the objective function for the optimization
        obj <- function(x) objective_function(
            model,
            data,
            x, 
            dynamics = dynamics,
            na.rm = na.rm
        )

        # Perform the estimation procedure according to the optimizer chosen
        # by the user
        if(!is.character(optimizer) & is.function(optimizer)) {
            # Run the custom optimization
            result <- optimizer(
                obj, 
                lower = bounds$lower, 
                upper = bounds$upper,
                ...
            )

            # Extract those things we need
            parameters <- result$parameters 
            objective <- result$objective
        
        } else if(optimizer == "DEoptim") {
            # Check whether DEoptim is installed
            if(!require("DEoptim", quietly = TRUE)) {
                stop("The package \"DEoptim\" is required for the estimation but not installed.")
            }

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
            # Check whether DEoptim is installed
            if(!require("nloptr", quietly = TRUE)) {
                stop("The package \"nloptr\" is required for the estimation but not installed.")
            }

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

        } else if(optimizer == "optim") {
            # Define the initial condition
            x0 <- runif(
                length(bounds$lower),
                min = bounds$lower, 
                max = bounds$upper
            )

            # Change the upper and lower bounds if necessary: Only L-BFGS-B 
            # allows for bounded estimation in optim. Change back to the 
            # defaults in this case
            if(method != "L-BFGS-B") {
                bounds$lower <- -Inf
                bounds$upper <- Inf
            }

            # Run the optimization
            result <- stats::optim(
                x0, 
                fn = obj,
                method = method,
                lower = bounds$lower,
                upper = bounds$upper,
                control = list(
                    ...
                )
            )

            # Extract those things we definitely need
            parameters <- result$par
            objective <- result$value

        } else {
            stop("Optimizer is not recognized. Please use \"DEoptim\", \"nloptr\", or \"optim\".")
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
        non_missing_residuals <- residuals[!is.na(rowSums(residuals)), , drop = FALSE]

        if(covariance == "symmetric") {
            v <- cov(non_missing_residuals)

            model@covariance <- as.matrix(v)
            parameters <- c(parameters, v[lower.tri(v, diag = TRUE)])

        } else if(covariance == "isotropic") {
            if(is.matrix(non_missing_residuals)) {
                v <- sapply(
                    seq_len(ncol(non_missing_residuals)),
                    \(i) var(non_missing_residuals[, i])
                )
            } else {
                v <- var(non_missing_residuals)
            }

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

#' @param deterministic_fit Logical denoting whether to estimate the parameters 
#' of the distributed lag model through the analytic least-squares solution 
#' (\code{TRUE}) or through the numerical optimization used for the other 
#' models (\code{FALSE}). Only used for the 
#' \code{\link[discounting]{distributed_lag-class}}. When \code{TRUE}, the 
#' arguments \code{dynamics}, \code{optimizer}, \code{lower}, \code{upper}, 
#' \code{method}, and \code{...} are ignored, as no numerical optimization 
#' (and therefore no bounds) is involved. Defaults to the value of the option
#' \code{"discounting.deterministic_fit"}, or \code{FALSE} if this option is not
#' set. Setting \code{options(discounting.deterministic_fit = TRUE)} therefore 
#' makes all fits of the distributed lag model analytic, including those 
#' performed inside of other functions (e.g., \code{\link[discounting]{recovery}}),
#' while the other models remain estimated through numerical optimization.
#' 
#' @rdname fit
#' @export  
setMethod(
    "fit",
    c("distributed_lag", "dataset"),
    function(model, 
             data, 
             deterministic_fit = getOption("discounting.deterministic_fit", FALSE),
             dynamics = "isotropic",
             covariance = "symmetric",
             optimizer = "optim",
             lower = NULL, 
             upper = NULL,
             method = "Nelder-Mead",
             na.rm = TRUE,
             ...) {

        # If no deterministic fit is requested, default to the numerical 
        # optimization defined for all models
        if(!deterministic_fit) {
            return(
                callNextMethod(
                    model, 
                    data, 
                    dynamics = dynamics,
                    covariance = covariance,
                    optimizer = optimizer,
                    lower = lower,
                    upper = upper,
                    method = method,
                    na.rm = na.rm,
                    ...
                )
            )
        }

        # Extract the dimensionalities of the model
        d <- model@d
        k <- model@k
        lag <- model@lag

        # Extract the data and check whether these are consistent with the 
        # model
        X <- data@X
        Y <- data@Y

        if(ncol(X) != k) {
            stop("Number of independent variables in the data does not match k of the model.")
        }

        if(ncol(Y) != d) {
            stop("Number of dependent variables in the data does not match d of the model.")
        }

        # Create the design matrix containing an intercept and the lagged 
        # values of the independent variables. This is done in the same way as
        # in `predict`: Values before the start of the data are assumed to be 
        # 0, and the history is reset whenever the independent variables 
        # contain an NA. The ordering of the columns follows the ordering of 
        # the columns in beta, namely B = [B_0, B_1, ..., B_lag].
        Z <- matrix(0, nrow = data@N, ncol = 1 + k * (lag + 1))
        H <- matrix(0, nrow = k, ncol = lag + 1)

        for(i in seq_len(data@N)) {
            if(any(is.na(X[i, ]))) {
                Z[i, ] <- NA
                H[] <- 0

                next
            }

            H <- cbind(X[i, ], H[, seq_len(lag), drop = FALSE])
            Z[i, ] <- c(1, as.numeric(H))
        }

        # Only use the complete cases for the estimation
        idx <- stats::complete.cases(Z, Y)
        if(sum(idx) < ncol(Z)) {
            stop("Not enough complete observations to estimate the parameters of the model.")
        }

        # Compute the analytic least-squares solution
        Z_c <- Z[idx, , drop = FALSE]
        Y_c <- Y[idx, , drop = FALSE]
        coefficients <- solve(t(Z_c) %*% Z_c) %*% t(Z_c) %*% Y_c
        
        # Put the estimated parameters in the model. Ordering of the parameters
        # follows the conventions of `fill` and `index`
        model@parameters[["alpha"]] <- as.numeric(coefficients[1, ])
        model@parameters[["beta"]] <- t(coefficients[-1, , drop = FALSE])

        parameters <- c(
            model@parameters[["alpha"]], 
            as.numeric(model@parameters[["beta"]])
        )

        # Compute the residuals and the objective function
        residuals <- Y - predict(model, data)@Y
        objective <- sum(residuals^2, na.rm = na.rm)

        # Estimate the covariance matrix according to the structure of the 
        # model
        non_missing_residuals <- residuals[!is.na(rowSums(residuals)), , drop = FALSE]

        if(covariance == "symmetric") {
            v <- cov(non_missing_residuals)

            model@covariance <- as.matrix(v)
            parameters <- c(parameters, v[lower.tri(v, diag = TRUE)])

        } else if(covariance == "isotropic") {
            v <- sapply(
                seq_len(ncol(non_missing_residuals)),
                \(i) var(non_missing_residuals[, i])
            )

            model@covariance[] <- 0
            diag(model@covariance) <- v
            parameters <- c(parameters, v)

        } else {
            stop(
                paste(
                    "Structure for covariance is not know.",
                    "Please use \"isotropic\" or \"symmetric\"."
                )
            )
        }

        # Define the number of parameters of the model and adjust accordingly
        model@n <- count_parameters(
            model,
            covariance = covariance,
            parameters_only = FALSE
        )

        # Add context to the vector of parameters of the model.
        names(parameters) <- parameter_names(
            model,
            covariance = covariance,
            parameters_only = FALSE
        )

        # Join everything together in a named list and return
        fitobj <- list(
            "model" = model,
            "fit" = coefficients,
            "objective" = objective,
            "residuals" = residuals,
            "parameters" = parameters
        )

        return(fitobj)
    }
)

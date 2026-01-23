################################################################################
# FILL

#' Change model parameters
#' 
#' Fill the model parameters of a particular instance of the 
#' \code{\link[discounting]{model-class}} with the ones provided in a numeric
#' vector. Used in \code{\link[discounting]{objective_function}} to allow for
#' predictions with the model while cohering to the input typical numerical 
#' optimizers such as \code{\link[DEoptim]{DEoptim}} and 
#' \code{\link[nloptr]{nloptr}} provide within the objective function.
#' 
#' @param model Instance of the \code{\link[discounting]{model-class}}
#' @param parameters Numeric vector containing the values of the parameters 
#' that should be assigned to the model. Importantly, the number of parameters
#' within this vector should correspond exactly to the number of parameters 
#' needed by the model. Ideally, you let functions internal to this package
#' handle this for you, rather than you defining these parameters manually.
#' @param dynamics Character denoting the structure of the dynamical matrices.
#' Can either be \code{"anisotropic"} (completely free), \code{"symmetric"}
#' (symmetric around the diagonal), and \code{"isotropic"} (diagonal). Note that
#' this influences different parameters for different models, namely 
#' \eqn{\Gamma} for the exponential discounting model, \eqn{N} and \eqn{K} for
#' the quasi-hyperbolic discounting model, and \eqn{\Gamma} and \eqn{N} for the
#' double-exponential discounting model. Defaults to \code{"isotropic"}.
#' @param parameters_only Logical denoting whether to only fill the 
#' parameters in de \code{parameter} slot of the model (\code{TRUE}), or to 
#' fill the covariance matrix as well (\code{FALSE}). Defaults to \code{TRUE}.
#' @param ... Additional arguments passed on to the methods.
#' @inheritParams fill_covariance
#' 
#' @return Instance of the \code{\link[discounting]{model-class}} containing 
#' the values \code{parameters} in its \code{parameter}-slot.
#' 
#' @examples 
#' # Create an empty instance of the exponential discounting model
#' my_model <- exponential()
#' 
#' # Assign the parameters of a vector to this exponential function
#' fill(
#'   my_model,
#'   c(1, 2, 0.5)
#' )
#' 
#' @rdname fill
#' @export 
setGeneric(
    "fill",
    function(model, parameters, ...) standardGeneric("fill")
)

#' @rdname fill
#' @export 
setMethod(
    "fill",
    "exponential",
    function(model,
             parameters,
             dynamics = "isotropic",
             covariance = "symmetric",
             parameters_only = TRUE,
             cholesky = TRUE) {
        
        # Extract relevant dimensionalities from the model
        d <- model@d 
        k <- model@k
        n <- count_parameters(
            model, 
            dynamics = dynamics, 
            covariance = covariance,
            parameters_only = parameters_only
        )

        # Check whether a sufficient number of parameters are defined
        if(length(parameters) < n) {
            stop(
                paste(
                    n, 
                    "parameters needed, but only",
                    length(parameters),
                    "provided for an exponential model with",
                    dynamics,
                    "forgetting matrix",
                    ifelse(
                        parameters_only,
                        ".",
                        paste(
                            "and",
                            covariance,
                            "covariance matrix."
                        )
                    )
                )
            )
        } else if(length(parameters) > n) {
            warning(
                paste(
                    n, 
                    "parameters needed, but",
                    length(parameters),
                    "provided for an exponential model with",
                    dynamics,
                    "forgetting matrix",
                    ifelse(
                        parameters_only,
                        ".",
                        paste(
                            "and",
                            covariance,
                            "covariance matrix."
                        )
                    ),
                    "Using only the first",
                    n, 
                    "to define the model."
                )
            )

            parameters <- parameters[1:n]
        }

        # Once defined, we can start assigning values to the parameters of the 
        # model. When necessary, dispath on the structure of the parameters
        params <- model@parameters 

        params[["alpha"]][] <- parameters[1:d]
        params[["beta"]][] <- parameters[(d + 1):(d + d * k)]

        if(dynamics == "isotropic") {
            idx <- (d + d * k + 1):(d + d * k + d)
            diag(params[["gamma"]]) <- parameters[idx]
        
        } else if(dynamics == "symmetric") {
            idx <- (d + d * k + 1):(d + d * k + d * (d + 1) / 2)

            idy <- lower.tri(params[["gamma"]], diag = TRUE)
            params[["gamma"]][idy] <- parameters[idx]

            idy <- upper.tri(params[["gamma"]], diag = FALSE)
            params[["gamma"]][idy] <- t(params[["gamma"]])[idy]

        } else if(dynamics == "anisotropic") {
            idx <- (d + d * k + 1):(d + d * k + d^2)
            params[["gamma"]][] <- parameters[idx]
        }

        # If we need to fill the covariance as well, then do so
        if(!parameters_only) {
            model@covariance <- fill_covariance(
                d,
                parameters = parameters[(max(idx) + 1):(length(parameters))],
                covariance = covariance,
                cholesky = cholesky
            )
        }

        # Assign the parameters to the model and return
        model@parameters <- params
        
        return(model)
    }
)

#' @rdname fill
#' @export 
setMethod(
    "fill",
    "quasi_hyperbolic",
    function(model,
             parameters,
             dynamics = "isotropic",
             covariance = "symmetric",
             parameters_only = TRUE,
             cholesky = TRUE) {
        
        # Extract relevant dimensionalities from the model
        d <- model@d 
        k <- model@k
        n <- count_parameters(
            model, 
            dynamics = dynamics, 
            covariance = covariance,
            parameters_only = parameters_only
        )

        # Check whether a sufficient number of parameters are defined
        if(length(parameters) < n) {
            stop(
                paste(
                    n, 
                    "parameters needed, but only",
                    length(parameters),
                    "provided for an quasi-hyperbolic model with",
                    dynamics,
                    "forgetting matrix",
                    ifelse(
                        parameters_only,
                        ".",
                        paste(
                            "and",
                            covariance,
                            "covariance matrix."
                        )
                    )
                )
            )
        } else if(length(parameters) > n) {
            warning(
                paste(
                    n, 
                    "parameters needed, but",
                    length(parameters),
                    "provided for an quasi-hyperbolic model with",
                    dynamics,
                    "forgetting matrix",
                    ifelse(
                        parameters_only,
                        ".",
                        paste(
                            "and",
                            covariance,
                            "covariance matrix."
                        )
                    ),
                    "Using only the first",
                    n, 
                    "to define the model."
                )
            )

            parameters <- parameters[1:n]
        }

        # Once defined, we can start assigning values to the parameters of the 
        # model. When necessary, dispath on the structure of the parameters
        params <- model@parameters 

        params[["alpha"]][] <- parameters[1:d]
        params[["beta"]][] <- parameters[(d + 1):(d + d * k)]

        if(dynamics == "isotropic") {
            idx_1 <- (d + d * k + 1):(d + d * k + d)
            idx_2 <- (d + d * k + d + 1):(d + d * k + 2 * d)

            diag(params[["nu"]]) <- parameters[idx_1]
            diag(params[["kappa"]]) <- parameters[idx_2]
        
        } else if(dynamics == "symmetric") {
            idx_1 <- (d + d * k + 1):(d + d * k + d * (d + 1) / 2)
            idx_2 <- (d + d * k + d * (d + 1) / 2 + 1):(d + d * k + 2 * d * (d + 1) / 2)

            idy <- lower.tri(params[["nu"]], diag = TRUE)
            params[["nu"]][idy] <- parameters[idx_1]
            params[["kappa"]][idy] <- parameters[idx_2]

            idy <- upper.tri(params[["nu"]], diag = FALSE)
            params[["nu"]][idy] <- t(params[["nu"]])[idy]
            params[["kappa"]][idy] <- t(params[["kappa"]])[idy]

        } else if(dynamics == "anisotropic") {
            idx_1 <- (d + d * k + 1):(d + d * k + d^2)
            idx_2 <- (d + d * k + d^2 + 1):(d + d * k + 2 * d^2)

            params[["nu"]][] <- parameters[idx_1]
            params[["kappa"]][] <- parameters[idx_2]

        } 

        # If we need to fill the covariance as well, then do so
        if(!parameters_only) {
            model@covariance <- fill_covariance(
                d,
                parameters = parameters[(max(idx_2) + 1):(length(parameters))],
                covariance = covariance,
                cholesky = cholesky
            )
        }

        # Assign the parameters to the model and return
        model@parameters <- params
        
        return(model)
    }
)

#' @rdname fill
#' @export 
setMethod(
    "fill",
    "double_exponential",
    function(model,
             parameters,
             dynamics = "isotropic",
             covariance = "symmetric",
             parameters_only = TRUE,
             cholesky = TRUE) {
        
        # Extract relevant dimensionalities from the model
        d <- model@d 
        k <- model@k
        n <- count_parameters(
            model, 
            dynamics = dynamics, 
            covariance = covariance,
            parameters_only = parameters_only
        )

        # Check whether a sufficient number of parameters are defined
        if(length(parameters) < n) {
            stop(
                paste(
                    n, 
                    "parameters needed, but only",
                    length(parameters),
                    "provided for an double exponential model with",
                    dynamics,
                    "forgetting matrix",
                    ifelse(
                        parameters_only,
                        ".",
                        paste(
                            "and",
                            covariance,
                            "covariance matrix."
                        )
                    )
                )
            )
        } else if(length(parameters) > n) {
            warning(
                paste(
                    n, 
                    "parameters needed, but",
                    length(parameters),
                    "provided for an double exponential model with",
                    dynamics,
                    "forgetting matrix",
                    ifelse(
                        parameters_only,
                        ".",
                        paste(
                            "and",
                            covariance,
                            "covariance matrix."
                        )
                    ),
                    "Using only the first",
                    n, 
                    "to define the model."
                )
            )

            parameters <- parameters[1:n]
        }

        # Once defined, we can start assigning values to the parameters of the 
        # model. When necessary, dispath on the structure of the parameters
        params <- model@parameters 

        params[["alpha"]][] <- parameters[1:d]
        params[["beta"]][] <- parameters[(d + 1):(d + d * k)]
        params[["omega"]][] <- parameters[(d + d * k + 1)]

        if(dynamics == "isotropic") {
            idx_1 <- (d + d * k + 1 + 1):(d + d * k + d + 1)
            idx_2 <- (d + d * k + d + 1 + 1):(d + d * k + 2 * d + 1)

            diag(params[["gamma"]]) <- parameters[idx_1]
            diag(params[["nu"]]) <- parameters[idx_2]
        
        } else if(dynamics == "symmetric") {
            idx_1 <- (d + d * k + 1 + 1):(d + d * k + 1 + d * (d + 1) / 2)
            idx_2 <- (d + d * k + 1 + d * (d + 1) / 2 + 1):(d + d * k + 1 + 2 * d * (d + 1) / 2)

            idy <- lower.tri(params[["nu"]], diag = TRUE)
            params[["gamma"]][idy] <- parameters[idx_1]
            params[["nu"]][idy] <- parameters[idx_2]

            idy <- upper.tri(params[["gamma"]], diag = FALSE)
            params[["gamma"]][idy] <- t(params[["gamma"]])[idy]
            params[["nu"]][idy] <- t(params[["nu"]])[idy]

        } else if(dynamics == "anisotropic") {
            idx_1 <- (d + d * k + 1 + 1):(d + d * k + 1 + d^2)
            idx_2 <- (d + d * k + 1 + d^2 + 1):(d + d * k + 1 + 2 * d^2)

            params[["gamma"]][] <- parameters[idx_1]
            params[["nu"]][] <- parameters[idx_2]

        } 

        # If we need to fill the covariance as well, then do so
        if(!parameters_only) {
            model@covariance <- fill_covariance(
                d,
                parameters = parameters[(max(idx_2) + 1):(length(parameters))],
                covariance = covariance,
                cholesky = cholesky
            )
        }

        # Assign the parameters to the model and return
        model@parameters <- params
        
        return(model)
    }
)

#' Fill the covariance matrix with values
#' 
#' @param d Integer denoting the dimensionality of the model.
#' @param parameters Numeric vector containing the values of the parameters 
#' that should be assigned to the model. Importantly, the number of parameters
#' within this vector should correspond exactly to the number of parameters 
#' needed by the model. Ideally, you let functions internal to this package
#' handle this for you, rather than you defining these parameters manually.
#' @param covariance Character denoting the structure of the covariance matrix.
#' Can either be \code{"symmetric"} (symmetric around the diagonal) or 
#' \code{"isotropic"} (diagonal). Defaults to \code{"symmetric"}.
#' @param cholesky Logical denoting whether the values of the covariance matrix 
#' should be taken as the values of its Cholesky decomposition instead. Defaults 
#' to \code{TRUE}.
#' 
#' @return Matrix filled with the specified values.
#' 
#' @examples 
#' fill_covariance(
#'   2,
#'   1:3,
#'   covariance = "symmetric",
#'   cholesky = FALSE
#' )
#' 
#' fill_covariance(
#'   2,
#'   1:2,
#'   covariance = "isotropic",
#'   cholesky = FALSE
#' )
#' 
#' @rdname fill_covariance
#' @export 
fill_covariance <- function(d, 
                            parameters,
                            covariance = "symmetric",
                            cholesky = TRUE) {
    
    # Check whether enough parameters are defined
    n <- count_covariance(d, covariance = covariance)

    if(length(parameters) < n) {
        stop(
            paste(
                n,
                "parameters needed to fill a",
                covariance,
                "covariance matrix but only",
                length(parameters), 
                "provided."
            )
        )

    } else if(length(parameters) > n) {
        warning(
            paste(
                n,
                "parameters needed to fill a",
                covariance,
                "covariance matrix and",
                length(parameters), 
                "are provided.",
                "Only using the first",
                n,
                "values."
            )
        )

        parameters <- parameters[1:n]
    }

    # Dispatch on the particular structure of the covariance matrix to fill it 
    # up
    result <- matrix(0, nrow = d, ncol = d)
    if(covariance == "symmetric") {
        idx <- lower.tri(result, diag = TRUE)
        result[idx] <- parameters

        # Check whether the Cholesky decomposition is used or not.
        if(cholesky) {
            result <- result %*% t(result)
        } else {
            idx <- upper.tri(result, diag = FALSE)
            result[idx] <- t(result)[idx]
        }

    } else if(covariance == "isotropic") {
        # Check whether the Cholesky decomposition is used or not
        if(cholesky) {
            parameters <- parameters^2
        }

        diag(result) <- parameters
    } 

    return(result)
}



################################################################################
# COUNT_PARAMETERS

#' Count number of parameters for a model
#' 
#' @param model An instance of the \code{\link[discounting]{model-class}}
#' @param dynamics Character denoting the structure of the dynamical matrices.
#' Can either be \code{"anisotropic"} (completely free), \code{"symmetric"}
#' (symmetric around the diagonal), and \code{"isotropic"} (diagonal). Note that
#' this influences different parameters for different models, namely 
#' \eqn{\Gamma} for the exponential discounting model, \eqn{N} and \eqn{K} for
#' the quasi-hyperbolic discounting model, and \eqn{\Gamma} and \eqn{N} for the
#' double-exponential discounting model. Defaults to \code{"isotropic"}.
#' @param covariance Character denoting the structure of the covariance matrix.
#' Can either be \code{"symmetric"} (symmetric around the diagonal) or 
#' \code{"isotropic"} (diagonal). Defaults to \code{"symmetric"}.
#' @param parameters_only Logical denoting whether to only count the number of 
#' parameters in de \code{parameter} slot of the model (\code{TRUE}), or to 
#' count the number of parameters in the covariance matrix as well 
#' (\code{FALSE}). Defaults to \code{FALSE}.
#' @param ... Arguments passed on to the methods.
#' 
#' @return Integer denoting the number of parameters the model contains within
#' the current specifications.
#' 
#' @examples 
#' # Define a model with a particular dimensionality
#' my_model <- exponential(
#'   parameters = list(
#'     "alpha" = numeric(2),
#'     "beta" = matrix(0, nrow = 2, ncol = 5),
#'     "gamma" = matrix(0, nrow = 2, ncol = 2)
#'   ),
#'   covariance = matrix(0, nrow = 2, ncol = 2)
#' )
#' 
#' # Get the number of parameters for this model under no restrictions (i.e., 
#' # anisotropic forgetting factors and symmetric covariances)
#' count_parameters(
#'   my_model, 
#'   dynamics = "anisotropic",
#'   covariance = "symmetric"
#' )
#' 
#' # Get the number of parameters for this model in the most limited case 
#' # (i.e., isotropic forgetting factors and covariances)
#' count_parameters(
#'   my_model,
#'   dynamics = "isotropic",
#'   covariance = "isotropic"
#' )
#' 
#' @rdname count_parameters
#' @export 
setGeneric(
    "count_parameters",
    function(model, ...) standardGeneric("count_parameters")
)

#' @rdname count_parameters
#' @export 
setMethod(
    "count_parameters",
    "exponential",
    function(model, 
             dynamics = "isotropic",
             covariance = "symmetric",
             parameters_only = FALSE) {
        
        # Extract relevant dimensionalities from the model
        d <- model@d 
        k <- model@k
        
        # Define the number of parameters needed for each the different structures
        # for \Gamma
        if(dynamics == "isotropic") {
            n <- d + d * k + d

        } else if(dynamics == "symmetric") {
            n <- d + d * k + d * (d + 1) / 2

        } else if(dynamics == "anisotropic") {
            n <- d + d * k + d^2
            
        } else {
            stop(
                paste(
                    "Structure for dynamics is not know.",
                    "Please use \"isotropic\", \"symmetric\", or \"anisotropic\"."
                )
            )
        }

        # Check whether the covariances should be counted as well. If not, we 
        # can immediately skip ahead. If so, then we need to account for them
        # here
        if(!parameters_only) {
            n <- n + count_covariance(model@d, covariance = covariance)
        }

        return(n)
    }
)

#' @rdname count_parameters
#' @export 
setMethod(
    "count_parameters",
    "quasi_hyperbolic",
    function(model, 
             dynamics = "isotropic",
             covariance = "symmetric",
             parameters_only = FALSE) {
        
        # Extract relevant dimensionalities from the model
        d <- model@d 
        k <- model@k
        
        # Define the number of parameters needed for each the different structures
        # for \Gamma
        if(dynamics == "isotropic") {
            n <- d + d * k + 2 * d

        } else if(dynamics == "symmetric") {
            n <- d + d * k + 2 * d * (d + 1) / 2

        } else if(dynamics == "anisotropic") {
            n <- d + d * k + 2 * d^2
            
        } else {
            stop(
                paste(
                    "Structure for dynamics is not know.",
                    "Please use \"isotropic\", \"symmetric\", or \"anisotropic\"."
                )
            )
        }

        # Check whether the covariances should be counted as well. If not, we 
        # can immediately skip ahead. If so, then we need to account for them
        # here
        if(!parameters_only) {
            n <- n + count_covariance(model@d, covariance = covariance)
        }

        return(n)
    }
)

#' @rdname count_parameters
#' @export 
setMethod(
    "count_parameters",
    "double_exponential",
    function(model, 
             dynamics = "isotropic",
             covariance = "symmetric",
             parameters_only = FALSE) {
        
        # Extract relevant dimensionalities from the model
        d <- model@d 
        k <- model@k
        
        # Define the number of parameters needed for each the different structures
        # for \Gamma
        if(dynamics == "isotropic") {
            n <- d + d * k + 2 * d + 1

        } else if(dynamics == "symmetric") {
            n <- d + d * k + 2 * d * (d + 1) / 2 + 1

        } else if(dynamics == "anisotropic") {
            n <- d + d * k + 2 * d^2 + 1
            
        } else {
            stop(
                paste(
                    "Structure for dynamics is not know.",
                    "Please use \"isotropic\", \"symmetric\", or \"anisotropic\"."
                )
            )
        }

        # Check whether the covariances should be counted as well. If not, we 
        # can immediately skip ahead. If so, then we need to account for them
        # here
        if(!parameters_only) {
            n <- n + count_covariance(model@d, covariance = covariance)
        }

        return(n)
    }
)

#' Count the number of parameters in the covariance matrix
#' 
#' @param d Integer denoting the dimensionality of the model.
#' @param covariance Character denoting the structure of the covariance matrix.
#' Can either be \code{"symmetric"} (symmetric around the diagonal) or 
#' \code{"isotropic"} (diagonal). Defaults to \code{"symmetric"}.
#' 
#' @return Integer denoting the number of parameters in the covariance matrix
#' under the specified conditions.
#' 
#' @examples 
#' count_covariance(2, "symmetric")
#' count_covariance(2, "isotropic")
#' 
#' @rdname count_covariance
#' @export 
count_covariance <- function(d, 
                             covariance = "symmetric") {

    # Dispatch on structure
    if(covariance == "symmetric") {
        n <- d * (d + 1) / 2

    } else if(covariance == "isotropic") {
        n <- d

    } else {
        stop(
            paste(
                "Structure for covariance is not know.",
                "Please use \"isotropic\" or \"symmetric\"."
            )
        )
    }

    return(n)
}



################################################################################
# GET_BOUNDS

#' Get bounds of the parameters of a model
#' 
#' @param model Instance of the \code{\link[discounting]{model-class}}.
#' @param dynamics Character denoting the structure of the dynamical matrices.
#' Can either be \code{"anisotropic"} (completely free), \code{"symmetric"}
#' (symmetric around the diagonal), and \code{"isotropic"} (diagonal). Note that
#' this influences different parameters for different models, namely 
#' \eqn{\Gamma} for the exponential discounting model, \eqn{N} and \eqn{K} for
#' the quasi-hyperbolic discounting model, and \eqn{\Gamma} and \eqn{N} for the
#' double-exponential discounting model. Defaults to \code{"isotropic"}.
#' @param covariance Character denoting the structure of the covariance matrix.
#' Can either be \code{"symmetric"} (symmetric around the diagonal) or 
#' \code{"isotropic"} (diagonal). Defaults to \code{"symmetric"}.
#' @param parameters_only Logical denoting whether to only get bounds for the 
#' parameters in de \code{parameter} slot of the model (\code{TRUE}), or to 
#' fill the covariance matrix as well (\code{FALSE}). Defaults to \code{TRUE}.
#' @param lower Numeric vector of the same length as the substantive parameters
#' in the model, denoting the lower bounds for each one. For the exponential
#' discounting model, for example, a value of `c(0, 0, 0, 0)` would denote a 
#' minimal value of 0 for all values in \eqn{\bm{\alpha}}, \eqn{B}, \eqn{\Gamma},
#' and \eqn{\Sigma}. Defaults depend on the model itself and have been chosen 
#' while accounting for natural bounds in the data used in this project. Note
#' that if \code{parameters_only = TRUE}, there is no need to specify a bound 
#' for the covariances.
#' @param upper Numeric vector that functions the same as \code{lower}, but 
#' specifying the upper bounds instead.
#' @param ... Additional arguments passed on to the methods.
#' 
#' @return Named list containing numeric vectors denoting the lower and upper 
#' bounds for the parameters (under \code{"lower"} and \code{"upper"} resp.).
#' The order maintained in this 
#' 
#' @examples 
#' # Create an empty instance of the exponential discounting model
#' my_model <- exponential()
#' 
#' # Get the bounds for the model
#' get_bounds(
#'   my_model,
#'   dynamics = "anisotropic",
#'   covariance = "isotropic",
#'   parameters_only = FALSE
#' )
#' 
#' # Get the bounds for the model when specifying your personal lower and upper
#' # bounds
#' get_bounds(
#'   my_model,
#'   dynamics = "anisotropic",
#'   covariance = "isotropic",
#'   parameters_only = FALSE,
#'   lower = c(0, -100, 0, 0.01),
#'   upper = c(1, 100, 0.5, 1)
#' )
#' 
#' @rdname get_bounds
#' @export 
setGeneric(
    "get_bounds",
    function(model, ...) standardGeneric("get_bounds")
)

#' @rdname get_bounds
#' @export 
setMethod(
    "get_bounds",
    "exponential",
    function(model,
             dynamics = "isotropic",
             covariance = "symmetric",
             parameters_only = TRUE,
             lower = NULL,
             upper = NULL) {

        # Check whether the bounds are specified by the user, or whether we 
        # should use the defaults instead
        if(is.null(lower)) {
            if(parameters_only) {
                lower <- c(-1, -5, 0)
            } else {
                lower <- c(-1, -5, 0, 10^(-5))
            }
        }

        if(is.null(upper)) {
            if(parameters_only) {
                upper <- c(1, 5, 1)
            } else {
                upper <- c(1, 5, 1, 1)
            }
        }

        # Check whether enough lower and upper bounds are provided. If too few,
        # throw an error. If too many, only use the first few. Note that the 
        # correct specification depends on whether the bounds of the covariances
        # are also needed.
        if(parameters_only) {
            bounds <- check_bound_length(3, lower, upper)

        } else {
            bounds <- check_bound_length(4, lower, upper)

        }

        lower <- bounds[[1]]
        upper <- bounds[[2]]
        
        # Extract relevant dimensionalities from the model
        d <- model@d 
        k <- model@k

        # Assign the lower and upper bounds to the particular values of the 
        # parameters, which also depends on the structure of the model.
        alpha_lb <- rep(lower[1], each = d)
        alpha_ub <- rep(upper[1], each = d)

        beta_lb <- rep(lower[2], each = d * k)
        beta_ub <- rep(upper[2], each = d * k)

        repetition <- ifelse(
            dynamics == "anisotropic",
            d^2,
            ifelse(
                dynamics == "symmetric",
                d * (d + 1) / 2,
                ifelse(
                    dynamics == "isotropic",
                    d,
                    stop("Structure of the dynamics is not recognized.")
                )
            )
        )

        gamma_lb <- rep(lower[3], each = repetition)
        gamma_ub <- rep(upper[3], each = repetition)

        # If we need to fill the covariance as well, then do so. Additionally, 
        # create the list that we will return to the user
        if(!parameters_only) {
            v <- get_bounds_covariance(
                d,
                lower[4],
                upper[4],
                covariance = covariance
            )

            bounds <- list(
                "lower" = c(alpha_lb, beta_lb, gamma_lb, v[[1]]),
                "upper" = c(alpha_ub, beta_ub, gamma_ub, v[[2]])
            )

        } else {
            bounds <- list(
                "lower" = c(alpha_lb, beta_lb, gamma_lb),
                "upper" = c(alpha_ub, beta_ub, gamma_ub)
            )
        }

        return(bounds)
    }
)

#' @rdname get_bounds
#' @export 
setMethod(
    "get_bounds",
    "quasi_hyperbolic",
    function(model,
             dynamics = "isotropic",
             covariance = "symmetric",
             parameters_only = TRUE,
             lower = NULL,
             upper = NULL) {

        # Check whether the bounds are specified by the user, or whether we 
        # should use the defaults instead
        if(is.null(lower)) {
            if(parameters_only) {
                lower <- c(-1, -5, 0, 0)
            } else {
                lower <- c(-1, -5, 0, 0, 10^(-5))
            }
        }

        if(is.null(upper)) {
            if(parameters_only) {
                upper <- c(1, 5, 1, 1)
            } else {
                upper <- c(1, 5, 1, 1, 1)
            }
        }

        # Check whether enough lower and upper bounds are provided. If too few,
        # throw an error. If too many, only use the first few. Note that the 
        # correct specification depends on whether the bounds of the covariances
        # are also needed.
        if(parameters_only) {
            bounds <- check_bound_length(4, lower, upper)

        } else {
            bounds <- check_bound_length(5, lower, upper)

        }

        lower <- bounds[[1]]
        upper <- bounds[[2]]
        
        # Extract relevant dimensionalities from the model
        d <- model@d 
        k <- model@k

        # Assign the lower and upper bounds to the particular values of the 
        # parameters, which also depends on the structure of the model.
        alpha_lb <- rep(lower[1], each = d)
        alpha_ub <- rep(upper[1], each = d)

        beta_lb <- rep(lower[2], each = d * k)
        beta_ub <- rep(upper[2], each = d * k)

        repetition <- ifelse(
            dynamics == "anisotropic",
            d^2,
            ifelse(
                dynamics == "symmetric",
                d * (d + 1) / 2,
                ifelse(
                    dynamics == "isotropic",
                    d,
                    stop("Structure of the dynamics is not recognized.")
                )
            )
        )

        nu_lb <- rep(lower[3], each = repetition)
        nu_ub <- rep(upper[3], each = repetition)

        kappa_lb <- rep(lower[4], each = repetition)
        kappa_ub <- rep(upper[4], each = repetition)

        # If we need to fill the covariance as well, then do so. Additionally, 
        # create the list that we will return to the user
        if(!parameters_only) {
            v <- get_bounds_covariance(
                d,
                lower[5],
                upper[5],
                covariance = covariance
            )

            bounds <- list(
                "lower" = c(alpha_lb, beta_lb, nu_lb, kappa_lb, v[[1]]),
                "upper" = c(alpha_ub, beta_ub, nu_ub, kappa_ub, v[[2]])
            )

        } else {
            bounds <- list(
                "lower" = c(alpha_lb, beta_lb, nu_lb, kappa_lb),
                "upper" = c(alpha_ub, beta_ub, nu_ub, kappa_ub)
            )
        }

        return(bounds)
    }
)

#' @rdname get_bounds
#' @export 
setMethod(
    "get_bounds",
    "double_exponential",
    function(model,
             dynamics = "isotropic",
             covariance = "symmetric",
             parameters_only = TRUE,
             lower = NULL,
             upper = NULL) {

        # Check whether the bounds are specified by the user, or whether we 
        # should use the defaults instead
        if(is.null(lower)) {
            if(parameters_only) {
                lower <- c(-1, -5, 0, 0, 0)
            } else {
                lower <- c(-1, -5, 0, 0, 0, 10^(-5))
            }
        }

        if(is.null(upper)) {
            if(parameters_only) {
                upper <- c(1, 5, 0.5, 1, 1)
            } else {
                upper <- c(1, 5, 0.5, 1, 1, 1)
            }
        }

        # Check whether enough lower and upper bounds are provided. If too few,
        # throw an error. If too many, only use the first few. Note that the 
        # correct specification depends on whether the bounds of the covariances
        # are also needed.
        if(parameters_only) {
            bounds <- check_bound_length(5, lower, upper)

        } else {
            bounds <- check_bound_length(6, lower, upper)

        }

        lower <- bounds[[1]]
        upper <- bounds[[2]]
        
        # Extract relevant dimensionalities from the model
        d <- model@d 
        k <- model@k

        # Assign the lower and upper bounds to the particular values of the 
        # parameters, which also depends on the structure of the model.
        alpha_lb <- rep(lower[1], each = d)
        alpha_ub <- rep(upper[1], each = d)

        beta_lb <- rep(lower[2], each = d * k)
        beta_ub <- rep(upper[2], each = d * k)

        repetition <- ifelse(
            dynamics == "anisotropic",
            d^2,
            ifelse(
                dynamics == "symmetric",
                d * (d + 1) / 2,
                ifelse(
                    dynamics == "isotropic",
                    d,
                    stop("Structure of the dynamics is not recognized.")
                )
            )
        )

        omega_lb <- lower[3]
        omega_ub <- upper[3]

        gamma_lb <- rep(lower[4], each = repetition)
        gamma_ub <- rep(upper[4], each = repetition)

        nu_lb <- rep(lower[5], each = repetition)
        nu_ub <- rep(upper[5], each = repetition)

        # If we need to fill the covariance as well, then do so. Additionally, 
        # create the list that we will return to the user
        if(!parameters_only) {
            v <- get_bounds_covariance(
                d,
                lower[6],
                upper[6],
                covariance = covariance
            )

            bounds <- list(
                "lower" = c(alpha_lb, beta_lb, omega_lb, gamma_lb, nu_lb, v[[1]]),
                "upper" = c(alpha_ub, beta_ub, omega_ub, gamma_ub, nu_ub, v[[2]])
            )

        } else {
            bounds <- list(
                "lower" = c(alpha_lb, beta_lb, omega_lb, gamma_lb, nu_lb),
                "upper" = c(alpha_ub, beta_ub, omega_ub, gamma_ub, nu_ub)
            )
        }

        return(bounds)
    }
)

#' Get bounds for the parameters in the covariance matrix
#' 
#' @param d Integer denoting the dimensionality of the model.
#' @param lower,upper Numeric value containing the value of the lower and 
#' upper bounds for the covariance matrix.
#' @param covariance Character denoting the structure of the covariance matrix.
#' Can either be \code{"symmetric"} (symmetric around the diagonal) or 
#' \code{"isotropic"} (diagonal). Defaults to \code{"symmetric"}.
#' 
#' @return List containing two numeric vectors of lower and upper bounds for the 
#' covariance matrix in that order.
#' 
#' @examples 
#' get_bounds_covariance(
#'   2, 
#'   0,
#'   1,
#'   covariance = "symmetric"
#' )
#' 
#' get_bounds_covariance(
#'   2,
#'   0, 
#'   1,
#'   covariance = "isotropic"
#' )
#' 
#' @rdname get_bounds_covariance
#' @export 
get_bounds_covariance <- function(d, 
                                  lower,
                                  upper,
                                  covariance = "symmetric") {

    # Dispatch on structure
    if(covariance == "symmetric") {
        bounds <- list(
            rep(lower, each = d * (d + 1) / 2),
            rep(upper, each = d * (d + 1) / 2)
        )

    } else if(covariance == "isotropic") {
        bounds <- list(
            rep(lower, each = d),
            rep(upper, each = d)
        )

    } else {
        stop(
            paste(
                "Structure for covariance is not know.",
                "Please use \"isotropic\" or \"symmetric\"."
            )
        )
    }

    return(bounds)
}

#' Check performed within the \code{\link[discounting]{get_bounds}} function
#' 
#' @param n Integer denoting the needed length of the bounds
#' @param lower,upper Numeric vector containing the user-specified lower and 
#' upper bounds for which the length should be checked.
#' 
#' @return List containing the corrected lower and upper bounds (in that order)
#' 
#' @rdname check_bound_length
#' @keywords internal
check_bound_length <- function(n, 
                               lower, 
                               upper){

    # If there are fewer than n values in lower or upper, we should throw an 
    # error
    if(length(lower) < n | length(upper) < n) {
        stop(
            paste(
                "Too few values for the lower and/or upper bound are given.",
                "To proceed, please specify",
                n, 
                "values for \"lower\" and \"upper\"."
            )
        )

    # If there are more than n values, we throw a warning and change the values
    # of lower and upper
    } else if(length(lower) > n | length(upper) > n) {
        warning(
            paste(
                "Too many values for the lower and/or upper bounds are given.",
                "Only using the first",
                n,
                "values."
            )
        )

        lower <- lower[1:n]
        upper <- upper[1:n]
    }

    return(
        list(
            lower, 
            upper
        )
    )
}



################################################################################
# PARAMETER_NAMES

#' Name the relevant parameters of the model
#' 
#' @param model Instance of the \code{\link[discounting]{model-class}}.
#' @param dynamics Character denoting the structure of the dynamical matrices.
#' Can either be \code{"anisotropic"} (completely free), \code{"symmetric"}
#' (symmetric around the diagonal), and \code{"isotropic"} (diagonal). Note that
#' this influences different parameters for different models, namely 
#' \eqn{\Gamma} for the exponential discounting model, \eqn{N} and \eqn{K} for
#' the quasi-hyperbolic discounting model, and \eqn{\Gamma} and \eqn{N} for the
#' double-exponential discounting model. Defaults to \code{"isotropic"}.
#' @param covariance Character denoting the structure of the covariance matrix.
#' Can either be \code{"symmetric"} (symmetric around the diagonal) or 
#' \code{"isotropic"} (diagonal). Defaults to \code{"symmetric"}.
#' @param parameters_only Logical denoting whether to only name the 
#' parameters in de \code{parameter} slot of the model (\code{TRUE}), or to 
#' fill the covariance matrix as well (\code{FALSE}). Defaults to \code{TRUE}.
#' @param ... Additional arguments passed on to the methods.
#' 
#' @return Character vector containing the names for the parameters in the 
#' model. The order corresponds to the assumptions of 
#' \code{\link[discounting]{get_bounds}} and \code{\link[discounting]{fill}}.
#' 
#' @examples 
#' # Create an empty instance of the exponential discounting model
#' my_model <- exponential()
#' 
#' # Get the names for the model, once with and once without the covariances 
#' # included
#' parameter_names(
#'   my_model,
#'   dynamics = "anisotropic",
#'   covariance = "isotropic",
#'   parameters_only = FALSE
#' )
#' 
#' parameter_names(
#'   my_model,
#'   dynamics = "anisotropic",
#'   covariance = "isotropic",
#'   parameters_only = TRUE
#' )
#' 
#' @rdname parameter_names
#' @export 
setGeneric(
    "parameter_names",
    function(model, ...) standardGeneric("parameter_names")
)

#' @rdname parameter_names
#' @export 
setMethod(
    "parameter_names",
    "exponential",
    function(model,
             dynamics = "isotropic", 
             covariance = "symmetric",
             parameters_only = TRUE) {
        
        # Get the dimensionality of the model
        d <- model@d 
        k <- model@k 

        # Generate the names for "alpha"
        alpha <- paste(
            "alpha_",
            1:d, 
            sep = ""
        )

        # Generate the names for "beta"
        rows <- matrix(1:d, nrow = d, ncol = k) |>
            as.numeric()
        columns <- matrix(1:k, nrow = d, ncol = k, byrow = TRUE) |>
            as.numeric()

        beta <- paste(
            "beta_", 
            rows, 
            columns,
            sep = ""
        )

        # Generate the names for "gamma". Here, we need to take the structure 
        # of the matrix into account
        rows <- matrix(1:d, nrow = d, ncol = d)
        columns <- matrix(1:d, nrow = d, ncol = d, byrow = TRUE) 

        if(dynamics == "isotropic") {
            gamma <- paste(
                "gamma_",
                1:d,
                1:d,
                sep = ""
            )

        } else if(dynamics == "symmetric") {
            gamma <- paste(
                "gamma_",
                rows[lower.tri(rows, diag = TRUE)],
                columns[lower.tri(columns, diag = TRUE)],
                sep = ""
            )

        } else if(dynamics == "anisotropic") {
            gamma <- paste(
                "gamma_",
                as.numeric(rows),
                as.numeric(columns),
                sep = ""
            )

        } else {
            stop(
                paste(
                    "Structure for the forgetting matrix is not know.",
                    "Please use \"isotropic\", \"symmetric\", or \"anisotropic\"."
                )
            )

        }

        # Check whether the covariances should be given a name as well.
        if(parameters_only) {
            names <- c(alpha, beta, gamma)

        } else {
            names <- c(
                alpha, 
                beta,
                gamma,
                parameter_names_covariance(d, covariance = covariance)
            )

        }

        return(names)        
    }
)

#' @rdname parameter_names
#' @export 
setMethod(
    "parameter_names",
    "quasi_hyperbolic",
    function(model,
             dynamics = "isotropic", 
             covariance = "symmetric",
             parameters_only = TRUE) {
        
        # Get the dimensionality of the model
        d <- model@d 
        k <- model@k 

        # Generate the names for "alpha"
        alpha <- paste(
            "alpha_",
            1:d, 
            sep = ""
        )

        # Generate the names for "beta"
        rows <- matrix(1:d, nrow = d, ncol = k) |>
            as.numeric()
        columns <- matrix(1:k, nrow = d, ncol = k, byrow = TRUE) |>
            as.numeric()

        beta <- paste(
            "beta_", 
            rows, 
            columns,
            sep = ""
        )

        # Generate the names for "nu" and "kappa". Here, we need to take the 
        # structure of the matrix into account
        rows <- matrix(1:d, nrow = d, ncol = d)
        columns <- matrix(1:d, nrow = d, ncol = d, byrow = TRUE) 

        if(dynamics == "isotropic") {
            nu <- paste(
                "nu_",
                1:d,
                1:d,
                sep = ""
            )
            kappa <- paste(
                "kappa_",
                1:d,
                1:d,
                sep = ""
            )

        } else if(dynamics == "symmetric") {
            nu <- paste(
                "nu_",
                rows[lower.tri(rows, diag = TRUE)],
                columns[lower.tri(columns, diag = TRUE)],
                sep = ""
            )
            kappa <- paste(
                "kappa_",
                rows[lower.tri(rows, diag = TRUE)],
                columns[lower.tri(columns, diag = TRUE)],
                sep = ""
            )

        } else if(dynamics == "anisotropic") {
            nu <- paste(
                "nu_",
                as.numeric(rows),
                as.numeric(columns),
                sep = ""
            )
            kappa <- paste(
                "kappa_",
                as.numeric(rows),
                as.numeric(columns),
                sep = ""
            )

        } else {
            stop(
                paste(
                    "Structure for the forgetting matrices is not know.",
                    "Please use \"isotropic\", \"symmetric\", or \"anisotropic\"."
                )
            )

        }

        # Check whether the covariances should be given a name as well.
        if(parameters_only) {
            names <- c(alpha, beta, nu, kappa)

        } else {
            names <- c(
                alpha, 
                beta,
                nu,
                kappa,
                parameter_names_covariance(d, covariance = covariance)
            )

        }

        return(names)        
    }
)

#' @rdname parameter_names
#' @export 
setMethod(
    "parameter_names",
    "double_exponential",
    function(model,
             dynamics = "isotropic", 
             covariance = "symmetric",
             parameters_only = TRUE) {
        
        # Get the dimensionality of the model
        d <- model@d 
        k <- model@k 

        # Generate the names for "alpha"
        alpha <- paste(
            "alpha_",
            1:d, 
            sep = ""
        )

        # Generate the names for "beta"
        rows <- matrix(1:d, nrow = d, ncol = k) |>
            as.numeric()
        columns <- matrix(1:k, nrow = d, ncol = k, byrow = TRUE) |>
            as.numeric()

        beta <- paste(
            "beta_", 
            rows, 
            columns,
            sep = ""
        )

        # Generate the names for "nu" and "kappa". Here, we need to take the 
        # structure of the matrix into account
        rows <- matrix(1:d, nrow = d, ncol = d)
        columns <- matrix(1:d, nrow = d, ncol = d, byrow = TRUE) 

        if(dynamics == "isotropic") {
            gamma <- paste(
                "gamma_",
                1:d,
                1:d,
                sep = ""
            )
            nu <- paste(
                "nu_",
                1:d,
                1:d,
                sep = ""
            )

        } else if(dynamics == "symmetric") {
            gamma <- paste(
                "gamma_",
                rows[lower.tri(rows, diag = TRUE)],
                columns[lower.tri(columns, diag = TRUE)],
                sep = ""
            )
            nu <- paste(
                "nu_",
                rows[lower.tri(rows, diag = TRUE)],
                columns[lower.tri(columns, diag = TRUE)],
                sep = ""
            )

        } else if(dynamics == "anisotropic") {
            gamma <- paste(
                "gamma_",
                as.numeric(rows),
                as.numeric(columns),
                sep = ""
            )
            nu <- paste(
                "nu_",
                as.numeric(rows),
                as.numeric(columns),
                sep = ""
            )

        } else {
            stop(
                paste(
                    "Structure for the forgetting matrices is not know.",
                    "Please use \"isotropic\", \"symmetric\", or \"anisotropic\"."
                )
            )

        }

        # Check whether the covariances should be given a name as well.
        if(parameters_only) {
            names <- c(alpha, beta, "omega", gamma, nu)

        } else {
            names <- c(
                alpha, 
                beta,
                "omega",
                gamma,
                nu,
                parameter_names_covariance(d, covariance = covariance)
            )

        }

        return(names)        
    }
)

#' Get the names for the parameters in the covariance matrix
#' 
#' @param d Integer denoting the dimensionality of the model.
#' @param covariance Character denoting the structure of the covariance matrix.
#' Can either be \code{"symmetric"} (symmetric around the diagonal) or 
#' \code{"isotropic"} (diagonal). Defaults to \code{"symmetric"}.
#' 
#' @return Character vector containing the names for all parameters in the 
#' covariance matrix. The order corresponds to the assumptions of 
#' \code{\link[discounting]{get_bounds}} and \code{\link[discounting]{fill}}.
#' 
#' @examples 
#' parameter_names_covariance(2, covariance = "symmetric")
#' 
#' parameter_names_covariance(2, covariance = "isotropic")
#' 
#' @rdname parameter_names_covariance
#' @export 
parameter_names_covariance <- function(d, 
                                       covariance = "symmetric") {
    
    # Dimensionality as rows and columns in a matrix
    rows <- matrix(1:d, nrow = d, ncol = d)
    columns <- matrix(1:d, nrow = d, ncol = d, byrow = TRUE)

    # Dispatch on structure
    if(covariance == "symmetric") {
        names <- paste(
            "sigma_",
            rows[lower.tri(rows, diag = TRUE)],
            columns[lower.tri(columns, diag = TRUE)],
            sep = ""
        )

    } else if(covariance == "isotropic") {
        names <- paste(
            "sigma_",
            diag(rows),
            diag(columns),
            sep = ""
        )

    } else {
        stop(
            paste(
                "Structure for covariance is not know.",
                "Please use \"isotropic\" or \"symmetric\"."
            )
        )
    }

    return(names)
}



################################################################################
# GENERATE_PARAMETERS

#' Generate model parameters
#' 
#' Generation of model parameters within this function always occurs according 
#' to a uniform distribution within the bounds defined for the models.
#' 
#' @param model Instance of the \code{\link[discounting]{model-class}}
#' @param ... Additional arguments passed on to 
#' \code{\link[discounting]{get_bounds}}
#' 
#' @return Numeric vector of parameters for the model.
#' 
#' @examples 
#' generate_parameters(
#'   double_exponential(d = 2, k = 3),
#'   dynamics = "isotropic",
#'   covariance = "isotropic",
#'   parameters_only = FALSE
#' )
#' 
#' @rdname generate_parameters
#' @export 
setGeneric(
    "generate_parameters",
    function(model, ...) standardGeneric("generate_parameters")
)

#' @rdname generate_parameters
#' @export 
setMethod(
    "generate_parameters",
    "model",
    function(model,
             ...) {
        
        # Extract the bounds of the model
        bounds <- get_bounds(
            model,
            ...
        )

        # Sample parameters from a uniform distribution and return
        parameters <- runif(
            length(bounds$lower),
            min = bounds$lower,
            max = bounds$upper
        )
        
        return(parameters)
    }
)
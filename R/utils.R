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
#' @param covariance Character denoting the structure of the covariance matrix.
#' Can either be \code{"symmetric"} (symmetric around the diagonal) or 
#' \code{"isotropic"} (diagonal). Defaults to \code{"symmetric"}.
#' @param parameters_only Logical denoting whether to only fill the 
#' parameters in de \code{parameter} slot of the model (\code{TRUE}), or to 
#' fill the covariance matrix as well (\code{FALSE}). Defaults to \code{TRUE}.
#' @param ... Additional arguments passed on to the methods.
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
             parameters_only = TRUE) {
        
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
                covariance = covariance
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
             parameters_only = TRUE) {
        
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
                covariance = covariance
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
             parameters_only = TRUE) {
        
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

            diag(params[["nu"]]) <- parameters[idx_1]
            diag(params[["kappa"]]) <- parameters[idx_2]
        
        } else if(dynamics == "symmetric") {
            idx_1 <- (d + d * k + 1 + 1):(d + d * k + 1 + d * (d + 1) / 2)
            idx_2 <- (d + d * k + 1 + d * (d + 1) / 2 + 1):(d + d * k + 1 + 2 * d * (d + 1) / 2)

            idy <- lower.tri(params[["nu"]], diag = TRUE)
            params[["nu"]][idy] <- parameters[idx_1]
            params[["kappa"]][idy] <- parameters[idx_2]

            idy <- upper.tri(params[["nu"]], diag = FALSE)
            params[["nu"]][idy] <- t(params[["nu"]])[idy]
            params[["kappa"]][idy] <- t(params[["kappa"]])[idy]

        } else if(dynamics == "anisotropic") {
            idx_1 <- (d + d * k + 1 + 1):(d + d * k + 1 + d^2)
            idx_2 <- (d + d * k + 1 + d^2 + 1):(d + d * k + 1 + 2 * d^2)

            params[["nu"]][] <- parameters[idx_1]
            params[["kappa"]][] <- parameters[idx_2]

        } 

        # If we need to fill the covariance as well, then do so
        if(!parameters_only) {
            model@covariance <- fill_covariance(
                d,
                parameters = parameters[(max(idx_2) + 1):(length(parameters))],
                covariance = covariance
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
#' 
#' @return Matrix filled with the specified values.
#' 
#' @examples 
#' fill_covariance(
#'   2,
#'   1:3,
#'   covariance = "symmetric"
#' )
#' 
#' fill_covariance(
#'   2,
#'   1:2,
#'   covariance = "isotropic"
#' )
#' 
#' @rdname fill_covariance
#' @export 
fill_covariance <- function(d, 
                            parameters,
                            covariance = "symmetric") {
    
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

        idx <- upper.tri(result, diag = FALSE)
        result[idx] <- t(result)[idx]

    } else if(covariance == "isotropic") {
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
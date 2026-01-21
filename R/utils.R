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
             dynamics = "isotropic") {
        
        # Extract relevant dimensionalities from the model
        d <- model@d 
        k <- model@k
        n <- count_parameters(
            model, 
            dynamics = dynamics, 
            count_covariance = FALSE
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
                    "forgetting matrix."
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
                    "forgetting matrix.",
                    "Using only the first",
                    n, 
                    "to define the model."
                )
            )
        }

        # Once defined, we can start assigning values to the parameters of the 
        # model. When necessary, dispath on the structure of the parameters
        params <- model@parameters 

        params[["alpha"]][] <- parameters[1:d]
        params[["beta"]][] <- parameters[(d + 1):(d + d * k)]

        left <- parameters[(d + d * k + 1):length(parameters)]
        if(dynamics == "isotropic") {
            diag(params[["gamma"]]) <- left
        
        } else if(dynamics == "symmetric") {
            idx <- lower.tri(params[["gamma"]], diag = TRUE)
            params[["gamma"]][idx] <- left

            idx <- upper.tri(params[["gamma"]], diag = FALSE)
            params[["gamma"]][idx] <- t(params[["gamma"]])[idx]

        } else if(dynamics == "anisotropic") {
            params[["gamma"]][] <- left
        }

        # Assign the parameters to the model and return
        model@parameters <- params
        
        return(model)
    }
)

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
#' \code{"isotropic"} (diagonal). Defaults to \code{"isotropic"}.
#' @param count_covariance Logical denoting whether to count the parameters of
#' the covariance matrix to the total. Defaults to \code{TRUE}.
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
             count_covariance = TRUE) {
        
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
        if(count_covariance) {
            if(covariance == "symmetric") {
                n <- n + d * (d + 1) / 2

            } else if(covariance == "isotropic") {
                n <- n + d

            } else {
                stop(
                    paste(
                        "Structure for covariance is not know.",
                        "Please use \"isotropic\" or \"symmetric\"."
                    )
                )
            }
        }

        return(n)
    }
)
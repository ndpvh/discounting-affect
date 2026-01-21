#' Method for simulating from a model
#' 
#' Takes in an instance of the \code{\link[discounting]{model-class}} and 
#' simulates data according to this model and the specifications of the user.
#' Depends heavily on the trajectories created by 
#' \code{\link[discounting]{predict}}.
#' 
#' @param object An instance of the \code{\link[discounting]{model-class}} 
#' defining the model
#' @param X Numeric vector or numeric matrix containing the values of the 
#' predictor variable(s). If a numeric vector, the dimension \code{k} of the 
#' predictor variables within \code{object} should be equal to \code{1}. If 
#' a numeric matrix, the dimension \code{k} of the predictor variables within 
#' \code{object} should be equal to the number of columns provided to this 
#' argument. The length of the provided vector or the number of rows of the 
#' provided matrix will determine the number of simulated datapoints. Defaults
#' to \code{NULL}. If provided, it overrides the value of \code{Xfun}.
#' @param Xfun Function or a list of functions that define how the values of 
#' the predictor variable(s) should be generated. The functions should take in 
#' a single argument \code{N} defining the number of values generated for this 
#' predictor variable, and hence the number of datapoints that should be 
#' generated for the simulation. If a list, then the result of each function in
#' the list will be bound together in a matrix. Defaults to \code{NULL}.
#' @param N Number of datapoints to simulate. Passed on to \code{Xfun} when 
#' defined. Defaults to \code{NULL}.
#' @param ... Arguments passed on to the implemented methods.
#' 
#' @return An instance of the \code{\link[discounting]{dataset-class}} containing
#' the simulated data.
#' 
#' @examples 
#' # Define the model
#' my_model <- exponential(
#'   parameters = list(
#'     "alpha" = numeric(2), 
#'     "beta" = matrix(1:2, ncol = 1),
#'     "gamma" = diag(2) * 0.75
#'   ),
#'   covariance = diag(2)
#' )
#' 
#' # Simulate data for this model using a function for generating X
#' simulate(
#'   my_model,
#'   Xfun = \(x) rnorm(x, mean = 0, sd = 1),
#'   N = 10
#' )
#' 
#' # Simulate data for this model using predefined values of X
#' simulate(
#'   my_model,
#'   X = rnorm(10, mean = 0, sd = 1)
#' )
#' 
#' @rdname simulate
#' @export
# setGeneric(
#     "simulate",
#     function(object, ...) standardGeneric("simulate")
# )

# #' @rdname simulate
# #' @export
setMethod(
    "simulate",
    "model",
    function(object,
             X = NULL,
             Xfun = NULL,
             N = NULL) {
        
        # Check whether sufficient information is provided to be able to 
        # simulate
        if(is.null(X) & is.null(Xfun)) {
            stop(
                paste(
                    "Neither \"X\" nor \"Xfun\" are specified.",
                    "Provide at least one of the two arguments to simulate data."
                )
            )
        }

        # If Xfun is provided and X is not, then we have to generate some values
        # for X
        if(!is.null(Xfun) & is.null(X)) {
            # Check if Xfun is a function or a list. Each one should be handled
            # differently
            if(is.function(Xfun)) {
                X <- Xfun(N)
            } else if(is.list(Xfun)) {
                X <- lapply(Xfun, \(fx) fx(N))
                X <- do.call("cbind", X)
            } else {
                stop("Value provided to \"Xfun\" is neither a function nor a list.")
            }
        }

        # Check whether X has the required type
        if(!is.numeric(X)) {
            stop("\"X\" should contain numeric values.")
        }

        if(!is.matrix(X)) {
            X <- matrix(X, ncol = 1)
        }

        # Check the dimensionality of X and whether it corresponds to the one 
        # needed
        if(ncol(X) > object@k) {
            warning(
                paste(
                    "Dimensionality of \"X\" is greater than dimensionality",
                    "provided in the model.",
                    "Only using the first",
                    object@k,
                    "columns in \"X\" for the simulation."
                )
            )

            X <- X[, 1:object@k]

        } else if(ncol(X) < object@k) {
            stop(
                paste(
                    "Dimensionality of \"X\" is smaller than the one required",
                    "for the model.",
                    "Provide a value for \"X\" with",
                    object@k, 
                    "columns to proceed."
                )
            )
        }

        # Create a dataset around the values of X
        data <- dataset(X = X)

        # Create predicted data based on the values of X
        data <- predict(
            object, 
            data
        )

        # Add the residuals to the mix
        residuals <- MASS::mvrnorm(
            data@N, 
            mu = rep(0, object@d), 
            Sigma = object@covariance
        )
        data@Y <- data@Y + residuals

        # Return the results
        return(data)
    }
)
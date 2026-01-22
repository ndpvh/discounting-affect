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


#' @param covariance Character denoting the structure of covariance matrix.
#' Can either by \code{"symmetric"} (symmetric around the diagonal) and 
#' \code{"isotropic"} (diagonal). Defaults to \code{"symmetric"}.
# , 
#              covariance = "symmetric"
# ALSO CHANGE PARAMETER NUMBER DEPENDING ON THE DYNAMICS + COVARIANCE
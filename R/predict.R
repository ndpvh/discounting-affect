#' Compute the model expectations
#' 
#' Within this package, the \code{predict} method computes the expectations of 
#' a particular model provided to \code{object} using the data provided to 
#' \code{data}. It plays a pivotal role in both the estimation and simulation of 
#' the models provided in this package.
#' 
#' @param object Instance of the \code{\link[discounting]{model-class}}
#' @param data Instance of the \code{\link[discounting]{dataset-class}}
#' 
#' @return Instance of the \code{\link[discounting]{dataset-class}} where the
#' slot \code{y} contains the predictions of the model
#' 
#' @examples 
#' # Create an exponential discounting model
#' my_model <- exponential(
#'   parameters = list(
#'     "alpha" = 0,
#'     "beta" = 1,
#'     "gamma" = 0.5
#'   ),
#'   covariance = 1
#' )
#' 
#' # Create an instance of the dataset with only predictor values. It will throw
#' # a warning because Y is implied to be empty, but we capture this warning 
#' # with suppressWarnings()
#' data <- dataset(
#'   X = matrix(
#'     rnorm(10),
#'     nrow = 10,
#'     ncol = 1
#'   )
#' ) |>
#'   suppressWarnings()
#' 
#' # Compute the values of Y as expected by the exponential discounting model 
#' # defined in my_model
#' predict(
#'   my_model,
#'   data
#' )
#' 
#' @rdname predict
#' @export 
setMethod(
    "predict",
    "exponential",
    function(object, 
             data) {
        
        # Extract the values of the independent variables
        X <- data@X

        # Extract the parameters
        params <- object@parameters

        # Reserve memory for the output variable Y
        Y <- matrix(0, nrow = data@N, ncol = object@d)

        # Reserve memory for the temporary output of the cumulative sum
        S <- numeric(object@d)

        # Loop over the values of X and define compute the values of Y
        for(i in seq_len(data@N)) {
            # Define the new instance of the sum
            S[] <- params[["beta"]] %*% X[i, ] + params[["gamma"]] %*% S

            # Create the value for the ouput variable Y
            Y[i, ] <- params[["alpha"]] + S
        }

        # Update the slot Y in the `dataset` class
        data@Y <- Y 

        return(data)
    }
)

#' @rdname predict
#' @export 
setMethod(
    "predict",
    "quasi_hyperbolic",
    function(object, 
             data) {
        
        # Extract the values of the independent variables
        X <- data@X

        # Extract the parameters
        params <- object@parameters

        # Reserve memory for the output variable Y
        Y <- matrix(0, nrow = data@N, ncol = object@d)

        # Reserve memory for the temporary output of the cumulative sum. We 
        # distinguish between the intermediate value and the sum in which the 
        # discounting effect of K has already been accounted for
        S <- numeric(object@d)
        KS <- numeric(object@d)

        # Loop over the values of X and define compute the values of Y
        for(i in seq_len(data@N)) {
            # Define the new instance of the sum
            S[] <- params[["beta"]] %*% X[i, ] + params[["nu"]] %*% KS[]
            KS[] <- params[["kappa"]] %*% params[["beta"]] %*% X[i, ] + params[["nu"]] %*% KS[]

            # Create the value for the ouput variable Y
            Y[i, ] <- params[["alpha"]] + S
        }

        # Update the slot Y in the `dataset` class
        data@Y <- Y 

        return(data)
    }
)

#' @rdname predict
#' @export 
setMethod(
    "predict",
    "double_exponential",
    function(object, 
             data) {
        
        # Extract the values of the independent variables
        X <- data@X

        # Extract the parameters
        params <- object@parameters

        # Reserve memory for the output variable Y
        Y <- matrix(0, nrow = data@N, ncol = object@d)

        # Reserve memory for the temporary output of the cumulative sum
        GS <- numeric(object@d)
        NS <- numeric(object@d)

        # Loop over the values of X and define compute the values of Y
        for(i in seq_len(data@N)) {
            # Define the new instance of the sum for \Gamma and N
            GS[] <- params[["beta"]] %*% X[i, ] + params[["gamma"]] %*% GS
            NS[] <- params[["beta"]] %*% X[i, ] + params[["nu"]] %*% NS 

            # Create the value for the output variable Y
            Y[i, ] <- params[["alpha"]] + 
                params[["omega"]] * GS + 
                (1 - params[["omega"]]) * NS
        }

        # Update the slot Y in the `dataset` class
        data@Y <- Y 

        return(data)
    }
)
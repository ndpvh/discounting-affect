#' Model Class
#' 
#' Class that created the general structure of all models introduced within this 
#' package. 
#' 
#' @slot d Integer denoting the number of dimensions of the model
#' @slot k Integer denoting the number of independent variables
#' @slot parameters List containing the parameters relevant to a particular 
#' model instance
#' @slot covariance Numeric matrix denoting the residual covariance of the 
#' model
#' 
#' @export 
setClass(
    "model",
    slots = c(
        d = "numeric",
        k = "numeric",
        parameters = "list",
        covariance = "matrix"
    ),
    prototype = list(
        d = as.integer(1),
        k = as.integer(0), 
        parameters = list(),
        covariance = matrix(0, nrow = 1, ncol = 1)
    )
)

#' Constructor for the \code{\link[discounting]{model}} class
#' 
#' @param d Integer denoting the number of dimensions of the model. Defaults to 
#' \code{1}.
#' @param k Integer denoting the number of independent variables. Defaults to 
#' \code{0}.
#' @param parameters List containing the parameters relevant to a particular 
#' model instance. Defaults to an empty list, meaning the model cannot be 
#' properly used.
#' @param covariance Numeric matrix denoting the residual covariance of the 
#' model. Default to a matrix of \code{0}s with the dimensionality implied by 
#' \code{d}
#' 
#' @return Instance of \code{\link[discounting]{model-class}}
#' 
#' @examples 
#' model(
#'   d = 2,
#'   k = 2,
#'   parameters = list(
#'     "delta" = numeric(2),
#'     "theta" = diag(2)
#'   ),
#'   covariance = diag(2)
#' )
#' 
#' @export 
model <- function(d = 1, 
                  k = 0,
                  parameters = list(), 
                  covariance = matrix(0, nrow = d, ncol = d)) {
        
    # If d or k are ill-defined, then we cannot proceed
    if(!is.numeric(d) | !is.numeric(k)) {
        stop("Dimensionalities d and k are not numerics.")
    }

    if(d <= 0) {
        stop("An impossible dimensionality d of smaller than or equal to 0 is defined.")
    }

    if(k < 0) {
        stop("An impossible dimensionality k of smaller than 0 is defined.")
    }
    
    # Check whether the numerics are whole numbers
    if(as.integer(d) != d | as.integer(k) != k) {
        stop("Dimensionalities d and k are not whole numbers.")
    }

    # If the parameters are not defined, then we cannot proceed
    if(is.null(parameters)) {
        stop("Parameters should be defined when creating a model")
    }

    if(any(is.na(parameters))) {
        stop("Missing values found in the defined parameters.")
    }

    # If the covariance is not defined, then we will assume 0 covariance
    if(is.null(covariance)) {
        warning("Covariance is left undefined. Returning empty matrix.")
        covariance <- matrix(
            0, 
            nrow = d, 
            ncol = d
        )
    }

    if(any(is.na(covariance))) {
        stop("Missing values found in the covariance matrix.")
    }

    # Check whether the provided dimensionality and the dimensionality of 
    # the covariance matches
    if(nrow(covariance) != d | ncol(covariance) != d) {
        stop("Provided dimensionality d does not match the covariance matrix.")
    }
    
    # Add the dimensionality to the object. Default dimensionality 
    # corresponds to a mean model
    .Object <- new(
        "model",
        d = d,
        k = k,
        parameters = parameters,
        covariance = covariance
    )

    return(.Object)
}
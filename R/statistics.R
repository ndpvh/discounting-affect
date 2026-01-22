#' Compute the \eqn{AIC} based on \eqn{SSE}
#' 
#' @param fitobj Output of the \code{\link[discounting]{fit}} function.
#' 
#' @return Numeric denoting the \eqn{AIC}
#' 
#' @examples 
#' # Generate a dataset
#' data <- dataset(Y = rnorm(100), X = rnorm(100)) |>
#'   suppressWarnings()
#' 
#' # Estimate the exponential discounting model on these data
#' fitobj <- fit(
#'   exponential(),
#'   data,
#'   itermax = 50,
#'   trace = FALSE
#' )
#' 
#' # Compute the AIC based on this fitobj
#' aic(fitobj)
#' 
#' @rdname aic 
#' @export 
aic <- function(fitobj) {

    # Get the number of parameters and the amount of data to use. Note that the 
    # SSE is based on N x d datapoints, therefore using the full length of the 
    # residuals instead of only the number of rows in it.
    n <- length(fitobj$residuals)
    p <- length(fitobj$parameters)

    return(n * log(fitobj$objective / n) + 2 * p)
}

#' Compute the \eqn{BIC} based on \eqn{SSE}
#' 
#' @param fitobj Output of the \code{\link[discounting]{fit}} function.
#' 
#' @return Numeric denoting the \eqn{BIC}
#' 
#' @examples 
#' # Generate a dataset
#' data <- dataset(Y = rnorm(100), X = rnorm(100)) |>
#'   suppressWarnings()
#' 
#' # Estimate the exponential discounting model on these data
#' fitobj <- fit(
#'   exponential(),
#'   data,
#'   itermax = 50,
#'   trace = FALSE
#' )
#' 
#' # Compute the BIC based on this fitobj
#' bic(fitobj)
#' 
#' @rdname bic 
#' @export 
bic <- function(fitobj) {

    # Get the number of parameters and the amount of data to use. Note that the 
    # SSE is based on N x d datapoints, therefore using the full length of the 
    # residuals instead of only the number of rows in it.
    n <- length(fitobj$residuals)
    p <- length(fitobj$parameters)

    return(n * log(fitobj$objective / n) + p * log(n))
}

#' Compute the autocorrelation of the residuals
#' 
#' @param fitobj Output of the \code{\link[discounting]{fit}} function.
#' @param lag The lag of the autocorrelation to compute. Defaults to \code{1}.
#' 
#' @return Numeric denoting the autocorrelation of the residuals. If multiple
#' dimensions exist in the residuals, then the autocorrelation will be computed
#' for each dimension separately and a mean of them will be returned.
#' 
#' @examples 
#' # Generate a dataset
#' data <- dataset(Y = rnorm(100), X = rnorm(100)) |>
#'   suppressWarnings()
#' 
#' # Estimate the exponential discounting model on these data
#' fitobj <- fit(
#'   exponential(),
#'   data,
#'   itermax = 50,
#'   trace = FALSE
#' )
#' 
#' # Compute the autocorrelation based on this fitobj
#' autocorrelation(fitobj)
#' 
#' @rdname autocorrelation 
#' @export 
autocorrelation <- function(fitobj,
                            lag = 1) {

    # Extract the residuals
    residuals <- fitobj$residuals
    
    # Check if the residuals are a matrix. If not, make them one
    if(!is.matrix(residuals)) {
        residuals <- matrix(residuals, ncol = 1)
    }

    # Loop over all of the columns in the residuals and compute the 
    # autocorrelation for the specified lag
    acf <- sapply(
        seq_len(ncol(residuals)),
        function(i) {
            x <- residuals[, i]
            return(cor(x[1:(length(x) - lag)], x[(lag + 1):length(x)]))
        }
    )

    # Return the average
    return(mean(acf))
}

#' Compute the mean of the residuals
#' 
#' @param fitobj Output of the \code{\link[discounting]{fit}} function.
#' 
#' @return Numeric denoting the mean of the residuals. If multiple
#' dimensions exist in the residuals, then the mean will be computed
#' for each dimension separately and the average of them will be returned.
#' 
#' @examples 
#' # Generate a dataset
#' data <- dataset(Y = rnorm(100), X = rnorm(100)) |>
#'   suppressWarnings()
#' 
#' # Estimate the exponential discounting model on these data
#' fitobj <- fit(
#'   exponential(),
#'   data,
#'   itermax = 50,
#'   trace = FALSE
#' )
#' 
#' # Compute the autocorrelation based on this fitobj
#' bias(fitobj)
#' 
#' @rdname bias 
#' @export 
bias <- function(fitobj) {

    # Extract the residuals
    residuals <- fitobj$residuals
    
    # Check if the residuals are a matrix. If not, make them one
    if(!is.matrix(residuals)) {
        residuals <- matrix(residuals, ncol = 1)
    }

    # Loop over all of the columns in the residuals and compute the 
    # autocorrelation for the specified lag
    output <- sapply(
        seq_len(ncol(residuals)),
        function(i) mean(residuals[, i])
    )

    # Return the average
    return(mean(output))
}

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
#' 
#' @concept statistics
#' 
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
#' 
#' @concept statistics
#' 
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
#' 
#' @concept statistics
#' 
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
#' 
#' @concept statistics
#' 
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

#' Compute the \eqn{R^2} of a fitted model
#' 
#' @details 
#' The \eqn{R^2} is computed for each dependent variable \eqn{j} separately as:
#' 
#' \deqn{R^2_j = 1 - \frac{SSE_j}{SST_j}}
#' 
#' where \eqn{SSE_j} is the sum of squared residuals and \eqn{SST_j} the total 
#' sum of squares of the observed values around their mean. Only time points 
#' for which the residuals are available (i.e., no missing values in the 
#' dependent or independent variables) are used, so that \eqn{SSE_j} and 
#' \eqn{SST_j} are based on the same observations.
#' 
#' Note that as the models in this package are nonlinear and not necessarily 
#' estimated to the least-squares optimum, the \eqn{R^2} is not guaranteed to 
#' lie between 0 and 1: Negative values indicate a fit that is worse than the 
#' mean of the data.
#' 
#' @param fitobj Output of the \code{\link[discounting]{fit}} function.
#' @param data Instance of the \code{\link[discounting]{dataset-class}} 
#' containing the data that the model was fitted on. Needed as \code{fitobj}
#' does not contain the observed values of the dependent variables.
#' @param per_dimension Logical denoting whether to return the \eqn{R^2} for 
#' each dependent variable separately (\code{TRUE}) or their average 
#' (\code{FALSE}). Defaults to \code{FALSE}.
#' 
#' @return Numeric denoting the \eqn{R^2}. If multiple dimensions exist in the
#' data, then the \eqn{R^2} will be computed for each dimension separately and
#' the average of them will be returned, unless \code{per_dimension = TRUE}.
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
#' # Compute the R^2 based on this fitobj
#' rsquared(fitobj, data)
#' 
#' @rdname rsquared 
#' 
#' @concept statistics
#' 
#' @export 
rsquared <- function(fitobj, 
                     data,
                     per_dimension = FALSE) {

    # Extract the residuals and the observed values
    residuals <- fitobj$residuals
    Y <- data@Y

    # Check if the residuals are a matrix. If not, make them one
    if(!is.matrix(residuals)) {
        residuals <- matrix(residuals, ncol = 1)
    }

    # Check whether the data correspond to the fitted model
    if(any(dim(residuals) != dim(Y))) {
        stop("Dimensions of the residuals in `fitobj` do not match those of `data`.")
    }

    # Check whether you want to compute the R^2 for each dimension or to use 
    # the total deviation instead
    if(per_dimension) {
        # Loop over all of the columns and compute the R^2 for each, only using 
        # those time points for which the residuals are available
        output <- sapply(
            seq_len(ncol(residuals)),
            function(i) {
                idx <- !is.na(residuals[, i]) & !is.na(Y[, i])

                sse <- sum(residuals[idx, i]^2)
                sst <- sum((Y[idx, i] - mean(Y[idx, i]))^2)

                return(1 - sse / sst)
            }
        )

    } else {
        residuals <- as.vector(residuals)
        Y <- as.vector(Y) 

        idx <- !is.na(residuals) & !is.na(Y)

        sse <- sum(residuals[idx]^2)
        sst <- sum((Y[idx] - mean(Y[idx]))^2)

        output <- 1 - sse / sst
    }

    return(output)
}

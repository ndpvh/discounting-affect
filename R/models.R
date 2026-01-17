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
#' @seealso 
#' \code{\link[discounting]{model}}
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

#' Constructor for the \code{\link[discounting]{model-class}}
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
#' @seealso 
#' \code{\link[discounting]{exponential-class}}
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

    if(length(d) > 1) {
        warning("Too many dimensions d provided. Using only the first one.")
        d <- d[1]
    }

    if(length(k) > 1) {
        warning("Too many dimensions k provided. Using only the first one.")
        k <- k[1]
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

    # Parameters should be of the correct type
    if(length(parameters) > 0) {
        if(!any(sapply(parameters, \(x) is.numeric(parameters[[x]])))) {
            stop("Some of the provided parameters are not numeric.")
        }
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

    # Check positive definiteness of covariance matrix
    # <TO DO>
    
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

#' Exponential Class
#' 
#' Class for the exponential discounting model. Follows a similar structure to
#' the \code{\link[discounting]{model-class}}.
#' 
#' @slot d Integer denoting the number of dimensions of the model
#' @slot k Integer denoting the number of independent variables
#' @slot parameters List containing the parameters relevant to a particular 
#' model instance
#' @slot covariance Numeric matrix denoting the residual covariance of the 
#' model
#' 
#' @seealso 
#' \code{\link[discounting]{exponential}}
#' 
#' @export 
setClass(
    "exponential",
    slots = c(
        d = "numeric",
        k = "numeric",
        parameters = "list",
        covariance = "matrix"
    ),
    prototype = list(
        d = as.integer(1),
        k = as.integer(1), 
        parameters = list(
            "alpha" = 0,
            "beta" = matrix(0, nrow = 1, ncol = 1),
            "gamma" = matrix(0, nrow = 1, ncol = 1)
        ),
        covariance = matrix(0, nrow = 1, ncol = 1)
    ),
    contains = "model"
)

#' Constructor for the \code{\link[discounting]{exponential-class}}
#' 
#' Defines an instance of the \code{\link[discounting]{exponential-class}}, 
#' that is the class defining the exponential discounting model. For the 
#' mathematical equations and how to specify them in this constructor, users can 
#' look at the details. For more information on the model itself, see the 
#' vignette on the model definitions.
#' 
#' @details 
#' The exponential discounting model assumes that the effect of stimuli on affect
#' fades away at an exponential rate and can be defined as:
#' 
#' \deqn{\boldsymbol{y}_t = \boldsymbol{\alpha} + \sum_{j = 0}^t \Gamma^j B
#' \boldsymbol{x}_{t - j} + \boldsymbol{\epsilon}_t}
#' 
#' where \eqn{\boldsymbol{\alpha}} is a \eqn{d}-dimensional vector representing
#' the mean of the process \eqn{\boldsymbol{y}}, \eqn{\Gamma} is a 
#' \eqn{d \times d} matrix containing the forgetting factors, determining how 
#' long the effect of the independent variables \eqn{\boldsymbol{x}} on the 
#' process \eqn{\boldsymbol{y}} lingers on and therefore determining the 
#' dynamics of the system, \eqn{B} is a \eqn{d \times k}matrix containing the 
#' slopes of the independent variables \eqn{\boldsymbol{x}}, and 
#' \eqn{\boldsymbol{\epsilon}} represents the residuals of the system. Within 
#' this package, we assume that:
#' 
#' \deqn{\boldsymbol{\epsilon} \overset{iid}{\sim} N(\boldsymbol{0}, \Sigma)}
#' 
#' where \eqn{\Sigma} is a \eqn{d \times d} matrix representing the residual 
#' covariance matrix.
#' 
#' The exponential discounting model can also be written as an instance of the
#' \eqn{VARMAX(1, 1)}, so that:
#' 
#' \deqn{\boldsymbol{y}_t = (I_d - \Gamma) \boldsymbol{\alpha} + 
#' B \boldsymbol{x}_t + \Gamma \boldsymbol{y}_{t - 1} - 
#' \Gamma \boldsymbol{\epsilon}_{t - 1} + \boldsymbol{\epsilon}_t}
#' 
#' where \eqn{I_d} is a \eqn{d \times d} identity matrix.
#' 
#' To define the model, one should minimally define the parameters of the model
#' through \code{parameters} and \code{covariance}. In \code{covariance}, one 
#' either provides the \eqn{d \times d} residual covariance matrix \eqn{\Sigma}
#' (\code{cholesky = FALSE}) or a lower-triangular \eqn{d \times d} matrix 
#' \eqn{G} that represents part of the decomposition of this matrix 
#' (\code{cholesky = TRUE}), so that we can retrieve the covariance matrix as:
#' 
#' \deqn{\Sigma = G G^T}
#' 
#' The latter option ensures that the covariance matrix \eqn{\Sigma} defined 
#' within the model is positive-definite, but may require some additional 
#' thinking on the side of the user.
#' 
#' In \code{parameters}, one should provide a named list with instances 
#' \code{"alpha"}, \code{"beta"}, and \code{"gamma"}, each defining the 
#' respective parameters \eqn{\boldsymbol{\alpha}}, \eqn{B}, and \eqn{\Gamma}
#' of the model. Note that the dimensionalities of these parameters should 
#' match up and are actively checked when constructing this class. 
#' 
#' @param d Integer denoting the number of dimensions of the model. Defaults to 
#' \code{NA}, in which case this dimensionality is inferred from the parameters.
#' @param k Integer denoting the number of independent variables. Defaults to 
#' \code{NA}, in which case this dimensionality is inferred from the parameters.
#' @param parameters List containing the parameters relevant to a particular 
#' model instance. Defaults to an empty model with \eqn{d = 1} and \eqn{k = 1}.
#' @param covariance Numeric matrix denoting the residual covariance of the 
#' model. Default to a matrix of \code{0}s with the dimensionality implied by 
#' \code{d}.
#' @param cholesky Logical denoting whether \code{covariance} is a 
#' lower-triangular decomposition matrix instead of an actual covariance matrix.
#' Defaults to \code{FALSE}.
#' 
#' @return Instance of \code{\link[discounting]{exponential-class}}
#' 
#' @examples 
#' exponential(
#'   d = 2,
#'   k = 2,
#'   parameters = list(
#'     "alpha" = numeric(2),
#'     "beta" = diag(2) * 2,
#'     "gamma" = diag(2) * 0.5
#'   ),
#'   covariance = diag(2)
#' )
#' 
#' @seealso 
#' \code{\link[discounting]{model-class}}
#' \code{\link[discounting]{exponential-class}}
#' 
#' @export 
exponential <- function(d = NA, 
                        k = NA,
                        parameters = list(
                            "alpha" = 0,
                            "beta" = matrix(0, nrow = 1, ncol = 1)
                            "gamma" = matrix(0, nrow = 1, ncol = 1)
                        ), 
                        covariance = matrix(0, nrow = 1, ncol = 1)) {
        
    # Check whether all parameters are defined in the list of parameters
    if(!all(c("alpha", "beta", "gamma") %in% names(parameters))) {
        stop(
            paste(
                "Not all parameters are defined in the list.",
                "Ensure that the list contains slots \"alpha\", \"beta\", and \"gamma\"."
            )
        )
    }

    # Check whether too many parameters are defined in the list. If so, delete 
    # them from the list
    if(length(parameters) > 3) {
        warnings(
            paste(
                "Too many different parameters provided to `parameters`.",
                "Deleting the redundant ones."
            )
        )

        idx <- names(parameters) %in% c("alpha", "beta", "gamma")
        parameters <- parameters[idx]
    }

    # Parameters should be of the correct type
    if(!is.matrix(parameters[["gamma"]]) & length(parameters[["gamma"]]) == 1) {
        warning("The parameter \"gamma\" should be a matrix: Changing type.")
        parameters[["gamma"]] <- as.matrix(parameters[["gamma"]])

    } else if(!is.matrix(parameters[["gamma"]])) {
        stop("The parameter \"gamma\" should be a matrix.")
    }

    if(!is.matrix(parameters[["beta"]])) {
        warning(
            paste(
                "The parameter \"beta\" should be a matrix:",
                "Changing type assuming a single independent variable."
            )
        )

        parameters[["beta"]] <- matrix(parameters[["beta"]], ncol = 1)
    }

    if(!is.matrix(covariance) & length(covariance) == 1) {
        warning("The argument \"covariance\" should be a matrix: Changing type.")
        covariance <- as.matrix(covariance)

    } else if(!is.matrix(covariance)) {
        stop("The argument \"covariance\" should be a matrix.")
    }

    # If the dimensionalities d and/or k are not defined, try to derive them 
    # from the parameters that were given.
    if(is.na(d)) {
        d <- length(parameters[["alpha"]])
    }

    if(is.na(k)) {
        k <- ncol(parameters[["beta"]])
    }

    # Check for inconsistencies in the parameter dimensions and whatever 
    # dimensionalities have been provided.
    if(length(parameters[["alpha"]]) != d) {
        stop("Dimensionality d is not in accord with the dimensionality of \"alpha\".")
    } 

    if(any(dim(parameters[["gamma"]]) != d)) {
        stop("Dimensionality d is not in accord with the dimensionality of \"gamma\".")
    }

    if(any(dim(parameters[["beta"]]) != c(d, k))) {
        stop("Dimensionalities d and/or k are not in accord with the dimensionalilty of \"beta\".")
    }

    # Check whether the covariance matrix needs to be derived through the 
    # Cholesky decomposition
    if(cholesky) {
        covariance <- covariance %*% t(covariance)
    }

    # Check eigenvalues of Gamma
    decomposed <- eigen(parameters[["gamma"]])
    if(any(decomposed$values <= 0) | any(decomposed$values >= 1)) {
        stop("The eigenvalues of \"gamma\" should lie between 0 and 1.")
    }
    
    # Create a new model and change its class to exponential. This ensures some
    # additional checks are performed
    .Object <- model(
        d = d,
        k = k,
        parameters = parameters,
        covariance = covariance
    )
    class(.Object) <- "exponential"

    return(.Object)
}
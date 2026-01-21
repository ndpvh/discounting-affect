#' Dataset Class
#' 
#' This class contains the standard format for the dataset to be used within this 
#' project, ensuring a standardized approach for all computations.
#' 
#' @slot Y Numeric matrix of observations to be fitted, that is the dependent 
#' variables
#' @slot X Numeric matrix of the stimuli that should be related to \code{Y}, 
#' that is the independent variables
#' 
#' @seealso 
#' \code{\link[discounting]{dataset}}
#' 
#' @export
setClass(
    "dataset",
    slots = c(
        Y = "matrix",
        X = "matrix",
        N = "numeric"
    ),
    prototype = list(
        Y = matrix(0, nrow = 0, ncol = 0),
        X = matrix(0, nrow = 0, ncol = 0),
        N = 0
    )
)

#' Constructor for the \code{\link[discounting]{dataset-class}}
#' 
#' Takes in a \code{data.frame} and, based on the value of the arguments, 
#' constructs an instance of the \code{\link[discounting]{dataset-class}}. Through
#' this type of class definition, it is ensured that the estimation, simulation,
#' cross-validation, and recovery routines can all make use of the same 
#' data structure, making their definition easier within this package. Note 
#' that this class is only defined on the within-subject level, meaning that 
#' the data provided to this constructor should be on this level.
#' 
#' @param data A \code{data.frame} containing the dependent and independent 
#' variables of interest for a single person. Defaults to \code{NULL}, in which
#' case an empty instance of the \code{\link[discounting]{dataset-class}} will 
#' be created.
#' @param y_cols A character vector denoting the names of the columns for the 
#' dependent variables. Defaults to \code{NULL}, in which case an error will 
#' be thrown asking for more information.
#' @param x_cols A character vector denoting the names of the columns for the 
#' independent variables. Defaults to \code{NULL}, in which case an error will 
#' be thrown asking for more information.
#' @param Y Numeric vector or numeric matrix containing the values for the 
#' dependent variable(s). Defaults to \code{NULL}, in which case it will 
#' become an empty matrix of length \eqn{N x 1}. Ignored if \code{data} is 
#' specified instead.
#' @param X Numeric vector or numeric matrix containing the values for the 
#' independent variable(s). Defaults to \code{NULL}, in which case the 
#' \code{data} should be specified instead.
#' @param sorting_variable Character denoting the columnn name of a variable 
#' in the \code{data} according to which the values for \code{Y} and \code{X}
#' should be sorted. Defaults to \code{NULL}, meaning that no sorting should 
#' happen.
#' 
#' @return Instance of the \code{\link[discounting]{dataset-class}}.
#' 
#' @examples 
#' # Create a data.frame for reference
#' my_data <- data.frame(
#'   DV_1 = rep(1, each = 10),
#'   DV_2 = rep(2, each = 10),
#'   IV_1 = rep(3, each = 10),
#'   IV_2 = rep(4, each = 10),
#'   IV_3 = rep(5, each = 10)
#' )
#' 
#' # Create a dataset with only a single DV and IV
#' dataset(
#'   data = my_data, 
#'   y_cols = "DV_1",
#'   x_cols = "IV_1"
#' )
#' 
#' # Create a dataset with multiple DV and IV
#' dataset(
#'   data = my_data,
#'   y_cols = c("DV_1", "DV_2"),
#'   x_cols = c("IV_1", "IV_2", "IV_3")
#' )
#'  
#' @seealso 
#' \code{\link[discounting]{dataset-class}}
#' 
#' @export 
dataset <- function(data = NULL,
                    y_cols = NULL,
                    x_cols = NULL,
                    Y = NULL,
                    X = NULL,
                    sorting_variable = NULL) {
    
    # Check if data is NULL. If so, return the empty prototype
    if(is.null(data) & is.null(Y) & is.null(X)) {
        warning("No data provided: Returning an empty instance of the dataset-class.")

        return(new("dataset"))
    }

    # Dispatch on whether data are provided or not. If so, we have to check the 
    # data-relevant columns, otherwise we have to check the raw arguments
    # Y and X
    if(!is.null(data)) {
        # If either y_cols or x_cols is NULL, ask for more information
        if(is.null(y_cols)) {
            stop(
                paste(
                    "No dependent variables are specified in \"y_cols\".",
                    "Please provide these to the constructor \`dataset\`."
                )
            )
        }

        if(is.null(x_cols)) {
            stop(
                paste(
                    "No independent variables are specified in \"x_cols\".",
                    "Please provide these to the constructor \`dataset\`."
                )
            )
        }

        # For each of the relevant columns, check whether they are numeric or not.
        # If not, then throw an error
        check <- sapply(c(y_cols, x_cols), function(x) is.numeric(data[, x]))
        if(!all(check)) {
            stop("Some of the provided variables are not numeric.")        
        }

        # Check whether the data should be sorted. If so, then sort the data 
        # according to the specified variable
        if(!is.null(sorting_variable)) {
            data <- data[order(data[, sorting_variable]), ]
        }

        # Extract the information from the data.frame
        Y <- data[, y_cols] |>
            unlist() |>
            matrix(ncol = length(y_cols)) |>
            `names<-` (NULL) |>
            `colnames<-` (y_cols)
        X <- data[, x_cols] |>
            unlist() |>
            matrix(ncol = length(x_cols)) |>
            `names<-` (NULL) |>
            `colnames<-` (x_cols)

    } else {
        # Check whether X is provided: If not, throw an error
        if(is.null(X)) {
            stop("No value for \"data\" nor \"X\" is provided. Cannot proceed.")
        }

        # Check whether X is of the correct type. If not then we need to make it 
        # so
        if(!is.numeric(X)) {
            stop("Provided value for \"X\" is not numeric.")
        }

        if(!is.matrix(X)) {
            warning(
                paste(
                    "Provided value for \"X\" is not a matrix.",
                    "Transforming in a matrix with 1 column."
                )
            )

            X <- matrix(X, ncol = 1)
        }

        # Check whether Y is defined. If not, make an empty value for Y. 
        # Otherwise perform the same checks as for X
        if(is.null(Y)) {
            Y <- matrix(0, nrow = nrow(X), ncol = 1)

        } else {
            if(!is.numeric(Y)) {
                stop("Provided value for \"Y\" is not numeric.")
            }

            if(!is.matrix(Y)) {
                warning(
                    paste(
                        "Provided value for \"Y\" is not a matrix.",
                        "Transforming in a matrix with 1 column."
                    )
                )

                Y <- matrix(Y, ncol = 1)
            }

            # Check whether the sizes of Y and X correspond
            if(nrow(Y) != nrow(X)) {
                stop("Number of rows in \"Y\" and \"X\" are not the same.")
            }
        }
    }

    # If they are provided and correctly typed, then construct the dataset class
    return(
        new(
            "dataset",
            Y = Y,
            X = X,
            N = nrow(Y)
        )
    )
}

#' Show method for the \code{\link[discounting]{dataset-class}}
#' 
#' @param object Object of the \code{\link[discounting]{dataset-class}}
#' 
#' @noRd
setMethod(
    "show",
    "dataset",
    function(object) {
        cat("An object of class \"dataset\"\n\n")

        # Get the head for slot Y
        cat(
            "Slot \"Y\": ",
            nrow(object@Y),
            "x",
            ncol(object@Y),
            "matrix\n",
            sep = ""
        )
        print(head(object@Y))
        cat("\n")

        # Get the head for slot X
        cat(
            "Slot \"X\": ",
            nrow(object@X),
            "x",
            ncol(object@X),
            "matrix\n",
            sep = ""
        )
        print(head(object@X))
    }
)

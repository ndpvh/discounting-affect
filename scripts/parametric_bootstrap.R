################################################################################
# PURPOSE:
#
# For each of the datasets and models, also perform a parametric bootstrap 
# checking whether the models can capture interesting phenomena in the data.
################################################################################

################################################################################
# PHENOMENA OF INTEREST
################################################################################

# Autocorrelation at a particular lag. According to the VARMAX equivalence, the
# exponential and quasi-hyperbolic should be lag-1 only and not capture any lags
# after that, while the double-exponential should be able to capture these as 
# well (no current equivalence to a lag-1 VARMAX has been proven)
#
# Note that this one doesn't work well with the VANHASBROECK_2021 dataset, as 
# NAs are typically paired with observations. Therefore correlation taken of 
# observations alone
autocorrelation <- function(dataset, 
                            lag = 1,
                            ...) {
    
    # Extract the Y-matrix
    Y <- dataset@Y 

    # Remove NAs
    Y <- Y[!is.na(Y[, 1]), , drop = FALSE]

    # Create y and y0 for a particular lag
    y <- Y[(1 + lag):nrow(Y), , drop = FALSE]
    y0 <- Y[1:(nrow(Y) - lag), , drop = FALSE]

    # Compute the autocorrelation for each dimension separately
    return(
        cor(y, y0) |>
            diag() |>
            `names<-` (colnames(Y))
    )
}

# Autocorrelation of the residuals: As an assumption check. Again note that 
# this doesn't work well with the VANHASBROECK_2021 dataset, as NAs are again 
# paired with observations. Therefore correlation taken of observations alone.
residual_autocorrelation <- function(dataset, 
                                     model = NULL,
                                     lag = 1,
                                     ...) {

    # If the model is NULL, we can't perform the analyses
    if(is.null(model)) {
        stop("Model can't be NULL for residual_autocorrelation.")
    }
    
    # Extract the Y-matrix
    Y <- dataset@Y 

    # Given the model, we can predict this matrix's values
    y <- predict(model, dataset)@Y

    # Subtract them from each other to get the residuals
    residuals <- matrix(Y - y, ncol = ncol(Y))
    
    # Remove NAs 
    residuals <- residuals[!is.na(residuals[, 1]), , drop = FALSE] 

    # Lag these residuals according to the specified lag 
    e <- residuals[(1 + lag):nrow(residuals), , drop = FALSE]
    e0 <- residuals[1:(nrow(residuals) - lag), , drop = FALSE]

    # Compute the autocorrelation for each separate dimension
    return(
        cor(e, e0) |>
            diag() |>
            `names<-` (colnames(Y))
    )
} 

# Correlation between the outcomes and the stimuli at different lags. Allows us
# to check how well the discounting functions capture these relationships and 
# whether they conform to reality.
#
# Note that the NAs in the VANHASBROECK_2021 dataset are not problematic here, 
# as the relationship with the outcomes is still known on every measured occasion.
outcome_correlation <- function(dataset, 
                                lag = 0, 
                                ...) {
    
    # Extract Y and X
    Y <- dataset@Y
    X <- dataset@X 

    # Add the specified lag in the matrices
    y <- Y[(1 + lag):nrow(Y), , drop = FALSE]
    x <- X[1:(nrow(X) - lag), , drop = FALSE]

    # Create the correlations
    return(
        cor(y, x, use = "pairwise.complete.obs") |>
            as.numeric() |>
            `names<-` (paste(
                rep(colnames(Y), each = ncol(X)), 
                rep(colnames(X), times = ncol(Y)),
                sep = "_"
            ))
    )
}

# Moments: A measure of the four first moments of the data, which in theory 
# should be captured well by the models. These are unstandardized moments and 
# these moments retain their original power (e.g., the variance rather than sd
# is used). 
moment <- function(dataset, 
                   order = 1, 
                   ...) {
    
    # Extract Y
    Y <- dataset@Y 

    # Remove missing values
    Y <- Y[!is.na(Y[, 1]), , drop = FALSE]

    # Compute the mean of the observed data. Is used in all moment-computations
    m <- colMeans(Y)

    # Compute the moments. If order = 1, then the means are just returned
    if(order == 1) {
        return(
            m |>
                `names<-` (colnames(Y))
        )
    } else {
        return(
            sapply(
                seq_len(ncol(Y)),
                function(i) mean((Y[, i] - m[i])^order)
            ) |>
                `names<-` (colnames(Y))
        )
    }
    
}


# Bimodality coefficient: A measure of nonlinearity in which multimodality is 
# measured. Was a primary phenomenon examined in previous studies that made use
# of these data, so its inclusion is interesting here as well to examine the 
# capacity of the models to capture this type of multimodality.
bimodality <- function(dataset, ...) {
    # Compute the four different moments, based on which we can compute the BC
    m1 <- moment(dataset, order = 1)
    m2 <- moment(dataset, order = 2)
    m3 <- moment(dataset, order = 3)
    m4 <- moment(dataset, order = 4)

    # Compute the sample skewness and excess kurtosis as used in the BC formula
    g <- m3 / m2^(3/2)
    k <- m4 / m2^2 - 3

    n <- nrow(dataset@Y)

    # Compute the bimodality coefficient
    return(
        ((g^2 + 1) / (k + 3 * (n - 1)^2 / ((n - 1) * (n - 2)))) |>
            `names<-` (names(m1))
    )
}



################################################################################
# PARAMETRIC BOOTSTRAP
################################################################################

# Define the number of datasets to be generated per person per model per dataset
N <- 1000

# Define the phenomena, the models, and the datasets of interest
datasets <- list(
    "VANHASBROECK_2021" = c(1, 3),
    "VANHASBROECK_2022" = c(2, 1),
    "VANHASBROECK_2024" = c(2, 1)
)
models <- list(
    "exponential" = exponential,
    "quasi_hyperbolic" = quasi_hyperbolic,
    "double_exponential" = double_exponential
)
phenomena <- list(
    "autocorrelation_1" = function(...) autocorrelation(..., lag = 1),
    "autocorrelation_2" = function(...) autocorrelation(..., lag = 2),
    "autocorrelation_3" = function(...) autocorrelation(..., lag = 3),

    "residual_autocorrelation_1" = function(...) residual_autocorrelation(..., lag = 1),
    "residual_autocorrelation_2" = function(...) residual_autocorrelation(..., lag = 2),
    "residual_autocorrelation_3" = function(...) residual_autocorrelation(..., lag = 3),

    "outcome_correlation_1" = function(...) outcome_correlation(..., lag = 1),
    "outcome_correlation_2" = function(...) outcome_correlation(..., lag = 2),
    "outcome_correlation_3" = function(...) outcome_correlation(..., lag = 3),
    "outcome_correlation_4" = function(...) outcome_correlation(..., lag = 4),
    "outcome_correlation_5" = function(...) outcome_correlation(..., lag = 5),

    "moment_1" = function(...) moment(..., order = 1),
    "moment_2" = function(...) moment(..., order = 2),
    "moment_3" = function(...) moment(..., order = 3),
    "moment_4" = function(...) moment(..., order = 4),

    "bimodality_coefficient" = bimodality
)

# Combine datasets and models into one matrix
set.seed(5) # Sufferer - Again
conditions <- cbind(
    rep(names(datasets), each = length(models)),
    rep(names(models), times = length(datasets)),
    (rnorm(length(models) * length(datasets)) * 1000) |>
        abs() |>
        round()
)

# Loop over datasets and models to perform the analyses
results <- lapply(#parallel::mclapply(
    seq_len(nrow(conditions)), 
    function(i) {
        set.seed(conditions[i, 3])

        # Load the parameters for the specified dataset and model
        parameters <- read.csv(
            file.path(
                "scripts",
                "results",
                "estimation",
                paste(
                    conditions[i, 1],
                    "_",
                    conditions[i, 2],
                    ".csv",
                    sep = ""
                )
            )
        )

        # Loop over all participants
        results <- lapply(
            seq_len(nrow(parameters)),
            function(j) {
                # Provide an indication of the dataset, model, and parameter set
                # we're looking at
                cat("\r", conditions[i, 1], ", ", conditions[i, 2], ": ", j, "    ", sep = "")

                # Get the participant id
                id <- parameters$participant_id[j]

                # Define the model of the participant.
                params <- parameters[j, ] |>
                    dplyr::select(-participant_id, -(aic:objective_sse)) |>
                    unlist() |>
                    as.numeric()

                model <- models[[conditions[i, 2]]](
                    d = datasets[[conditions[i, 1]]][1], 
                    k = datasets[[conditions[i, 1]]][2]
                ) |>
                    fill(
                        params, 
                        dynamics = "isotropic",
                        covariance = "symmetric",
                        parameters_only = FALSE,
                        cholesky = FALSE
                    )

                # Compute the empirical values of the phenomena of interest. 
                # For this, we load the dataset and then loop over each function
                # while providing this dataset to the functions
                data <- readRDS(
                    file.path(
                        "scripts",
                        "data",
                        paste0(conditions[i, 1], "_per_participant"),
                        paste0(id, ".rds")
                    )
                )

                true <- lapply(
                    names(phenomena), 
                    function(x) {
                        # Compute the values of the statistics that serve to 
                        # quanify our phenomena of interest
                        statistic <- phenomena[[x]](
                            data, 
                            model = model
                        )

                        # Save in a data.frame. When there are multiple values 
                        # for the phenomena, we adjust the data.frame to take 
                        # these variables into account                        
                        return(
                            data.frame(
                                dataset = conditions[i, 1],
                                model = conditions[i, 2],
                                phenomenon = x, 
                                variables = names(statistic),
                                true = statistic |>
                                    `names<-` (NULL)
                            )
                        )
                    }
                )
                true <- do.call("rbind", true)

                # Create N new datasets based on the estimated parameters of the
                # participant. For each of these datasets, then compute the 
                # values of the statistics, and finally summarize into one 
                # coherent data.frame
                browser()
                simulated <- lapply(
                    1:N, 
                    function(k) {
                        # Generate a dataset based on the provided model
                        simdata <- simulate(
                            model, 
                            X = data@X
                        )

                        # Loop over the phenomena and return a data.frame
                        simulated <- lapply(
                            names(phenomena), 
                            function(x) {
                                # Compute the values of the statistics that serve to 
                                # quanify our phenomena of interest
                                statistic <- phenomena[[x]](
                                    data, 
                                    model = model
                                )

                                # Save in a data.frame. When there are multiple values 
                                # for the phenomena, we adjust the data.frame to take 
                                # these variables into account                        
                                return(
                                    data.frame(
                                        dataset = conditions[i, 1],
                                        model = conditions[i, 2],
                                        phenomenon = x, 
                                        variables = names(statistic),
                                        iteration = k, 
                                        value = statistic |>
                                            `names<-` (NULL)
                                    )
                                )
                            }
                        )
                        return(do.call("rbind", simulated))
                    }
                )
                simulated <- do.call("rbind", simulated) |>
                    dplyr::group_by(dataset, model, phenomenon, variables) |>
                    dplyr::summarize(
                        mean = mean(value),
                        sd = sd(value),
                        min = min(value),
                        q025 = quantile(value, prob = 0.025),
                        q25 = quantile(value, prob = 0.25),
                        q50 = quantile(value, prob = 0.50),
                        q75 = quantile(value, prob = 0.75),
                        q975 = quantile(value, prob = 0.975),
                        max = max(value)
                    ) |>
                    dplyr::ungroup()

                # Bind both data.frames together so that all results are bundled.
                # Then return this result
                return(
                    simulated |>
                        dplyr::full_join(estimated)
                )
            }
        )

        # Bind the results together and save in a csv-file
        results <- do.call("rbind", results)
        write.csv(
            file.path(
                "scripts",
                "results",
                "bootstrap",
                paste(
                    conditions[i, 1],
                    "_",
                    conditions[i, 2],
                    ".csv",
                    sep = ""
                )
            )
        )

        return(NULL)
    }#,
    # mc.cores = ifelse(
    #     Sys.info()["sysname"] == "Windows",
    #     1,
    #     round(parallel::detectCores() / 2) - 1  # Optimized for my own Mac/Linux system
    # )
)

################################################################################
# PURPOSE:
#
# For each of the datasets and models, perform the (computationally expensive)
# parametric bootstrap checking whether the models can capture interesting
# phenomena in the data: bootstrap datasets are simulated from each
# participant's estimated parameters, the phenomena of interest are evaluated
# on the simulated data, and the raw per-dataset / per-model bootstrap results
# are saved as CSV files in PATHS$parametric_bootstrap.
#
# This script ONLY performs bootstrap generation. The downstream, inexpensive
# summary/coverage analysis of these saved results is performed separately by
# analysis/10_summarize_parametric_bootstrap.R, so existing results can be
# (re)analyzed without rerunning the expensive bootstrap procedure.
################################################################################

# Source shared infrastructure
config_file <- if (file.exists(file.path("analysis", "_config.R"))) {
  file.path("analysis", "_config.R")
} else if (file.exists("_config.R")) {
  "_config.R"
} else {
  stop(
    "Could not find analysis/_config.R. ",
    "Run this script from the repository root or analysis/ directory."
  )
}
source(config_file)
source(file.path(PATHS$analysis, "_helpers.R"))
rm(config_file)

devtools::load_all(PROJECT_ROOT)

################################################################################
# PHENOMENA OF INTEREST
################################################################################

# Shared lagged-correlation helper.
#
# Lagged pairs are created BEFORE incomplete pairs are removed. This preserves
# the scheduled time axis: missing responses break affected pairs instead of
# making observations on either side of a gap appear consecutive.
#
# NIEMEIJER_2022 has 10 scheduled beeps per day followed by a structural NA
# separator (row 11 of each daily block). For lags > 1, endpoint-wise listwise
# deletion alone could still pair observations across that separator. The
# `block_size` check prevents any pair from crossing a daily block boundary.
lagged_autocorrelation_matrix <- function(values,
                                          lag = 1L,
                                          block_size = NULL) {

    if (!is.matrix(values)) {
        values <- matrix(values, ncol = 1)
    }

    n <- nrow(values)

    if (n <= lag) {
        return(setNames(rep(NA_real_, ncol(values)), colnames(values)))
    }

    now_idx <- (lag + 1L):n
    lag_idx <- 1L:(n - lag)

    same_block <- rep(TRUE, length(now_idx))

    if (!is.null(block_size)) {
        block_id <- (seq_len(n) - 1L) %/% block_size
        same_block <- block_id[now_idx] == block_id[lag_idx]
    }

    out <- vapply(
        seq_len(ncol(values)),
        function(i) {
            pairs <- data.frame(
                current = values[now_idx, i],
                lagged  = values[lag_idx, i]
            )

            keep <- same_block & complete.cases(pairs)
            pairs <- pairs[keep, , drop = FALSE]

            if (nrow(pairs) < 2L ||
                stats::sd(pairs$current) == 0 ||
                stats::sd(pairs$lagged) == 0) {
                return(NA_real_)
            }

            stats::cor(pairs$current, pairs$lagged)
        },
        numeric(1)
    )

    names(out) <- colnames(values)
    out
}


# Autocorrelation of observed affect at a particular lag.
#
# NIEMEIJER_2022 preserves the scheduled time axis and uses lag-first listwise
# deletion with daily-block protection. Other datasets retain the historical
# behavior of correlating the observed rows after missing responses are removed.
autocorrelation <- function(dataset,
                            lag = 1,
                            dataset_name = NULL,
                            ...) {

    Y <- dataset@Y

    if (identical(dataset_name, "NIEMEIJER_2022")) {
        return(
            lagged_autocorrelation_matrix(
                Y,
                lag = lag,
                block_size = 11L
            )
        )
    }

    # Historical behavior for the non-Niemeijer datasets.
    Y <- Y[!is.na(Y[, 1]), , drop = FALSE]

    if (nrow(Y) <= lag) {
        return(setNames(rep(NA_real_, ncol(Y)), colnames(Y)))
    }

    y <- Y[(1 + lag):nrow(Y), , drop = FALSE]
    y0 <- Y[1:(nrow(Y) - lag), , drop = FALSE]

    cor(y, y0) |>
        diag() |>
        `names<-`(colnames(Y))
}


# Autocorrelation of model residuals, used as an assumption check.
#
# NIEMEIJER_2022 uses lag-first listwise deletion so ordinary missed beeps do
# not collapse the time axis; the 11-row block definition also prevents lag-2
# and lag-3 pairs from crossing day boundaries. Other datasets retain the
# historical observed-row behavior.
residual_autocorrelation <- function(dataset,
                                     model = NULL,
                                     lag = 1,
                                     dataset_name = NULL,
                                     ...) {

    if (is.null(model)) {
        stop("Model can't be NULL for residual_autocorrelation.")
    }

    Y <- dataset@Y
    y <- predict(model, dataset)@Y
    residuals <- matrix(Y - y, ncol = ncol(Y))
    colnames(residuals) <- colnames(Y)

    if (identical(dataset_name, "NIEMEIJER_2022")) {
        return(
            lagged_autocorrelation_matrix(
                residuals,
                lag = lag,
                block_size = 11L
            )
        )
    }

    # Historical behavior for the non-Niemeijer datasets.
    residuals <- residuals[!is.na(residuals[, 1]), , drop = FALSE]

    if (nrow(residuals) <= lag) {
        return(setNames(rep(NA_real_, ncol(residuals)), colnames(Y)))
    }

    e <- residuals[(1 + lag):nrow(residuals), , drop = FALSE]
    e0 <- residuals[1:(nrow(residuals) - lag), , drop = FALSE]

    cor(e, e0) |>
        diag() |>
        `names<-`(colnames(Y))
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
N <- 10000

# Ensure the output directories exist before anything is written.
ensure_dir(PATHS$parametric_bootstrap)

# Define the phenomena, the models, and the datasets of interest
datasets <- list(
    # Keep these dimensions synchronized with 01_process_data.R /
    # 02_estimate_models.R. The 2021 and 2022 specifications include the
    # cumulative `total` predictor in the current analysis pipeline.
    "VANHASBROECK_2021" = c(1, 4),
    "VANHASBROECK_2022" = c(2, 2),
    "VANHASBROECK_2024_1" = c(1, 1),
    "VANHASBROECK_2024_2" = c(2, 1),
    "NIEMEIJER_2022" = c(2, 2)
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

    "outcome_correlation_0" = function(...) outcome_correlation(..., lag = 0),
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

# Remove the expected raw bootstrap CSVs from an earlier run before starting.
# A failed rerun therefore cannot be mistaken for a complete new result set.
expected_bootstrap_files <- file.path(
    PATHS$parametric_bootstrap,
    paste0(conditions[, 1], "_", conditions[, 2], ".csv")
)
unlink(expected_bootstrap_files[file.exists(expected_bootstrap_files)])

# The summary RDS is derived from these raw CSVs and becomes stale as soon as a
# new bootstrap generation starts. Remove it so an interrupted rerun cannot leave
# an old summary that looks current.
bootstrap_summary_file <- file.path(
    PATHS$parametric_bootstrap, "bootstrap_summary.Rds"
)
if (file.exists(bootstrap_summary_file)) unlink(bootstrap_summary_file)

# Keep the historical half-the-available-cores strategy while ensuring that
# low-core systems never receive mc.cores = 0.
detected_cores <- parallel::detectCores()
if (is.na(detected_cores)) detected_cores <- 1L
bootstrap_cores <- if (Sys.info()["sysname"] == "Windows") {
    1L
} else {
    max(1L, as.integer(round(detected_cores / 2) - 1L))
}

# Loop over datasets and models to perform the analyses
results <- parallel::mclapply(
    seq_len(nrow(conditions)), 
    function(i) {
        set.seed(conditions[i, 3])

        # Load the parameters for the specified dataset and model
        parameters <- read.csv(
            file.path(
                PATHS$estimation,
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
                cat(
                    "\r", conditions[i, 1], ", ", conditions[i, 2], ": ", j, "              ", 
                    sep = ""
                )

                # Get the participant id
                id <- parameters$participant_id[j]

                # Extract the fitted parameter vector. The model itself is
                # reconstructed after loading this participant's processed data
                # so the stored dataset dimensions can be validated first.
                params <- parameters[j, ] |>
                    dplyr::select(-participant_id, -(aic:objective_sse)) |>
                    unlist() |>
                    as.numeric()

                # Compute the empirical values of the phenomena of interest. 
                # For this, we load the dataset and then loop over each function
                # while providing this dataset to the functions
                #
                # Special trick needed for the VANHASBROECK_2024 data.
                if(conditions[i, 1] %in% c("VANHASBROECK_2024_1", "VANHASBROECK_2024_2")) {
                    data <- readRDS(
                        file.path(
                            PATHS$processed_data,
                            "VANHASBROECK_2024",
                            paste0(id, ".rds")
                        )
                    )
                
                } else {
                    data <- readRDS(
                        file.path(
                            PATHS$processed_data,
                            conditions[i, 1],
                            paste0(id, ".rds")
                        )
                    )
                }

                expected_dimensions <- datasets[[conditions[i, 1]]]
                observed_dimensions <- c(ncol(data@Y), ncol(data@X))
                if (!identical(
                    as.integer(observed_dimensions),
                    as.integer(expected_dimensions)
                )) {
                    stop(
                        "Processed-data dimensions do not match the parametric-bootstrap ",
                        "configuration for ", conditions[i, 1], " / participant ", id,
                        ". Expected d=", expected_dimensions[1],
                        ", k=", expected_dimensions[2],
                        "; observed d=", observed_dimensions[1],
                        ", k=", observed_dimensions[2], "."
                    )
                }

                model <- models[[conditions[i, 2]]](
                    d = observed_dimensions[1],
                    k = observed_dimensions[2]
                ) |>
                    fill(
                        params,
                        dynamics = "isotropic",
                        covariance = "symmetric",
                        parameters_only = FALSE,
                        cholesky = FALSE
                    )

                true <- lapply(
                    names(phenomena), 
                    function(x) {
                        # Compute the values of the statistics that serve to 
                        # quanify our phenomena of interest
                        statistic <- phenomena[[x]](
                            data,
                            model = model,
                            dataset_name = conditions[i, 1]
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
                simulated <- lapply(
                    1:N, 
                    function(k) {
                        # Generate a dataset based on the provided model
                        simdata <- simulate(
                            model, 
                            X = data@X
                        )

                        # Add the names of the dependent and independent variables
                        # to the dataset
                        colnames(simdata@Y) <- colnames(data@Y)
                        colnames(simdata@X) <- colnames(data@X)

                        # Loop over the phenomena and return a data.frame
                        simulated <- lapply(
                            names(phenomena), 
                            function(x) {
                                # Compute the values of the statistics that serve to 
                                # quantify our phenomena of interest
                                statistic <- phenomena[[x]](
                                    simdata,
                                    model = model,
                                    dataset_name = conditions[i, 1]
                                ) |>
                                    suppressWarnings()

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
                        mean = mean(value, na.rm = TRUE),
                        sd = sd(value, na.rm = TRUE),
                        min = min(value, na.rm = TRUE),
                        q025 = quantile(value, prob = 0.025, na.rm = TRUE),
                        q25 = quantile(value, prob = 0.25, na.rm = TRUE),
                        q50 = quantile(value, prob = 0.50, na.rm = TRUE),
                        q75 = quantile(value, prob = 0.75, na.rm = TRUE),
                        q975 = quantile(value, prob = 0.975, na.rm = TRUE),
                        max = max(value, na.rm = TRUE)
                    ) |>
                    dplyr::ungroup() |>
                    suppressMessages()

                # Bind both data.frames together so that all results are bundled.
                # Then return this result
                return(
                    simulated |>
                        dplyr::full_join(true) |>
                        suppressMessages()
                )
            }
        )

        # Bind the results together
        results <- do.call("rbind", results)

        # Add a logical that checks whether the empirical values fall within the 
        # simulated 95%CI: The main check that we want to perform here.
        results$covered <- (results$true <= results$q975) & (results$true >= results$q025)

        # Save the results in a .csv file
        write.csv(
            results,
            file.path(
                PATHS$parametric_bootstrap,
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
    },
    mc.cores = bootstrap_cores
)

worker_failures <- which(vapply(
    results, function(x) inherits(x, "try-error"), logical(1)
))
if (length(worker_failures) > 0) {
    failed_conditions <- apply(
        conditions[worker_failures, 1:2, drop = FALSE],
        1,
        paste,
        collapse = " / "
    )
    stop(
        "Parametric-bootstrap worker failure(s): ",
        paste(failed_conditions, collapse = "; "),
        ". The bootstrap result set is incomplete."
    )
}

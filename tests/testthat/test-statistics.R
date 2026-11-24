################################################################################
# R SQUARED
################################################################################

test_that(
    "Test known errors for rsquared",
    {
        # Setup of this test
        set.seed(3)
        true_model <- distributed_lag(
            parameters = list(
                "alpha" = 0,
                "beta" = as.matrix(1)
            ),
            covariance = as.matrix(1)
        )

        data <- simulate(true_model, X = rnorm(100))

        fitobj <- fit(
            distributed_lag(d = 1, k = 1),
            data,
            deterministic_fit = TRUE
        )

        # Data do not correspond to the fitted model
        other <- simulate(true_model, X = rnorm(50))
        expect_error(rsquared(fitobj, other))
    }
)

test_that(
    "Check output of rsquared for controlled residual variances",
    {
        set.seed(1)
        N <- 100000

        # Create the noise-free process of a distributed lag model
        true_model <- distributed_lag(
            lag = 2,
            parameters = list(
                "alpha" = c(1, -1),
                "beta" = cbind(
                    diag(2),
                    diag(2) * 0.5,
                    matrix(0.25, nrow = 2, ncol = 2)
                )
            ),
            covariance = matrix(0, nrow = 2, ncol = 2)
        )

        X <- matrix(rnorm(N), nrow = N / 2, ncol = 2)
        signal <- predict(
            true_model,
            dataset(X = X) |>
                suppressWarnings()
        )@Y

        # Compute the total sum of squares error of the signal in each dimension.
        # This will be used to scale the residuals to achieve a known R^2 in 
        # each dimension. Allowing us to test the efficacy of the function
        MSE_total <- matrixStats::colSds(signal)^2

        # Function to create a fit object and data for a given R^2 per
        # dimension. The residual variance is based on the variance of the
        # process without errors: For an R^2 of r, the residual sum of squares
        # should be (1 - r) / r times the signal sum of squares, i.e., 0, 1/3,
        # 1, and 3 times the signal variance for r = 1, 0.75, 0.5, and 0.25.
        create_case <- function(r2) {
            MSE_residual <- MSE_total * (1 - r2) / r2
            residuals <- rnorm(N, mean = 0, sd = 1) |>
                matrix(nrow = N / 2, ncol = 2, byrow = TRUE)

            # Ensure they are scaled with the correct variances
            residuals[, 1] <- residuals[, 1] * sqrt(MSE_residual[1])
            residuals[, 2] <- residuals[, 2] * sqrt(MSE_residual[2])

            list(
                fitobj = list(
                    "model" = true_model,
                    "residuals" = residuals
                ),
                data = dataset(
                    Y = signal + residuals,
                    X = X
                )
            )
        }

        # Increasing residual variance
        for(r2 in c(1, 0.75, 0.5, 0.25)) {
            tst <- create_case(r2)
            expect_equal(
                rsquared(tst$fitobj, tst$data), 
                r2,
                tolerance = 0.1
            )
            expect_equal(
                rsquared(tst$fitobj, tst$data, per_dimension = TRUE), 
                c(r2, r2),
                tolerance = 0.1
            )
        }

        # Same function, but now allowing for two different R^2 values for the
        # two dimensions. Ensures that we can adequately measure the R^2 per 
        # dimension
        create_case <- function(r2_1, r2_2) {
            MSE_residual <- MSE_total * (1 - c(r2_1, r2_2)) / c(r2_1, r2_2)
            residuals <- rnorm(N, mean = 0, sd = 1) |>
                matrix(nrow = N / 2, ncol = 2, byrow = TRUE)

            # Ensure they are scaled with the correct variances
            residuals[, 1] <- residuals[, 1] * sqrt(MSE_residual[1])
            residuals[, 2] <- residuals[, 2] * sqrt(MSE_residual[2])

            list(
                fitobj = list(
                    "model" = true_model,
                    "residuals" = residuals
                ),
                data = dataset(
                    Y = signal + residuals,
                    X = X
                )
            )
        }

        # Increasing residual variance
        for(r2_1 in c(1, 0.75, 0.5, 0.25)) {
            for(r2_2 in c(1, 0.75, 0.5, 0.25)) {
                tst <- create_case(r2_1, r2_2)
                expect_equal(
                    rsquared(tst$fitobj, tst$data, per_dimension = TRUE), 
                    c(r2_1, r2_2),
                    tolerance = 0.1
                )
            }
        }
    }
)

test_that(
    "Rsquared function is robust against NAs", 
    {
        set.seed(1)
        N <- 100000

        # Create the noise-free process of a distributed lag model
        true_model <- distributed_lag(
            lag = 2,
            parameters = list(
                "alpha" = c(1, -1),
                "beta" = cbind(
                    diag(2),
                    diag(2) * 0.5,
                    matrix(0.25, nrow = 2, ncol = 2)
                )
            ),
            covariance = matrix(0, nrow = 2, ncol = 2)
        )

        X <- matrix(rnorm(N), nrow = N / 2, ncol = 2)
        signal <- predict(
            true_model,
            dataset(X = X) |>
                suppressWarnings()
        )@Y

        # Compute the total sum of squares error of the signal in each dimension.
        # This will be used to scale the residuals to achieve a known R^2 in 
        # each dimension. Allowing us to test the efficacy of the function
        MSE_total <- matrixStats::colSds(signal)^2

        # Function to create a fit object and data for a given R^2 per
        # dimension. Now also imputes some missing values in both the residuals
        # and the data
        create_case <- function(r2) {
            MSE_residual <- MSE_total * (1 - r2) / r2
            residuals <- rnorm(N, mean = 0, sd = 1) |>
                matrix(nrow = N / 2, ncol = 2, byrow = TRUE)

            # Ensure they are scaled with the correct variances
            residuals[, 1] <- residuals[, 1] * sqrt(MSE_residual[1])
            residuals[, 2] <- residuals[, 2] * sqrt(MSE_residual[2])

            # Impute missing values
            residuals[seq(1, N / 2, by = 50), ] <- NA

            list(
                fitobj = list(
                    "model" = true_model,
                    "residuals" = residuals
                ),
                data = dataset(
                    Y = signal + residuals,
                    X = X
                )
            )
        }

        # Loop over several different values and compute the R^2
        for(r2 in c(1, 0.75, 0.5, 0.25)) {
            tst <- create_case(r2)
            expect_equal(
                rsquared(tst$fitobj, tst$data),
                r2,
                tolerance = 0.1
            )
            expect_equal(
                rsquared(tst$fitobj, tst$data, per_dimension = TRUE), 
                c(r2, r2), 
                tolerance = 0.1
            )
        }
    }
)

test_that(
    "External validation of rsquared: Distributed lag model against lm",
    {
        # Function to create the design matrix of a distributed lag model in
        # the same way as `predict` and `fit` do: Values before the start of
        # the data are 0, and the history is reset after a missing value
        design_matrix <- function(X, lag) {
            Z <- matrix(0, nrow = nrow(X), ncol = ncol(X) * (lag + 1))
            H <- matrix(0, nrow = ncol(X), ncol = lag + 1)

            for(i in seq_len(nrow(X))) {
                if(any(is.na(X[i, ]))) {
                    Z[i, ] <- NA
                    H[] <- 0
                    next
                }

                H <- cbind(X[i, ], H[, seq_len(lag), drop = FALSE])
                Z[i, ] <- as.numeric(H)
            }

            return(Z)
        }

        set.seed(1)
        for(lag in c(0, 1, 3)) {
            for(with_na in c(FALSE, TRUE)) {
                # Create the data-generating model and simulate data
                true_model <- distributed_lag(
                    lag = lag,
                    parameters = list(
                        "alpha" = c(0.5, -0.5),
                        "beta" = matrix(
                            runif(2 * 2 * (lag + 1), -1, 1),
                            nrow = 2
                        )
                    ),
                    covariance = matrix(c(1, 0.25, 0.25, 1), nrow = 2, ncol = 2)
                )

                X <- matrix(rnorm(400), nrow = 200, ncol = 2)
                if(with_na) {
                    X[c(20, 120), 1] <- NA
                }

                data <- simulate(true_model, X = X)

                # Fit the model analytically for speed purposes
                fitobj <- fit(
                    distributed_lag(d = 2, k = 2, lag = lag),
                    data,
                    deterministic_fit = TRUE
                )

                # Compute the R^2 based on this fitobj, per dimension
                tst <- rsquared(fitobj, data, per_dimension = TRUE)

                # Compare to the R^2 from lm on the same design
                Z <- design_matrix(X, lag)
                ref <- sapply(
                    1:2,
                    function(i) summary(lm(data@Y[, i] ~ Z))$r.squared
                )

                expect_equal(tst, ref)
                expect_equal(rsquared(fitobj, data), mean(ref))
            }
        }
    }
)

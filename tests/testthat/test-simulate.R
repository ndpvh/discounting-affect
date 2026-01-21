test_that(
    "Test known errors", 
    {
        my_model <- exponential(
            parameters = list(
                "alpha" = numeric(2),
                "beta" = matrix(0, nrow = 2, ncol = 2),
                "gamma" = matrix(0, nrow = 2, ncol = 2)
            ),
            covariance = diag(2)
        )

        # No input is provided
        expect_error(simulate(my_model))

        # Provided X's do not conform to the dimension
        expect_error(simulate(my_model, X = matrix(0, nrow = 10, ncol = 1)))

        # Provided X's are not the correct type
        expect_error(simulate(my_model, X = matrix("1", nrow = 10, ncol = 2)))
        expect_error(simulate(my_model, X = matrix(TRUE, nrow = 10, ncol = 2)))

        # Provided result of Xfun does not conform to the dimension
        expect_error(simulate(my_model, Xfun = rnorm, N = 10))

        # Xfun is provided, but no N
        expect_error(simulate(my_model, Xfun = rnorm))

        # Provided value to Xfun is not a list, nor a function
        expect_error(simulate(my_model, Xfun = numeric(10)))
        expect_error(simulate(my_model, Xfun = logical(10)))
        expect_error(simulate(my_model, Xfun = character(10)))

        # Provided value to Xfun can be a function, but needs to be sensical
        expect_error(simulate(my_model, Xfun = `[[`, N = 10))
        expect_error(simulate(my_model, Xfun = subset, N = 10))
    }
)

test_that(
    "Test known warnings",
    {
        my_model <- exponential(
            parameters = list(
                "alpha" = numeric(2),
                "beta" = matrix(0, nrow = 2, ncol = 2),
                "gamma" = matrix(0, nrow = 2, ncol = 2)
            ),
            covariance = diag(2)
        )
        
        # Provided X's do not conform to the dimension, but can be corrected
        expect_warning(
            simulate(
                my_model,
                X = matrix(1, nrow = 10, ncol = 3)
            )
        )

        tst <- simulate(
            my_model,
            X = matrix(1:30, nrow = 10, ncol = 3)
        ) |>
            suppressWarnings()
        expect_equal(tst@X, matrix(1:20, nrow = 10, ncol = 2))

        # Provided results of Xfun does not conform to the dimension, but can be
        # corrected
        expect_warning(
            simulate(
                my_model,
                Xfun = list(
                    \(x) matrix(1:x, nrow = x, ncol = 1),
                    \(x) matrix(1:x, nrow = x, ncol = 1),
                    \(x) matrix(1:x, nrow = x, ncol = 1)
                ),
                N = 10
            )
        )

        tst <- simulate(
            my_model,
            Xfun = list(
                \(x) matrix(1:x, nrow = x, ncol = 1),
                \(x) matrix(1:x, nrow = x, ncol = 1),
                \(x) matrix(1:x, nrow = x, ncol = 1)
            ),
            N = 10
        ) |>
            suppressWarnings()
        expect_equal(tst@X, matrix(1:10, nrow = 10, ncol = 2))
    }
)

test_that(
    "Test output for the predictor variables X",
    {
        my_model <- exponential(
            parameters = list(
                "alpha" = c(1, -1),
                "beta" = matrix(
                    c(1, 1, 0.5, 0.5),
                    nrow = 2, 
                    ncol = 2
                ),
                "gamma" = diag(2) * 0.75
            ),
            covariance = diag(2)
        )

        # Define X and Xfun
        X <- rnorm(200) |>
            matrix(nrow = 100, ncol = 2)
        Xfun <- \(x) rnorm(x * 2) |>
            matrix(ncol = 2)
        Xfun2 <- list(
            \(x) rnorm(x),
            \(x) rnorm(x)
        )
        
        # Values of X when provided
        tst <- simulate(
            my_model,
            X = X
        )
        expect_equal(tst@X, X)

        # Values of X when generated through Xfun
        set.seed(1)
        ref <- rnorm(200) |>
            matrix(nrow = 100, ncol = 2)

        set.seed(1)
        tst <- simulate(
            my_model,
            Xfun = Xfun,
            N = 100
        )

        expect_equal(tst@X, ref)

        # Values are X when both X and Xfun are provided
        tst <- simulate(
            my_model,
            X = X, 
            Xfun = Xfun, 
            N = 100
        )
        expect_equal(tst@X, X)

        # Values are X when Xfun is a list
        set.seed(1)
        ref <- lapply(
            Xfun2,
            \(fx) fx(100)
        )
        ref <- do.call("cbind", ref)
        
        set.seed(1)
        tst <- simulate(
            my_model, 
            Xfun = Xfun2,
            N = 100
        )

        expect_equal(tst@X, ref)
    }
)

test_that(
    "Test output for the dependent variables Y: Univariate case",
    {        
        # Define a list of models to test this for
        models <- list(
            exponential(
                parameters = list(
                    "alpha" = 1,
                    "beta" = as.matrix(2),
                    "gamma" = as.matrix(0.75)
                ),
                covariance = as.matrix(1)
            ),
            exponential(
                parameters = list(
                    "alpha" = 1,
                    "beta" = matrix(1:2, nrow = 1),
                    "gamma" = as.matrix(0.75)
                ),
                covariance = as.matrix(1)
            ),
            quasi_hyperbolic(
                parameters = list(
                    "alpha" = 1,
                    "beta" = as.matrix(2),
                    "nu" = as.matrix(0.75),
                    "kappa" = as.matrix(0.5)
                ),
                covariance = as.matrix(1)
            ),
            quasi_hyperbolic(
                parameters = list(
                    "alpha" = 1,
                    "beta" = matrix(1:2, nrow = 1),
                    "nu" = as.matrix(0.75),
                    "kappa" = as.matrix(0.5)
                ),
                covariance = as.matrix(1)
            ),
            double_exponential(
                parameters = list(
                    "alpha" = 1,
                    "beta" = as.matrix(2),
                    "gamma" = as.matrix(0.75),
                    "nu" = as.matrix(0.5),
                    "omega" = 0.25
                ),
                covariance = as.matrix(1)
            ),
            double_exponential(
                parameters = list(
                    "alpha" = 1,
                    "beta" = matrix(1:2, nrow = 1),
                    "gamma" = as.matrix(0.75),
                    "nu" = as.matrix(0.5),
                    "omega" = 0.25
                ),
                covariance = as.matrix(1)
            )
        )

        # Define the values of X to be used
        X_list <- list(
            rnorm(1000),
            cbind(
                rnorm(1000),
                rnorm(1000)
            )
        )

        # Loop over each of the models
        tst <- matrix(
            FALSE,
            nrow = length(models),
            ncol = 2
        )

        for(i in seq_along(models)) {
            # Define the value of X to use
            X <- X_list[[as.integer(models[[i]]@k)]]

            # Generate values for Y using the model
            y <- simulate(
                models[[i]],
                X = X
            )

            # Generate predictions for the same model
            y_hat <- predict(
                models[[i]],
                y
            )

            # Compute the residuals
            residuals <- y@Y - y_hat@Y

            # Check whether the properties of the residuals hold
            tst[i, 1] <- abs(mean(residuals)) < 10^(-1)
            tst[i, 2] <- abs(sd(residuals) - 1) < 10^(-1)
        }

        expect_true(all(tst))
    }
)

test_that(
    "Test output for the dependent variables Y: Bivariate case",
    {
        # Define a list of models to test this for
        covariance <- diag(2)
        covariance[c(2, 3)] <- 0.25

        models <- list(
            exponential(
                parameters = list(
                    "alpha" = c(1, -1),
                    "beta" = matrix(2, nrow = 2, ncol = 1),
                    "gamma" = diag(2) * 0.75
                ),
                covariance = covariance
            ),
            exponential(
                parameters = list(
                    "alpha" = c(1, -1),
                    "beta" = matrix(2:1, nrow = 2, ncol = 2, byrow = TRUE),
                    "gamma" = diag(2) * 0.75
                ),
                covariance = covariance
            ),
            quasi_hyperbolic(
                parameters = list(
                    "alpha" = c(1, -1),
                    "beta" = matrix(2, nrow = 2, ncol = 1),
                    "nu" = diag(2) * 0.75,
                    "kappa" = diag(2) * 0.5
                ),
                covariance = covariance
            ),
            quasi_hyperbolic(
                parameters = list(
                    "alpha" = c(1, -1),
                    "beta" = matrix(2:1, nrow = 2, ncol = 2, byrow = TRUE),
                    "nu" = diag(2) * 0.75,
                    "kappa" = diag(2) * 0.5
                ),
                covariance = covariance
            ),
            double_exponential(
                parameters = list(
                    "alpha" = c(1, -1),
                    "beta" = matrix(2, nrow = 2, ncol = 1),
                    "gamma" = diag(2) * 0.75,
                    "nu" = diag(2) * 0.5,
                    "omega" = 0.25
                ),
                covariance = covariance
            ),
            double_exponential(
                parameters = list(
                    "alpha" = c(1, -1),
                    "beta" = matrix(2:1, nrow = 2, ncol = 2, byrow = TRUE),
                    "gamma" = diag(2) * 0.75,
                    "nu" = diag(2) * 0.5,
                    "omega" = 0.25
                ),
                covariance = covariance
            )
        )

        # Define the values of X to be used
        X_list <- list(
            rnorm(2500),
            cbind(
                rnorm(2500),
                rnorm(2500)
            )
        )

        # Loop over each of the models
        tst <- matrix(
            FALSE,
            nrow = length(models),
            ncol = 2
        )

        for(i in seq_along(models)) {
            # Define the value of X to use
            X <- X_list[[as.integer(models[[i]]@k)]]

            # Generate values for Y using the model
            y <- simulate(
                models[[i]],
                X = X
            )

            # Generate predictions for the same model
            y_hat <- predict(
                models[[i]],
                y
            )

            # Compute the residuals
            residuals <- y@Y - y_hat@Y

            # Check whether the properties of the residuals hold
            tst[i, 1] <- abs(mean(residuals)) < 10^(-1)
            tst[i, 2] <- all(abs(cov(residuals) - covariance) < 10^(-1))
        }

        expect_true(all(tst))
    }
)

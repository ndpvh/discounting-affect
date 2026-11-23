test_that(
    "Check output of the objective function",
    {
        set.seed(1)
        ref <- MASS::mvrnorm(
            100, 
            mu = c(0, 0), 
            Sigma = matrix(c(1, 0.25, 0.25, 1), nrow = 2, ncol = 2)
        )^2 |>
            sum()
        X <- matrix(rnorm(200), nrow = 100, ncol = 2)
        
        # Exponential discounting model
        my_model <- exponential(
            parameters = list(
                "alpha" = c(1, -1),
                "beta" = matrix(2:1, nrow = 2, ncol = 2, byrow = TRUE),
                "gamma" = diag(2) * 0.75
            ),
            covariance = matrix(c(1, 0.25, 0.25, 1), nrow = 2, ncol = 2)
        )        

        set.seed(1)
        data <- simulate(
            my_model,
            X = X
        )

        tst <- objective_function(
            my_model,
            data,
            c(1, -1, 2, 2, 1, 1, 0.75, 0.75),
            dynamics = "isotropic"
        )

        expect_equal(tst, ref)
        
        # Quasi-hyperbolic discounting model
        my_model <- quasi_hyperbolic(
            parameters = list(
                "alpha" = c(1, -1),
                "beta" = matrix(2:1, nrow = 2, ncol = 2, byrow = TRUE),
                "nu" = diag(2) * 0.75,
                "kappa" = diag(2) * 0.5
            ),
            covariance = matrix(c(1, 0.25, 0.25, 1), nrow = 2, ncol = 2)
        )        

        set.seed(1)
        data <- simulate(
            my_model,
            X = X
        )

        tst <- objective_function(
            my_model,
            data,
            c(1, -1, 2, 2, 1, 1, 0.75, 0.75, 0.5, 0.5),
            dynamics = "isotropic"
        )

        expect_equal(tst, ref)

        
        # Double exponential discounting model
        my_model <- double_exponential(
            parameters = list(
                "alpha" = c(1, -1),
                "beta" = matrix(2:1, nrow = 2, ncol = 2, byrow = TRUE),
                "omega" = 0.25,
                "gamma" = diag(2) * 0.5,
                "nu" = diag(2) * 0.75
            ),
            covariance = matrix(c(1, 0.25, 0.25, 1), nrow = 2, ncol = 2)
        )        

        set.seed(1)
        data <- simulate(
            my_model,
            X = X
        )

        tst <- objective_function(
            my_model,
            data,
            c(1, -1, 2, 2, 1, 1, 0.25, 0.5, 0.5, 0.75, 0.75),
            dynamics = "isotropic"
        )

        expect_equal(tst, ref)

        
        # Distributed lag model without lags
        my_model <- distributed_lag(
            lag = 0,
            parameters = list(
                "alpha" = c(1, -1),
                "beta" = matrix(2:1, nrow = 2, ncol = 2, byrow = TRUE)
            ),
            covariance = matrix(c(1, 0.25, 0.25, 1), nrow = 2, ncol = 2)
        )        

        set.seed(1)
        data <- simulate(
            my_model,
            X = X
        )

        tst <- objective_function(
            my_model,
            data,
            c(1, -1, 2, 2, 1, 1)
        )

        expect_equal(tst, ref)

        # Distributed lag model with a single lag
        my_model <- distributed_lag(
            lag = 1,
            parameters = list(
                "alpha" = c(1, -1),
                "beta" = cbind(
                    matrix(2:1, nrow = 2, ncol = 2, byrow = TRUE),
                    diag(2) * 0.5
                )
            ),
            covariance = matrix(c(1, 0.25, 0.25, 1), nrow = 2, ncol = 2)
        )        

        set.seed(1)
        data <- simulate(
            my_model,
            X = X
        )

        params <- c(1, -1, 2, 2, 1, 1, 0.5, 0, 0, 0.5)
        tst <- objective_function(
            my_model,
            data,
            params
        )

        expect_equal(tst, ref)

        # The dynamics do not influence the result, as there are no dynamical
        # matrices in the model
        expect_equal(
            objective_function(my_model, data, params, dynamics = "anisotropic"),
            ref
        )

        # Parameters that deviate from the data-generating ones result in a 
        # larger sum of squared errors
        expect_gt(
            objective_function(my_model, data, c(1, -1, 2, 2, 1, 1, 0, 0, 0, 0)),
            ref
        )

        # Parameters outside of the bounds result in an infinite objective
        expect_equal(
            objective_function(my_model, data, c(1, -1, 2, 2, 1, 1, 0.5, 0, 0, 10)),
            Inf
        )
        expect_equal(
            objective_function(my_model, data, c(5, -1, 2, 2, 1, 1, 0.5, 0, 0, 0.5)),
            Inf
        )
    }
)

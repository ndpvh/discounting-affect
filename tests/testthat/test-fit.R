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
                "gamma" = diag(2) * 0.75,
                "nu" = diag(2) * 0.5
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
            c(1, -1, 2, 2, 1, 1, 0.25, 0.75, 0.75, 0.5, 0.5),
            dynamics = "isotropic"
        )

        expect_equal(tst, ref)
    }
)

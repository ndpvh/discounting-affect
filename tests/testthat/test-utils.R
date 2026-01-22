test_that(
    "Test known errors for count_parameters and count_covariance",
    {
        # Dynamics not defined
        expect_error(count_parameters(exponential(), dynamics = "test"))

        # Covariance not defined
        expect_error(count_parameters(exponential(), covariance = "test"))
        expect_error(count_covariance(2, covariance = "test"))
    }
)

test_that(
    "Check output for count_covariance",
    {
        expect_equal(
            count_covariance(1, covariance = "symmetric"),
            1
        )
        expect_equal(
            count_covariance(1, covariance = "isotropic"),
            1
        )
        expect_equal(
            count_covariance(2, covariance = "symmetric"),
            3
        )
        expect_equal(
            count_covariance(2, covariance = "isotropic"),
            2
        )
        expect_equal(
            count_covariance(3, covariance = "symmetric"),
            6
        )
        expect_equal(
            count_covariance(3, covariance = "isotropic"),
            3
        )
    }
)

test_that(
    "Check output of count_parameters: Exponential discounting", 
    {
        # Univariate, single predictor
        my_model <- exponential(
            parameters = list(
                "alpha" = 0,
                "beta" = as.matrix(0),
                "gamma" = as.matrix(0)
            ),
            covariance = as.matrix(0)
        )
        expect_equal(
            count_parameters(my_model, parameters_only = FALSE),
            4
        )
        expect_equal(
            count_parameters(my_model, parameters_only = TRUE),
            3
        )

        # Univariate, two predictors
        my_model <- exponential(
            parameters = list(
                "alpha" = 0,
                "beta" = matrix(0, nrow = 1, ncol = 2),
                "gamma" = as.matrix(0)
            ),
            covariance = as.matrix(0)
        )
        expect_equal(
            count_parameters(my_model, parameters_only = FALSE),
            5
        )
        expect_equal(
            count_parameters(my_model, parameters_only = TRUE),
            4
        )

        # Bivariate, single predictor
        my_model <- exponential(
            parameters = list(
                "alpha" = numeric(2),
                "beta" = matrix(0, nrow = 2, ncol = 1),
                "gamma" = diag(2) * 0.5
            ),
            covariance = diag(2)
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            11
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            10
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            8
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            10
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            9
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            7
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            9
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            8
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            6
        )

        # Bivariate, two predictors
        my_model <- exponential(
            parameters = list(
                "alpha" = numeric(2),
                "beta" = matrix(0, nrow = 2, ncol = 2),
                "gamma" = diag(2) * 0.5
            ),
            covariance = diag(2)
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            13
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            12
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            10
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            12
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            11
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            9
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            11
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            10
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            8
        )
    }
)

test_that(
    "Check output of count_parameters: Quasi-hyperbolic discounting", 
    {
        # Univariate, single predictor
        my_model <- quasi_hyperbolic(
            parameters = list(
                "alpha" = 0,
                "beta" = as.matrix(0),
                "nu" = as.matrix(0),
                "kappa" = as.matrix(0)
            ),
            covariance = as.matrix(0)
        )
        expect_equal(
            count_parameters(my_model, parameters_only = FALSE),
            5
        )
        expect_equal(
            count_parameters(my_model, parameters_only = TRUE),
            4
        )

        # Univariate, two predictors
        my_model <- quasi_hyperbolic(
            parameters = list(
                "alpha" = 0,
                "beta" = matrix(0, nrow = 1, ncol = 2),
                "nu" = as.matrix(0),
                "kappa" = as.matrix(0)
            ),
            covariance = as.matrix(0)
        )
        expect_equal(
            count_parameters(my_model, parameters_only = FALSE),
            6
        )
        expect_equal(
            count_parameters(my_model, parameters_only = TRUE),
            5
        )

        # Bivariate, single predictor
        my_model <- quasi_hyperbolic(
            parameters = list(
                "alpha" = numeric(2),
                "beta" = matrix(0, nrow = 2, ncol = 1),
                "nu" = diag(2) * 0.75,
                "kappa" = diag(2) * 0.5
            ),
            covariance = diag(2)
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            15
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            14
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            12
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            13
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            12
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            10
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            11
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            10
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            8
        )

        # Bivariate, two predictors
        my_model <- quasi_hyperbolic(
            parameters = list(
                "alpha" = numeric(2),
                "beta" = matrix(0, nrow = 2, ncol = 2),
                "nu" = diag(2) * 0.75,
                "kappa" = diag(2) * 0.5
            ),
            covariance = diag(2)
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            17
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            16
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            14
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            15
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            14
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            12
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            13
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            12
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            10
        )
    }
)

test_that(
    "Check output of count_parameters: Double-exponential discounting", 
    {
        # Univariate, single predictor
        my_model <- double_exponential(
            parameters = list(
                "alpha" = 0,
                "beta" = as.matrix(0),
                "nu" = as.matrix(0),
                "gamma" = as.matrix(0),
                "omega" = 0.5
            ),
            covariance = as.matrix(0)
        )
        expect_equal(
            count_parameters(my_model, parameters_only = FALSE),
            6
        )
        expect_equal(
            count_parameters(my_model, parameters_only = TRUE),
            5
        )

        # Univariate, two predictors
        my_model <- double_exponential(
            parameters = list(
                "alpha" = 0,
                "beta" = matrix(0, nrow = 1, ncol = 2),
                "nu" = as.matrix(0),
                "gamma" = as.matrix(0),
                "omega" = 0.5
            ),
            covariance = as.matrix(0)
        )
        expect_equal(
            count_parameters(my_model, parameters_only = FALSE),
            7
        )
        expect_equal(
            count_parameters(my_model, parameters_only = TRUE),
            6
        )

        # Bivariate, single predictor
        my_model <- double_exponential(
            parameters = list(
                "alpha" = numeric(2),
                "beta" = matrix(0, nrow = 2, ncol = 1),
                "nu" = diag(2) * 0.75,
                "gamma" = diag(2) * 0.5,
                "omega" = 0.5
            ),
            covariance = diag(2)
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            16
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            15
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            13
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            14
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            13
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            11
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            12
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            11
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            9
        )

        # Bivariate, two predictors
        my_model <- double_exponential(
            parameters = list(
                "alpha" = numeric(2),
                "beta" = matrix(0, nrow = 2, ncol = 2),
                "nu" = diag(2) * 0.75,
                "gamma" = diag(2) * 0.5,
                "omega" = 0.5
            ),
            covariance = diag(2)
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            18
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            17
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            15
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            16
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            15
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            13
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            14
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            13
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            11
        )
    }
)
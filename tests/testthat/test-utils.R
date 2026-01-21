test_that(
    "Test known errors for count_parameters",
    {
        # Dynamics not defined
        expect_error(count_parameters(exponential(), dynamics = "test"))

        # Covariance not defined
        expect_error(count_parameters(exponential(), covariance = "test"))
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
            count_parameters(my_model, count_covariance = TRUE),
            4
        )
        expect_equal(
            count_parameters(my_model, count_covariance = FALSE),
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
            count_parameters(my_model, count_covariance = TRUE),
            5
        )
        expect_equal(
            count_parameters(my_model, count_covariance = FALSE),
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
                count_covariance = TRUE
            ),
            11
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "isotropic",
                count_covariance = TRUE
            ),
            10
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                count_covariance = FALSE
            ),
            8
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "symmetric",
                count_covariance = TRUE
            ),
            10
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "isotropic",
                count_covariance = TRUE
            ),
            9
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                count_covariance = FALSE
            ),
            7
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "symmetric",
                count_covariance = TRUE
            ),
            9
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "isotropic",
                count_covariance = TRUE
            ),
            8
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                count_covariance = FALSE
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
                count_covariance = TRUE
            ),
            13
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                covariance = "isotropic",
                count_covariance = TRUE
            ),
            12
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "anisotropic",
                count_covariance = FALSE
            ),
            10
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "symmetric",
                count_covariance = TRUE
            ),
            12
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                covariance = "isotropic",
                count_covariance = TRUE
            ),
            11
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "symmetric",
                count_covariance = FALSE
            ),
            9
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "symmetric",
                count_covariance = TRUE
            ),
            11
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                covariance = "isotropic",
                count_covariance = TRUE
            ),
            10
        )
        expect_equal(
            count_parameters(
                my_model, 
                dynamics = "isotropic",
                count_covariance = FALSE
            ),
            8
        )
    }
)

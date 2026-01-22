################################################################################
# FILL

test_that(
    "Test known errors for fill",
    {
        # Not enough parameters provided for the covariance matrix
        expect_error(fill_covariance(2, 1:2, covariance = "symmetric"))
        expect_error(fill_covariance(2, 1, covariance = "isotropic"))
    }
)

test_that(
    "Test known warnings for fill",
    {
        # More than enough parameters provided for the covariance matrix
        expect_warning(fill_covariance(2, 1:4, covariance = "symmetric"))

        tst <- fill_covariance(2, 1:4, covariance = "symmetric") |>
            suppressWarnings()
        expect_equal(tst, matrix(c(1, 2, 2, 3), nrow = 2, ncol = 2))

        expect_warning(fill_covariance(2, 1:4, covariance = "isotropic")) 

        tst <- fill_covariance(2, 1:4, covariance = "isotropic") |>
            suppressWarnings()
        expect_equal(tst, diag(2) * 1:2)
    }
)

test_that(
    "Check output for fill: Covariance matrix",
    {
        tst <- fill_covariance(1, 1)
        expect_equal(tst, as.matrix(1))

        tst <- fill_covariance(2, 1:3, covariance = "symmetric")
        expect_equal(tst, matrix(c(1, 2, 2, 3), nrow = 2, ncol = 2))

        tst <- fill_covariance(2, 1:2, covariance = "isotropic")
        expect_equal(tst, matrix(c(1, 0, 0, 2), nrow = 2, ncol = 2))

        tst <- fill_covariance(3, 1:6, covariance = "symmetric")
        expect_equal(
            tst, 
            matrix(
                c(1, 2, 3, 2, 4, 5, 3, 5, 6), 
                nrow = 3, 
                ncol = 3
            )
        )

        tst <- fill_covariance(
            3, 
            1:3, 
            covariance = "isotropic"
        )
        expect_equal(
            tst, 
            diag(3) * 1:3
        )
    }
)

test_that(
    "Check output for fill: Exponential discounting, no covariances included",
    {
        # Single dimension and single predictor
        my_model <- exponential(d = 1, k = 1)
        tst <- fill(my_model, 1:3)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@parameters[["gamma"]], as.matrix(3))

        # Single dimension and two predictors
        my_model <- exponential(d = 1, k = 2)
        tst <- fill(my_model, 1:4)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@parameters[["gamma"]], as.matrix(4))

        # Two dimensions and single predictor
        my_model <- exponential(d = 2, k = 1)
        tst <- fill(my_model, 1:6, dynamics = "isotropic")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 5:6)

        tst <- fill(my_model, 1:7, dynamics = "symmetric")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(c(5, 6, 6, 7), nrow = 2, ncol = 2))

        tst <- fill(my_model, 1:8, dynamics = "anisotropic")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(5:8, nrow = 2, ncol = 2))

        # Two dimensions and two predictors
        my_model <- exponential(d = 2, k = 2)
        tst <- fill(my_model, 1:8, dynamics = "isotropic")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 7:8)

        tst <- fill(my_model, 1:9, dynamics = "symmetric")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(c(7, 8, 8, 9), nrow = 2, ncol = 2))

        tst <- fill(my_model, 1:10, dynamics = "anisotropic")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(7:10, nrow = 2, ncol = 2))
    }
)

test_that(
    "Check output for fill: Exponential discounting, covariances included",
    {
        # Single dimension and single predictor
        my_model <- exponential(d = 1, k = 1)
        tst <- fill(my_model, 1:4, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@parameters[["gamma"]], as.matrix(3))
        expect_equal(tst@covariance, as.matrix(4))

        # Single dimension and two predictors
        my_model <- exponential(d = 1, k = 2)
        tst <- fill(my_model, 1:5, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@parameters[["gamma"]], as.matrix(4))
        expect_equal(tst@covariance, as.matrix(5))

        # Two dimensions and single predictor
        my_model <- exponential(d = 2, k = 1)
        tst <- fill(
            my_model, 
            1:8, 
            dynamics = "isotropic", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 5:6)
        expect_equal(tst@covariance, diag(2) * 7:8)

        tst <- fill(
            my_model, 
            1:9, 
            dynamics = "isotropic", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 5:6)
        expect_equal(tst@covariance, matrix(c(7, 8, 8, 9), nrow = 2, ncol = 2))

        tst <- fill(
            my_model, 
            1:9, 
            dynamics = "symmetric", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(c(5, 6, 6, 7), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 8:9)

        tst <- fill(
            my_model, 
            1:10, 
            dynamics = "symmetric", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(c(5, 6, 6, 7), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(8, 9, 9, 10), nrow = 2, ncol = 2))

        tst <- fill(
            my_model, 
            1:10, 
            dynamics = "anisotropic", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 9:10)

        tst <- fill(
            my_model, 
            1:11, 
            dynamics = "anisotropic", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))

        # Two dimensions and two predictors
        my_model <- exponential(d = 2, k = 2)
        tst <- fill(
            my_model, 
            1:10, 
            dynamics = "isotropic", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 7:8)
        expect_equal(tst@covariance, diag(2) * 9:10)

        tst <- fill(
            my_model, 
            1:11, 
            dynamics = "isotropic", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 7:8)
        expect_equal(tst@covariance, matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))

        tst <- fill(
            my_model, 
            1:11, 
            dynamics = "symmetric", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(c(7, 8, 8, 9), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 10:11)

        tst <- fill(
            my_model, 
            1:12, 
            dynamics = "symmetric", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(c(7, 8, 8, 9), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(10, 11, 11, 12), nrow = 2, ncol = 2))

        tst <- fill(
            my_model, 
            1:12, 
            dynamics = "anisotropic", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 11:12)

        tst <- fill(
            my_model, 
            1:13, 
            dynamics = "anisotropic", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))
    }
)



################################################################################
# COUNT_PARAMETERS

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
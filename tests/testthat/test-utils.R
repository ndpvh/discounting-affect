################################################################################
# FILL

test_that(
    "Test known errors for fill",
    {
        # Not enough parameters provided for the covariance matrix
        expect_error(fill_covariance(2, 1:2, covariance = "symmetric"))
        expect_error(fill_covariance(2, 1, covariance = "isotropic"))

        # Not enough parameters provided for the exponential discounting model
        expect_error(fill(exponential(d = 2, k = 2), 1:2, dynamics = "anisotropic"))
        expect_error(fill(exponential(d = 2, k = 2), 1:2, dynamics = "symmetric"))
        expect_error(fill(exponential(d = 2, k = 2), 1:2, dynamics = "isotropic"))

        # Not enough parameters provided for the quasi-hyperbolic discounting model
        expect_error(fill(quasi_hyperbolic(d = 2, k = 2), 1:2, dynamics = "anisotropic"))
        expect_error(fill(quasi_hyperbolic(d = 2, k = 2), 1:2, dynamics = "symmetric"))
        expect_error(fill(quasi_hyperbolic(d = 2, k = 2), 1:2, dynamics = "isotropic"))

        # Not enough parameters provided for the double exponential discounting model
        expect_error(fill(double_exponential(d = 2, k = 2), 1:2, dynamics = "anisotropic"))
        expect_error(fill(double_exponential(d = 2, k = 2), 1:2, dynamics = "symmetric"))
        expect_error(fill(double_exponential(d = 2, k = 2), 1:2, dynamics = "isotropic"))
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

        # Too many parameters provided for the exponential discounting model
        expect_warning(fill(exponential(d = 2, k = 2), 1:20, dynamics = "anisotropic"))
        expect_warning(fill(exponential(d = 2, k = 2), 1:20, dynamics = "symmetric"))
        expect_warning(fill(exponential(d = 2, k = 2), 1:20, dynamics = "isotropic"))

        # Too many parameters provided for the quasi-hyperbolic discounting model
        expect_warning(fill(quasi_hyperbolic(d = 2, k = 2), 1:20, dynamics = "anisotropic"))
        expect_warning(fill(quasi_hyperbolic(d = 2, k = 2), 1:20, dynamics = "symmetric"))
        expect_warning(fill(quasi_hyperbolic(d = 2, k = 2), 1:20, dynamics = "isotropic"))

        # Too many parameters provided for the double exponential discounting model
        expect_warning(fill(double_exponential(d = 2, k = 2), 1:20, dynamics = "anisotropic"))
        expect_warning(fill(double_exponential(d = 2, k = 2), 1:20, dynamics = "symmetric"))
        expect_warning(fill(double_exponential(d = 2, k = 2), 1:20, dynamics = "isotropic"))
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

test_that(
    "Check output for fill: Quasi-hyperbolic discounting, no covariances included",
    {
        # Single dimension and single predictor
        my_model <- quasi_hyperbolic(d = 1, k = 1)
        tst <- fill(my_model, 1:4)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@parameters[["nu"]], as.matrix(3))
        expect_equal(tst@parameters[["kappa"]], as.matrix(4))

        # Single dimension and two predictors
        my_model <- quasi_hyperbolic(d = 1, k = 2)
        tst <- fill(my_model, 1:5)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@parameters[["nu"]], as.matrix(4))
        expect_equal(tst@parameters[["kappa"]], as.matrix(5))

        # Two dimensions and single predictor
        my_model <- quasi_hyperbolic(d = 2, k = 1)
        tst <- fill(my_model, 1:8, dynamics = "isotropic")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], diag(2) * 5:6)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 7:8)

        tst <- fill(my_model, 1:10, dynamics = "symmetric")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(c(5, 6, 6, 7), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(8, 9, 9, 10), nrow = 2, ncol = 2))

        tst <- fill(my_model, 1:12, dynamics = "anisotropic")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(9:12, nrow = 2, ncol = 2))

        # Two dimensions and two predictors
        my_model <- quasi_hyperbolic(d = 2, k = 2)
        tst <- fill(my_model, 1:10, dynamics = "isotropic")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], diag(2) * 7:8)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 9:10)

        tst <- fill(my_model, 1:12, dynamics = "symmetric")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(7, 8, 8, 9), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(10, 11, 11, 12), nrow = 2, ncol = 2))

        tst <- fill(my_model, 1:14, dynamics = "anisotropic")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(11:14, nrow = 2, ncol = 2))
    }
)

test_that(
    "Check output for fill: Quasi-hyperbolic discounting, covariances included",
    {
        # Single dimension and single predictor
        my_model <- quasi_hyperbolic(d = 1, k = 1)
        tst <- fill(my_model, 1:5, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@parameters[["nu"]], as.matrix(3))
        expect_equal(tst@parameters[["kappa"]], as.matrix(4))
        expect_equal(tst@covariance, as.matrix(5))

        # Single dimension and two predictors
        my_model <- quasi_hyperbolic(d = 1, k = 2)
        tst <- fill(my_model, 1:6, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@parameters[["nu"]], as.matrix(4))
        expect_equal(tst@parameters[["kappa"]], as.matrix(5))
        expect_equal(tst@covariance, as.matrix(6))

        # Two dimensions and single predictor
        my_model <- quasi_hyperbolic(d = 2, k = 1)
        tst <- fill(
            my_model, 
            1:10, 
            dynamics = "isotropic", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], diag(2) * 5:6)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 7:8)
        expect_equal(tst@covariance, diag(2) * 9:10)

        tst <- fill(
            my_model, 
            1:11, 
            dynamics = "isotropic", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], diag(2) * 5:6)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 7:8)
        expect_equal(tst@covariance, matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))

        tst <- fill(
            my_model, 
            1:12, 
            dynamics = "symmetric", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(c(5, 6, 6, 7), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(8, 9, 9, 10), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 11:12)

        tst <- fill(
            my_model, 
            1:13, 
            dynamics = "symmetric", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(c(5, 6, 6, 7), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(8, 9, 9, 10), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))

        tst <- fill(
            my_model, 
            1:14, 
            dynamics = "anisotropic", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(9:12, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 13:14)

        tst <- fill(
            my_model, 
            1:15, 
            dynamics = "anisotropic", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(9:12, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(13, 14, 14, 15), nrow = 2, ncol = 2))

        # Two dimensions and two predictors
        my_model <- quasi_hyperbolic(d = 2, k = 2)
        tst <- fill(
            my_model, 
            1:12, 
            dynamics = "isotropic", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], diag(2) * 7:8)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 9:10)
        expect_equal(tst@covariance, diag(2) * 11:12)

        tst <- fill(
            my_model, 
            1:13, 
            dynamics = "isotropic", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], diag(2) * 7:8)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 9:10)
        expect_equal(tst@covariance, matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))

        tst <- fill(
            my_model, 
            1:14, 
            dynamics = "symmetric", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(7, 8, 8, 9), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(10, 11, 11, 12), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 13:14)

        tst <- fill(
            my_model, 
            1:15, 
            dynamics = "symmetric", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(7, 8, 8, 9), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(10, 11, 11, 12), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(13, 14, 14, 15), nrow = 2, ncol = 2))

        tst <- fill(
            my_model, 
            1:16, 
            dynamics = "anisotropic", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(11:14, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 15:16)

        tst <- fill(
            my_model, 
            1:17, 
            dynamics = "anisotropic", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(11:14, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(15, 16, 16, 17), nrow = 2, ncol = 2))
    }
)

test_that(
    "Check output for fill: Double exponential discounting, no covariances included",
    {
        # Single dimension and single predictor
        my_model <- double_exponential(d = 1, k = 1)
        tst <- fill(my_model, 1:5)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@parameters[["omega"]], 3)
        expect_equal(tst@parameters[["gamma"]], as.matrix(4))
        expect_equal(tst@parameters[["nu"]], as.matrix(5))

        # Single dimension and two predictors
        my_model <- double_exponential(d = 1, k = 2)
        tst <- fill(my_model, 1:6)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@parameters[["omega"]], 4)
        expect_equal(tst@parameters[["gamma"]], as.matrix(5))
        expect_equal(tst@parameters[["nu"]], as.matrix(6))

        # Two dimensions and single predictor
        my_model <- double_exponential(d = 2, k = 1)
        tst <- fill(my_model, 1:9, dynamics = "isotropic")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 6:7)
        expect_equal(tst@parameters[["nu"]], diag(2) * 8:9)

        tst <- fill(my_model, 1:11, dynamics = "symmetric")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(c(6, 7, 7, 8), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))

        tst <- fill(my_model, 1:13, dynamics = "anisotropic")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(6:9, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(10:13, nrow = 2, ncol = 2))

        # Two dimensions and two predictors
        my_model <- double_exponential(d = 2, k = 2)
        tst <- fill(my_model, 1:11, dynamics = "isotropic")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 8:9)
        expect_equal(tst@parameters[["nu"]], diag(2) * 10:11)

        tst <- fill(my_model, 1:13, dynamics = "symmetric")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(c(8, 9, 9, 10), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))

        tst <- fill(my_model, 1:15, dynamics = "anisotropic")

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(8:11, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(12:15, nrow = 2, ncol = 2))
    }
)

test_that(
    "Check output for fill: Double exponential discounting, covariances included",
    {
        # Single dimension and single predictor
        my_model <- double_exponential(d = 1, k = 1)
        tst <- fill(my_model, 1:6, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@parameters[["omega"]], 3)
        expect_equal(tst@parameters[["gamma"]], as.matrix(4))
        expect_equal(tst@parameters[["nu"]], as.matrix(5))
        expect_equal(tst@covariance, as.matrix(6))

        # Single dimension and two predictors
        my_model <- double_exponential(d = 1, k = 2)
        tst <- fill(my_model, 1:7, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@parameters[["omega"]], 4)
        expect_equal(tst@parameters[["gamma"]], as.matrix(5))
        expect_equal(tst@parameters[["nu"]], as.matrix(6))
        expect_equal(tst@covariance, as.matrix(7))

        # Two dimensions and single predictor
        my_model <- double_exponential(d = 2, k = 1)
        tst <- fill(
            my_model, 
            1:11, 
            dynamics = "isotropic", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 6:7)
        expect_equal(tst@parameters[["nu"]], diag(2) * 8:9)
        expect_equal(tst@covariance, diag(2) * 10:11)

        tst <- fill(
            my_model, 
            1:12, 
            dynamics = "isotropic", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 6:7)
        expect_equal(tst@parameters[["nu"]], diag(2) * 8:9)
        expect_equal(tst@covariance, matrix(c(10, 11, 11, 12), nrow = 2, ncol = 2))

        tst <- fill(
            my_model, 
            1:13, 
            dynamics = "symmetric", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(c(6, 7, 7, 8), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 12:13)

        tst <- fill(
            my_model, 
            1:14, 
            dynamics = "symmetric", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(c(6, 7, 7, 8), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(12, 13, 13, 14), nrow = 2, ncol = 2))

        tst <- fill(
            my_model, 
            1:15, 
            dynamics = "anisotropic", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(6:9, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(10:13, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 14:15)

        tst <- fill(
            my_model, 
            1:16, 
            dynamics = "anisotropic", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(6:9, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(10:13, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(14, 15, 15, 16), nrow = 2, ncol = 2))

        # Two dimensions and two predictors
        my_model <- double_exponential(d = 2, k = 2)
        tst <- fill(
            my_model, 
            1:13, 
            dynamics = "isotropic", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 8:9)
        expect_equal(tst@parameters[["nu"]], diag(2) * 10:11)
        expect_equal(tst@covariance, diag(2) * 12:13)

        tst <- fill(
            my_model, 
            1:14, 
            dynamics = "isotropic", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 8:9)
        expect_equal(tst@parameters[["nu"]], diag(2) * 10:11)
        expect_equal(tst@covariance, matrix(c(12, 13, 13, 14), nrow = 2, ncol = 2))

        tst <- fill(
            my_model, 
            1:15, 
            dynamics = "symmetric", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(c(8, 9, 9, 10), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 14:15)

        tst <- fill(
            my_model, 
            1:16, 
            dynamics = "symmetric", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(c(8, 9, 9, 10), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(14, 15, 15, 16), nrow = 2, ncol = 2))

        tst <- fill(
            my_model, 
            1:17, 
            dynamics = "anisotropic", 
            covariance = "isotropic",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(8:11, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(12:15, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 16:17)

        tst <- fill(
            my_model, 
            1:18, 
            dynamics = "anisotropic", 
            covariance = "symmetric",
            parameters_only = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(8:11, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(12:15, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(16, 17, 17, 18), nrow = 2, ncol = 2))
    }
)



################################################################################
# COUNT_PARAMETERS

test_that(
    "Test known errors for count_parameters and count_covariance",
    {
        # Dynamics not defined
        expect_error(count_parameters(exponential(), dynamics = "test"))
        expect_error(count_parameters(quasi_hyperbolic(), dynamics = "test"))
        expect_error(count_parameters(double_exponential(), dynamics = "test"))

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



################################################################################
# GET_BOUNDS

test_that(
    "Test known errors for get_bounds and get_bounds_covariance",
    {
        # Dynamics not defined
        expect_error(get_bounds(exponential(), dynamics = "test"))
        expect_error(get_bounds(quasi_hyperbolic(), dynamics = "test"))
        expect_error(get_bounds(double_exponential(), dynamics = "test"))

        # Covariance not defined
        expect_error(get_bounds(exponential(), covariance = "test", parameters_only = FALSE))
        expect_error(get_bounds_covariance(2, covariance = "test"))

        # Too few parameters
        expect_error(
            get_bounds(
                exponential(),
                lower = 1:2, 
                upper = 1:2,
                dynamics = "anisotropic"
            )
        )
        expect_error(
            get_bounds(
                exponential(),
                lower = 1:2, 
                upper = 1:2,
                parameters_only = FALSE
            )
        )
        expect_error(
            get_bounds(
                quasi_hyperbolic(),
                lower = 1:2, 
                upper = 1:2,
                dynamics = "anisotropic"
            )
        )
        expect_error(
            get_bounds(
                quasi_hyperbolic(),
                lower = 1:2, 
                upper = 1:2,
                parameters_only = FALSE
            )
        )
        expect_error(
            get_bounds(
                double_exponential(),
                lower = 1:2, 
                upper = 1:2,
                dynamics = "anisotropic"
            )
        )
        expect_error(
            get_bounds(
                double_exponential(),
                lower = 1:2, 
                upper = 1:2,
                parameters_only = FALSE
            )
        )
    }
)

test_that(
    "Test known warnings for get_bounds and get_bounds_covariance",
    {

    }
)

test_that(
    "Check the output of get_bounds_covariance",
    {
        expect_equal(
            get_bounds_covariance(1, 0, 1),
            list(0, 1)
        )

        expect_equal(
            get_bounds_covariance(2, 0, 1, covariance = "symmetric"),
            list(rep(0, 3), rep(1, 3))
        )
        expect_equal(
            get_bounds_covariance(2, 0, 1, covariance = "isotropic"),
            list(rep(0, 2), rep(1, 2))
        )

        expect_equal(
            get_bounds_covariance(3, 0, 1, covariance = "symmetric"),
            list(rep(0, 6), rep(1, 6))
        )
        expect_equal(
            get_bounds_covariance(3, 0, 1, covariance = "isotropic"),
            list(rep(0, 3), rep(1, 3))
        )
    }
)

test_that(
    "Check the output of get_bounds: Exponential discounting",
    {
        # One dimension, one predictor
        expect_equal(
            get_bounds(
                exponential(d = 1, k = 1),
                lower = -(1:3),
                upper = 1:3,
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -2, -3),
                "upper" = c(1, 2, 3)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 1, k = 1),
                lower = -(1:4),
                upper = 1:4,
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -2, -3, -4),
                "upper" = c(1, 2, 3, 4)
            )
        )

        # One dimension, two predictors
        expect_equal(
            get_bounds(
                exponential(d = 1, k = 2),
                lower = -(1:3),
                upper = 1:3,
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -2, -2, -3),
                "upper" = c(1, 2, 2, 3)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 1, k = 2),
                lower = -(1:4),
                upper = 1:4,
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -2, -2, -3, -4),
                "upper" = c(1, 2, 2, 3, 4)
            )
        )

        # Two dimension, one predictor
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 1),
                lower = -(1:3),
                upper = 1:3,
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -3, -3),
                "upper" = c(1, 1, 2, 2, 3, 3)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 1),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -3, -3, -4, -4),
                "upper" = c(1, 1, 2, 2, 3, 3, 4, 4)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 1),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -3, -3, -4, -4, -4),
                "upper" = c(1, 1, 2, 2, 3, 3, 4, 4, 4)
            )
        )

        expect_equal(
            get_bounds(
                exponential(d = 2, k = 1),
                lower = -(1:3),
                upper = 1:3,
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -3, -3, -3),
                "upper" = c(1, 1, 2, 2, 3, 3, 3)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 1),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -3, -3, -3, -4, -4),
                "upper" = c(1, 1, 2, 2, 3, 3, 3, 4, 4)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 1),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -3, -3, -3, -4, -4, -4),
                "upper" = c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4)
            )
        )

        expect_equal(
            get_bounds(
                exponential(d = 2, k = 1),
                lower = -(1:3),
                upper = 1:3,
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -3, -3, -3, -3),
                "upper" = c(1, 1, 2, 2, 3, 3, 3, 3)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 1),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -3, -3, -3, -3, -4, -4),
                "upper" = c(1, 1, 2, 2, 3, 3, 3, 3, 4, 4)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 1),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -3, -3, -3, -3, -4, -4, -4),
                "upper" = c(1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4)
            )
        )

        # Two dimension, two predictors
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 2),
                lower = -(1:3),
                upper = 1:3,
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -2, -2, -3, -3),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 2),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -2, -2, -3, -3, -4, -4),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 4, 4)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 2),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -2, -2, -3, -3, -4, -4, -4),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 4)
            )
        )

        expect_equal(
            get_bounds(
                exponential(d = 2, k = 2),
                lower = -(1:3),
                upper = 1:3,
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -2, -2, -3, -3, -3),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 2),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -2, -2, -3, -3, -3, -4, -4),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 2),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -2, -2, -3, -3, -3, -4, -4, -4),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4)
            )
        )

        expect_equal(
            get_bounds(
                exponential(d = 2, k = 2),
                lower = -(1:3),
                upper = 1:3,
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -2, -2, -3, -3, -3, -3),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 2),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -2, -2, -3, -3, -3, -3, -4, -4),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4)
            )
        )
        expect_equal(
            get_bounds(
                exponential(d = 2, k = 2),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -2, -2, -3, -3, -3, -3, -4, -4, -4),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4)
            )
        )
    }
)

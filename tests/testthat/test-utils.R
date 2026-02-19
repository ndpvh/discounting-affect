################################################################################
# INDEX

test_that(
    "Check output for index: Covariance matrix", 
    {
        # Symmetric matrices, Cholesky is TRUE
        tst <- index_covariance(
            3, 
            5, 
            covariance = "symmetric",
            cholesky = TRUE
        )
        ref <- matrix(0, nrow = 3, ncol = 3)
        ref[lower.tri(ref, diag = TRUE)] <- 5:10

        expect_equal(
            tst, 
            ref
        )

        # Symmetric matrices, Cholesky is FALSE
        tst <- index_covariance(
            3, 
            5, 
            covariance = "symmetric",
            cholesky = FALSE
        )
        ref <- matrix(0, nrow = 3, ncol = 3)
        ref[lower.tri(ref, diag = TRUE)] <- 5:10
        ref[upper.tri(ref, diag = FALSE)] <- t(ref[lower.tri(ref, diag = FALSE)])

        expect_equal(
            tst, 
            ref
        )

        # Isotropic matrices, Cholesky is TRUE
        tst <- index_covariance(
            3, 
            5, 
            covariance = "isotropic",
            cholesky = TRUE
        )
        ref <- diag(5:7)

        expect_equal(
            tst, 
            ref
        )

        # Symmetric matrices, Cholesky is FALSE
        tst <- index_covariance(
            3, 
            5, 
            covariance = "isotropic",
            cholesky = FALSE
        )
        ref <- diag(5:7)

        expect_equal(
            tst, 
            ref
        )
    }
)

test_that(
    "Check output for index: Exponential discounting, no covariances included",
    {
        ########################################################################
        # Single dimension and single predictor
        my_model <- exponential(d = 1, k = 1)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@parameters[["gamma"]], as.matrix(3))
        expect_equal(tst@covariance, as.matrix(0))


        ########################################################################
        # Single dimension and two predictors
        my_model <- exponential(d = 1, k = 2)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@parameters[["gamma"]], as.matrix(4))
        expect_equal(tst@covariance, as.matrix(0))


        ########################################################################
        # Two dimensions and single predictor

        my_model <- exponential(d = 2, k = 1)

        # Isotropic, full = TRUE
        tst <- index(my_model, dynamics = "isotropic", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 5:6)
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Isotropic, full = FALSE
        tst <- index(my_model, dynamics = "isotropic", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 5:6)
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Symmetric, full = TRUE
        tst <- index(my_model, dynamics = "symmetric", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(c(5, 6, 6, 7), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Symmetric, full = FALSE
        tst <- index(my_model, dynamics = "symmetric", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(c(5, 6, 0, 7), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Anisotropic, full = TRUE
        tst <- index(my_model, dynamics = "anisotropic", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Anisotropic, full = FALSE
        tst <- index(my_model, dynamics = "anisotropic", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))


        ########################################################################
        # Two dimensions and two predictors

        my_model <- exponential(d = 2, k = 2)

        # Isotropic, full = TRUE
        tst <- index(my_model, dynamics = "isotropic", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 7:8)
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Isotropic, full = FALSE
        tst <- index(my_model, dynamics = "isotropic", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 7:8)
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Symmetric, full = TRUE
        tst <- index(my_model, dynamics = "symmetric", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(c(7, 8, 8, 9), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Symmetric, full = FALSE
        tst <- index(my_model, dynamics = "symmetric", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(c(7, 8, 0, 9), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Anisotropic, full = TRUE
        tst <- index(my_model, dynamics = "anisotropic", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Anisotropic, full = FALSE
        tst <- index(my_model, dynamics = "anisotropic", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))
    }
)

test_that(
    "Check output for index: Exponential discounting, covariances included",
    {
        ########################################################################
        # Single dimension and single predictor
        my_model <- exponential(d = 1, k = 1)
        tst <- index(my_model, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@parameters[["gamma"]], as.matrix(3))
        expect_equal(tst@covariance, as.matrix(4))


        ########################################################################
        # Single dimension and two predictors
        my_model <- exponential(d = 1, k = 2)
        tst <- index(my_model, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@parameters[["gamma"]], as.matrix(4))
        expect_equal(tst@covariance, as.matrix(5))


        ########################################################################
        # Two dimensions and single predictor

        my_model <- exponential(d = 2, k = 1)

        # Isotropic, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "isotropic", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 5:6)
        expect_equal(tst@covariance, diag(2) * 7:8)

        # Isotropic, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "isotropic", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 5:6)
        expect_equal(tst@covariance, diag(2) * 7:8)

        # Symmetric, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "symmetric", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(c(5, 6, 6, 7), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 8:9)

        # Symmetric, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "symmetric", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(c(5, 6, 0, 7), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 8:9)

        # Anisotropic, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "anisotropic", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 9:10)

        # Anisotropic, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "anisotropic", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 9:10)


        ########################################################################
        # Two dimensions and two predictors

        my_model <- exponential(d = 2, k = 2)

        # Isotropic, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "isotropic", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 7:8)
        expect_equal(tst@covariance, diag(2) * 9:10)

        # Isotropic, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "isotropic", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 7:8)
        expect_equal(tst@covariance, diag(2) * 9:10)

        # Symmetric, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "symmetric", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(c(7, 8, 8, 9), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 10:11)

        # Symmetric, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "symmetric", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(c(7, 8, 0, 9), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 10:11)

        # Anisotropic, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "anisotropic", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 11:12)

        # Anisotropic, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "anisotropic", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 11:12)
    }
)

test_that(
    "Check output for index: Quasi-hyperbolic discounting, no covariances included",
    {
        ########################################################################
        # Single dimension and single predictor
        my_model <- quasi_hyperbolic(d = 1, k = 1)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@parameters[["nu"]], as.matrix(3))
        expect_equal(tst@parameters[["kappa"]], as.matrix(4))
        expect_equal(tst@covariance, as.matrix(0))


        ########################################################################
        # Single dimension and two predictors
        my_model <- quasi_hyperbolic(d = 1, k = 2)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@parameters[["nu"]], as.matrix(4))
        expect_equal(tst@parameters[["kappa"]], as.matrix(5))
        expect_equal(tst@covariance, as.matrix(0))


        ########################################################################
        # Two dimensions and single predictor

        my_model <- quasi_hyperbolic(d = 2, k = 1)

        # Isotropic, full = TRUE
        tst <- index(my_model, dynamics = "isotropic", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], diag(2) * 5:6)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 7:8)
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Isotropic, full = FALSE
        tst <- index(my_model, dynamics = "isotropic", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], diag(2) * 5:6)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 7:8)
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Symmetric, full = TRUE
        tst <- index(my_model, dynamics = "symmetric", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(c(5, 6, 6, 7), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(8, 9, 9, 10), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Symmetric, full = FALSE
        tst <- index(my_model, dynamics = "symmetric", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(c(5, 6, 0, 7), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(8, 9, 0, 10), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Anisotropic, full = TRUE
        tst <- index(my_model, dynamics = "anisotropic", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(9:12, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Anisotropic, full = FALSE
        tst <- index(my_model, dynamics = "anisotropic", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(9:12, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))


        ########################################################################
        # Two dimensions and two predictors

        my_model <- quasi_hyperbolic(d = 2, k = 2)

        # Isotropic, full = TRUE
        tst <- index(my_model, dynamics = "isotropic", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], diag(2) * 7:8)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 9:10)
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Isotropic, full = FALSE
        tst <- index(my_model, dynamics = "isotropic", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], diag(2) * 7:8)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 9:10)
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Symmetric, full = TRUE
        tst <- index(my_model, dynamics = "symmetric", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(7, 8, 8, 9), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(10, 11, 11, 12), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Symmetric, full = FALSE
        tst <- index(my_model, dynamics = "symmetric", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(7, 8, 0, 9), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(10, 11, 0, 12), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Anisotropic, full = TRUE
        tst <- index(my_model, dynamics = "anisotropic", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(11:14, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Anisotropic, full = FALSE
        tst <- index(my_model, dynamics = "anisotropic", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(11:14, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))
    }
)

test_that(
    "Check output for index: Quasi-hyperbolic discounting, covariances included",
    {
        ########################################################################
        # Single dimension and single predictor
        my_model <- quasi_hyperbolic(d = 1, k = 1)
        tst <- index(my_model, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@parameters[["nu"]], as.matrix(3))
        expect_equal(tst@parameters[["kappa"]], as.matrix(4))
        expect_equal(tst@covariance, as.matrix(5))


        ########################################################################
        # Single dimension and two predictors
        my_model <- quasi_hyperbolic(d = 1, k = 2)
        tst <- index(my_model, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@parameters[["nu"]], as.matrix(4))
        expect_equal(tst@parameters[["kappa"]], as.matrix(5))
        expect_equal(tst@covariance, as.matrix(6))


        ########################################################################
        # Two dimensions and single predictor

        my_model <- quasi_hyperbolic(d = 2, k = 1)

        # Isotropic, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "isotropic", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], diag(2) * 5:6)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 7:8)
        expect_equal(tst@covariance, diag(2) * 9:10)

        # Isotropic, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "isotropic", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], diag(2) * 5:6)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 7:8)
        expect_equal(tst@covariance, diag(2) * 9:10)

        # Symmetric, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "symmetric", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(c(5, 6, 6, 7), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(8, 9, 9, 10), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 11:12)

        # Symmetric, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "symmetric", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(c(5, 6, 0, 7), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(8, 9, 0, 10), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 11:12)

        # Anisotropic, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "anisotropic", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(9:12, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 13:14)

        # Anisotropic, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "anisotropic", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(5:8, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(9:12, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 13:14)


        ########################################################################
        # Two dimensions and two predictors

        my_model <- quasi_hyperbolic(d = 2, k = 2)

        # Isotropic, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "isotropic", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], diag(2) * 7:8)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 9:10)
        expect_equal(tst@covariance, diag(2) * 11:12)

        # Isotropic, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "isotropic", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], diag(2) * 7:8)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 9:10)
        expect_equal(tst@covariance, diag(2) * 11:12)

        # Symmetric, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "symmetric", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(7, 8, 8, 9), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(10, 11, 11, 12), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 13:14)

        # Symmetric, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "symmetric", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(7, 8, 0, 9), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(10, 11, 0, 12), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 13:14)

        # Anisotropic, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "anisotropic", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(11:14, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 15:16)

        # Anisotropic, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "anisotropic", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(11:14, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 15:16)
    }
)

test_that(
    "Check output for index: Double exponential discounting, no covariances included",
    {
        ########################################################################
        # Single dimension and single predictor
        my_model <- double_exponential(d = 1, k = 1)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@parameters[["omega"]], 3)
        expect_equal(tst@parameters[["gamma"]], as.matrix(4))
        expect_equal(tst@parameters[["nu"]], as.matrix(5))
        expect_equal(tst@covariance, as.matrix(0))


        ########################################################################
        # Single dimension and two predictors
        my_model <- double_exponential(d = 1, k = 2)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@parameters[["omega"]], 4)
        expect_equal(tst@parameters[["gamma"]], as.matrix(5))
        expect_equal(tst@parameters[["nu"]], as.matrix(6))
        expect_equal(tst@covariance, as.matrix(0))


        ########################################################################
        # Two dimensions and single predictor

        my_model <- double_exponential(d = 2, k = 1)

        # Isotropic, full = TRUE
        tst <- index(my_model, dynamics = "isotropic", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 6:7)
        expect_equal(tst@parameters[["nu"]], diag(2) * 8:9)
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Isotropic, full = FALSE
        tst <- index(my_model, dynamics = "isotropic", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 6:7)
        expect_equal(tst@parameters[["nu"]], diag(2) * 8:9)
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Symmetric, full = TRUE
        tst <- index(my_model, dynamics = "symmetric", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(c(6, 7, 7, 8), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Symmetric, full = FALSE
        tst <- index(my_model, dynamics = "symmetric", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(c(6, 7, 0, 8), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(9, 10, 0, 11), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Anisotropic, full = TRUE
        tst <- index(my_model, dynamics = "anisotropic", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(6:9, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(10:13, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Anisotropic, full = FALSE
        tst <- index(my_model, dynamics = "anisotropic", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(6:9, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(10:13, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))


        ########################################################################
        # Two dimensions and two predictors

        my_model <- double_exponential(d = 2, k = 2)

        # Isotropic, full = TRUE
        tst <- index(my_model, dynamics = "isotropic", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 8:9)
        expect_equal(tst@parameters[["nu"]], diag(2) * 10:11)
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Isotropic, full = FALSE
        tst <- index(my_model, dynamics = "isotropic", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 8:9)
        expect_equal(tst@parameters[["nu"]], diag(2) * 10:11)
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Symmetric, full = TRUE
        tst <- index(my_model, dynamics = "symmetric", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(c(8, 9, 9, 10), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Symmetric, full = FALSE
        tst <- index(my_model, dynamics = "symmetric", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(c(8, 9, 0, 10), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(11, 12, 0, 13), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Anisotropic, full = TRUE
        tst <- index(my_model, dynamics = "anisotropic", full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(8:11, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(12:15, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Anisotropic, full = FALSE
        tst <- index(my_model, dynamics = "anisotropic", full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(8:11, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(12:15, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))
    }
)

test_that(
    "Check output for index: Double exponential discounting, covariances included",
    {
        ########################################################################
        # Single dimension and single predictor
        my_model <- double_exponential(d = 1, k = 1)
        tst <- index(my_model, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@parameters[["omega"]], 3)
        expect_equal(tst@parameters[["gamma"]], as.matrix(4))
        expect_equal(tst@parameters[["nu"]], as.matrix(5))
        expect_equal(tst@covariance, as.matrix(6))


        ########################################################################
        # Single dimension and two predictors
        my_model <- double_exponential(d = 1, k = 2)
        tst <- index(my_model, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@parameters[["omega"]], 4)
        expect_equal(tst@parameters[["gamma"]], as.matrix(5))
        expect_equal(tst@parameters[["nu"]], as.matrix(6))
        expect_equal(tst@covariance, as.matrix(7))


        ########################################################################
        # Two dimensions and single predictor

        my_model <- double_exponential(d = 2, k = 1)

        # Isotropic, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "isotropic", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 6:7)
        expect_equal(tst@parameters[["nu"]], diag(2) * 8:9)
        expect_equal(tst@covariance, diag(2) * 10:11)

        # Isotropic, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "isotropic", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 6:7)
        expect_equal(tst@parameters[["nu"]], diag(2) * 8:9)
        expect_equal(tst@covariance, diag(2) * 10:11)

        # Symmetric, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "symmetric", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(c(6, 7, 7, 8), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 12:13)

        # Symmetric, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "symmetric", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(c(6, 7, 0, 8), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(9, 10, 0, 11), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 12:13)

        # Anisotropic, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "anisotropic", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(6:9, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(10:13, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 14:15)

        # Anisotropic, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "anisotropic", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 5)
        expect_equal(tst@parameters[["gamma"]], matrix(6:9, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(10:13, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 14:15)


        ########################################################################
        # Two dimensions and two predictors

        my_model <- double_exponential(d = 2, k = 2)

        # Isotropic, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "isotropic", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 8:9)
        expect_equal(tst@parameters[["nu"]], diag(2) * 10:11)
        expect_equal(tst@covariance, diag(2) * 12:13)

        # Isotropic, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "isotropic", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 8:9)
        expect_equal(tst@parameters[["nu"]], diag(2) * 10:11)
        expect_equal(tst@covariance, diag(2) * 12:13)

        # Symmetric, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "symmetric", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(c(8, 9, 9, 10), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 14:15)

        # Symmetric, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "symmetric", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(c(8, 9, 0, 10), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(11, 12, 0, 13), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 14:15)

        # Anisotropic, full = TRUE
        tst <- index(
            my_model, 
            dynamics = "anisotropic", 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(8:11, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(12:15, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 16:17)

        # Anisotropic, full = FALSE
        tst <- index(
            my_model, 
            dynamics = "anisotropic", 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], matrix(8:11, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(12:15, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 16:17)
    }
)





################################################################################
# FILL

test_that(
    "Test known errors for fill",
    {
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
    "Check output for fill: Exponential discounting",
    {        
        ########################################################################
        # Single dimension and single predictor

        my_model <- exponential(d = 1, k = 1)
        tst <- fill(my_model, 5:8, parameters_only = FALSE, cholesky = FALSE)

        expect_equal(tst@parameters[["alpha"]], 5)
        expect_equal(tst@parameters[["beta"]], as.matrix(6))
        expect_equal(tst@parameters[["gamma"]], as.matrix(7))
        expect_equal(tst@covariance, as.matrix(8))



        #######################################################################
        # Single dimension and two predictors

        my_model <- exponential(d = 1, k = 2)
        tst <- fill(my_model, 5:9, parameters_only = FALSE, cholesky = FALSE)

        expect_equal(tst@parameters[["alpha"]], 5)
        expect_equal(tst@parameters[["beta"]], matrix(6:7, nrow = 1))
        expect_equal(tst@parameters[["gamma"]], as.matrix(8))
        expect_equal(tst@covariance, as.matrix(9))



        ########################################################################
        # Two dimensions and single predictor

        my_model <- exponential(d = 2, k = 1)

        # Isotropic - Isotropic
        tst <- fill(
            my_model, 
            5:12, 
            dynamics = "isotropic", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 9:10)
        expect_equal(tst@covariance, diag(2) * 11:12)

        # Isotropic - Symmetric
        tst <- fill(
            my_model, 
            5:13, 
            dynamics = "isotropic", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 9:10)
        expect_equal(tst@covariance, matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))

        # Symmetric - Isotropic
        tst <- fill(
            my_model, 
            5:13, 
            dynamics = "symmetric", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 12:13)

        # Symmetric - Symmetric
        tst <- fill(
            my_model, 
            5:14,
            dynamics = "symmetric", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(12, 13, 13, 14), nrow = 2, ncol = 2))

        # Anisotropic - Isotropic
        tst <- fill(
            my_model, 
            5:14, 
            dynamics = "anisotropic", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(9:12, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 13:14)

        # Anisotropic - Symmetric
        tst <- fill(
            my_model, 
            5:15, 
            dynamics = "anisotropic", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["gamma"]], matrix(9:12, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(13, 14, 14, 15), nrow = 2, ncol = 2))



        ########################################################################
        # Two dimensions and two predictors

        my_model <- exponential(d = 2, k = 2)

        # Isotropic - Isotropic
        tst <- fill(
            my_model, 
            5:14, 
            dynamics = "isotropic", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 11:12)
        expect_equal(tst@covariance, diag(2) * 13:14)

        # Isotropic - Symmetric
        tst <- fill(
            my_model, 
            5:15, 
            dynamics = "isotropic", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 11:12)
        expect_equal(tst@covariance, matrix(c(13, 14, 14, 15), nrow = 2, ncol = 2))

        # Symmetric - Isotropic
        tst <- fill(
            my_model, 
            5:15, 
            dynamics = "symmetric", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 14:15)

        # Symmetric - Symmetric
        tst <- fill(
            my_model, 
            5:16,
            dynamics = "symmetric", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(14, 15, 15, 16), nrow = 2, ncol = 2))

        # Anisotropic - Isotropic
        tst <- fill(
            my_model, 
            5:16, 
            dynamics = "anisotropic", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(11:14, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 15:16)

        # Anisotropic - Symmetric
        tst <- fill(
            my_model, 
            5:17, 
            dynamics = "anisotropic", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["gamma"]], matrix(11:14, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(15, 16, 16, 17), nrow = 2, ncol = 2))
    }
)

test_that(
    "Check output for fill: Quasi-hyperbolic discounting",
    {
        ########################################################################
        # Single dimension and single predictor

        my_model <- quasi_hyperbolic(d = 1, k = 1)
        tst <- fill(my_model, 5:9, parameters_only = FALSE, cholesky = FALSE)

        expect_equal(tst@parameters[["alpha"]], 5)
        expect_equal(tst@parameters[["beta"]], as.matrix(6))
        expect_equal(tst@parameters[["nu"]], as.matrix(7))
        expect_equal(tst@parameters[["kappa"]], as.matrix(8))
        expect_equal(tst@covariance, as.matrix(9))



        #######################################################################
        # Single dimension and two predictors

        my_model <- quasi_hyperbolic(d = 1, k = 2)
        tst <- fill(my_model, 5:10, parameters_only = FALSE, cholesky = FALSE)

        expect_equal(tst@parameters[["alpha"]], 5)
        expect_equal(tst@parameters[["beta"]], matrix(6:7, nrow = 1))
        expect_equal(tst@parameters[["nu"]], as.matrix(8))
        expect_equal(tst@parameters[["kappa"]], as.matrix(9))
        expect_equal(tst@covariance, as.matrix(10))



        ########################################################################
        # Two dimensions and single predictor

        my_model <- quasi_hyperbolic(d = 2, k = 1)

        # Isotropic - Isotropic
        tst <- fill(
            my_model, 
            5:14, 
            dynamics = "isotropic", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], diag(2) * 9:10)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 11:12)
        expect_equal(tst@covariance, diag(2) * 13:14)

        # Isotropic - Symmetric
        tst <- fill(
            my_model, 
            5:15, 
            dynamics = "isotropic", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], diag(2) * 9:10)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 11:12)
        expect_equal(tst@covariance, matrix(c(13, 14, 14, 15), nrow = 2, ncol = 2))

        # Symmetric - Isotropic
        tst <- fill(
            my_model, 
            5:16, 
            dynamics = "symmetric", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(12, 13, 13, 14), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 15:16)

        # Symmetric - Symmetric
        tst <- fill(
            my_model, 
            5:17,
            dynamics = "symmetric", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(12, 13, 13, 14), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(15, 16, 16, 17), nrow = 2, ncol = 2))

        # Anisotropic - Isotropic
        tst <- fill(
            my_model, 
            5:18, 
            dynamics = "anisotropic", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(9:12, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(13:16, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 17:18)

        # Anisotropic - Symmetric
        tst <- fill(
            my_model, 
            5:19, 
            dynamics = "anisotropic", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["nu"]], matrix(9:12, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(13:16, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(17, 18, 18, 19), nrow = 2, ncol = 2))



        ########################################################################
        # Two dimensions and two predictors

        my_model <- quasi_hyperbolic(d = 2, k = 2)

        # Isotropic - Isotropic
        tst <- fill(
            my_model, 
            5:16, 
            dynamics = "isotropic", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], diag(2) * 11:12)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 13:14)
        expect_equal(tst@covariance, diag(2) * 15:16)

        # Isotropic - Symmetric
        tst <- fill(
            my_model, 
            5:17, 
            dynamics = "isotropic", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], diag(2) * 11:12)
        expect_equal(tst@parameters[["kappa"]], diag(2) * 13:14)
        expect_equal(tst@covariance, matrix(c(15, 16, 16, 17), nrow = 2, ncol = 2))

        # Symmetric - Isotropic
        tst <- fill(
            my_model, 
            5:18, 
            dynamics = "symmetric", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(14, 15, 15, 16), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 17:18)

        # Symmetric - Symmetric
        tst <- fill(
            my_model, 
            5:19,
            dynamics = "symmetric", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(c(14, 15, 15, 16), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(17, 18, 18, 19), nrow = 2, ncol = 2))

        # Anisotropic - Isotropic
        tst <- fill(
            my_model, 
            5:20, 
            dynamics = "anisotropic", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(11:14, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(15:18, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 19:20)

        # Anisotropic - Symmetric
        tst <- fill(
            my_model, 
            5:21, 
            dynamics = "anisotropic", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(11:14, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["kappa"]], matrix(15:18, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(19, 20, 20, 21), nrow = 2, ncol = 2))
    }
)

test_that(
    "Check output for fill: Double exponential discounting",
    {
        ########################################################################
        # Single dimension and single predictor

        my_model <- double_exponential(d = 1, k = 1)
        tst <- fill(my_model, 5:10, parameters_only = FALSE, cholesky = FALSE)

        expect_equal(tst@parameters[["alpha"]], 5)
        expect_equal(tst@parameters[["beta"]], as.matrix(6))
        expect_equal(tst@parameters[["omega"]], 7)
        expect_equal(tst@parameters[["gamma"]], as.matrix(8))
        expect_equal(tst@parameters[["nu"]], as.matrix(9))
        expect_equal(tst@covariance, as.matrix(10))



        #######################################################################
        # Single dimension and two predictors

        my_model <- double_exponential(d = 1, k = 2)
        tst <- fill(my_model, 5:11, parameters_only = FALSE, cholesky = FALSE)

        expect_equal(tst@parameters[["alpha"]], 5)
        expect_equal(tst@parameters[["beta"]], matrix(6:7, nrow = 1))
        expect_equal(tst@parameters[["omega"]], 8)
        expect_equal(tst@parameters[["gamma"]], as.matrix(9))
        expect_equal(tst@parameters[["nu"]], as.matrix(10))
        expect_equal(tst@covariance, as.matrix(11))



        ########################################################################
        # Two dimensions and single predictor

        my_model <- double_exponential(d = 2, k = 1)

        # Isotropic - Isotropic
        tst <- fill(
            my_model, 
            5:15, 
            dynamics = "isotropic", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 9)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 10:11)
        expect_equal(tst@parameters[["nu"]], diag(2) * 12:13)
        expect_equal(tst@covariance, diag(2) * 14:15)

        # Isotropic - Symmetric
        tst <- fill(
            my_model, 
            5:16, 
            dynamics = "isotropic", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 9)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 10:11)
        expect_equal(tst@parameters[["nu"]], diag(2) * 12:13)
        expect_equal(tst@covariance, matrix(c(14, 15, 15, 16), nrow = 2, ncol = 2))

        # Symmetric - Isotropic
        tst <- fill(
            my_model, 
            5:17, 
            dynamics = "symmetric", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 9)
        expect_equal(tst@parameters[["gamma"]], matrix(c(10, 11, 11, 12), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(13, 14, 14, 15), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 16:17)

        # Symmetric - Symmetric
        tst <- fill(
            my_model, 
            5:18,
            dynamics = "symmetric", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 9)
        expect_equal(tst@parameters[["gamma"]], matrix(c(10, 11, 11, 12), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(13, 14, 14, 15), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(16, 17, 17, 18), nrow = 2, ncol = 2))

        # Anisotropic - Isotropic
        tst <- fill(
            my_model, 
            5:19, 
            dynamics = "anisotropic", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 9)
        expect_equal(tst@parameters[["gamma"]], matrix(10:13, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(14:17, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 18:19)

        # Anisotropic - Symmetric
        tst <- fill(
            my_model, 
            5:20, 
            dynamics = "anisotropic", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@parameters[["omega"]], 9)
        expect_equal(tst@parameters[["gamma"]], matrix(10:13, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(14:17, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(18, 19, 19, 20), nrow = 2, ncol = 2))



        ########################################################################
        # Two dimensions and two predictors

        my_model <- double_exponential(d = 2, k = 2)

        # Isotropic - Isotropic
        tst <- fill(
            my_model, 
            5:17, 
            dynamics = "isotropic", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 11)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 12:13)
        expect_equal(tst@parameters[["nu"]], diag(2) * 14:15)
        expect_equal(tst@covariance, diag(2) * 16:17)

        # Isotropic - Symmetric
        tst <- fill(
            my_model, 
            5:18, 
            dynamics = "isotropic", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 11)
        expect_equal(tst@parameters[["gamma"]], diag(2) * 12:13)
        expect_equal(tst@parameters[["nu"]], diag(2) * 14:15)
        expect_equal(tst@covariance, matrix(c(16, 17, 17, 18), nrow = 2, ncol = 2))

        # Symmetric - Isotropic
        tst <- fill(
            my_model, 
            5:19, 
            dynamics = "symmetric", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 11)
        expect_equal(tst@parameters[["gamma"]], matrix(c(12, 13, 13, 14), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(15, 16, 16, 17), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 18:19)

        # Symmetric - Symmetric
        tst <- fill(
            my_model, 
            5:20,
            dynamics = "symmetric", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 11)
        expect_equal(tst@parameters[["gamma"]], matrix(c(12, 13, 13, 14), nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(c(15, 16, 16, 17), nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(18, 19, 19, 20), nrow = 2, ncol = 2))

        # Anisotropic - Isotropic
        tst <- fill(
            my_model, 
            5:21, 
            dynamics = "anisotropic", 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 11)
        expect_equal(tst@parameters[["gamma"]], matrix(12:15, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(16:19, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 20:21)

        # Anisotropic - Symmetric
        tst <- fill(
            my_model, 
            5:22, 
            dynamics = "anisotropic", 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["omega"]], 11)
        expect_equal(tst@parameters[["gamma"]], matrix(12:15, nrow = 2, ncol = 2))
        expect_equal(tst@parameters[["nu"]], matrix(16:19, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(20, 21, 21, 22), nrow = 2, ncol = 2))
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
        # Too many parameters provided
        expect_warning(
            get_bounds(
                exponential(),
                lower = 1:20,
                upper = 1:20,
                parameters_only = FALSE
            )
        )
        expect_warning(
            get_bounds(
                exponential(),
                lower = 1:20,
                upper = 1:20,
                parameters_only = TRUE
            )
        )

        expect_warning(
            get_bounds(
                quasi_hyperbolic(),
                lower = 1:20,
                upper = 1:20,
                parameters_only = FALSE
            )
        )
        expect_warning(
            get_bounds(
                quasi_hyperbolic(),
                lower = 1:20,
                upper = 1:20,
                parameters_only = TRUE
            )
        )

        expect_warning(
            get_bounds(
                double_exponential(),
                lower = 1:20,
                upper = 1:20,
                parameters_only = FALSE
            )
        )
        expect_warning(
            get_bounds(
                double_exponential(),
                lower = 1:20,
                upper = 1:20,
                parameters_only = TRUE
            )
        )
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

test_that(
    "Check the output of get_bounds: Quasi-hyperbolic discounting",
    {
        # One dimension, one predictor
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 1, k = 1),
                lower = -(1:4),
                upper = 1:4,
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 2, 3, 4),
                "upper" = c(1, 2, 3, 4)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 1, k = 1),
                lower = -(1:5),
                upper = 1:5,
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 2, 3, 4, 5),
                "upper" = c(1, 2, 3, 4, 5)
            )
        )

        # One dimension, two predictors
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 1, k = 2),
                lower = -(1:4),
                upper = 1:4,
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 2, 2, 3, 4),
                "upper" = c(1, 2, 2, 3, 4)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 1, k = 2),
                lower = -(1:5),
                upper = 1:5,
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 2, 2, 3, 4, 5),
                "upper" = c(1, 2, 2, 3, 4, 5)
            )
        )

        # Two dimension, one predictor
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 1),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 3, 4, 4),
                "upper" = c(1, 1, 2, 2, 3, 3, 4, 4)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 1),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
                "upper" = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 1),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5),
                "upper" = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5)
            )
        )

        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 1),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4),
                "upper" = c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 1),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5),
                "upper" = c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 1),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5),
                "upper" = c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)
            )
        )

        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 1),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4),
                "upper" = c(1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 1),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5),
                "upper" = c(1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 1),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5),
                "upper" = c(1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5)
            )
        )

        # Two dimension, two predictors
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 2),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 3, 4, 4),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 4, 4)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 2),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 2),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5, 5),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5, 5)
            )
        )

        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 2),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 2),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 2),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)
            )
        )

        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 2),
                lower = -(1:4),
                upper = 1:4,
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 2),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                quasi_hyperbolic(d = 2, k = 2),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5)
            )
        )
    }
)

test_that(
    "Check the output of get_bounds: Double exponential discounting",
    {
        # One dimension, one predictor
        expect_equal(
            get_bounds(
                double_exponential(d = 1, k = 1),
                lower = -(1:5),
                upper = 1:5,
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 2, 3, 4, 5),
                "upper" = c(1, 2, 3, 4, 5)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 1, k = 1),
                lower = -(1:6),
                upper = 1:6,
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 2, 3, 4, 5, 6),
                "upper" = c(1, 2, 3, 4, 5, 6)
            )
        )

        # One dimension, two predictors
        expect_equal(
            get_bounds(
                double_exponential(d = 1, k = 2),
                lower = -(1:5),
                upper = 1:5,
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 2, 2, 3, 4, 5),
                "upper" = c(1, 2, 2, 3, 4, 5)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 1, k = 2),
                lower = -(1:6),
                upper = 1:6,
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 2, 2, 3, 4, 5, 6),
                "upper" = c(1, 2, 2, 3, 4, 5, 6)
            )
        )

        # Two dimension, one predictor
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 1),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 4, 4, 5, 5),
                "upper" = c(1, 1, 2, 2, 3, 4, 4, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 1),
                lower = -(1:6),
                upper = 1:6,
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6),
                "upper" = c(1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 1),
                lower = -(1:6),
                upper = 1:6,
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6, 6),
                "upper" = c(1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6, 6)
            )
        )

        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 1),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 4, 4, 4, 5, 5, 5),
                "upper" = c(1, 1, 2, 2, 3, 4, 4, 4, 5, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 1),
                lower = -(1:6),
                upper = 1:6,
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6),
                "upper" = c(1, 1, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 1),
                lower = -(1:6),
                upper = 1:6,
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6),
                "upper" = c(1, 1, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6)
            )
        )

        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 1),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 4, 4, 4, 4, 5, 5, 5, 5),
                "upper" = c(1, 1, 2, 2, 3, 4, 4, 4, 4, 5, 5, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 1),
                lower = -(1:6),
                upper = 1:6,
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6),
                "upper" = c(1, 1, 2, 2, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 1),
                lower = -(1:6),
                upper = 1:6,
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6),
                "upper" = c(1, 1, 2, 2, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6)
            )
        )

        # Two dimension, two predictors
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 2),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 4, 4, 5, 5),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 4, 4, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 2),
                lower = -(1:6),
                upper = 1:6,
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 4, 4, 5, 5, 6, 6),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 4, 4, 5, 5, 6, 6)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 2),
                lower = -(1:6),
                upper = 1:6,
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 4, 4, 5, 5, 6, 6, 6),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 4, 4, 5, 5, 6, 6, 6)
            )
        )

        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 2),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 2),
                lower = -(1:6),
                upper = 1:6,
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 2),
                lower = -(1:6),
                upper = 1:6,
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6)
            )
        )

        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 2),
                lower = -(1:5),
                upper = 1:5,
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 4, 5, 5, 5, 5),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 4, 5, 5, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 2),
                lower = -(1:6),
                upper = 1:6,
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6)
            )
        )
        expect_equal(
            get_bounds(
                double_exponential(d = 2, k = 2),
                lower = -(1:6),
                upper = 1:6,
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = -c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6)
            )
        )
    }
)




################################################################################
# PARAMETER_NAMES

test_that(
    "Test known errors for parameter_names and parameter_names_covariance",
    {
        # Dynamics not defined
        expect_error(parameter_names(exponential(), dynamics = "test"))
        expect_error(parameter_names(quasi_hyperbolic(), dynamics = "test"))
        expect_error(parameter_names(double_exponential(), dynamics = "test"))

        # Covariance not defined
        expect_error(parameter_names(exponential(), covariance = "test", parameters_only = FALSE))
        expect_error(parameter_names_covariance(2, covariance = "test"))
    }
)

test_that(
    "Check output of parameter_names_covariance",
    {
        expect_equal(
            parameter_names_covariance(1),
            c("sigma_11")
        )

        expect_equal(
            parameter_names_covariance(2, covariance = "isotropic"),
            c("sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names_covariance(2, covariance = "symmetric"),
            c("sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names_covariance(3, covariance = "isotropic"),
            c("sigma_11", "sigma_22", "sigma_33")
        )
        expect_equal(
            parameter_names_covariance(3, covariance = "symmetric"),
            c("sigma_11", "sigma_21", "sigma_31", "sigma_22", "sigma_32", "sigma_33")
        )
    }
)

test_that(
    "Check output of parameter_names: Exponential discounting model",
    {
        # One dimension, one predictor
        expect_equal(
            parameter_names(
                exponential(d = 1, k = 1),
                parameters_only = TRUE
            ),
            c("alpha_1", "beta_11", "gamma_11")
        )
        expect_equal(
            parameter_names(
                exponential(d = 1, k = 1),
                parameters_only = FALSE
            ),
            c("alpha_1", "beta_11", "gamma_11", "sigma_11")
        )

        # One dimension, two predictors
        expect_equal(
            parameter_names(
                exponential(d = 1, k = 2),
                parameters_only = TRUE
            ),
            c("alpha_1", "beta_11", "beta_12", "gamma_11")
        )
        expect_equal(
            parameter_names(
                exponential(d = 1, k = 2),
                parameters_only = FALSE
            ),
            c("alpha_1", "beta_11", "beta_12", "gamma_11", "sigma_11")
        )

        # Two dimensions, one predictor
        iso <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", 
            "gamma_11", "gamma_22"
        )
        sym <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", 
            "gamma_11", "gamma_21", "gamma_22"
        )
        ani <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", 
            "gamma_11", "gamma_21", "gamma_12", "gamma_22"
        )

        expect_equal(
            parameter_names(
                exponential(d = 2, k = 1),
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            iso
        )
        expect_equal(
            parameter_names(
                exponential(d = 2, k = 1),
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(iso, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                exponential(d = 2, k = 1),
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(iso, "sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names(
                exponential(d = 2, k = 1),
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            sym
        )
        expect_equal(
            parameter_names(
                exponential(d = 2, k = 1),
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(sym, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                exponential(d = 2, k = 1),
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(sym, "sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names(
                exponential(d = 2, k = 1),
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            ani
        )
        expect_equal(
            parameter_names(
                exponential(d = 2, k = 1),
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(ani, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                exponential(d = 2, k = 1),
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(ani, "sigma_11", "sigma_21", "sigma_22")
        )

        # Two dimensions, two predictors
        iso <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", "beta_12", "beta_22",
            "gamma_11", "gamma_22"
        )
        sym <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", "beta_12", "beta_22",
            "gamma_11", "gamma_21", "gamma_22"
        )
        ani <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", "beta_12", "beta_22",
            "gamma_11", "gamma_21", "gamma_12", "gamma_22"
        )

        expect_equal(
            parameter_names(
                exponential(d = 2, k = 2),
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            iso
        )
        expect_equal(
            parameter_names(
                exponential(d = 2, k = 2),
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(iso, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                exponential(d = 2, k = 2),
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(iso, "sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names(
                exponential(d = 2, k = 2),
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            sym
        )
        expect_equal(
            parameter_names(
                exponential(d = 2, k = 2),
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(sym, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                exponential(d = 2, k = 2),
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(sym, "sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names(
                exponential(d = 2, k = 2),
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            ani
        )
        expect_equal(
            parameter_names(
                exponential(d = 2, k = 2),
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(ani, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                exponential(d = 2, k = 2),
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(ani, "sigma_11", "sigma_21", "sigma_22")
        )
    }
)

test_that(
    "Check output of parameter_names: Quasi-hyperbolic discounting model",
    {
        # One dimension, one predictor
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 1, k = 1),
                parameters_only = TRUE
            ),
            c("alpha_1", "beta_11", "nu_11", "kappa_11")
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 1, k = 1),
                parameters_only = FALSE
            ),
            c("alpha_1", "beta_11", "nu_11", "kappa_11", "sigma_11")
        )

        # One dimension, two predictors
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 1, k = 2),
                parameters_only = TRUE
            ),
            c("alpha_1", "beta_11", "beta_12", "nu_11", "kappa_11")
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 1, k = 2),
                parameters_only = FALSE
            ),
            c("alpha_1", "beta_11", "beta_12", "nu_11", "kappa_11", "sigma_11")
        )

        # Two dimensions, one predictor
        iso <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", 
            "nu_11", "nu_22", "kappa_11", "kappa_22"
        )
        sym <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", 
            "nu_11", "nu_21", "nu_22", "kappa_11", "kappa_21", "kappa_22"
        )
        ani <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", 
            "nu_11", "nu_21", "nu_12", "nu_22",
            "kappa_11", "kappa_21", "kappa_12", "kappa_22"
        )

        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 1),
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            iso
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 1),
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(iso, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 1),
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(iso, "sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 1),
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            sym
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 1),
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(sym, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 1),
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(sym, "sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 1),
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            ani
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 1),
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(ani, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 1),
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(ani, "sigma_11", "sigma_21", "sigma_22")
        )

        # Two dimensions, two predictors
        iso <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", "beta_12", "beta_22",
            "nu_11", "nu_22", "kappa_11", "kappa_22"
        )
        sym <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", "beta_12", "beta_22",
            "nu_11", "nu_21", "nu_22", "kappa_11", "kappa_21", "kappa_22"
        )
        ani <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", "beta_12", "beta_22",
            "nu_11", "nu_21", "nu_12", "nu_22",
            "kappa_11", "kappa_21", "kappa_12", "kappa_22"
        )

        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 2),
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            iso
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 2),
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(iso, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 2),
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(iso, "sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 2),
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            sym
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 2),
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(sym, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 2),
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(sym, "sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 2),
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            ani
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 2),
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(ani, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                quasi_hyperbolic(d = 2, k = 2),
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(ani, "sigma_11", "sigma_21", "sigma_22")
        )
    }
)

test_that(
    "Check output of parameter_names: Double exponential discounting model",
    {
        # One dimension, one predictor
        expect_equal(
            parameter_names(
                double_exponential(d = 1, k = 1),
                parameters_only = TRUE
            ),
            c("alpha_1", "beta_11", "omega", "gamma_11", "nu_11")
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 1, k = 1),
                parameters_only = FALSE
            ),
            c("alpha_1", "beta_11", "omega", "gamma_11", "nu_11", "sigma_11")
        )

        # One dimension, two predictors
        expect_equal(
            parameter_names(
                double_exponential(d = 1, k = 2),
                parameters_only = TRUE
            ),
            c("alpha_1", "beta_11", "beta_12", "omega", "gamma_11", "nu_11")
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 1, k = 2),
                parameters_only = FALSE
            ),
            c("alpha_1", "beta_11", "beta_12", "omega", "gamma_11", "nu_11", "sigma_11")
        )

        # Two dimensions, one predictor
        iso <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", "omega",
            "gamma_11", "gamma_22", "nu_11", "nu_22"
        )
        sym <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", "omega", 
            "gamma_11", "gamma_21", "gamma_22", "nu_11", "nu_21", "nu_22"
        )
        ani <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", "omega", 
            "gamma_11", "gamma_21", "gamma_12", "gamma_22",
            "nu_11", "nu_21", "nu_12", "nu_22"
        )

        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 1),
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            iso
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 1),
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(iso, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 1),
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(iso, "sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 1),
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            sym
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 1),
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(sym, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 1),
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(sym, "sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 1),
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            ani
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 1),
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(ani, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 1),
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(ani, "sigma_11", "sigma_21", "sigma_22")
        )

        # Two dimensions, two predictors
        iso <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", "beta_12", "beta_22",
            "omega", "gamma_11", "gamma_22", "nu_11", "nu_22"
        )
        sym <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", "beta_12", "beta_22",
            "omega", "gamma_11", "gamma_21", "gamma_22", "nu_11", "nu_21", "nu_22"
        )
        ani <- c(
            "alpha_1", "alpha_2", "beta_11", "beta_21", "beta_12", "beta_22",
            "omega", "gamma_11", "gamma_21", "gamma_12", "gamma_22",
            "nu_11", "nu_21", "nu_12", "nu_22"
        )

        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 2),
                dynamics = "isotropic",
                parameters_only = TRUE
            ),
            iso
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 2),
                dynamics = "isotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(iso, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 2),
                dynamics = "isotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(iso, "sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 2),
                dynamics = "symmetric",
                parameters_only = TRUE
            ),
            sym
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 2),
                dynamics = "symmetric",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(sym, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 2),
                dynamics = "symmetric",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(sym, "sigma_11", "sigma_21", "sigma_22")
        )

        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 2),
                dynamics = "anisotropic",
                parameters_only = TRUE
            ),
            ani
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 2),
                dynamics = "anisotropic",
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(ani, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                double_exponential(d = 2, k = 2),
                dynamics = "anisotropic",
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(ani, "sigma_11", "sigma_21", "sigma_22")
        )
    }
)



################################################################################
# PARAMETERS

test_that(
    "Check the output of parameters: Exponential discounting",
    {
        # Anisotropic, Symmetric
        model <- fill(
            exponential(d = 2, k = 2),
            1:13,
            dynamics = "anisotropic",
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "anisotropic", covariance = "symmetric"),
            1:13
        )

        # Symmetric, Symmetric
        model <- fill(
            exponential(d = 2, k = 2),
            1:12,
            dynamics = "symmetric",
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "symmetric", covariance = "symmetric"),
            1:12
        )

        # Isotropic, Symmetric
        model <- fill(
            exponential(d = 2, k = 2),
            1:11,
            dynamics = "isotropic",
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "isotropic", covariance = "symmetric"),
            1:11
        )

        # Anisotropic, Isotropic
        model <- fill(
            exponential(d = 2, k = 2),
            1:12,
            dynamics = "anisotropic",
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "anisotropic", covariance = "isotropic"),
            1:12
        )

        # Symmetric, Isotropic
        model <- fill(
            exponential(d = 2, k = 2),
            1:11,
            dynamics = "symmetric",
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "symmetric", covariance = "isotropic"),
            1:11
        )

        # Isotropic, Isotropic
        model <- fill(
            exponential(d = 2, k = 2),
            1:10,
            dynamics = "isotropic",
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "isotropic", covariance = "isotropic"),
            1:10
        )
    }
)

test_that(
    "Check the output of parameters: Quasi-hyperbolic discounting",
    {
        # Anisotropic, Symmetric
        model <- fill(
            quasi_hyperbolic(d = 2, k = 2),
            1:17,
            dynamics = "anisotropic",
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "anisotropic", covariance = "symmetric"),
            1:17
        )

        # Symmetric, Symmetric
        model <- fill(
            quasi_hyperbolic(d = 2, k = 2),
            1:15,
            dynamics = "symmetric",
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "symmetric", covariance = "symmetric"),
            1:15
        )

        # Isotropic, Symmetric
        model <- fill(
            quasi_hyperbolic(d = 2, k = 2),
            1:13,
            dynamics = "isotropic",
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "isotropic", covariance = "symmetric"),
            1:13
        )

        # Anisotropic, Isotropic
        model <- fill(
            quasi_hyperbolic(d = 2, k = 2),
            1:16,
            dynamics = "anisotropic",
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "anisotropic", covariance = "isotropic"),
            1:16
        )

        # Symmetric, Isotropic
        model <- fill(
            quasi_hyperbolic(d = 2, k = 2),
            1:14,
            dynamics = "symmetric",
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "symmetric", covariance = "isotropic"),
            1:14
        )

        # Isotropic, Isotropic
        model <- fill(
            quasi_hyperbolic(d = 2, k = 2),
            1:12,
            dynamics = "isotropic",
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "isotropic", covariance = "isotropic"),
            1:12
        )
    }
)

test_that(
    "Check the output of parameters: Double-exponential discounting",
    {
        # Anisotropic, Symmetric
        model <- fill(
            double_exponential(d = 2, k = 2),
            1:18,
            dynamics = "anisotropic",
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "anisotropic", covariance = "symmetric"),
            1:18
        )

        # Symmetric, Symmetric
        model <- fill(
            double_exponential(d = 2, k = 2),
            1:16,
            dynamics = "symmetric",
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "symmetric", covariance = "symmetric"),
            1:16
        )

        # Isotropic, Symmetric
        model <- fill(
            double_exponential(d = 2, k = 2),
            1:14,
            dynamics = "isotropic",
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "isotropic", covariance = "symmetric"),
            1:14
        )

        # Anisotropic, Isotropic
        model <- fill(
            double_exponential(d = 2, k = 2),
            1:17,
            dynamics = "anisotropic",
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "anisotropic", covariance = "isotropic"),
            1:17
        )

        # Symmetric, Isotropic
        model <- fill(
            double_exponential(d = 2, k = 2),
            1:15,
            dynamics = "symmetric",
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "symmetric", covariance = "isotropic"),
            1:15
        )

        # Isotropic, Isotropic
        model <- fill(
            double_exponential(d = 2, k = 2),
            1:13,
            dynamics = "isotropic",
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, dynamics = "isotropic", covariance = "isotropic"),
            1:13
        )
    }
)

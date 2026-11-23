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

test_that(
    "Check output for index: Distributed lag, no covariances included",
    {
        ########################################################################
        # Single dimension and single predictor
        my_model <- distributed_lag(d = 1, k = 1)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@covariance, as.matrix(0))


        ########################################################################
        # Single dimension, single predictor, and two lags
        my_model <- distributed_lag(d = 1, k = 1, lag = 2)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:4, nrow = 1))
        expect_equal(tst@covariance, as.matrix(0))


        ########################################################################
        # Single dimension and two predictors
        my_model <- distributed_lag(d = 1, k = 2)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@covariance, as.matrix(0))


        ########################################################################
        # Two dimensions and single predictor

        my_model <- distributed_lag(d = 2, k = 1)

        # full = TRUE
        tst <- index(my_model, full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # full = FALSE
        tst <- index(my_model, full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))


        ########################################################################
        # Two dimensions, two predictors, and one lag

        my_model <- distributed_lag(d = 2, k = 2, lag = 1)

        # full = TRUE
        tst <- index(my_model, full = TRUE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:10, nrow = 2, ncol = 4))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # full = FALSE
        tst <- index(my_model, full = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:10, nrow = 2, ncol = 4))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # Dynamics do not influence the result, as there are no dynamical 
        # matrices in the model
        expect_equal(
            index(my_model, dynamics = "anisotropic"),
            index(my_model)
        )
        expect_equal(
            index(my_model, dynamics = "symmetric"),
            index(my_model)
        )
        
        ########################################################################
        # Explicit lags: Single dimension and single predictor

        # lag = 0
        my_model <- distributed_lag(d = 1, k = 1, lag = 0)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@covariance, as.matrix(0))
        expect_equal(tst, index(distributed_lag(d = 1, k = 1)))

        # lag = 1
        my_model <- distributed_lag(d = 1, k = 1, lag = 1)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@covariance, as.matrix(0))


        ########################################################################
        # Explicit lags: Two dimensions and single predictor

        # lag = 0
        my_model <- distributed_lag(d = 2, k = 1, lag = 0)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))

        # lag = 1
        my_model <- distributed_lag(d = 2, k = 1, lag = 1)
        tst <- index(my_model)

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))
    }
)

test_that(
    "Check output for index: Distributed lag, covariances included",
    {
        ########################################################################
        # Single dimension and single predictor
        my_model <- distributed_lag(d = 1, k = 1)
        tst <- index(my_model, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@covariance, as.matrix(3))


        ########################################################################
        # Single dimension, two predictors, and one lag
        my_model <- distributed_lag(d = 1, k = 2, lag = 1)
        tst <- index(my_model, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:5, nrow = 1))
        expect_equal(tst@covariance, as.matrix(6))


        ########################################################################
        # Two dimensions and single predictor

        my_model <- distributed_lag(d = 2, k = 1)

        # Isotropic covariance, full = TRUE
        tst <- index(
            my_model, 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@covariance, diag(2) * 5:6)

        # Isotropic covariance, full = FALSE
        tst <- index(
            my_model, 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@covariance, diag(2) * 5:6)

        # Symmetric covariance, full = TRUE, no Cholesky decomposition
        tst <- index(
            my_model, 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "symmetric",
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@covariance, matrix(c(5, 6, 6, 7), nrow = 2, ncol = 2))

        # Symmetric covariance, full = TRUE, Cholesky decomposition
        tst <- index(
            my_model, 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "symmetric",
            cholesky = TRUE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@covariance, matrix(c(5, 6, 0, 7), nrow = 2, ncol = 2))

        # Symmetric covariance, full = FALSE
        tst <- index(
            my_model, 
            full = FALSE, 
            parameters_only = FALSE,
            covariance = "symmetric",
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@covariance, matrix(c(5, 6, 0, 7), nrow = 2, ncol = 2))


        ########################################################################
        # Two dimensions, two predictors, and one lag

        my_model <- distributed_lag(d = 2, k = 2, lag = 1)

        # Isotropic covariance
        tst <- index(
            my_model, 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:10, nrow = 2, ncol = 4))
        expect_equal(tst@covariance, diag(2) * 11:12)

        # Symmetric covariance
        tst <- index(
            my_model, 
            full = TRUE, 
            parameters_only = FALSE,
            covariance = "symmetric",
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:10, nrow = 2, ncol = 4))
        expect_equal(tst@covariance, matrix(c(11, 12, 12, 13), nrow = 2, ncol = 2))
        ########################################################################
        # Explicit lags: Single dimension and single predictor

        # lag = 0
        my_model <- distributed_lag(d = 1, k = 1, lag = 0)
        tst <- index(my_model, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], as.matrix(2))
        expect_equal(tst@covariance, as.matrix(3))

        # lag = 1
        my_model <- distributed_lag(d = 1, k = 1, lag = 1)
        tst <- index(my_model, parameters_only = FALSE)

        expect_equal(tst@parameters[["alpha"]], 1)
        expect_equal(tst@parameters[["beta"]], matrix(2:3, nrow = 1))
        expect_equal(tst@covariance, as.matrix(4))


        ########################################################################
        # Explicit lags: Two dimensions and single predictor

        # lag = 0
        my_model <- distributed_lag(d = 2, k = 1, lag = 0)
        tst <- index(
            my_model, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:4, nrow = 2, ncol = 1))
        expect_equal(tst@covariance, diag(2) * 5:6)

        # lag = 1
        my_model <- distributed_lag(d = 2, k = 1, lag = 1)
        tst <- index(
            my_model, 
            parameters_only = FALSE,
            covariance = "isotropic"
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 7:8)

        tst <- index(
            my_model, 
            parameters_only = FALSE,
            covariance = "symmetric",
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 1:2)
        expect_equal(tst@parameters[["beta"]], matrix(3:6, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, matrix(c(7, 8, 8, 9), nrow = 2, ncol = 2))
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

        # Not enough parameters provided for the distributed lag model
        expect_error(fill(distributed_lag(d = 2, k = 2), 1:2))
        expect_error(fill(distributed_lag(d = 2, k = 2, lag = 1), 1:6))
        expect_error(fill(distributed_lag(d = 2, k = 2, lag = 1), 1:10, parameters_only = FALSE))

        # = with explicit lags
        expect_error(fill(distributed_lag(d = 2, k = 1, lag = 0), 1:2))
        expect_error(fill(distributed_lag(d = 2, k = 1, lag = 1), 1:4))
        expect_error(fill(distributed_lag(d = 2, k = 1, lag = 1), 1:6, parameters_only = FALSE))
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

        # Too many parameters provided for the distributed lag model
        expect_warning(fill(distributed_lag(d = 2, k = 2), 1:20))
        expect_warning(fill(distributed_lag(d = 2, k = 2, lag = 1), 1:20))

        # = with explicit lags
        expect_warning(fill(distributed_lag(d = 2, k = 1, lag = 0), 1:20))
        expect_warning(fill(distributed_lag(d = 2, k = 1, lag = 1), 1:20))
        expect_warning(fill(distributed_lag(d = 2, k = 1, lag = 1), 1:20, parameters_only = FALSE))
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

test_that(
    "Check output for fill: Distributed lag",
    {        
        ########################################################################
        # Single dimension and single predictor

        my_model <- distributed_lag(d = 1, k = 1)
        tst <- fill(my_model, 5:7, parameters_only = FALSE, cholesky = FALSE)

        expect_equal(tst@parameters[["alpha"]], 5)
        expect_equal(tst@parameters[["beta"]], as.matrix(6))
        expect_equal(tst@covariance, as.matrix(7))

        # With Cholesky decomposition
        tst <- fill(my_model, 5:7, parameters_only = FALSE, cholesky = TRUE)

        expect_equal(tst@parameters[["alpha"]], 5)
        expect_equal(tst@parameters[["beta"]], as.matrix(6))
        expect_equal(tst@covariance, as.matrix(49))

        # Only the parameters
        tst <- fill(my_model, 5:6, parameters_only = TRUE)

        expect_equal(tst@parameters[["alpha"]], 5)
        expect_equal(tst@parameters[["beta"]], as.matrix(6))
        expect_equal(tst@covariance, as.matrix(0))



        ########################################################################
        # Single dimension, single predictor, and two lags

        my_model <- distributed_lag(d = 1, k = 1, lag = 2)
        tst <- fill(my_model, 5:9, parameters_only = FALSE, cholesky = FALSE)

        expect_equal(tst@parameters[["alpha"]], 5)
        expect_equal(tst@parameters[["beta"]], matrix(6:8, nrow = 1))
        expect_equal(tst@covariance, as.matrix(9))



        ########################################################################
        # Two dimensions and single predictor

        my_model <- distributed_lag(d = 2, k = 1)

        # Isotropic
        tst <- fill(
            my_model, 
            5:10, 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@covariance, diag(2) * 9:10)

        # Symmetric
        tst <- fill(
            my_model, 
            5:11, 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@covariance, matrix(c(9, 10, 10, 11), nrow = 2, ncol = 2))



        ########################################################################
        # Two dimensions, two predictors, and one lag

        my_model <- distributed_lag(d = 2, k = 2, lag = 1)

        # Isotropic
        tst <- fill(
            my_model, 
            5:16, 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:14, nrow = 2, ncol = 4))
        expect_equal(tst@covariance, diag(2) * 15:16)

        # Symmetric
        tst <- fill(
            my_model, 
            5:17, 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:14, nrow = 2, ncol = 4))
        expect_equal(tst@covariance, matrix(c(15, 16, 16, 17), nrow = 2, ncol = 2))

        # Only the parameters
        tst <- fill(my_model, 5:14, parameters_only = TRUE)

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:14, nrow = 2, ncol = 4))
        expect_equal(tst@covariance, matrix(0, nrow = 2, ncol = 2))



        ########################################################################
        # Explicit lags: Single dimension and single predictor

        # lag = 0
        my_model <- distributed_lag(d = 1, k = 1, lag = 0)
        tst <- fill(my_model, 5:7, parameters_only = FALSE, cholesky = FALSE)

        expect_equal(tst@parameters[["alpha"]], 5)
        expect_equal(tst@parameters[["beta"]], as.matrix(6))
        expect_equal(tst@covariance, as.matrix(7))

        # lag = 1
        my_model <- distributed_lag(d = 1, k = 1, lag = 1)
        tst <- fill(my_model, 5:8, parameters_only = FALSE, cholesky = FALSE)

        expect_equal(tst@parameters[["alpha"]], 5)
        expect_equal(tst@parameters[["beta"]], matrix(6:7, nrow = 1))
        expect_equal(tst@covariance, as.matrix(8))



        ########################################################################
        # Explicit lags: Two dimensions and single predictor

        # lag = 0
        my_model <- distributed_lag(d = 2, k = 1, lag = 0)
        tst <- fill(
            my_model, 
            5:10, 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:8, nrow = 2, ncol = 1))
        expect_equal(tst@covariance, diag(2) * 9:10)

        # lag = 1
        my_model <- distributed_lag(d = 2, k = 1, lag = 1)
        tst <- fill(
            my_model, 
            5:12, 
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
        expect_equal(tst@covariance, diag(2) * 11:12)

        tst <- fill(
            my_model, 
            5:13, 
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )

        expect_equal(tst@parameters[["alpha"]], 5:6)
        expect_equal(tst@parameters[["beta"]], matrix(7:10, nrow = 2, ncol = 2))
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

test_that(
    "Check output of count_parameters: Distributed lag", 
    {
        # Univariate, single predictor
        my_model <- distributed_lag(
            parameters = list(
                "alpha" = 0,
                "beta" = as.matrix(0)
            ),
            covariance = as.matrix(0)
        )
        expect_equal(
            count_parameters(my_model, parameters_only = FALSE),
            3
        )
        expect_equal(
            count_parameters(my_model, parameters_only = TRUE),
            2
        )

        # Univariate, two predictors
        my_model <- distributed_lag(
            parameters = list(
                "alpha" = 0,
                "beta" = matrix(0, nrow = 1, ncol = 2)
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

        # Univariate, single predictor, two lags
        my_model <- distributed_lag(
            lag = 2,
            parameters = list(
                "alpha" = 0,
                "beta" = matrix(0, nrow = 1, ncol = 3)
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
        my_model <- distributed_lag(
            parameters = list(
                "alpha" = numeric(2),
                "beta" = matrix(0, nrow = 2, ncol = 1)
            ),
            covariance = diag(2)
        )
        expect_equal(
            count_parameters(
                my_model, 
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            7
        )
        expect_equal(
            count_parameters(
                my_model, 
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            6
        )
        expect_equal(
            count_parameters(
                my_model, 
                parameters_only = TRUE
            ),
            4
        )

        # Bivariate, two predictors, one lag
        my_model <- distributed_lag(d = 2, k = 2, lag = 1)
        expect_equal(
            count_parameters(
                my_model, 
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            13
        )
        expect_equal(
            count_parameters(
                my_model, 
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            12
        )
        expect_equal(
            count_parameters(
                my_model, 
                parameters_only = TRUE
            ),
            10
        )

        # Trivariate, two predictors, two lags
        my_model <- distributed_lag(d = 3, k = 2, lag = 2)
        expect_equal(
            count_parameters(
                my_model, 
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            27
        )
        expect_equal(
            count_parameters(
                my_model, 
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            24
        )
        expect_equal(
            count_parameters(
                my_model, 
                parameters_only = TRUE
            ),
            21
        )

        # The number of parameters corresponds to the one in the model when 
        # the covariance is symmetric
        expect_equal(
            count_parameters(my_model, covariance = "symmetric"),
            my_model@n
        )

        # Dynamics do not influence the result, as there are no dynamical 
        # matrices in the model
        expect_equal(
            count_parameters(my_model, dynamics = "anisotropic"),
            count_parameters(my_model)
        )

        # Explicit lags: Univariate, single predictor
        my_model <- distributed_lag(d = 1, k = 1, lag = 0)
        expect_equal(count_parameters(my_model, parameters_only = FALSE), 3)
        expect_equal(count_parameters(my_model, parameters_only = TRUE), 2)

        my_model <- distributed_lag(d = 1, k = 1, lag = 1)
        expect_equal(count_parameters(my_model, parameters_only = FALSE), 4)
        expect_equal(count_parameters(my_model, parameters_only = TRUE), 3)

        # Explicit lags: Bivariate, single predictor
        my_model <- distributed_lag(d = 2, k = 1, lag = 0)
        expect_equal(count_parameters(my_model, covariance = "symmetric", parameters_only = FALSE), 7)
        expect_equal(count_parameters(my_model, covariance = "isotropic", parameters_only = FALSE), 6)
        expect_equal(count_parameters(my_model, parameters_only = TRUE), 4)

        my_model <- distributed_lag(d = 2, k = 1, lag = 1)
        expect_equal(count_parameters(my_model, covariance = "symmetric", parameters_only = FALSE), 9)
        expect_equal(count_parameters(my_model, covariance = "isotropic", parameters_only = FALSE), 8)
        expect_equal(count_parameters(my_model, parameters_only = TRUE), 6)
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
        expect_error(
            get_bounds(
                distributed_lag(),
                lower = 1, 
                upper = 1,
                parameters_only = TRUE
            )
        )
        expect_error(
            get_bounds(
                distributed_lag(),
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

        expect_warning(
            get_bounds(
                distributed_lag(),
                lower = 1:20,
                upper = 1:20,
                parameters_only = FALSE
            )
        )
        expect_warning(
            get_bounds(
                distributed_lag(),
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

test_that(
    "Check the output of get_bounds: Distributed lag",
    {
        # One dimension, one predictor
        expect_equal(
            get_bounds(
                distributed_lag(d = 1, k = 1),
                lower = -(1:2),
                upper = 1:2,
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -2),
                "upper" = c(1, 2)
            )
        )
        expect_equal(
            get_bounds(
                distributed_lag(d = 1, k = 1),
                lower = -(1:3),
                upper = 1:3,
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -2, -3),
                "upper" = c(1, 2, 3)
            )
        )

        # One dimension, two predictors
        expect_equal(
            get_bounds(
                distributed_lag(d = 1, k = 2),
                lower = -(1:2),
                upper = 1:2,
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -2, -2),
                "upper" = c(1, 2, 2)
            )
        )
        expect_equal(
            get_bounds(
                distributed_lag(d = 1, k = 2),
                lower = -(1:3),
                upper = 1:3,
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -2, -2, -3),
                "upper" = c(1, 2, 2, 3)
            )
        )

        # One dimension, one predictor, two lags
        expect_equal(
            get_bounds(
                distributed_lag(d = 1, k = 1, lag = 2),
                lower = -(1:3),
                upper = 1:3,
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -2, -2, -2, -3),
                "upper" = c(1, 2, 2, 2, 3)
            )
        )

        # Two dimensions, one predictor
        expect_equal(
            get_bounds(
                distributed_lag(d = 2, k = 1),
                lower = -(1:2),
                upper = 1:2,
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -1, -2, -2),
                "upper" = c(1, 1, 2, 2)
            )
        )
        expect_equal(
            get_bounds(
                distributed_lag(d = 2, k = 1),
                lower = -(1:3),
                upper = 1:3,
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -3, -3),
                "upper" = c(1, 1, 2, 2, 3, 3)
            )
        )
        expect_equal(
            get_bounds(
                distributed_lag(d = 2, k = 1),
                lower = -(1:3),
                upper = 1:3,
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -3, -3, -3),
                "upper" = c(1, 1, 2, 2, 3, 3, 3)
            )
        )

        # Two dimensions, two predictors, one lag
        expect_equal(
            get_bounds(
                distributed_lag(d = 2, k = 2, lag = 1),
                lower = -(1:2),
                upper = 1:2,
                parameters_only = TRUE
            ),
            list(
                "lower" = c(rep(-1, 2), rep(-2, 8)),
                "upper" = c(rep(1, 2), rep(2, 8))
            )
        )
        expect_equal(
            get_bounds(
                distributed_lag(d = 2, k = 2, lag = 1),
                lower = -(1:3),
                upper = 1:3,
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(rep(-1, 2), rep(-2, 8), rep(-3, 3)),
                "upper" = c(rep(1, 2), rep(2, 8), rep(3, 3))
            )
        )

        # Default bounds
        expect_equal(
            get_bounds(
                distributed_lag(d = 2, k = 1),
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -1, -5, -5),
                "upper" = c(1, 1, 5, 5)
            )
        )
        expect_equal(
            get_bounds(
                distributed_lag(d = 2, k = 1),
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -5, -5, 10^(-5), 10^(-5)),
                "upper" = c(1, 1, 5, 5, 1, 1)
            )
        )

        # Number of bounds corresponds to the number of parameters
        my_model <- distributed_lag(d = 3, k = 2, lag = 2)
        bounds <- get_bounds(my_model, covariance = "symmetric", parameters_only = FALSE)
        expect_equal(
            length(bounds[["lower"]]),
            count_parameters(my_model, covariance = "symmetric", parameters_only = FALSE)
        )
        expect_equal(
            length(bounds[["upper"]]),
            count_parameters(my_model, covariance = "symmetric", parameters_only = FALSE)
        )

        # Explicit lags: One dimension, one predictor
        expect_equal(
            get_bounds(
                distributed_lag(d = 1, k = 1, lag = 0),
                lower = -(1:3),
                upper = 1:3,
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -2, -3),
                "upper" = c(1, 2, 3)
            )
        )
        expect_equal(
            get_bounds(
                distributed_lag(d = 1, k = 1, lag = 1),
                lower = -(1:3),
                upper = 1:3,
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -2, -2, -3),
                "upper" = c(1, 2, 2, 3)
            )
        )

        # Explicit lags: Two dimensions, one predictor
        expect_equal(
            get_bounds(
                distributed_lag(d = 2, k = 1, lag = 0),
                lower = -(1:2),
                upper = 1:2,
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -1, -2, -2),
                "upper" = c(1, 1, 2, 2)
            )
        )
        expect_equal(
            get_bounds(
                distributed_lag(d = 2, k = 1, lag = 1),
                lower = -(1:2),
                upper = 1:2,
                parameters_only = TRUE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -2, -2),
                "upper" = c(1, 1, 2, 2, 2, 2)
            )
        )
        expect_equal(
            get_bounds(
                distributed_lag(d = 2, k = 1, lag = 1),
                lower = -(1:3),
                upper = 1:3,
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            list(
                "lower" = c(-1, -1, -2, -2, -2, -2, -3, -3, -3),
                "upper" = c(1, 1, 2, 2, 2, 2, 3, 3, 3)
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

test_that(
    "Check output of parameter_names: Distributed lag model",
    {
        # One dimension, one predictor
        expect_equal(
            parameter_names(
                distributed_lag(d = 1, k = 1),
                parameters_only = TRUE
            ),
            c("alpha_1", "beta_11_0")
        )
        expect_equal(
            parameter_names(
                distributed_lag(d = 1, k = 1),
                parameters_only = FALSE
            ),
            c("alpha_1", "beta_11_0", "sigma_11")
        )

        # One dimension, two predictors
        expect_equal(
            parameter_names(
                distributed_lag(d = 1, k = 2),
                parameters_only = TRUE
            ),
            c("alpha_1", "beta_11_0", "beta_12_0")
        )
        expect_equal(
            parameter_names(
                distributed_lag(d = 1, k = 2),
                parameters_only = FALSE
            ),
            c("alpha_1", "beta_11_0", "beta_12_0", "sigma_11")
        )

        # One dimension, one predictor, two lags
        expect_equal(
            parameter_names(
                distributed_lag(d = 1, k = 1, lag = 2),
                parameters_only = TRUE
            ),
            c("alpha_1", "beta_11_0", "beta_11_1", "beta_11_2")
        )

        # Two dimensions, one predictor
        expect_equal(
            parameter_names(
                distributed_lag(d = 2, k = 1),
                parameters_only = TRUE
            ),
            c("alpha_1", "alpha_2", "beta_11_0", "beta_21_0")
        )
        expect_equal(
            parameter_names(
                distributed_lag(d = 2, k = 1),
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c("alpha_1", "alpha_2", "beta_11_0", "beta_21_0", "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                distributed_lag(d = 2, k = 1),
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(
                "alpha_1", "alpha_2", "beta_11_0", "beta_21_0", 
                "sigma_11", "sigma_21", "sigma_22"
            )
        )

        # Two dimensions, two predictors, one lag
        ref <- c(
            "alpha_1", "alpha_2", 
            "beta_11_0", "beta_21_0", "beta_12_0", "beta_22_0",
            "beta_11_1", "beta_21_1", "beta_12_1", "beta_22_1"
        )
        expect_equal(
            parameter_names(
                distributed_lag(d = 2, k = 2, lag = 1),
                parameters_only = TRUE
            ),
            ref
        )
        expect_equal(
            parameter_names(
                distributed_lag(d = 2, k = 2, lag = 1),
                covariance = "isotropic",
                parameters_only = FALSE
            ),
            c(ref, "sigma_11", "sigma_22")
        )
        expect_equal(
            parameter_names(
                distributed_lag(d = 2, k = 2, lag = 1),
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(ref, "sigma_11", "sigma_21", "sigma_22")
        )

        # Number of names corresponds to the number of parameters
        my_model <- distributed_lag(d = 3, k = 2, lag = 2)
        expect_equal(
            length(parameter_names(my_model, covariance = "symmetric", parameters_only = FALSE)),
            count_parameters(my_model, covariance = "symmetric", parameters_only = FALSE)
        )

        # Explicit lags: One dimension, one predictor
        expect_equal(
            parameter_names(
                distributed_lag(d = 1, k = 1, lag = 0),
                parameters_only = FALSE
            ),
            c("alpha_1", "beta_11_0", "sigma_11")
        )
        expect_equal(
            parameter_names(
                distributed_lag(d = 1, k = 1, lag = 1),
                parameters_only = FALSE
            ),
            c("alpha_1", "beta_11_0", "beta_11_1", "sigma_11")
        )

        # Explicit lags: Two dimensions, one predictor
        expect_equal(
            parameter_names(
                distributed_lag(d = 2, k = 1, lag = 0),
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(
                "alpha_1", "alpha_2", "beta_11_0", "beta_21_0", 
                "sigma_11", "sigma_21", "sigma_22"
            )
        )
        expect_equal(
            parameter_names(
                distributed_lag(d = 2, k = 1, lag = 1),
                parameters_only = TRUE
            ),
            c(
                "alpha_1", "alpha_2", 
                "beta_11_0", "beta_21_0", "beta_11_1", "beta_21_1"
            )
        )
        expect_equal(
            parameter_names(
                distributed_lag(d = 2, k = 1, lag = 1),
                covariance = "symmetric",
                parameters_only = FALSE
            ),
            c(
                "alpha_1", "alpha_2", 
                "beta_11_0", "beta_21_0", "beta_11_1", "beta_21_1",
                "sigma_11", "sigma_21", "sigma_22"
            )
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

test_that(
    "Check the output of parameters: Distributed lag",
    {
        # Symmetric
        model <- fill(
            distributed_lag(d = 2, k = 2, lag = 1),
            1:13,
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, covariance = "symmetric"),
            1:13
        )

        # Isotropic
        model <- fill(
            distributed_lag(d = 2, k = 2, lag = 1),
            1:12,
            covariance = "isotropic",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, covariance = "isotropic"),
            1:12
        )

        # Only the parameters
        model <- fill(
            distributed_lag(d = 2, k = 2, lag = 1),
            1:10,
            parameters_only = TRUE
        )
        expect_equal(
            parameters(model, parameters_only = TRUE),
            1:10
        )

        # Explicit lags: lag = 0
        model <- fill(
            distributed_lag(d = 2, k = 1, lag = 0),
            1:7,
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, covariance = "symmetric"),
            1:7
        )

        # Explicit lags: lag = 1
        model <- fill(
            distributed_lag(d = 2, k = 1, lag = 1),
            1:9,
            covariance = "symmetric",
            parameters_only = FALSE,
            cholesky = FALSE
        )
        expect_equal(
            parameters(model, covariance = "symmetric"),
            1:9
        )
    }
)

test_that(
    "Check that parameters does not return covariances when parameters_only = TRUE",
    {
        # Models with non-zero covariance matrices
        my_models <- list(
            exponential(
                parameters = list(
                    "alpha" = c(1, -1),
                    "beta" = matrix(2, nrow = 2, ncol = 1),
                    "gamma" = diag(2) * 0.5
                ),
                covariance = matrix(c(1, 0.25, 0.25, 1), nrow = 2, ncol = 2)
            ),
            distributed_lag(
                lag = 1,
                parameters = list(
                    "alpha" = c(1, -1),
                    "beta" = cbind(diag(2), diag(2) * 0.5)
                ),
                covariance = matrix(c(1, 0.25, 0.25, 1), nrow = 2, ncol = 2)
            )
        )

        # Only the deterministic parameters are returned
        expect_equal(
            parameters(my_models[[1]], parameters_only = TRUE),
            c(1, -1, 2, 2, 0.5, 0.5)
        )
        expect_equal(
            parameters(my_models[[2]], parameters_only = TRUE),
            c(1, -1, 1, 0, 0, 1, 0.5, 0, 0, 0.5)
        )

        # The covariances are still included when parameters_only = FALSE
        expect_equal(
            parameters(my_models[[1]], parameters_only = FALSE),
            c(1, -1, 2, 2, 0.5, 0.5, 1, 0.25, 1)
        )
        expect_equal(
            parameters(my_models[[2]], parameters_only = FALSE),
            c(1, -1, 1, 0, 0, 1, 0.5, 0, 0, 0.5, 1, 0.25, 1)
        )

        # The number of parameters corresponds to count_parameters
        for(my_model in my_models) {
            expect_equal(
                length(parameters(my_model, parameters_only = TRUE)),
                count_parameters(my_model, parameters_only = TRUE)
            )
            expect_equal(
                length(parameters(my_model, parameters_only = FALSE)),
                count_parameters(my_model, parameters_only = FALSE)
            )
        }
    }
)

test_that(
    "Known errors in constructing a model",
    {
        covariance <- matrix(0, nrow = 2, ncol = 2)
        uneven <- matrix(0, nrow = 3, ncol = 2)

        # Dimensionalities are not correct
        expect_error(model(-1, 2))
        expect_error(model(2, -1))
        expect_error(model(0, 2))

        # Dimensionalities are not whole numbers
        expect_error(model(1.4, 2))
        expect_error(model(2, 1.4))
        expect_error(model(1.4, 1.4))

        # Parameters are not defined
        expect_error(model(2, 2, parameters = NULL, covariance = covariance))
        expect_error(model(2, 2, parameters = NA, covariance = covariance))
        expect_error(model(2, 2, parameters = list(), covariance = NA))

        # Dimensionality of the covariance matrix and the dimensionality of 
        # that is provided do not match
        expect_error(model(1, 2, parameters = list(), covariance = covariance))
        expect_error(model(3, 2, parameters = list(), covariance = covariance))
        expect_error(model(2, 2, parameters = list(), covariance = uneven))

        # Missing values in the parameters and covariances
        expect_error(model(2, 2, parameters = list(NA), covariance = covariance))
        expect_error(model(2, 2, parameters = list(), covariance = matrix(NA, nrow = 2, ncol = 2)))

        # Improper definition of the slots
        expect_error(model("2", 2, parameters = list(), covariance = covariance)) 
        expect_error(model(TRUE, 2, parameters = list(), covariance = covariance))
        expect_error(model(2, "2", parameters = list(), covariance = covariance)) 
        expect_error(model(2, TRUE, parameters = list(), covariance = covariance)) 
        expect_error(model(2, 2, parameters = "params", covariance = covariance)) 
        expect_error(model(2, 2, parameters = 1, covariance = covariance)) 
        expect_error(model(2, 2, parameters = TRUE, covariance = covariance)) 
        expect_error(model(2, 2, parameters = list(), covariance = "cov")) 
        expect_error(model(2, 2, parameters = list(), covariance = 1)) 
        expect_error(model(2, 2, parameters = list(), covariance = TRUE)) 

        # Parameters should be numeric
        expect_error(model(2, 2, parameters = list("1", 2, 3), covariance = covariance))
        expect_error(model(2, 2, parameters = list(TRUE, 2, 3), covariance = covariance))
    }
)

test_that(
    "Known warnings in constructing a model",
    {
        # Covariance is not defined, but the dimensionality is provided: In this
        # case, the covariance should be an empty matrix
        expect_warning(model(2, 2, parameters = list(), covariance = NULL))
        
        tst <- model(2, 2, parameters = list(), covariance = NULL) |>
            suppressWarnings()
        expect_equal(
            matrix(0, nrow = 2, ncol = 2), 
            tst@covariance
        )
    }
)

test_that(
    "Known cases in which constructing a model works", 
    {
        expect_no_error(model(1, 0, parameters = list(), covariance = as.matrix(0)))
        expect_no_error(model())
    }
)

test_that(
    "Check properties of the model when constructed", 
    {
        # Check the prototype
        tst <- new("model")
        expect_equal(tst@d, 1)
        expect_equal(tst@k, 0)
        expect_equal(tst@parameters, list())
        expect_equal(tst@covariance, matrix(0, nrow = 1, ncol = 1))

        # Check one that is created by the user
        tst <- model(
            d = 2, 
            k = 10,
            parameters = list(matrix(10, nrow = 2, ncol = 2)),
            covariance = matrix(1, nrow = 2, ncol = 2)
        )
        expect_equal(tst@d, 2)
        expect_equal(tst@k, 10)
        expect_equal(tst@parameters, list(matrix(10, nrow = 2, ncol = 2)))
        expect_equal(tst@covariance, matrix(1, nrow = 2, ncol = 2))

        # Check the implied one by the defaults of the function
        tst <- model()
        expect_equal(tst@d, 1)
        expect_equal(tst@k, 0)
        expect_equal(tst@parameters, list())
        expect_equal(tst@covariance, matrix(0, nrow = 1, ncol = 1))
    }
)

test_that(
    "Test the specialized errors for the exponential discounting model",
    {
        # Not all parameters are defined
        expect_error(exponential(parameters = list("alpha" = 1, "beta" = 1)))
        expect_error(exponential(parameters = list("beta" = 1, "gamma" = 0.5)))
        expect_error(exponential(parameters = list("alpha" = 1, "gamma" = 0.5)))

        # Gamma should be a matrix and cannot be corrected to one
        params <- list(
            "alpha" = numeric(2),
            "beta" = matrix(1, nrow = 2, ncol = 1),
            "gamma" = c(0.5, 0.5)
        )
        expect_error(exponential(parameters = params, covariance = diag(2)))

        # Covariance should be a matrix and cannot be corrected to one
        params <- list(
            "alpha" = numeric(2),
            "beta" = diag(2),
            "gamma" = diag(2) * 0.5
        )
        expect_error(exponential(parameters = params, covariance = c(1, 1)))

        # Dimensionality doesn't match up with the parameters: Error in d, k
        correct <- list(
            "alpha" = numeric(2),
            "beta" = matrix(1, nrow = 2, ncol = 5),
            "gamma" = diag(2) * 0.5
        )

        params <- correct
        expect_error(exponential(d = 1, parameters = params, covariance = diag(2)))
        expect_error(exponential(k = 4, parameters = params, covariance = diag(2)))

        # Dimensionality doesn't match up with parameters: Error in parameters
        params[["alpha"]] <- numeric(1)
        expect_error(exponential(d = 2, parameters = params, covariance = diag(2)))

        params <- correct
        params[["beta"]] <- matrix(1, nrow = 1, ncol = 5)
        expect_error(exponential(d = 2, k = 5, parameters = params, covariance = diag(2)))

        params <- correct
        params[["beta"]] <- matrix(1, nrow = 2, ncol = 4)
        expect_error(exponential(d = 2, k = 5, parameters = params, covariance = diag(2)))

        params <- correct
        params[["gamma"]] <- diag(1) * 0.5
        expect_error(exponential(d = 2, k = 5, parameters = params, covariance = diag(2)))

        params <- correct
        expect_error(exponential(d = 2, k = 5, parameters = params, covariance = diag(1)))

        # Dimensionality doesn't match up with parameters: Error in inconsistency
        params <- correct
        params[["alpha"]] <- numeric(1)
        expect_error(exponential(parameters = params, covariance = diag(2)))

        params <- correct
        params[["beta"]] <- matrix(1, nrow = 1, ncol = 5)
        expect_error(exponential(parameters = params, covariance = diag(2)))

        params <- correct
        params[["gamma"]] <- diag(1) * 0.5
        expect_error(exponential(parameters = params, covariance = diag(2)))

        params <- correct
        expect_error(exponential(parameters = params, covariance = diag(1)))

        # Eigenvalues of \Gamma should lie between 0 and 1
        params <- correct
        params[["gamma"]] <- diag(2) * -0.5
        expect_error(exponential(parameters = params, covariance = diag(2)))

        params[["gamma"]] <- diag(2) * 1.5
        expect_error(exponential(parameters = params, covariance = diag(2)))

        params[["gamma"]] <- c(0.5, 075, 0.75, 0.5) |>
            matrix(nrow = 2, ncol = 2)
        expect_error(exponential(parameters = params, covariance = diag(2)))
    }
)

test_that(
    "Test the specialized warnings for the exponential discounting model",
    {
        # Too many parameters are defined
        params <- list(
            "alpha" = numeric(2),
            "beta" = matrix(1, nrow = 2, ncol = 3),
            "gamma" = diag(2) * 0.5,
            "tst" = diag(2)
        )
        expect_warning(exponential(parameters = params, covariance = diag(2)))

        tst <- exponential(parameters = params, covariance = diag(2)) |>
            suppressWarnings()
        expect_equal(tst@d, 2)
        expect_equal(tst@k, 3)
        expect_equal(tst@parameters[["alpha"]], numeric(2))
        expect_equal(tst@parameters[["beta"]], matrix(1, nrow = 2, ncol = 3))
        expect_equal(tst@parameters[["gamma"]], diag(2) * 0.5)
        expect_equal(names(tst@parameters), c("alpha", "beta", "gamma"))
        expect_equal(tst@covariance, diag(2))

        # Gamma should be a matrix, but only one value is defined
        params <- list(
            "alpha" = numeric(1),
            "beta" = matrix(1, nrow = 1, ncol = 1),
            "gamma" = 0.5
        )
        expect_warning(exponential(parameters = params, covariance = diag(1)))

        tst <- exponential(parameters = params, covariance = diag(1)) |>
            suppressWarnings()
        expect_equal(tst@d, 1)
        expect_equal(tst@k, 1)
        expect_equal(tst@parameters[["gamma"]], matrix(0.5, nrow = 1, ncol = 1))

        # Beta should be a matrix, but can have one column
        params <- list(
            "alpha" = numeric(1),
            "beta" = numeric(10),
            "gamma" = matrix(0.5, nrow = 1, ncol = 1)
        )
        expect_warning(exponential(parameters = params, covariance = diag(1)))

        tst <- exponential(parameters = params, covariance = diag(1)) |>
            suppressWarnings()
        expect_equal(tst@d, 1)
        expect_equal(tst@k, 10)
        expect_equal(tst@parameters[["beta"]], matrix(0, nrow = 1, ncol = 10))

        # Covariance should be a matrix, but one value is defined
        params <- list(
            "alpha" = numeric(1),
            "beta" = diag(1),
            "gamma" = diag(1) * 0.5
        )
        expect_warning(exponential(parameters = params, covariance = 1))

        tst <- exponential(parameters = params, covariance = 1) |>
            suppressWarnings()
        expect_equal(tst@d, 1)
        expect_equal(tst@k, 1)
        expect_equal(tst@covariance, diag(1))
    }
)

test_that(
    "Check properties of the exponential discounting model when constructed", 
    {
        # Check the prototype
        tst <- new("exponential")
        ref <- list(
            "alpha" = 0,
            "beta" = matrix(0, nrow = 1, ncol = 1),
            "gamma" = matrix(0, nrow = 1, ncol = 1)
        )
        expect_equal(tst@d, 1)
        expect_equal(tst@k, 1)
        expect_equal(tst@parameters, ref)
        expect_equal(tst@covariance, matrix(0, nrow = 1, ncol = 1))

        # Check one that is created by the user
        ref <- list(
            "alpha" = numeric(2),
            "beta" = matrix(5, nrow = 2, ncol = 5),
            "gamma" = diag(2) * 0.5
        )
        tst <- exponential(
            d = 2, 
            k = 5,
            parameters = ref,
            covariance = diag(2)
        )
        expect_equal(tst@d, 2)
        expect_equal(tst@k, 5)
        expect_equal(tst@parameters, ref)
        expect_equal(tst@covariance, diag(2))

        # Check the implied one by the defaults of the function
        tst <- exponential()
        ref <- list(
            "alpha" = 0,
            "beta" = matrix(0, nrow = 1, ncol = 1),
            "gamma" = matrix(0, nrow = 1, ncol = 1)
        )
        expect_equal(tst@d, 1)
        expect_equal(tst@k, 1)
        expect_equal(tst@parameters, ref)
        expect_equal(tst@covariance, matrix(0, nrow = 1, ncol = 1))

        # Dimensionality can be correctly inferred from the parameters
        params <- list(
            "alpha" = numeric(10),
            "beta" = matrix(1, nrow = 10, ncol = 3),
            "gamma" = diag(10) * 0.5
        )
        covariance <- diag(10)

        tst <- exponential(parameters = params, covariance = covariance)
        expect_equal(tst@d, 10)
        expect_equal(tst@k, 3)

        # Check whether the Cholesky decomposition works
        params <- list(
            "alpha" = numeric(2),
            "beta" = matrix(1, nrow = 2, ncol = 3),
            "gamma" = diag(2) * 0.5
        )
        ref <- diag(2)
        ref[c(2, 3)] <- 0.25

        G <- chol(ref) |>
            t()

        tst <- exponential(parameters = params, covariance = G, cholesky = TRUE)
        expect_equal(
            tst@covariance, 
            ref,
            tolerance = 1e-2
        )

        # Check the class of the model
        expect_equal(class(tst), "exponential")
    }
)
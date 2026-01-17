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

        # Not all parameters are defined
        expect_error(exponential(parameters = list("alpha" = 1, "beta" = 1)))
        expect_error(exponential(parameters = list("beta" = 1, "gamma" = 0.5)))
        expect_error(exponential(parameters = list("alpha" = 1, "gamma" = 0.5)))

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
    }
)

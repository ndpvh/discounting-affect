test_that(
    "Test known errors",
    {
        # Dimensionalities don't conform
        expect_error(recovery(exponential(d = 1, k = 1), exponential(d = 2, k = 1)))
        expect_error(recovery(exponential(d = 1, k = 1), exponential(d = 1, k = 2)))
        expect_error(recovery(exponential(d = 1, k = 1), exponential(d = 2, k = 2)))

        # fit_model is not a model
        expect_error(recovery(exponential(), NULL))
        expect_error(recovery(exponential(), logical(10)))
        expect_error(recovery(exponential(), character(10)))
        expect_error(recovery(exponential(), numeric(10)))
    }
)
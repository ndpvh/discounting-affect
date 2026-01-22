test_that(
    "Test known errors",
    {
        # Dimensionalities don't conform
        expect_error(recover(exponential(d = 1, k = 1), exponential(d = 2, k = 1)))
        expect_error(recover(exponential(d = 1, k = 1), exponential(d = 1, k = 2)))
        expect_error(recover(exponential(d = 1, k = 1), exponential(d = 2, k = 2)))
    }
)
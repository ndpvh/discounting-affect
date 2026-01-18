test_that(
    "Test known errors in constructing the dataset class",
    {
        data <- data.frame(
            y_1 = rep(1, each = 10),
            y_2 = rep(2, each = 10),
            y_3 = rep(3, each = 10),
            x_1 = rep(4, each = 10),
            x_2 = rep(5, each = 10),
            x_3 = rep(6, each = 10)
        )
        
        # No y_cols or x_cols are provided
        expect_error(dataset(data, x_cols = "x_1"))
        expect_error(dataset(data, y_cols = "y_1"))

        # The columns are not numeric
        correct <- data

        data <- correct
        data$y_1 <- as.character(data$y_1)
        expect_error(dataset(data, y_cols = "y_1", x_cols = "x_1"))
        expect_error(dataset(data, y_cols = c("y_1", "y_2"), x_cols = c("x_1", "x_2")))

        data <- correct
        data$y_1 <- as.logical(data$y_1)
        expect_error(dataset(data, y_cols = "y_1", x_cols = "x_1"))
        expect_error(dataset(data, y_cols = c("y_1", "y_2"), x_cols = c("x_1", "x_2")))

        data <- correct
        data$x_1 <- as.character(data$x_1)
        expect_error(dataset(data, y_cols = "y_1", x_cols = "x_1"))
        expect_error(dataset(data, y_cols = c("y_1", "y_2"), x_cols = c("x_1", "x_2")))

        data <- correct
        data$x_1 <- as.logical(data$x_1)
        expect_error(dataset(data, y_cols = "y_1", x_cols = "x_1"))
        expect_error(dataset(data, y_cols = c("y_1", "y_2"), x_cols = c("x_1", "x_2")))
    }
)

test_that(
    "Test known warnings in constructing the dataset class",
    {
        # When no data is provided, we should get the prototype
        expect_warning(dataset())

        tst <- dataset() |>
            suppressWarnings()
        expect_equal(tst@Y, matrix(0, nrow = 0, ncol = 0))
        expect_equal(tst@X, matrix(0, nrow = 0, ncol = 0))
    }
)

test_that(
    "Test the properties of the dataset class",
    {
        # Test the values of the prototype
        tst <- new("dataset")
        expect_equal(tst@Y, matrix(0, nrow = 0, ncol = 0))
        expect_equal(tst@X, matrix(0, nrow = 0, ncol = 0))

        # Check the slots
        data <- data.frame(
            y_1 = rep(1, each = 10),
            y_2 = rep(2, each = 10),
            y_3 = rep(3, each = 10),
            x_1 = rep(4, each = 10),
            x_2 = rep(5, each = 10),
            x_3 = rep(6, each = 10)
        )

        tst <- dataset(data, y_cols = "y_1", x_cols = "x_1")
        expect_equal(
            tst@Y, 
            matrix(1, nrow = 10, ncol = 1) |>
                `colnames<-` ("y_1")
        )
        expect_equal(
            tst@X, 
            matrix(4, nrow = 10, ncol = 1) |>
                `colnames<-` ("x_1")
        )

        tst <- dataset(
            data,
            y_cols = c("y_1", "y_2", "y_3"),
            x_cols = c("x_1", "x_2", "x_3")
        )
        expect_equal(
            tst@Y, 
            matrix(1:3, nrow = 10, ncol = 3, byrow = TRUE) |>
                `colnames<-` (c("y_1", "y_2", "y_3"))
        )
        expect_equal(
            tst@X, 
            matrix(4:6, nrow = 10, ncol = 3, byrow = TRUE) |>
                `colnames<-` (c("x_1", "x_2", "x_3"))
        )

        # Test whether the sorting works
        data <- data.frame(
            time = 10:1,
            y_1 = 1:10, 
            y_2 = 1:10, 
            x_1 = 1:10,
            x_2 = 1:10
        )
        
        tst <- dataset(
            data, 
            y_cols = "y_1",
            x_cols = "x_1",
            sorting_variable = "time"
        )
        expect_equal(
            tst@Y, 
            matrix(10:1, nrow = 10, ncol = 1) |>
                `colnames<-` ("y_1")
        )
        expect_equal(
            tst@X, 
            matrix(10:1, nrow = 10, ncol = 1) |>
                `colnames<-` ("x_1")
        )

        tst <- dataset(
            data,
            y_cols = c("y_1", "y_2"),
            x_cols = c("x_1", "x_2"),
            sorting_variable = "time"
        )
        expect_equal(
            tst@Y, 
            matrix(10:1, nrow = 10, ncol = 2) |>
                `colnames<-` (c("y_1", "y_2"))
        )
        expect_equal(
            tst@X, 
            matrix(10:1, nrow = 10, ncol = 2) |>
                `colnames<-` (c("x_1", "x_2"))
        )
    }
)
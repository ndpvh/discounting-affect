test_that(
    "Predictions for the exponential discounting function hold up",
    {

        # Create an impulse dataset for dimensions k = 1, k = 2
        impulse <- c(1, rep(0, 9))
        impulse_1 <- dataset(X = matrix(impulse, ncol = 1))
        impulse_2 <- dataset(X = cbind(impulse, impulse))

        # Create a non-impulse dataset for dimensions k = 1, k = 2
        stimuli <- 1:10
        stimuli_1 <- dataset(X = matrix(stimuli, ncol = 1))
        stimuli_2 <- dataset(X = cbind(stimuli, stimuli))

        ########################################################################
        # d = 1, k = 1

        # Create the parameters
        my_model <- exponential(
            parameters = list(
                "alpha" = 1,
                "beta" = matrix(2, nrow = 1, ncol = 1),
                "gamma" = matrix(0.75, nrow = 1, ncol = 1)
            ),
            covariance = matrix(0, nrow = 1, ncol = 1)
        )

        # Perform the test with impulse-response sets
        tst <- predict(my_model, impulse_1)
        expect_equal(tst@X, impulse_1@X)
        expect_equal(tst@Y, matrix(1 + 0.75^seq(0, 9, 1) * 2, ncol = 1))

        # Perform the test with stimuli instead
        X <- stimuli_1@X
        X <- lapply(
            seq_len(nrow(X)), 
            function(i) {
                x <- numeric(nrow(X))
                x[1:i] <- rev(X[1:i])
                return(x)
            }
        )
        X <- do.call("rbind", X)

        tst <- predict(my_model, stimuli_1)
        expect_equal(tst@X, stimuli_1@X)
        expect_equal(
            tst@Y, 
            matrix(
                1 + 2 * X %*% 0.75^seq(0, 9, 1), 
                ncol = 1
            )
        )

        ########################################################################
        # d = 2, k = 1

        # Create the parameters
        my_model <- exponential(
            parameters = list(
                "alpha" = c(1, - 1),
                "beta" = matrix(2, nrow = 2, ncol = 1),
                "gamma" = diag(2) * 0.75
            ),
            covariance = matrix(0, nrow = 2, ncol = 2)
        )

        # Perform the test with impulse-response sets
        tst <- predict(my_model, impulse_1)
        expect_equal(tst@X, impulse_1@X)
        expect_equal(
            tst@Y, 
            cbind(
                1 + 0.75^seq(0, 9, 1) * 2,
                -1 + 0.75^seq(0, 9, 1) * 2
            )
        )

        # Perform the test with stimuli instead
        X <- stimuli_1@X
        X <- lapply(
            seq_len(nrow(X)), 
            function(i) {
                x <- numeric(nrow(X))
                x[1:i] <- rev(X[1:i])
                return(x)
            }
        )
        X <- do.call("rbind", X)

        tst <- predict(my_model, stimuli_1)
        expect_equal(tst@X, stimuli_1@X)
        expect_equal(
            tst@Y, 
            cbind(
                1 + 2 * X %*% 0.75^seq(0, 9, 1), 
                -1 + 2 * X %*% 0.75^seq(0, 9, 1)
            )
        )

        ########################################################################
        # d = 1, k = 2

        # Create the parameters
        B <- matrix(2:1, nrow = 1, ncol = 2)

        my_model <- exponential(
            parameters = list(
                "alpha" = 1,
                "beta" = B,
                "gamma" = matrix(0.75, nrow = 1, ncol = 1)
            ),
            covariance = matrix(0, nrow = 1, ncol = 1)
        )

        # Perform the test with impulse-response sets
        tst <- predict(my_model, impulse_2)
        expect_equal(tst@X, impulse_2@X)
        expect_equal(tst@Y, matrix(1 + 0.75^seq(0, 9, 1) * 3, ncol = 1))

        # Perform the test with stimuli instead
        X <- stimuli_2@X
        S <- lapply(
            seq_len(nrow(X)), 
            function(i) {
                x <- matrix(B %*% X[i, ], nrow = 1)
                return(x)
            }
        )
        S <- do.call("rbind", S)
        S <- sapply(
            seq_len(nrow(S)),
            function(i) {
                s <- (S[1:i, ] * rev(0.75^seq(0, i - 1, 1))) |>
                    sum()
                return(s)
            }
        )
        S <- matrix(S, ncol = 1)

        tst <- predict(my_model, stimuli_2)
        expect_equal(tst@X, stimuli_2@X)
        expect_equal(
            tst@Y, 
            1 + S
        )

        ########################################################################
        # d = 2, k = 2

        # Create the parameters
        B <- matrix(2, nrow = 2, ncol = 2)
        B[c(2, 3)] <- 1

        my_model <- exponential(
            parameters = list(
                "alpha" = c(1, - 1),
                "beta" = B,
                "gamma" = diag(2) * 0.75
            ),
            covariance = matrix(0, nrow = 2, ncol = 2)
        )

        # Perform the test with impulse-response sets
        tst <- predict(my_model, impulse_2)
        expect_equal(tst@X, impulse_2@X)
        expect_equal(
            tst@Y, 
            cbind(
                1 + 0.75^seq(0, 9, 1) * sum(B[1, ]),
                -1 + 0.75^seq(0, 9, 1) * sum(B[2, ])
            )
        )

        # Perform the test with stimuli instead
        X <- stimuli_2@X
        S <- lapply(
            seq_len(nrow(X)), 
            function(i) {
                x <- matrix(B %*% X[i, ], nrow = 1)
                return(x)
            }
        )
        S <- do.call("rbind", S)
        S <- lapply(
            seq_len(nrow(S)),
            function(i) {
                s <- (S[1:i, ] * rev(0.75^seq(0, i - 1, 1))) |>
                    matrix(ncol = 2) |>
                    colSums()
                return(s)
            }
        )
        S <- do.call("rbind", S)

        tst <- predict(my_model, stimuli_2)
        expect_equal(tst@X, stimuli_2@X)
        expect_equal(
            tst@Y, 
            cbind(
                1 + S[, 1], 
                -1 + S[, 2]
            )
        )
    }
)

test_that(
    "Predictions for the quasi-hyperbolic discounting function hold up",
    {

        # Create an impulse dataset for dimensions k = 1, k = 2
        impulse <- c(1, rep(0, 9))
        impulse_1 <- dataset(X = matrix(impulse, ncol = 1))
        impulse_2 <- dataset(X = cbind(impulse, impulse))

        # Create a non-impulse dataset for dimensions k = 1, k = 2
        stimuli <- 1:10
        stimuli_1 <- dataset(X = matrix(stimuli, ncol = 1))
        stimuli_2 <- dataset(X = cbind(stimuli, stimuli))

        ########################################################################
        # d = 1, k = 1

        # Create the parameters
        my_model <- quasi_hyperbolic(
            parameters = list(
                "alpha" = 1,
                "beta" = matrix(2, nrow = 1, ncol = 1),
                "nu" = matrix(0.75, nrow = 1, ncol = 1),
                "kappa" = matrix(0.5, nrow = 1, ncol = 1)
            ),
            covariance = matrix(0, nrow = 1, ncol = 1)
        )

        # Perform the test with impulse-response sets
        tst <- predict(my_model, impulse_1)
        expect_equal(tst@X, impulse_1@X)
        expect_equal(
            tst@Y, 
            matrix(1 + 0.5^c(0, rep(1, 9)) * 0.75^seq(0, 9, 1) * 2, ncol = 1)
        )

        # Perform the test with stimuli instead
        X <- stimuli_1@X
        X <- lapply(
            seq_len(nrow(X)), 
            function(i) {
                x <- numeric(nrow(X))
                x[1:i] <- rev(X[1:i])
                return(x)
            }
        )
        X <- do.call("rbind", X)

        tst <- predict(my_model, stimuli_1)
        expect_equal(tst@X, stimuli_1@X)
        expect_equal(
            tst@Y, 
            matrix(
                1 + 2 * X %*% (0.75^seq(0, 9, 1) * 0.5^c(0, rep(1, 9))), 
                ncol = 1
            )
        )

        ########################################################################
        # d = 2, k = 1

        # Create the parameters
        my_model <- quasi_hyperbolic(
            parameters = list(
                "alpha" = c(1, - 1),
                "beta" = matrix(2, nrow = 2, ncol = 1),
                "nu" = diag(2) * 0.75,
                "kappa" = diag(2) * 0.5
            ),
            covariance = matrix(0, nrow = 2, ncol = 2)
        )

        # Perform the test with impulse-response sets
        tst <- predict(my_model, impulse_1)
        expect_equal(tst@X, impulse_1@X)
        expect_equal(
            tst@Y, 
            cbind(
                1 + 0.75^seq(0, 9, 1) * 0.5^c(0, rep(1, 9)) * 2,
                -1 + 0.75^seq(0, 9, 1) * 0.5^c(0, rep(1, 9)) * 2
            )
        )

        # Perform the test with stimuli instead
        X <- stimuli_1@X
        X <- lapply(
            seq_len(nrow(X)), 
            function(i) {
                x <- numeric(nrow(X))
                x[1:i] <- rev(X[1:i])
                return(x)
            }
        )
        X <- do.call("rbind", X)

        tst <- predict(my_model, stimuli_1)
        expect_equal(tst@X, stimuli_1@X)
        expect_equal(
            tst@Y, 
            cbind(
                1 + 2 * X %*% (0.75^seq(0, 9, 1) * 0.5^c(0, rep(1, 9))), 
                -1 + 2 * X %*% (0.75^seq(0, 9, 1) * 0.5^c(0, rep(1, 9)))
            )
        )

        ########################################################################
        # d = 1, k = 2

        # Create the parameters
        B <- matrix(2:1, nrow = 1, ncol = 2)

        my_model <- quasi_hyperbolic(
            parameters = list(
                "alpha" = 1,
                "beta" = B,
                "nu" = matrix(0.75, nrow = 1, ncol = 1),
                "kappa" = matrix(0.5, nrow = 1, ncol = 1)
            ),
            covariance = matrix(0, nrow = 1, ncol = 1)
        )

        # Perform the test with impulse-response sets
        tst <- predict(my_model, impulse_2)
        expect_equal(tst@X, impulse_2@X)
        expect_equal(
            tst@Y, 
            matrix(1 + 0.75^seq(0, 9, 1) * 0.5^c(0, rep(1, 9)) * 3, ncol = 1)
        )

        # Perform the test with stimuli instead
        X <- stimuli_2@X
        S <- lapply(
            seq_len(nrow(X)), 
            function(i) {
                x <- matrix(B %*% X[i, ], nrow = 1)
                return(x)
            }
        )
        S <- do.call("rbind", S)
        S <- sapply(
            seq_len(nrow(S)),
            function(i) {
                kappa <- numeric(i)
                if(i != 1) {
                    kappa[2:i] <- 1
                }

                s <- (S[1:i, ] * rev(0.75^seq(0, i - 1, 1) * 0.5^kappa)) |>
                    sum()
                return(s)
            }
        )
        S <- matrix(S, ncol = 1)

        tst <- predict(my_model, stimuli_2)
        expect_equal(tst@X, stimuli_2@X)
        expect_equal(
            tst@Y, 
            1 + S
        )

        ########################################################################
        # d = 2, k = 2

        # Create the parameters
        B <- matrix(2, nrow = 2, ncol = 2)
        B[c(2, 3)] <- 1

        my_model <- quasi_hyperbolic(
            parameters = list(
                "alpha" = c(1, - 1),
                "beta" = B,
                "nu" = diag(2) * 0.75,
                "kappa" = diag(2) * 0.5
            ),
            covariance = matrix(0, nrow = 2, ncol = 2)
        )

        # Perform the test with impulse-response sets
        tst <- predict(my_model, impulse_2)
        expect_equal(tst@X, impulse_2@X)
        expect_equal(
            tst@Y, 
            cbind(
                1 + 0.75^seq(0, 9, 1) * 0.5^c(0, rep(1, 9)) * sum(B[1, ]),
                -1 + 0.75^seq(0, 9, 1) * 0.5^c(0, rep(1, 9)) * sum(B[2, ])
            )
        )

        # Perform the test with stimuli instead
        X <- stimuli_2@X
        S <- lapply(
            seq_len(nrow(X)), 
            function(i) {
                x <- matrix(B %*% X[i, ], nrow = 1)
                return(x)
            }
        )
        S <- do.call("rbind", S)
        S <- lapply(
            seq_len(nrow(S)),
            function(i) {
                kappa <- numeric(i)
                if(i != 1) {
                    kappa[2:i] <- 1
                }

                s <- (S[1:i, ] * rev(0.75^seq(0, i - 1, 1) * 0.5^kappa)) |>
                    matrix(ncol = 2) |>
                    colSums()
                return(s)
            }
        )
        S <- do.call("rbind", S)

        tst <- predict(my_model, stimuli_2)
        expect_equal(tst@X, stimuli_2@X)
        expect_equal(
            tst@Y, 
            cbind(
                1 + S[, 1], 
                -1 + S[, 2]
            )
        )
    }
)

test_that(
    "Predictions for the double exponential discounting function hold up",
    {

        # Create an impulse dataset for dimensions k = 1, k = 2
        impulse <- c(1, rep(0, 9))
        impulse_1 <- dataset(X = matrix(impulse, ncol = 1))
        impulse_2 <- dataset(X = cbind(impulse, impulse))

        # Create a non-impulse dataset for dimensions k = 1, k = 2
        stimuli <- 1:10
        stimuli_1 <- dataset(X = matrix(stimuli, ncol = 1))
        stimuli_2 <- dataset(X = cbind(stimuli, stimuli))

        ########################################################################
        # d = 1, k = 1

        # Create the parameters
        my_model <- double_exponential(
            parameters = list(
                "alpha" = 1,
                "beta" = matrix(2, nrow = 1, ncol = 1),
                "gamma" = matrix(0.75, nrow = 1, ncol = 1),
                "nu" = matrix(0.5, nrow = 1, ncol = 1),
                "omega" = 0.5
            ),
            covariance = matrix(0, nrow = 1, ncol = 1)
        )

        # Perform the test with impulse-response sets
        tst <- predict(my_model, impulse_1)
        expect_equal(tst@X, impulse_1@X)
        expect_equal(
            tst@Y, 
            matrix(
                1 + (0.5 * 0.75^seq(0, 9, 1) + 0.5 * 0.5^seq(0, 9, 1)) * 2, 
                ncol = 1
            )
        )

        # Perform the test with stimuli instead
        X <- stimuli_1@X
        X <- lapply(
            seq_len(nrow(X)), 
            function(i) {
                x <- numeric(nrow(X))
                x[1:i] <- rev(X[1:i])
                return(x)
            }
        )
        X <- do.call("rbind", X)

        tst <- predict(my_model, stimuli_1)
        expect_equal(tst@X, stimuli_1@X)
        expect_equal(
            tst@Y, 
            matrix(
                1 + 2 * X %*% (0.5 * 0.75^seq(0, 9, 1) + 0.5 * 0.5^seq(0, 9, 1)), 
                ncol = 1
            )
        )

        ########################################################################
        # d = 2, k = 1

        # Create the parameters
        my_model <- double_exponential(
            parameters = list(
                "alpha" = c(1, - 1),
                "beta" = matrix(2, nrow = 2, ncol = 1),
                "gamma" = diag(2) * 0.75,
                "nu" = diag(2) * 0.5,
                "omega" = 0.5
            ),
            covariance = matrix(0, nrow = 2, ncol = 2)
        )

        # Perform the test with impulse-response sets
        tst <- predict(my_model, impulse_1)
        expect_equal(tst@X, impulse_1@X)
        expect_equal(
            tst@Y, 
            cbind(
                1 + (0.5 * 0.75^seq(0, 9, 1) + 0.5 * 0.5^seq(0, 9, 1)) * 2,
                -1 + (0.5 * 0.75^seq(0, 9, 1) + 0.5 * 0.5^seq(0, 9, 1)) * 2
            )
        )

        # Perform the test with stimuli instead
        X <- stimuli_1@X
        X <- lapply(
            seq_len(nrow(X)), 
            function(i) {
                x <- numeric(nrow(X))
                x[1:i] <- rev(X[1:i])
                return(x)
            }
        )
        X <- do.call("rbind", X)

        tst <- predict(my_model, stimuli_1)
        expect_equal(tst@X, stimuli_1@X)
        expect_equal(
            tst@Y, 
            cbind(
                1 + 2 * X %*% (0.5 * 0.75^seq(0, 9, 1) + 0.5 * 0.5^seq(0, 9, 1)), 
                -1 + 2 * X %*% (0.5 * 0.75^seq(0, 9, 1) + 0.5 * 0.5^seq(0, 9, 1))
            )
        )

        ########################################################################
        # d = 1, k = 2

        # Create the parameters
        B <- matrix(2:1, nrow = 1, ncol = 2)

        my_model <- double_exponential(
            parameters = list(
                "alpha" = 1,
                "beta" = B,
                "gamma" = matrix(0.75, nrow = 1, ncol = 1),
                "nu" = matrix(0.5, nrow = 1, ncol = 1),
                "omega" = 0.5
            ),
            covariance = matrix(0, nrow = 1, ncol = 1)
        )

        # Perform the test with impulse-response sets
        tst <- predict(my_model, impulse_2)
        expect_equal(tst@X, impulse_2@X)
        expect_equal(
            tst@Y, 
            matrix(
                1 + (0.5 * 0.75^seq(0, 9, 1) + 0.5 * 0.5^seq(0, 9, 1)) * 3, 
                ncol = 1
            )
        )

        # Perform the test with stimuli instead
        X <- stimuli_2@X
        S <- lapply(
            seq_len(nrow(X)), 
            function(i) {
                x <- matrix(B %*% X[i, ], nrow = 1)
                return(x)
            }
        )
        S <- do.call("rbind", S)
        S <- sapply(
            seq_len(nrow(S)),
            function(i) {
                s <- (S[1:i, ] * rev((0.5 * 0.75^seq(0, i - 1, 1) + 0.5 * 0.5^seq(0, i - 1, 1)))) |>
                    sum()
                return(s)
            }
        )
        S <- matrix(S, ncol = 1)

        tst <- predict(my_model, stimuli_2)
        expect_equal(tst@X, stimuli_2@X)
        expect_equal(
            tst@Y, 
            1 + S
        )

        ########################################################################
        # d = 2, k = 2

        # Create the parameters
        B <- matrix(2, nrow = 2, ncol = 2)
        B[c(2, 3)] <- 1

        my_model <- double_exponential(
            parameters = list(
                "alpha" = c(1, - 1),
                "beta" = B,
                "gamma" = diag(2) * 0.75,
                "nu" = diag(2) * 0.5,
                "omega" = 0.5
            ),
            covariance = matrix(0, nrow = 2, ncol = 2)
        )

        # Perform the test with impulse-response sets
        tst <- predict(my_model, impulse_2)
        expect_equal(tst@X, impulse_2@X)
        expect_equal(
            tst@Y, 
            cbind(
                1 + (0.5 * 0.75^seq(0, 9, 1) + 0.5 * 0.5^seq(0, 9, 1)) * sum(B[1, ]),
                -1 + (0.5 * 0.75^seq(0, 9, 1) + 0.5 * 0.5^seq(0, 9, 1)) * sum(B[2, ])
            )
        )

        # Perform the test with stimuli instead
        X <- stimuli_2@X
        S <- lapply(
            seq_len(nrow(X)), 
            function(i) {
                x <- matrix(B %*% X[i, ], nrow = 1)
                return(x)
            }
        )
        S <- do.call("rbind", S)
        S <- lapply(
            seq_len(nrow(S)),
            function(i) {
                s <- (S[1:i, ] * rev((0.5 * 0.75^seq(0, i - 1, 1) + 0.5 * 0.5^seq(0, i - 1, 1)))) |>
                    matrix(ncol = 2) |>
                    colSums()
                return(s)
            }
        )
        S <- do.call("rbind", S)

        tst <- predict(my_model, stimuli_2)
        expect_equal(tst@X, stimuli_2@X)
        expect_equal(
            tst@Y, 
            cbind(
                1 + S[, 1], 
                -1 + S[, 2]
            )
        )
    }
)
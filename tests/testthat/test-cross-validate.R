test_that("Consecutive folds cover each observation exactly once", {
  folds <- make_folds(23, 5)

  expect_length(folds, 5)
  expect_equal(unlist(folds), seq_len(23))
  expect_true(all(lengths(folds) > 0))

  expect_error(make_folds(5, 1), "between 2 and n")
  expect_error(make_folds(5, 6), "between 2 and n")
})


test_that("Niemeijer folds preserve complete days", {
  folds <- make_block_folds(231, block_size = 11, k = 7)

  expect_equal(lengths(folds), rep(33L, 7))
  expect_equal(unlist(folds), seq_len(231))
  expect_true(all(vapply(folds, function(rows) {
    (min(rows) - 1L) %% 11L == 0L && max(rows) %% 11L == 0L
  }, logical(1))))

  expect_true(is.function(get_folds_function("NIEMEIJER_2022", 7)))
  expect_null(get_folds_function("VANHASBROECK_2022", 5))
})


test_that("Model predictions do not depend on observed affect ratings", {
  model <- exponential(
    parameters = list(
      alpha = 0,
      beta = matrix(1, nrow = 1),
      gamma = matrix(0.5, nrow = 1)
    ),
    covariance = matrix(1, nrow = 1)
  )

  set.seed(1)
  data <- simulate(model, X = matrix(rnorm(8), ncol = 1))
  masked <- data
  masked@Y[3:5, ] <- NA_real_

  expect_equal(predict(model, data)@Y, predict(model, masked)@Y)
})


test_that("Participant RMSE pools errors using observed fold sizes", {
  model <- exponential(
    parameters = list(
      alpha = 0,
      beta = matrix(1, nrow = 1),
      gamma = matrix(0.5, nrow = 1)
    ),
    covariance = matrix(1, nrow = 1)
  )

  set.seed(1)
  data <- simulate(model, X = matrix(rnorm(6), ncol = 1))

  # Supply known fold errors without running an optimizer.
  local_mocked_bindings(
    cv_one_fold = function(data, model_empty, test_rows, optimizer, ...) {
      n <- length(test_rows)
      list(rmse = n, n_ratings = n, parameters = n)
    }
  )

  result <- cv_participant(
    data,
    model,
    optimizer = NULL,
    folds = list(1, 2:3, 4:6)
  )

  expect_equal(unname(result$pooled_rmse), sqrt(6))
  expect_equal(unname(result$n_valid_folds), 3)
  expect_equal(unname(result$fold_rmse[, 1]), c(1, 2, 3))
})


test_that("RMSE comparison matches participants and excludes invalid fits", {
  a <- data.frame(
    participant_id = c("p1", "p2", "p3"),
    rmse_y1 = c(1, 2, 3),
    n_valid_folds_y1 = c(5, 2, 5)
  )

  b <- data.frame(
    participant_id = c("p3", "p1", "p2"),
    rmse_y1 = c(4, 2, 5),
    n_valid_folds_y1 = c(5, 5, 5)
  )

  rmse <- build_rmse_matrix(list(A = a, B = b), "rmse_y1")

  expect_identical(rownames(rmse), c("p1", "p3"))
  expect_equal(unname(rmse), matrix(c(1, 3, 2, 4), ncol = 2))
})


test_that("Model wins and ties are reported without order bias", {
  rmse <- rbind(
    p1 = c(A = 1, B = 2, C = 3),
    p2 = c(A = 2, B = 1, C = 3),
    p3 = c(A = 1, B = 1, C = 3)
  )

  overview <- summarize_cv(rmse)
  pairwise <- pairwise_cv(rmse, n_boot = 100)
  ab <- subset(pairwise, model_1 == "A" & model_2 == "B")

  expect_equal(overview$win_pct, c(100 / 3, 100 / 3, 0))
  expect_equal(overview$tie_pct, rep(100 / 3, 3))
  expect_equal(ab$wins_1, 100 / 3)
  expect_equal(ab$wins_2, 100 / 3)
  expect_equal(ab$tie_pct, 100 / 3)
  expect_equal(ab$delta_pp, 0)
  expect_equal(ab$mean_diff, 0)
})


test_that("Saved model results produce an outcome-level summary", {
  output_dir <- tempfile("cross_validation_")
  dir.create(output_dir)

  a <- data.frame(
    participant_id = c("p1", "p2", "p3"),
    rmse_y1 = c(1, 2, 3),
    n_valid_folds_y1 = c(5, 2, 5)
  )

  b <- data.frame(
    participant_id = c("p3", "p1", "p2"),
    rmse_y1 = c(4, 2, 5),
    n_valid_folds_y1 = c(5, 5, 5)
  )

  write.csv(a, file.path(output_dir, "toy_A_cv.csv"), row.names = FALSE)
  write.csv(b, file.path(output_dir, "toy_B_cv.csv"), row.names = FALSE)

  summaries <- summarize_cv_dataset(
    output_dir,
    base_name = "toy",
    model_names = c("A", "B"),
    n_boot = 100
  )

  expect_named(summaries, "rmse_y1")
  expect_equal(summaries$rmse_y1$overview$n, rep(2, 2))
  expect_equal(nrow(summaries$rmse_y1$pairwise), 1)
  expect_error(load_cv_results(output_dir, "toy", "missing"), "Missing CV")
})

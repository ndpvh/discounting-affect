source(
  testthat::test_path(
    "..", "..", "analysis", "14_run_distinguishability.R"
  ),
  local = TRUE
)
source(
  testthat::test_path(
    "..", "..", "analysis", "15_summarize_distinguishability.R"
  ),
  local = TRUE
)


test_that("Distinguishability uses the main-analysis dimensions", {
  expect_equal(
    study_dimensions,
    data.frame(
      d = c(1L, 1L, 2L, 2L),
      k = c(1L, 3L, 1L, 2L)
    )
  )
})


test_that("Model selection handles unique winners and ties", {
  models <- c("A", "B", "C")

  expect_identical(select_best_model(c(1, 2, 3), models), "A")
  expect_identical(select_best_model(c(2, 1, 3), models), "B")
  expect_identical(select_best_model(c(1, 1, 3), models), "tie")
  expect_true(is.na(select_best_model(c(1, NA, 3), models)))
  expect_true(is.na(select_best_model(c(1, 2), models[1:2])))
  expect_true(is.na(select_best_model(c(1, 2, 3), c("A", "A", "C"))))
})


test_that("Summaries count correct selections, ties and failed fits", {
  models <- c(
    "exponential", "quasi_hyperbolic", "double_exponential"
  )

  fits <- data.frame(
    iteration = rep(1:3, each = 3),
    generating_model = "exponential",
    fitted_model = rep(models, 3),
    d = 1L,
    k = 1L,
    aic = c(10, 11, 12, 10, 10, 12, NA, 11, 12),
    bic = c(9, 8, 11, 10, 10, 13, NA, 11, 12),
    error = c(rep(NA_character_, 6), "failed", NA, NA)
  )

  results <- summarize_distinguishability(fits)

  aic <- subset(results$accuracy, criterion == "aic")
  bic <- subset(results$accuracy, criterion == "bic")

  expect_equal(aic$n_simulations, 3)
  expect_equal(aic$n_complete, 2)
  expect_equal(aic$n_correct, 1)
  expect_equal(aic$n_ties, 1)
  expect_equal(aic$n_incomplete, 1)
  expect_equal(aic$accuracy_pct, 50)

  expect_equal(bic$n_complete, 2)
  expect_equal(bic$n_correct, 0)
  expect_equal(bic$accuracy_pct, 0)

  aic_selection <- subset(results$confusion, criterion == "aic")
  expect_setequal(
    aic_selection$selected_model,
    c("exponential", "tie")
  )
  expect_equal(sum(aic_selection$pct), 100)

  qh <- subset(
    results$differences,
    criterion == "aic" & fitted_model == "quasi_hyperbolic"
  )
  expect_equal(qh$n_pairs, 2)
  expect_equal(qh$mean_difference, 0.5)

  failed <- subset(
    results$failures,
    fitted_model == "exponential"
  )
  expect_equal(failed$n_failed, 1)
})



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


test_that("A priori and a posteriori use their respective dimensions", {
  expect_equal(apriori_dimensions, expand.grid(d = 1:2, k = 1:2))
  expect_equal(
    aposteriori_dimensions,
    data.frame(d = 1:2, k = c(1, 1))
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
    study = "apriori",
    dataset = "synthetic",
    d = 1,
    k = 1,
    generating_model = "exponential",
    fitted_model = rep(models, 3),
    simulation_id = rep(paste0("sim_", 1:3), each = 3),
    aic = c(10, 11, 12, 10, 10, 12, NA, 11, 12),
    bic = c(9, 8, 11, 10, 10, 13, NA, 11, 12),
    objective_sse = c(4, 5, 6, 8, 7, 8, NA, 9, 10),
    rmse = c(0.2, 0.3, 0.4, 0.4, 0.3, 0.4, NA, 0.5, 0.6),
    mae = c(0.1, 0.2, 0.3, 0.3, 0.2, 0.3, NA, 0.4, 0.5),
    bias = c(0.1, 0.2, 0.3, -0.1, 0.2, 0.3, NA, 0.4, 0.5),
    autocorrelation = rep(NA_real_, 9),
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
  expect_equal(aic$accuracy_all_pct, 100 / 3)
  expect_equal(bic$n_correct, 0)

  selected <- subset(results$confusion, criterion == "aic")
  expect_setequal(selected$selected_model, c("exponential", "tie"))
  expect_equal(sum(selected$pct), 100)

  qh <- subset(
    results$differences,
    criterion == "aic" & fitted_model == "quasi_hyperbolic"
  )
  expect_equal(qh$n_pairs, 2)
  expect_equal(qh$mean_difference, 0.5)

  exp_failure <- subset(
    results$failures, fitted_model == "exponential"
  )
  expect_equal(exp_failure$n_failed, 1)

  gof <- subset(
    results$goodness_of_fit, fitted_model == "exponential"
  )
  expect_equal(gof$n_valid, 2)
  expect_equal(gof$rmse_mean, 0.3)
  expect_equal(gof$sse_mean, 6)
  expect_equal(gof$abs_bias_mean, 0.1)
  expect_true(is.na(gof$autocorrelation_mean))
})


test_that("Simulation IDs are kept separate between datasets", {
  models <- c(
    "exponential", "quasi_hyperbolic", "double_exponential"
  )
  fits <- data.frame(
    study = rep("aposteriori", 6),
    dataset = rep(c("dataset_a", "dataset_b"), each = 3),
    d = 1, k = 1,
    generating_model = "exponential",
    fitted_model = rep(models, 2),
    simulation_id = "participant_1",
    aic = rep(c(1, 2, 3), 2),
    bic = rep(c(1, 2, 3), 2),
    objective_sse = 1,
    rmse = 0.1, mae = 0.1, bias = 0,
    autocorrelation = 0,
    error = NA_character_
  )

  summary <- summarize_distinguishability(fits)
  expect_equal(nrow(summary$accuracy), 4)
  expect_true(all(summary$accuracy$n_simulations == 1))
})



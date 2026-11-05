################################################################################
# PURPOSE:
#
# Test whether AIC and BIC identify the model that generated simulated affect
# data. Each simulated dataset is fitted with all three discounting models, so
# comparisons are made on exactly the same observations.
#
# Simulations use the dimensionalities fitted in the main analysis. Results
# are saved separately for each generating model and dimensionality in
# analysis/results/distinguishability/.
#
# Run with the argument "pilot" for two simulations of each generating model
# at d = 1, k = 1. Pilot results are saved in a separate directory.
# Summarize completed results with 15_summarize_distinguishability.R.
################################################################################

study_dimensions <- data.frame(
  d = c(1L, 1L, 2L, 2L),
  k = c(1L, 3L, 1L, 2L)
)


# Use the same global and local optimization procedure as recovery.
optimizer <- function(obj, lower, upper,
                      algorithm = "NLOPT_LN_BOBYQA", maxeval = 1e3,
                      ftol_abs = 1e-15, xtol_abs = 1e-15,
                      print_level = 0, ...) {
  result_de <- DEoptim::DEoptim(
    obj,
    lower,
    upper,
    control = DEoptim::DEoptim.control(...)
  )

  result_nl <- nloptr::nloptr(
    result_de$optim$bestmem,
    obj,
    lb = lower,
    ub = upper,
    opts = list(
      algorithm = algorithm,
      maxeval = maxeval,
      ftol_abs = ftol_abs,
      xtol_abs = xtol_abs,
      print_level = print_level
    )
  )

  list(
    parameters = result_nl$solution,
    objective = result_nl$objective
  )
}


run_distinguishability <- function(pilot = FALSE) {
  config_file <- if (file.exists(file.path("analysis", "_config.R"))) {
    file.path("analysis", "_config.R")
  } else if (file.exists("_config.R")) {
    "_config.R"
  } else {
    stop("Could not find analysis/_config.R. Run from the repository root.")
  }

  source(config_file, local = TRUE)
  source(file.path(PATHS$analysis, "_helpers.R"), local = TRUE)
  devtools::load_all(PROJECT_ROOT)

  iterations <- if (pilot) 2L else 1000L
  n_observations <- 140L
  dimensions <- if (pilot) study_dimensions[1L, , drop = FALSE] else {
    study_dimensions
  }

  models <- list(
    exponential = exponential,
    quasi_hyperbolic = quasi_hyperbolic,
    double_exponential = double_exponential
  )

  settings <- list(
    dynamics = "isotropic",
    covariance = "symmetric",
    itermax = 1e3,
    NP = 150,
    CR = 0.75,
    strategy = 6,
    p = 0.8,
    reltol = 1e-15,
    steptol = 100,
    trace = FALSE,
    maxeval = 1e5,
    xtol_abs = 1e-20,
    ftol_abs = 1e-20
  )

  output_dir <- file.path(PATHS$results, "distinguishability")
  if (pilot) {
    output_dir <- file.path(output_dir, "pilot")
  }
  ensure_dir(output_dir)

  scenarios <- do.call(rbind, lapply(names(models), function(name) {
    data.frame(dimensions, generating_model = name)
  }))
  rownames(scenarios) <- NULL

  run_scenario <- function(j) {
    scenario <- scenarios[j, ]
    generator <- scenario$generating_model
    d <- scenario$d
    k <- scenario$k
    seed <- 1000L + j
    set.seed(seed)

    message("Generating ", generator, " (d = ", d, ", k = ", k, ")")

    simulated_parameters <- vector("list", iterations)
    results <- vector("list", iterations * length(models))
    row <- 1L

    for (i in seq_len(iterations)) {
      model_empty <- models[[generator]](d = d, k = k)
      generated <- generate_parameters(
        model_empty,
        dynamics = "isotropic",
        covariance = "symmetric",
        parameters_only = FALSE
      )

      sim_model <- fill(
        model_empty,
        generated,
        dynamics = "isotropic",
        covariance = "symmetric",
        parameters_only = FALSE,
        cholesky = TRUE
      )

      simulated_parameters[[i]] <- parameters(
        sim_model,
        dynamics = "isotropic",
        covariance = "symmetric",
        parameters_only = FALSE
      )

      X <- matrix(
        runif(n_observations * k, min = -2, max = 2),
        ncol = k
      )
      data <- simulate(sim_model, X = X)

      for (candidate in names(models)) {
        fit_result <- tryCatch(
          {
            fitted <- do.call(
              fit,
              c(
                list(
                  model = models[[candidate]](d = d, k = k),
                  data = data,
                  optimizer = optimizer
                ),
                settings
              )
            )

            list(
              aic = aic(fitted),
              bic = bic(fitted),
              objective = fitted$objective,
              error = NA_character_
            )
          },
          error = function(e) {
            list(
              aic = NA_real_,
              bic = NA_real_,
              objective = NA_real_,
              error = conditionMessage(e)
            )
          }
        )

        results[[row]] <- data.frame(
          iteration = i,
          generating_model = generator,
          fitted_model = candidate,
          d = d,
          k = k,
          aic = fit_result$aic,
          bic = fit_result$bic,
          objective = fit_result$objective,
          error = fit_result$error
        )
        row <- row + 1L
      }

      if (pilot || i %% 50L == 0L) {
        message(generator, " (d = ", d, ", k = ", k,
                "): ", i, "/", iterations)
      }
    }

    output <- list(
      settings = list(
        generating_model = generator,
        d = d,
        k = k,
        iterations = iterations,
        N = n_observations,
        seed = seed,
        predictors = "uniform(-2, 2)",
        dynamics = "isotropic",
        covariance = "symmetric"
      ),
      simulated_parameters = simulated_parameters,
      fits = do.call(rbind, results)
    )

    file_name <- paste0(generator, "_", d, k, ".Rds")
    saveRDS(output, file.path(output_dir, file_name))
    invisible(NULL)
  }

  detected_cores <- parallel::detectCores()
  if (is.na(detected_cores)) {
    detected_cores <- 1L
  }

  cores <- if (pilot || .Platform$OS.type == "windows") {
    1L
  } else {
    max(1L, as.integer(detected_cores - 1L))
  }

  workers <- parallel::mclapply(
    seq_len(nrow(scenarios)),
    run_scenario,
    mc.cores = cores
  )

  failed <- which(vapply(workers, inherits, logical(1), "try-error"))
  if (length(failed) > 0L) {
    stop("Distinguishability failed for scenario(s): ",
         paste(failed, collapse = ", "))
  }

  message("Distinguishability results saved in ", output_dir)
  invisible(NULL)
}


if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 1L || (length(args) == 1L && args != "pilot")) {
    stop('Usage: Rscript analysis/14_run_distinguishability.R [pilot]')
  }
  run_distinguishability(pilot = identical(args, "pilot"))
}

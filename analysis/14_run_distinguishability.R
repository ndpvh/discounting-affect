################################################################################
# PURPOSE:
#
# Can AIC and BIC identify which of the three models generated a dataset?
# For every simulated dataset, fit all three models and compare their AIC,
# BIC and absolute goodness of fit (SSE, RMSE, MAE and residual diagnostics).
#
# The study has two parts:
# 1. A PRIORI: Test distinguishability under controlled conditions, using the
#    original recovery study's random parameters, predictors and dimensions.
# 2. A POSTERIORI: Test distinguishability under realistic conditions, using
#    estimated parameters and observed stimulus sequences from actual people.
#
# The pilot exercises every model/dimension combination twice before a full
# run. File 15 summarizes the saved results without re-running any fits.
################################################################################

library(DEoptim)
library(nloptr)
library(parallel)

apriori_dimensions <- expand.grid(d = 1:2, k = 1:2)
aposteriori_dimensions <- data.frame(d = 1:2, k = 1)


################################################################################
# SHARED FUNCTIONS
#
# Both parts use the same optimizer and fit diagnostics so that differences
# between the studies come from the simulated data, not the fitting method.
################################################################################


# Match the original recovery study's global and local optimizers.
optimizer <- function(obj, lower, upper,
                      algorithm = "NLOPT_LN_BOBYQA", maxeval = 1e3,
                      ftol_abs = 1e-15, xtol_abs = 1e-15,
                      print_level = 0, ...) {
  result_de <- DEoptim(
    obj, lower, upper,
    control = DEoptim.control(...)
  )

  result_nl <- nloptr(
    result_de$optim$bestmem, obj,
    lb = lower, ub = upper,
    opts = list(
      algorithm = algorithm, maxeval = maxeval,
      ftol_abs = ftol_abs, xtol_abs = xtol_abs,
      print_level = print_level
    )
  )

  list(parameters = result_nl$solution, objective = result_nl$objective)
}


# Fit the three models to one dataset; retain failures as missing statistics.
fit_candidate_models <- function(data, models, settings, study, dataset,
                                 generator, d, k, simulation_id,
                                 participant_id = NA_character_,
                                 iteration = 1,
                                 simulation_error = NA_character_,
                                 n_clipped = NA_integer_) {
  rows <- lapply(names(models), function(candidate) {
    values <- if (!is.na(simulation_error)) {
      list(error = simulation_error)
    } else {
      tryCatch(
        {
          model <- models[[candidate]](d = d, k = k)
          fitted <- do.call(
            fit,
            c(
              list(model = model, data = data, optimizer = optimizer),
              settings
            )
          )

          estimated_model <- fill(
            model, fitted$parameters,
            dynamics = "isotropic", covariance = "symmetric",
            parameters_only = FALSE, cholesky = FALSE
          )
          predicted <- predict(estimated_model, data)@Y
          residual <- as.numeric(data@Y - predicted)
          residual <- residual[is.finite(residual)]
          if (length(residual) == 0) {
            stop("The fitted model has no finite residuals.")
          }

          acf <- tryCatch({
            values <- as.numeric(autocorrelation(fitted))
            values <- values[is.finite(values)]
            if (length(values) == 0) NA_real_ else mean(values)
          }, error = function(e) NA_real_)

          list(
            aic = aic(fitted),
            bic = bic(fitted),
            objective_sse = fitted$objective,
            rmse = sqrt(mean(residual^2)),
            mae = mean(abs(residual)),
            bias = mean(residual),
            autocorrelation = acf,
            error = NA_character_
          )
        },
        error = function(e) list(error = conditionMessage(e))
      )
    }

    metrics <- c(
      "aic", "bic", "objective_sse", "rmse", "mae", "bias",
      "autocorrelation"
    )
    for (metric in metrics) {
      if (is.null(values[[metric]])) values[[metric]] <- NA_real_
    }

    data.frame(
      study = study,
      dataset = dataset,
      d = d,
      k = k,
      generating_model = generator,
      fitted_model = candidate,
      simulation_id = simulation_id,
      participant_id = participant_id,
      iteration = iteration,
      n_clipped = n_clipped,
      aic = values$aic,
      bic = values$bic,
      objective_sse = values$objective_sse,
      rmse = values$rmse,
      mae = values$mae,
      bias = values$bias,
      autocorrelation = values$autocorrelation,
      error = values$error
    )
  })

  do.call(rbind, rows)
}


# Run scenarios in parallel for full studies, sequentially for the pilot.
run_scenarios <- function(n, fun, pilot) {
  detected <- detectCores()
  if (is.na(detected)) detected <- 1
  cores <- if (pilot || .Platform$OS.type == "windows") {
    1
  } else {
    min(n, max(1, detected - 1))
  }

  workers <- mclapply(seq_len(n), fun, mc.cores = cores)
  failed <- which(vapply(workers, inherits, logical(1), "try-error"))
  if (length(failed) > 0) {
    stop("Distinguishability failed for scenario(s): ",
         paste(failed, collapse = ", "))
  }
  invisible(NULL)
}


################################################################################
# PART 1: A PRIORI DISTINGUISHABILITY
#
# This is a controlled simulation, matching the ORIGINAL recovery study.
# There are no real participants: parameters and predictors are generated
# anew for each simulation. Use N = 140 and all d/k combinations from 1 to 2.
# Run 1,000 simulations per generating model and dimension (two in the pilot).
################################################################################

run_apriori <- function(models, settings, output_dir, pilot) {
  iterations <- if (pilot) 2 else 1000
  n_observations <- 140
  # Each model generates data at every dimensionality.
  scenarios <- merge(
    apriori_dimensions, data.frame(generating_model = names(models)), by = NULL
  )

  run_scenario <- function(j) {
    scenario <- scenarios[j, ]
    generator <- scenario$generating_model
    d <- scenario$d
    k <- scenario$k
    seed <- 1000 + j
    set.seed(seed)

    fitted <- vector("list", iterations)
    generating_parameters <- vector("list", iterations)

    for (i in seq_len(iterations)) {
      model <- models[[generator]](d = d, k = k)
      generated <- generate_parameters(
        model, dynamics = "isotropic", covariance = "symmetric",
        parameters_only = FALSE
      )
      model <- fill(
        model, generated, dynamics = "isotropic",
        covariance = "symmetric", parameters_only = FALSE,
        cholesky = TRUE
      )
      generating_parameters[[i]] <- parameters(
        model, dynamics = "isotropic", covariance = "symmetric",
        parameters_only = FALSE
      )

      X <- matrix(runif(n_observations * k, -2, 2), ncol = k)
      simulated <- simulate(model, X = X)
      fitted[[i]] <- fit_candidate_models(
        simulated, models, settings,
        study = "apriori", dataset = "synthetic", generator = generator,
        d = d, k = k, simulation_id = paste0("simulation_", i),
        iteration = i
      )

      if (pilot || i %% 50 == 0) {
        message("A priori / ", generator, " d = ", d, " k = ", k,
                ": ", i, "/", iterations)
      }
    }

    saveRDS(
      list(
        settings = list(
          study = "apriori", generating_model = generator, d = d, k = k,
          iterations = iterations, N = n_observations, seed = seed,
          predictors = "uniform(-2, 2)", dynamics = "isotropic",
          covariance = "symmetric", pilot = pilot
        ),
        simulated_parameters = generating_parameters,
        fits = do.call(rbind, fitted)
      ),
      file.path(output_dir, paste0(generator, "_d", d, "_k", k, ".Rds"))
    )
    invisible(NULL)
  }

  run_scenarios(nrow(scenarios), run_scenario, pilot)
}


################################################################################
# PART 2: A POSTERIORI DISTINGUISHABILITY
#
# This follows the REALISTIC recovery study for VANHASBROECK_2024.
# Instead of random parameters and predictors, each simulation uses one
# participant's estimated model parameters and original stimulus sequence.
# Responses are clipped to [0, 1], and observed missing values are restored.
# Participants have either one or two outcomes (d = 1 or 2), with k = 1.
# Run once per participant; the pilot uses two different valid participants
# for every generating model and dimension.
################################################################################

run_aposteriori <- function(models, settings, output_dir, pilot,
                           processed_dir, estimation_dir) {
  dataset_name <- "VANHASBROECK_2024"
  participant_dir <- file.path(processed_dir, dataset_name)
  participant_files <- sort(list.files(
    participant_dir, pattern = "\\.rds$", full.names = TRUE,
    ignore.case = TRUE
  ))
  if (length(participant_files) == 0) {
    stop("No processed participant files found in ", participant_dir)
  }

  dimensions <- vapply(participant_files, function(path) {
    data <- readRDS(path)
    c(d = ncol(data@Y), k = ncol(data@X))
  }, numeric(2))

  scenarios <- merge(
    aposteriori_dimensions, data.frame(generating_model = names(models)),
    by = NULL
  )

  run_scenario <- function(j) {
    scenario <- scenarios[j, ]
    generator <- scenario$generating_model
    d <- scenario$d
    k <- scenario$k
    model_empty <- models[[generator]](d = d, k = k)
    parameter_columns <- parameter_names(
      model_empty, dynamics = "isotropic", covariance = "symmetric",
      parameters_only = FALSE
    )

    estimate_file <- file.path(
      estimation_dir,
      paste0(dataset_name, "_", d, "_", generator, ".csv")
    )
    if (!file.exists(estimate_file)) stop("Missing: ", estimate_file)
    estimates <- read.csv(estimate_file, check.names = FALSE)
    if (!all(c("participant_id", parameter_columns) %in% names(estimates))) {
      stop("Missing participant or parameter columns in ", estimate_file)
    }
    if (anyNA(estimates$participant_id) ||
        anyDuplicated(estimates$participant_id)) {
      stop("Missing or duplicate participant IDs in ", estimate_file)
    }

    files <- participant_files[
      dimensions["d", ] == d & dimensions["k", ] == k
    ]
    if (length(files) == 0) {
      stop("No participants match d = ", d, ", k = ", k)
    }

    get_parameters <- function(path) {
      id <- tools::file_path_sans_ext(basename(path))
      row <- match(id, estimates$participant_id)
      if (is.na(row)) stop("No participant estimates: ", id)
      values <- as.numeric(unlist(
        estimates[row, parameter_columns, drop = FALSE],
        use.names = FALSE
      ))
      names(values) <- parameter_columns
      if (any(!is.finite(values))) stop("Non-finite estimates: ", id)
      values
    }

    prepare_participant <- function(path) {
      data <- readRDS(path)
      observed <- !is.na(data@Y)
      if (sum(complete.cases(data@Y)) < 2 ||
          any(!is.finite(data@Y[observed])) ||
          any(data@Y[observed] < 0 | data@Y[observed] > 1)) {
        stop("Insufficient or invalid observed ratings.")
      }

      values <- get_parameters(path)
      id <- tools::file_path_sans_ext(basename(path))
      row <- match(id, estimates$participant_id)
      if ("objective_sse" %in% names(estimates) &&
          is.finite(estimates$objective_sse[row])) {
        mean_columns <- parameter_names(
          model_empty, dynamics = "isotropic", covariance = "symmetric",
          parameters_only = TRUE
        )
        original_sse <- objective_function(
          model_empty, data, values[mean_columns], dynamics = "isotropic"
        )
        if (!isTRUE(all.equal(
          original_sse, estimates$objective_sse[row],
          tolerance = 1e-6, check.attributes = FALSE
        ))) {
          stop("Estimates do not match the participant's processed data.")
        }
      }

      model <- fill(
        model_empty, values, dynamics = "isotropic",
        covariance = "symmetric", parameters_only = FALSE,
        cholesky = FALSE
      )
      if (any(!is.finite(model@covariance)) ||
          !isSymmetric(model@covariance)) {
        stop("Invalid generating covariance.")
      }
      chol(model@covariance)
      list(data = data, model = model, observed = observed)
    }

    simulate_participant <- function(path, i) {
      id <- tools::file_path_sans_ext(basename(path))
      simulation_id <- paste0(id, "_1")
      result <- tryCatch(
        {
          prepared <- prepare_participant(path)
          data <- prepared$data
          observed <- prepared$observed

          set.seed(20261105 + j * 100000 + i)
          simulated <- simulate(prepared$model, X = data@X)
          clipped <- sum(simulated@Y[observed] < 0 |
                         simulated@Y[observed] > 1)
          data@Y[] <- pmin(pmax(simulated@Y, 0), 1)
          data@Y[!observed] <- NA_real_

          fit_candidate_models(
            data, models, settings, study = "aposteriori",
            dataset = dataset_name, generator = generator, d = d, k = k,
            simulation_id = simulation_id, participant_id = id,
            n_clipped = clipped
          )
        },
        error = function(e) {
          fit_candidate_models(
            NULL, models, settings, study = "aposteriori",
            dataset = dataset_name, generator = generator, d = d, k = k,
            simulation_id = simulation_id, participant_id = id,
            simulation_error = conditionMessage(e)
          )
        }
      )
      message("A posteriori / ", generator, " d = ", d,
              " / ", id, " (", i, ")")
      result
    }

    # Stop after finding two usable people: the pilot need not check everyone.
    if (pilot) {
      selected <- character()
      for (path in files) {
        valid <- tryCatch({
          prepare_participant(path)
          TRUE
        }, error = function(e) FALSE)
        if (valid) selected <- c(selected, path)
        if (length(selected) == 2) break
      }
      files <- selected
      if (length(files) < 2) {
        stop("Pilot needs two usable participants for ", generator,
             " d = ", d)
      }
    }

    fitted <- lapply(seq_along(files), function(i) {
      simulate_participant(files[i], i)
    })
    participant_parameters <- lapply(files, function(path) {
      tryCatch(get_parameters(path), error = function(e) NULL)
    })
    names(participant_parameters) <- tools::file_path_sans_ext(basename(files))

    saveRDS(
      list(
        settings = list(
          study = "aposteriori", dataset = dataset_name,
          generating_model = generator, d = d, k = k,
          iterations = 1, participants = length(files), pilot = pilot,
          seed = 20261105, source = estimate_file,
          dynamics = "isotropic", covariance = "symmetric"
        ),
        generating_parameters = participant_parameters,
        fits = do.call(rbind, fitted)
      ),
      file.path(
        output_dir, paste0(dataset_name, "_d", d, "_", generator, ".Rds")
      )
    )
    invisible(NULL)
  }

  run_scenarios(nrow(scenarios), run_scenario, pilot)
}


################################################################################
# RUN THE STUDIES
#
# Run both parts by default, or pass "apriori" or "aposteriori" to run just
# one. Pilot results are kept in their own folders, separate from full runs.
################################################################################

run_distinguishability <- function(pilot = FALSE, study = "both") {
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

  models <- list(
    exponential = exponential,
    quasi_hyperbolic = quasi_hyperbolic,
    double_exponential = double_exponential
  )
  settings <- list(
    dynamics = "isotropic", covariance = "symmetric",
    itermax = 1e3, NP = 150, CR = 0.75, strategy = 6,
    p = 0.8, reltol = 1e-15, steptol = 100, trace = FALSE,
    maxeval = 1e5, xtol_abs = 1e-20, ftol_abs = 1e-20
  )

  studies <- if (study == "both") {
    c("apriori", "aposteriori")
  } else {
    study
  }
  if (!all(studies %in% c("apriori", "aposteriori"))) {
    stop('Choose "both", "apriori", or "aposteriori".')
  }

  for (name in studies) {
    output_dir <- file.path(PATHS$results, "distinguishability", name)
    if (pilot) output_dir <- file.path(output_dir, "pilot")
    ensure_dir(output_dir)

    if (name == "apriori") {
      run_apriori(models, settings, output_dir, pilot)
    } else {
      run_aposteriori(
        models, settings, output_dir, pilot,
        PATHS$processed_data, PATHS$estimation
      )
    }
    message("Saved ", name, " results in ", output_dir)
  }

  invisible(NULL)
}


if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  allowed <- c("pilot", "apriori", "aposteriori")
  if (length(args) > 2 || any(!args %in% allowed) ||
      sum(args == "pilot") > 1 ||
      sum(args %in% c("apriori", "aposteriori")) > 1) {
    stop("Usage: Rscript analysis/14_run_distinguishability.R ",
         "[pilot] [apriori|aposteriori]")
  }
  study <- intersect(args, c("apriori", "aposteriori"))
  if (length(study) == 0) study <- "both"
  run_distinguishability(pilot = "pilot" %in% args, study = study)
}



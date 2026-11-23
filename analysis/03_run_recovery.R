################################################################################
# PARAMETER RECOVERY
#
# A priori:     random parameters and predictors (d = 1:2, k = 1:2).
# A posteriori: empirical parameters and predictors from VANHASBROECK_2024
#               (d = 1:2, k = 1), preserving missingness and clipping to [0,1].
#
# Run from the repository root:
#   Rscript analysis/03_run_recovery.R pilot             # both pilots
#   Rscript analysis/03_run_recovery.R apriori           # a priori only
#   Rscript analysis/03_run_recovery.R aposteriori pilot # a posteriori pilot
#   Rscript analysis/03_run_recovery.R                   # both full studies
#
# Functions recovery() and recovery_realistic() live in R/recovery.R.
################################################################################

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 2L || any(!args %in% c("pilot", "apriori", "aposteriori")) ||
    sum(args == "pilot") > 1L || sum(args %in% c("apriori", "aposteriori")) > 1L) {
  stop("Usage: Rscript analysis/03_run_recovery.R [pilot] [apriori|aposteriori]")
}
pilot <- "pilot" %in% args
studies <- intersect(args, c("apriori", "aposteriori"))
if (!length(studies)) studies <- c("apriori", "aposteriori")

config_file <- if (file.exists("analysis/_config.R")) {
  "analysis/_config.R"
} else if (file.exists("_config.R")) {
  "_config.R"
} else {
  stop("Run from the repository root or the analysis/ directory.")
}
source(config_file)
source(file.path(PATHS$analysis, "_helpers.R"))
devtools::load_all(PROJECT_ROOT)

models <- list(
  exponential = exponential,
  quasi_hyperbolic = quasi_hyperbolic,
  double_exponential = double_exponential
)

# Same global + local optimization settings as participant estimation.
optimizer <- function(obj, lower, upper, algorithm = "NLOPT_LN_BOBYQA",
                      maxeval = 1e3, ftol_abs = 1e-15, xtol_abs = 1e-15,
                      print_level = 0, ...) {
  global <- DEoptim::DEoptim(
    obj, lower, upper, control = DEoptim::DEoptim.control(...)
  )
  local <- nloptr::nloptr(
    global$optim$bestmem, obj, lb = lower, ub = upper,
    opts = list(algorithm = algorithm, maxeval = maxeval,
                ftol_abs = ftol_abs, xtol_abs = xtol_abs,
                print_level = print_level)
  )
  list(parameters = local$solution, objective = local$objective)
}

fit_settings <- list(
  dynamics = "isotropic", covariance = "symmetric",
  itermax = 1e3, NP = 150, CR = 0.75, strategy = 6,
  p = 0.8, reltol = 1e-15, steptol = 100, trace = FALSE,
  maxeval = 1e5, xtol_abs = 1e-20, ftol_abs = 1e-20,
  print_level = 0
)

destination <- function(study) {
  path <- file.path(PATHS$recovery, study, if (pilot) "pilot" else "full")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

# A PRIORI --------------------------------------------------------------------
run_apriori <- function() {
  out <- destination("apriori")
  scenarios <- merge(expand.grid(d = 1:2, k = 1:2),
                     data.frame(model = names(models)), by = NULL)
  iterations <- if (pilot) 2L else 1000L

  for (j in seq_len(nrow(scenarios))) {
    scenario <- scenarios[j, ]
    name <- as.character(scenario$model)
    d <- scenario$d
    k <- scenario$k
    set.seed(1000L + j)
    message("A priori: ", name, " d=", d, " k=", k)

    xfun <- if (k == 1L) {
      function(n) runif(n, -2, 2)
    } else {
      list(function(n) runif(n, -2, 2), function(n) runif(n, -2, 2))
    }
    result <- do.call(recovery, c(
      list(sim_model = models[[name]](d = d, k = k),
           iterations = iterations, N = 140L, Xfun = xfun,
           fx = list(aic = aic, bic = bic,
                     autocorrelation = autocorrelation, bias = bias),
           optimizer = optimizer, print_iteration = TRUE),
      fit_settings
    ))
    result$settings <- list(study = "apriori", model = name, d = d, k = k,
                            iterations = iterations, N = 140L,
                            pilot = pilot, seed = 1000L + j)
    saveRDS(result, file.path(out, paste0(name, "_d", d, "_k", k, ".Rds")))
  }
}

# A POSTERIORI ----------------------------------------------------------------
run_aposteriori <- function() {
  out <- destination("aposteriori")
  base <- "VANHASBROECK_2024"
  files <- sort(list.files(file.path(PATHS$processed_data, base),
                           pattern = "\\.rds$", full.names = TRUE,
                           ignore.case = TRUE))
  if (!length(files)) stop("No processed participant data found for ", base)

  # The realistic recovery function itself validates covariance, missingness,
  # observed response range, and generating/recovered parameter pairing.
  for (d in 1:2) {
    matching <- Filter(function(path) {
      ds <- readRDS(path)
      ncol(ds@Y) == d && ncol(ds@X) == 1L
    }, files)
    if (!length(matching)) stop("No participants for d=", d, ", k=1")

    for (j in seq_along(models)) {
      name <- names(models)[j]
      empty <- models[[name]](d = d, k = 1)
      pnames <- parameter_names(empty, dynamics = "isotropic",
                                covariance = "symmetric", parameters_only = FALSE)
      mean_names <- parameter_names(empty, dynamics = "isotropic",
                                    covariance = "symmetric", parameters_only = TRUE)
      estimate_path <- file.path(PATHS$estimation,
                                 paste0(base, "_", d, "_", name, ".csv"))
      if (!file.exists(estimate_path)) stop("Missing estimates: ", estimate_path)
      estimates <- read.csv(estimate_path, check.names = FALSE)
      if (!all(c("participant_id", pnames) %in% names(estimates)) ||
          anyNA(estimates$participant_id) || anyDuplicated(estimates$participant_id)) {
        stop("Invalid participant IDs or missing parameter columns: ", estimate_path)
      }

      message("A posteriori: ", name, " d=", d, " k=1")
      results <- list()
      status <- list()
      for (i in seq_along(matching)) {
        path <- matching[i]
        id <- tools::file_path_sans_ext(basename(path))
        row <- match(id, estimates$participant_id)
        one <- tryCatch({
          if (is.na(row)) stop("No matching estimates")
          ds <- readRDS(path)
          pars <- as.numeric(unlist(estimates[row, pnames, drop = FALSE],
                                    use.names = FALSE))
          names(pars) <- pnames
          if (any(!is.finite(pars))) stop("Non-finite estimates")

          # Catch mismatched processed data and estimation files before recovery.
          if ("objective_sse" %in% names(estimates) &&
              is.finite(estimates$objective_sse[row])) {
            sse <- objective_function(empty, ds, pars[mean_names],
                                      dynamics = "isotropic")
            if (!isTRUE(all.equal(sse, estimates$objective_sse[row],
                                  tolerance = 1e-6, check.attributes = FALSE))) {
              stop("Saved estimates do not match the processed data (SSE)")
            }
          }

          do.call(recovery_realistic, c(
            list(sim_model = empty, data = ds, estimated_parameters = pars,
                 participant_id = id, iterations = 1L,
                 seed = 20261105L + d * 100000L + j * 10000L + i,
                 fx = list(), optimizer = optimizer,
                 print_iteration = FALSE),
            fit_settings
          ))
        }, error = function(e) conditionMessage(e))

        if (is.character(one)) {
          status[[length(status) + 1L]] <- data.frame(
            participant_id = id, status = "skipped", reason = one)
        } else {
          results[[length(results) + 1L]] <- one
          status[[length(status) + 1L]] <- data.frame(
            participant_id = id, status = "ok", reason = "")
          if (pilot && length(results) == 2L) break
        }
      }

      prefix <- paste0(base, "_d", d, "_", name)
      status <- do.call(rbind, status)
      write.csv(status, file.path(out, paste0(prefix, "_status.csv")),
                row.names = FALSE)
      if (!length(results)) stop("No valid recovery cases for ", prefix)
      if (pilot && length(results) < 2L) {
        stop("Pilot requires two valid participants for ", prefix)
      }

      result <- list(
        simulate = do.call(rbind, lapply(results, `[[`, "simulate")),
        fit = do.call(rbind, lapply(results, `[[`, "fit")),
        metadata = do.call(rbind, lapply(results, `[[`, "metadata")),
        participant_status = status,
        settings = list(study = "aposteriori", dataset = base, model = name,
                        d = d, k = 1L, iterations = 1L, pilot = pilot)
      )
      rownames(result$simulate) <- NULL
      rownames(result$fit) <- NULL
      rownames(result$metadata) <- NULL
      saveRDS(result, file.path(out, paste0(prefix, ".Rds")))
      message("Saved ", prefix, ": ", nrow(result$fit), " recovery rows")
    }
  }
}

for (study in studies) {
  if (study == "apriori") run_apriori() else run_aposteriori()
}
message("Completed: ", paste(studies, collapse = ", "),
        if (pilot) " (pilot)" else " (full)")

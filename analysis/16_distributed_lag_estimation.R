################################################################################
# PURPOSE:
#
# Test the efficacy of the approximate estimation strategy used throughout this
# project (see 02_estimate_models.R and 03_run_recovery.R) on the distributed
# lag model. Unlike the discounting models, the distributed lag model is linear
# in its parameters, so that its least-squares solution can be computed
# analytically (`fit(..., deterministic_fit = TRUE)`). This gives us a ground
# truth against which the approximate strategy can be compared:
#
#   Step 1 – DEoptim  (global search, finds a good region)
#   Step 2 – nloptr / BOBYQA  (local refinement from DEoptim's best solution)
#
# For lags 0 through 10, we repeatedly:
#   1. Simulate N = 100 observations from a distributed lag model with random
#      parameters.
#   2. Estimate the model analytically and approximately.
#   3. Store the estimates, the SSE of both approaches, and the time needed.
#
# As the number of parameters grows with the lag (d + d * k * (lag + 1)), while
# the number of observations stays fixed at 100, this allows us to see whether
# the approximate strategy keeps finding the least-squares solution when the
# parameter space grows.
#
# Note that the approximate strategy is bounded (see `get_bounds`) while the
# analytic solution is not. Replications where the analytic solution falls
# outside of these bounds are flagged in the results (`analytic_in_bounds`),
# as the approximate strategy cannot reach the analytic solution there.
#
# Usage (from the repository root or the analysis/ directory):
#
#   Rscript analysis/16_distributed_lag_estimation.R
#   Rscript analysis/16_distributed_lag_estimation.R --reps=5 --itermax=50
#
# where the optional arguments overwrite the number of replications per lag
# and the maximal number of DEoptim generations (useful for a quick check).
#
# Outputs are written to:
#   analysis/results/distributed_lag_estimation/   (results)
#   analysis/figures/distributed_lag_estimation/   (figures)
#
################################################################################

config_file <- if (file.exists(file.path("analysis", "_config.R"))) {
  file.path("analysis", "_config.R")
} else if (file.exists("_config.R")) {
  "_config.R"
} else {
  stop(
    "Could not find analysis/_config.R. ",
    "Run this script from the repository root or analysis/ directory."
  )
}
source(config_file)
source(file.path(PATHS$analysis, "_helpers.R"))
rm(config_file)

devtools::load_all()

# ── packages needed beyond the discounting package ───────────────────────────
library(DEoptim)
library(nloptr)
library(ggplot2)


################################################################################
# SETTINGS
################################################################################

# Design of the simulation
n        <- 100                     # Number of observations per dataset
lags     <- seq(0, 10, 5)           # Lags of the distributed lag model
d        <- 2                       # Number of dependent variables
k        <- 2                       # Number of independent variables
n_reps   <- 100                     # Number of replications per lag
seed     <- 36                      # Static Dress - lip critic

# Generating distributions of the parameters. 
alpha_range <- c(-1, 1)
beta_range  <- c(-5, 5)
sigma       <- diag(d)

# Optimizer settings: Same as in 02_estimate_models.R
optim_settings <- list(
  # General settings
  dynamics   = "isotropic",
  covariance = "symmetric",

  # DEoptim control arguments
  itermax    = 1e3,
  NP         = 150,
  CR         = 0.75,
  strategy   = 6,
  p          = 0.8,
  reltol     = 1e-15,
  steptol    = 100,
  trace      = FALSE,

  # nloptr arguments
  maxeval    = 1e5,
  xtol_abs   = 1e-20,
  ftol_abs   = 1e-20
)

# Output locations
output_dir <- file.path(PATHS$results, "distributed_lag_estimation")
figure_dir <- file.path(PATHS$figures, "distributed_lag_estimation")

ensure_dir(output_dir)
ensure_dir(figure_dir)


################################################################################
# SHARED OPTIMIZER FUNCTION
#
# Copied from 02_estimate_models.R. The function:
#   1. Runs DEoptim.
#   2. Takes DEoptim's best solution as the starting point for nloptr/BOBYQA,
#      which then fine-tunes the result.
################################################################################

optimizer <- function(obj,
                      lower,
                      upper,
                      algorithm  = "NLOPT_LN_BOBYQA",
                      maxeval    = 1e3,
                      ftol_abs   = 1e-15,
                      xtol_abs   = 1e-15,
                      print_level = 0,
                      ...) {

  # Step 1: DEoptim global search
  result_de <- DEoptim::DEoptim(
    obj,
    lower,
    upper,
    control = DEoptim::DEoptim.control(...)
  )

  # Extract the best parameter vector found by DEoptim
  x0 <- result_de$optim$bestmem

  # Step 2: nloptr local refinement
  result_nl <- nloptr::nloptr(
    x0,
    obj,
    lb   = lower,
    ub   = upper,
    opts = list(
      algorithm   = algorithm,
      maxeval     = maxeval,
      ftol_abs    = ftol_abs,
      xtol_abs    = xtol_abs,
      print_level = print_level
    )
  )

  # Return in the format expected by discounting::fit()
  list(
    parameters = result_nl$solution,
    objective  = result_nl$objective
  )
}


################################################################################
# RUN ONE REPLICATION
#
# Simulates a single dataset for a given lag and estimates the distributed lag
# model both analytically and approximately. Returns a list with the parameter
# estimates (long format) and the fit statistics (one row).
################################################################################

run_replication <- function(lag, rep_id) {

  # Make every replication reproducible on its own, independent of the order
  # in which the parallel workers pick them up
  set.seed(seed + 1000 * lag + rep_id)

  # Generate the parameters and the data
  true_model <- distributed_lag(
    d = d,
    k = k,
    lag = lag,
    parameters = list(
      "alpha" = runif(d, alpha_range[1], alpha_range[2]),
      "beta" = matrix(
        runif(d * k * (lag + 1), beta_range[1], beta_range[2]),
        nrow = d
      )
    ),
    covariance = sigma
  )

  X <- matrix(rnorm(n * k), nrow = n, ncol = k)
  data <- simulate(true_model, X = X)

  empty <- distributed_lag(d = d, k = k, lag = lag)
  # Data-generating values of the deterministic parameters, in the same order
  # as the estimates (alpha, then beta column-wise)
  true_parameters <- parameters(true_model, parameters_only = TRUE)
  n_par <- length(true_parameters)

  # Analytic estimation
  time_analytic <- system.time(
    fit_analytic <- fit(
      empty,
      data,
      deterministic_fit = TRUE,
      covariance = optim_settings$covariance
    )
  )[["elapsed"]]

  # Approximate estimation, wrapped in a tryCatch so that a single failure
  # does not end the whole simulation
  time_approximate <- system.time(
    fit_approximate <- tryCatch(
      do.call(
        fit,
        c(
          list(
            empty,
            data,
            deterministic_fit = FALSE,
            optimizer = optimizer
          ),
          optim_settings
        )
      ),
      error = function(e) {
        message("  Approximate estimation failed: ", conditionMessage(e))
        NULL
      }
    )
  )[["elapsed"]]

  # Extract the estimates of the deterministic parameters only (i.e., not the
  # covariances)
  est_analytic <- fit_analytic$parameters[seq_len(n_par)]
  est_approximate <- if (is.null(fit_approximate)) {
    rep(NA_real_, n_par)
  } else {
    fit_approximate$parameters[seq_len(n_par)]
  }

  # Check whether the analytic solution lies within the bounds used by the
  # approximate strategy
  bounds <- get_bounds(empty, parameters_only = TRUE)
  in_bounds <- all(
    est_analytic >= bounds$lower & est_analytic <= bounds$upper
  )

  # Collect the results
  parameter_names <- names(est_analytic)

  estimates <- data.frame(
    lag = lag,
    replication = rep_id,
    parameter = parameter_names,
    type = sub("_.*", "", parameter_names),
    true = as.numeric(true_parameters),
    analytic = as.numeric(est_analytic),
    approximate = as.numeric(est_approximate)
  )

  fits <- data.frame(
    lag = lag,
    replication = rep_id,
    n_parameters = n_par,
    sse_analytic = fit_analytic$objective,
    sse_approximate = if (is.null(fit_approximate)) NA_real_ else fit_approximate$objective,
    rsquared_analytic = rsquared(fit_analytic, data),
    rsquared_approximate = if (is.null(fit_approximate)) NA_real_ else rsquared(fit_approximate, data),
    time_analytic = time_analytic,
    time_approximate = time_approximate,
    analytic_in_bounds = in_bounds
  )

  return(list(estimates = estimates, fits = fits))
}


################################################################################
# RUN THE SIMULATION
################################################################################

# Choose a safe worker count, using the same strategy as 02_estimate_models.R
detected_cores <- parallel::detectCores()
if (is.na(detected_cores)) detected_cores <- 1L
n_cores <- if (Sys.info()["sysname"] == "Windows") {
  1L
} else {
  max(1L, as.integer(round(detected_cores / 2) - 1L))
}

design <- expand.grid(
  replication = seq_len(n_reps),
  lag = lags
)

message(
  "Running ", nrow(design), " replications (", n_reps, " per lag, lags ",
  min(lags), "-", max(lags), ") on ", n_cores, " cores."
)

results <- parallel::mclapply(
  seq_len(nrow(design)),
  function(i) {
    message("  Lag ", design$lag[i], ", replication ", design$replication[i])
    run_replication(design$lag[i], design$replication[i])
  },
  mc.cores = n_cores,
  mc.preschedule = FALSE
)

worker_failures <- which(vapply(
  results, function(x) inherits(x, "try-error"), logical(1)
))
if (length(worker_failures) > 0) {
  stop(
    "Parallel worker failure(s) at design row(s): ",
    paste(worker_failures, collapse = ", ")
  )
}

estimates <- do.call(rbind, lapply(results, `[[`, "estimates"))
fits <- do.call(rbind, lapply(results, `[[`, "fits"))

# Differences between the approximate and the analytic solution. Since the
# analytic solution is the least-squares minimum, the SSE difference should be
# non-negative (up to numerical precision).
fits$sse_difference <- fits$sse_approximate - fits$sse_analytic
fits$sse_relative_difference <- fits$sse_difference / fits$sse_analytic

estimates$difference <- estimates$approximate - estimates$analytic


################################################################################
# SUMMARIZE
################################################################################

summary_fits <- do.call(
  rbind,
  lapply(
    split(fits, fits$lag),
    function(x) {
      data.frame(
        lag = x$lag[1],
        n_parameters = x$n_parameters[1],
        n_replications = nrow(x),
        n_failed = sum(is.na(x$sse_approximate)),
        prop_analytic_in_bounds = mean(x$analytic_in_bounds),
        median_sse_difference = median(x$sse_difference, na.rm = TRUE),
        max_sse_difference = max(x$sse_difference, na.rm = TRUE),
        median_relative_sse_difference = median(x$sse_relative_difference, na.rm = TRUE),
        max_relative_sse_difference = max(x$sse_relative_difference, na.rm = TRUE),
        # Proportion of replications where the approximate SSE lies within
        # 0.01% of the analytic minimum
        prop_converged = mean(x$sse_relative_difference < 1e-4, na.rm = TRUE),
        median_time_analytic = median(x$time_analytic),
        median_time_approximate = median(x$time_approximate)
      )
    }
  )
)

summary_estimates <- do.call(
  rbind,
  lapply(
    split(estimates, list(estimates$lag, estimates$type), drop = TRUE),
    function(x) {
      data.frame(
        lag = x$lag[1],
        type = x$type[1],
        max_abs_difference = max(abs(x$difference), na.rm = TRUE),
        rmse_difference = sqrt(mean(x$difference^2, na.rm = TRUE)),
        rmse_analytic_true = sqrt(mean((x$analytic - x$true)^2)),
        rmse_approximate_true = sqrt(mean((x$approximate - x$true)^2, na.rm = TRUE)),
        cor_approximate_analytic = cor(x$approximate, x$analytic, use = "complete.obs")
      )
    }
  )
)
summary_estimates <- summary_estimates[order(summary_estimates$type, summary_estimates$lag), ]
rownames(summary_estimates) <- NULL

# Summary of the R^2 of both approaches per lag (mean, median, SD, minimum, 
# maximum, and 2.5% and 97.5% quantiles; see `summarize_rsquared` in 
# _helpers.R)
summary_rsquared <- do.call(
  rbind,
  lapply(
    split(fits, fits$lag),
    function(x) {
      rbind(
        data.frame(lag = x$lag[1], estimation = "analytic", summarize_rsquared(x$rsquared_analytic)),
        data.frame(lag = x$lag[1], estimation = "approximate", summarize_rsquared(x$rsquared_approximate))
      )
    }
  )
)
rownames(summary_rsquared) <- NULL

write.csv(estimates, file.path(output_dir, "estimates.csv"), row.names = FALSE)
write.csv(fits, file.path(output_dir, "fits.csv"), row.names = FALSE)
write.csv(summary_fits, file.path(output_dir, "summary_fits.csv"), row.names = FALSE)
write.csv(summary_estimates, file.path(output_dir, "summary_estimates.csv"), row.names = FALSE)
write.csv(summary_rsquared, file.path(output_dir, "summary_rsquared.csv"), row.names = FALSE)

print(summary_fits, digits = 3)
print(summary_estimates, digits = 3)
print(summary_rsquared, digits = 3)


################################################################################
# PLOTS
################################################################################

lag_labels <- function(x) paste0("lag = ", x)

estimates$lag_label <- factor(
  lag_labels(estimates$lag),
  levels = lag_labels(lags)
)
fits$lag_factor <- factor(fits$lag, levels = lags)

# 1. Approximate against analytic estimates. If the approximate strategy finds
#    the least-squares solution, all points lie on the identity line.
plt <- ggplot(
  estimates,
  aes(x = analytic, y = approximate, color = type)
) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  geom_point(alpha = 0.5, size = 1) +
  facet_wrap(~ lag_label, ncol = 4) +
  scale_color_manual(
    values = c("alpha" = "#1b9e77", "beta" = "#7570b3"),
    labels = c("alpha" = expression(alpha), "beta" = expression(beta))
  ) +
  labs(
    x = "Analytic estimate",
    y = "Approximate estimate (DEoptim + BOBYQA)",
    color = "Parameter"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "estimates_approximate_vs_analytic.jpeg"),
  plt,
  width = 3000,
  height = 2600,
  units = "px",
  dpi = 300
)

# 2. Estimates of both approaches against the data-generating values
estimates_long <- rbind(
  data.frame(estimates[, c("lag_label", "type", "true")], method = "Analytic", estimate = estimates$analytic),
  data.frame(estimates[, c("lag_label", "type", "true")], method = "Approximate", estimate = estimates$approximate)
)

plt <- ggplot(
  estimates_long,
  aes(x = true, y = estimate, color = method)
) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  geom_point(alpha = 0.4, size = 1) +
  facet_wrap(~ lag_label, ncol = 4) +
  scale_color_manual(
    values = c("Analytic" = "#d95f02", "Approximate" = "#1b9e77")
  ) +
  labs(
    x = "Data-generating value",
    y = "Estimate",
    color = "Estimation"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "estimates_vs_true.jpeg"),
  plt,
  width = 3000,
  height = 2600,
  units = "px",
  dpi = 300
)

# 3. Difference between the approximate and analytic SSE per lag, both in
#    absolute and in relative terms. Values of 0 mean the approximate strategy
#    found the least-squares solution. A pseudo-log scale is used so that both
#    (near-)zero differences and large differences remain visible.
plt_absolute <- ggplot(
  fits,
  aes(x = lag_factor, y = sse_difference)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.shape = NA, fill = "grey90") +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5, size = 1, aes(color = analytic_in_bounds)) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 1e-3)) +
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "red"),
    labels = c("TRUE" = "Yes", "FALSE" = "No")
  ) +
  labs(
    x = "Lag",
    y = expression(SSE[approximate] - SSE[analytic]),
    color = "Analytic within bounds"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

plt_relative <- ggplot(
  fits,
  aes(x = lag_factor, y = 100 * sse_relative_difference)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.shape = NA, fill = "grey90") +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5, size = 1, aes(color = analytic_in_bounds)) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 1e-3)) +
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "red"),
    labels = c("TRUE" = "Yes", "FALSE" = "No")
  ) +
  labs(
    x = "Lag",
    y = "Relative SSE difference (%)",
    color = "Analytic within bounds"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

plt <- cowplot::plot_grid(
  plt_absolute,
  plt_relative,
  ncol = 2
)

ggsave(
  file.path(figure_dir, "sse_difference.jpeg"),
  plt,
  width = 3600,
  height = 1600,
  units = "px",
  dpi = 300
)

# 4. Time needed for both approaches per lag
times_long <- rbind(
  data.frame(lag_factor = fits$lag_factor, method = "Analytic", time = fits$time_analytic),
  data.frame(lag_factor = fits$lag_factor, method = "Approximate", time = fits$time_approximate)
)

plt <- ggplot(
  times_long,
  aes(x = lag_factor, y = time, fill = method)
) +
  geom_boxplot(outlier.size = 0.5) +
  scale_y_log10() +
  scale_fill_manual(
    values = c("Analytic" = "#d95f02", "Approximate" = "#1b9e77")
  ) +
  labs(
    x = "Lag",
    y = "Time (seconds, log scale)",
    fill = "Estimation"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "estimation_time.jpeg"),
  plt,
  width = 2400,
  height = 1600,
  units = "px",
  dpi = 300
)

message("Results written to ", output_dir)
message("Figures written to ", figure_dir)

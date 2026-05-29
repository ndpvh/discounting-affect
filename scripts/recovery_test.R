################################################################################
# This code test which recovery-study conditions affect parameter recovery.
# This script varies:
#   - model
#   - sample size N
#   - X-generation function
#   - dynamics / covariance
#   - DEoptim settings
#
# It saves:
#   - the raw recovery object for each condition
#   - a summary table with recovery metrics
#   - plots of the recoveries
################################################################################

dir.create(file.path("scripts", "results", "recovery_conditions"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("scripts", "figures", "recovery_conditions"), recursive = TRUE, showWarnings = FALSE)

library(parallel)
library(ggplot2)
library(cowplot)
library(discounting)

# -----------------------------
# User Settings

iterations <- 25
base_seed  <- 1234

models <- list(
  #"exponential_21"       = exponential(d = 2, k = 1),
  #"exponential_22"       = exponential(d = 2, k = 2),
  #"quasi_hyperbolic_21"  = quasi_hyperbolic(d = 2, k = 1),
  #"quasi_hyperbolic_22"  = quasi_hyperbolic(d = 2, k = 2),
  "double_exponential_21"= double_exponential(d = 2, k = 1)
  #"double_exponential_22"= double_exponential(d = 2, k = 2)
)

# Different X-generating conditions
x_generators <- list(
  "uniform_wide_1d" = function(n) runif(n, -2, 2),

  #"uniform_narrow_1d" = function(n) runif(n, -0.5, 0.5),

  #"normal_1d" = function(n) rnorm(n, 0, 1),

  "uniform_wide_2d" = function(n) cbind(
    runif(n, -2, 2),
    runif(n, -2, 2)
  ),

  #"uniform_narrow_2d" = function(n) cbind(
  #  runif(n, -0.5, 0.5),
  #  runif(n, -0.5, 0.5)
  #),

  #"normal_2d" = function(n) cbind(
  #  rnorm(n, 0, 1),
  #  rnorm(n, 0, 1)
  #),

  "uniform_wide_3d" = function(n) cbind(
    runif(n, -2, 2),
    runif(n, -2, 2),
    runif(n, -2, 2)
  )

  #"normal_3d" = function(n) cbind(
  #  rnorm(n, 0, 1),
  #  rnorm(n, 0, 1),
  #  rnorm(n, 0, 1)
  #)
)

# Model characteristics to test
characteristics <- expand.grid(
  dynamics = c("isotropic"),
  covariance = c("symmetric"),
  stringsAsFactors = FALSE
)

# Sample sizes to test
# Changed to 1400 as based on the last meeting

Ns <- c(1400)

# DEoptim configurations to compare
# Multiple configurations of each setting i.e "Throwing shit against the wall" strat
make_deoptim_settings <- function(
    itermax_values = c(300, 500, 1000),
    NP_values      = c(100, 200, 400),
    strategy_values = c(2, 6),
    p_values       = c(0.2, 0.5, 0.8),
    reltol_values  = c(1e-5, 1e-6),
    steptol_values = c(50, 100, 200)
) {
  
  grid <- expand.grid(
    itermax = itermax_values,
    NP = NP_values,
    strategy = strategy_values,
    p = p_values,
    reltol = reltol_values,
    steptol = steptol_values,
    stringsAsFactors = FALSE
  )
  
  configs <- vector("list", nrow(grid))
  
  for (i in seq_len(nrow(grid))) {
    configs[[i]] <- list(
      itermax = grid$itermax[i],
      NP = grid$NP[i],
      strategy = grid$strategy[i],
      p = grid$p[i],
      reltol = grid$reltol[i],
      steptol = grid$steptol[i],
      trace = FALSE
    )
  }
  
  names(configs) <- paste0(
    "DE",
    "_iter", grid$itermax,
    "_NP", grid$NP,
    "_strat", grid$strategy,
    "_p", grid$p,
    "_rel", grid$reltol,
    "_step", grid$steptol
  )
  
  configs
}

optim_configs <- make_deoptim_settings()

# Fit-object summaries
fx <- list(
  "aic" = aic,
  "bic" = bic,
  "autocorrelation" = autocorrelation,
  "bias" = bias
)

# -----------------------------
# Helpers


choose_xfun <- function(model, x_generators, label_prefix = NULL) {
  # Selects the correct x function based on model dimentions

  k <- as.integer(model@k)

  candidates <- switch(
    as.character(k),
    "1" = grep("_1d$", names(x_generators), value = TRUE),
    "2" = grep("_2d$", names(x_generators), value = TRUE),
    "3" = grep("_3d$", names(x_generators), value = TRUE),
    stop("Unsupported k value: ", k)
  )

  if (!is.null(label_prefix)) {
    candidates <- candidates[grepl(label_prefix, candidates, fixed = TRUE)]
  }

  candidates
}

safe_cor <- function(x, y) {
  #Prevents crashing when computing correlation

  if (all(is.na(x)) || all(is.na(y))) return(NA_real_)
  if (sd(x, na.rm = TRUE) == 0 || sd(y, na.rm = TRUE) == 0) return(NA_real_)
  suppressWarnings(cor(x, y, use = "pairwise.complete.obs"))
}

summarise_recovery <- function(result, meta_row) {
  #Summarizes parameter recoveries:
  #  cor: recovery accuracy
  #  rmse: estimation error
  #  mean_bias: over/underestimation
  #  abs_bias: average deviation
  #  sim_sd and fit_sd: variability
  
  sim <- as.data.frame(result$simulate)
  fit <- as.data.frame(result$fit)

  common_cols <- intersect(colnames(sim), colnames(fit))
  if (length(common_cols) == 0) {
    stop("No overlapping parameter columns between result$simulate and result$fit")
  }

  out <- lapply(common_cols, function(par_name) {
    x <- sim[[par_name]]
    y <- fit[[par_name]]

    data.frame(
      model = meta_row$model,
      dynamics = meta_row$dynamics,
      covariance = meta_row$covariance,
      N = meta_row$N,
      x_condition = meta_row$x_condition,
      optim_config = meta_row$optim_config,
      parameter = par_name,
      cor = safe_cor(x, y),
      rmse = sqrt(mean((y - x)^2, na.rm = TRUE)),
      mean_bias = mean(y - x, na.rm = TRUE),
      abs_bias = mean(abs(y - x), na.rm = TRUE),
      sim_sd = sd(x, na.rm = TRUE),
      fit_sd = sd(y, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

plot_recovery_panels <- function(result, title_prefix = "") {
  #Scatter plots for recovery visualization 
  sim <- as.data.frame(result$simulate)
  fit <- as.data.frame(result$fit)
  common_cols <- intersect(colnames(sim), colnames(fit))

  plot_list <- lapply(common_cols, function(par_name) {
    plot_data <- data.frame(
      x = sim[[par_name]],
      y = fit[[par_name]]
    )

    lims <- range(c(plot_data$x, plot_data$y), na.rm = TRUE)

    ggplot(plot_data, aes(x = x, y = y)) +
      geom_abline(intercept = 0, slope = 1, linewidth = 1) +
      geom_point(alpha = 0.35) +
      annotate(
        "text",
        x = lims[1] + 0.05 * diff(lims),
        y = lims[1] + 0.95 * diff(lims),
        label = paste0("r = ", round(safe_cor(plot_data$x, plot_data$y), 2)),
        hjust = 0,
        size = 4
      ) +
      lims(x = lims, y = lims) +
      labs(
        title = paste0(title_prefix, par_name),
        x = "Simulated",
        y = "Estimated"
      ) +
      theme_bw()
  })

  if (length(plot_list) %% 2 == 1) {
    plot_list[[length(plot_list) + 1]] <- ggplot() + theme_void()
  }

  cowplot::plot_grid(plotlist = plot_list, ncol = 2)
}

run_one_condition <- function(model_name, model, dynamics, covariance, N,
                              x_name, xfun, optim_name, optim_args, seed) {
  set.seed(seed)
  
  meta <- data.frame(
    model = model_name,
    dynamics = dynamics,
    covariance = covariance,
    N = N,
    x_condition = x_name,
    optim_config = optim_name,
    seed = seed,
    stringsAsFactors = FALSE
  )
  
  recovery_args <- c(
    list(
      model,
      iterations = iterations,
      fx = fx,
      Xfun = xfun,
      N = N,
      dynamics = dynamics,
      covariance = covariance,
      print_iteration = TRUE,
      print_content = paste0(
        model_name, " | ",
        dynamics, " | ",
        covariance, " | N=", N, " | ",
        x_name, " | ",
        optim_name, " | seed=", seed
      )
    ),
    optim_args
  )
  
  result <- tryCatch(
    {
      do.call(recovery, recovery_args)
    },
    error = function(e) {
      message("FAILED: ", optim_name, " | ", conditionMessage(e))
      return(structure(
        list(error_message = conditionMessage(e)),
        class = "recovery_error"
      ))
    }
  )
  
  if (inherits(result, "recovery_error")) {
    summary_df <- data.frame(
      model = meta$model,
      dynamics = meta$dynamics,
      covariance = meta$covariance,
      N = meta$N,
      x_condition = meta$x_condition,
      optim_config = meta$optim_config,
      parameter = NA_character_,
      cor = NA_real_,
      rmse = NA_real_,
      mean_bias = NA_real_,
      abs_bias = NA_real_,
      sim_sd = NA_real_,
      fit_sd = NA_real_,
      failed = TRUE,
      error_message = result$error_message,
      stringsAsFactors = FALSE
    )
    
    return(list(
      result = result,
      summary = summary_df,
      meta = meta
    ))
  }
  
  summary_df <- summarise_recovery(result, meta)
  summary_df$failed <- FALSE
  summary_df$error_message <- NA_character_
  
  list(
    result = result,
    summary = summary_df,
    meta = meta
  )
}

# -----------------------------
# Condition Grid


condition_grid <- list()
idx <- 1

for (model_name in names(models)) {
  model <- models[[model_name]]

  valid_x_names <- choose_xfun(model, x_generators)

  for (x_name in valid_x_names) {
    for (n_i in Ns) {
      for (char_i in seq_len(nrow(characteristics))) {
        for (optim_name in names(optim_configs)) {
          condition_grid[[idx]] <- list(
            model_name = model_name,
            model = model,
            dynamics = characteristics$dynamics[char_i],
            covariance = characteristics$covariance[char_i],
            N = n_i,
            x_name = x_name,
            xfun = x_generators[[x_name]],
            optim_name = optim_name,
            optim_args = optim_configs[[optim_name]],
            seed = base_seed + idx
          )
          idx <- idx + 1
        }
      }
    }
  }
}

# -----------------------------
# Running the analysis

all_outputs <- mclapply(
  X = seq_along(condition_grid),
  FUN = function(i) {
    cfg <- condition_grid[[i]]

    out <- run_one_condition(
      model_name = cfg$model_name,
      model = cfg$model,
      dynamics = cfg$dynamics,
      covariance = cfg$covariance,
      N = cfg$N,
      x_name = cfg$x_name,
      xfun = cfg$xfun,
      optim_name = cfg$optim_name,
      optim_args = cfg$optim_args,
      seed = cfg$seed
    )

    base_name <- paste(
      cfg$model_name,
      cfg$dynamics,
      cfg$covariance,
      paste0("N", cfg$N),
      cfg$x_name,
      cfg$optim_name,
      paste0("seed", cfg$seed),
      sep = "__"
    )

    write.csv(
      out$summary,
      file.path("scripts", "results", "recovery_conditions", paste0(base_name, "__summary.csv")),
      row.names = FALSE
    )

    if (!inherits(out$result, "recovery_error")) {
  
      saveRDS(
        out$result,
        file.path("scripts", "results", "recovery_conditions", paste0(base_name, ".rds"))
      )
      
      plt <- plot_recovery_panels(
        out$result,
        title_prefix = paste0(
          cfg$model_name, " | ",
          cfg$dynamics, " | ",
          cfg$covariance, " | N=", cfg$N, " | ",
          cfg$x_name, " | ",
          cfg$optim_name, "\n"
        )
      )
      
      ggsave(
        filename = file.path("scripts", "figures", "recovery_conditions", paste0(base_name, ".png")),
        plot = plt,
        width = 12,
        height = 8
      )
      
    } else {
      
      saveRDS(
        out$result,
        file.path("scripts", "results", "recovery_conditions", paste0(base_name, "__FAILED.rds"))
      )
    }

    out$summary
  },
  mc.cores = ifelse(
    Sys.info()[["sysname"]] == "Windows",
    1,
    max(1, parallel::detectCores() - 1)
  )
)

summary_table <- do.call(rbind, all_outputs)

write.csv(
  summary_table,
  file.path("scripts", "results", "recovery_conditions", "ALL_SUMMARIES.csv"),
  row.names = FALSE
)

# -----------------------------
# Summary

de_diagnostics <- aggregate(
  cbind(cor, rmse, mean_bias, abs_bias) ~ optim_config + parameter,
  data = subset(summary_table, failed == FALSE),
  FUN = function(x) mean(x, na.rm = TRUE)
)

failure_summary <- aggregate(
  failed ~ optim_config,
  data = summary_table,
  FUN = function(x) mean(x, na.rm = TRUE)
)

names(failure_summary)[names(failure_summary) == "failed"] <- "failure_rate"

de_diagnostics <- merge(
  de_diagnostics,
  failure_summary,
  by = "optim_config",
  all.x = TRUE
)

write.csv(
  de_diagnostics,
  file.path("scripts", "results", "recovery_conditions", "DE_DIAGNOSTICS.csv"),
  row.names = FALSE
)

cat("\nDone.\n")
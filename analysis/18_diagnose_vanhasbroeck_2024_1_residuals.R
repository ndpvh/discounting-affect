################################################################################
# PURPOSE:
#
# Diagnose the time trend in VANHASBROECK_2024_1 residuals using binned means
# with participant-bootstrap intervals and participant-level linear slopes.
# Also test participant-level bin means, slopes, and grand means against zero
# using two-sided one-sample t-tests with Holm adjustment by test family.
# The test CSV's 95% confidence intervals are unadjusted; use its adjusted
# p-values for the family-wise significance decisions.
# The script uses the saved model estimates and does not refit models.
#
# Run from the repository root after analysis/01_process_data.R:
#   Rscript analysis/18_diagnose_vanhasbroeck_2024_1_residuals.R
################################################################################

source_file <- tryCatch(
  get0("ofile", envir = sys.frame(1)),
  error = function(e) NULL
)
source_dir <- if (is.character(source_file) &&
                  length(source_file) == 1L &&
                  nzchar(source_file)) {
  dirname(normalizePath(source_file, mustWork = FALSE))
} else {
  NULL
}
residual_script_candidates <- c(
  file.path("analysis", "17_plot_mean_residuals.R"),
  "17_plot_mean_residuals.R",
  if (!is.null(source_dir)) {
    file.path(source_dir, "17_plot_mean_residuals.R")
  }
)
residual_script <- residual_script_candidates[
  file.exists(residual_script_candidates)
]
if (length(residual_script) == 0L) {
  stop(
    "Could not find analysis/17_plot_mean_residuals.R. ",
    "Run this script from the repository root or analysis/ directory."
  )
}
residual_script <- residual_script[[1]]
source(residual_script)


bin_participant_residuals <- function(participant_residuals,
                                      bin_width = 10L) {
  required_columns <- c(
    "dataset", "model", "participant_id", "trial_index", "residual"
  )
  missing_columns <- setdiff(required_columns, names(participant_residuals))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing residual column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (nrow(participant_residuals) == 0L) {
    stop("No participant residuals were provided.")
  }
  if (length(bin_width) != 1L ||
      !is.finite(bin_width) ||
      bin_width < 1 ||
      bin_width != as.integer(bin_width)) {
    stop("`bin_width` must be one positive whole number.")
  }
  if (any(!is.finite(participant_residuals$trial_index)) ||
      any(participant_residuals$trial_index < 1) ||
      any(participant_residuals$trial_index !=
          as.integer(participant_residuals$trial_index))) {
    stop("Trial indices must be positive whole numbers.")
  }
  
  participant_residuals$bin_id <- (
    (participant_residuals$trial_index - 1L) %/% as.integer(bin_width)
  ) + 1L
  participant_residuals$bin_start <- (
    (participant_residuals$bin_id - 1L) * as.integer(bin_width)
  ) + 1L
  participant_residuals$bin_end <- (
    participant_residuals$bin_id * as.integer(bin_width)
  )
  participant_residuals$bin_center <- (
    participant_residuals$bin_start + participant_residuals$bin_end
  ) / 2
  
  group <- interaction(
    participant_residuals[c(
      "dataset", "model", "participant_id", "bin_id"
    )],
    drop = TRUE,
    lex.order = TRUE
  )
  indices_by_group <- split(seq_len(nrow(participant_residuals)), group)
  binned <- lapply(indices_by_group, function(indices) {
    rows <- participant_residuals[indices, , drop = FALSE]
    valid <- is.finite(rows$residual)
    if (!any(valid)) {
      return(NULL)
    }
    
    data.frame(
      dataset = rows$dataset[[1]],
      model = rows$model[[1]],
      participant_id = rows$participant_id[[1]],
      bin_id = rows$bin_id[[1]],
      bin_start = rows$bin_start[[1]],
      bin_end = rows$bin_end[[1]],
      bin_center = rows$bin_center[[1]],
      participant_mean = mean(rows$residual[valid]),
      n_trials = sum(valid),
      stringsAsFactors = FALSE
    )
  })
  
  binned <- Filter(Negate(is.null), binned)
  if (length(binned) == 0L) {
    stop("No finite residuals were available to bin.")
  }
  result <- do.call(rbind, binned)
  result <- result[
    order(
      result$dataset,
      result$model,
      result$bin_id,
      result$participant_id
    ),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}


with_local_seed <- function(seed, expression) {
  has_existing_seed <- exists(
    ".Random.seed",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  if (has_existing_seed) {
    previous_seed <- get(".Random.seed", envir = .GlobalEnv)
  }
  on.exit(
    if (has_existing_seed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    },
    add = TRUE
  )
  set.seed(seed)
  force(expression)
}


bootstrap_binned_residual_means <- function(participant_bins,
                                            n_boot = 1000L,
                                            seed = 20261124L) {
  required_columns <- c(
    "dataset", "model", "participant_id", "bin_id", "bin_start",
    "bin_end", "bin_center", "participant_mean"
  )
  missing_columns <- setdiff(required_columns, names(participant_bins))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing binned residual column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (nrow(participant_bins) == 0L) {
    stop("No participant-bin residual means were provided.")
  }
  if (length(n_boot) != 1L ||
      !is.finite(n_boot) ||
      n_boot < 1 ||
      n_boot != as.integer(n_boot)) {
    stop("`n_boot` must be one positive whole number.")
  }
  if (length(seed) != 1L ||
      !is.finite(seed) ||
      seed != as.integer(seed)) {
    stop("`seed` must be one whole number.")
  }
  if (anyDuplicated(interaction(
    participant_bins[c("dataset", "model", "participant_id", "bin_id")],
    drop = TRUE,
    lex.order = TRUE
  ))) {
    stop("Participant-bin residual means must be unique.")
  }
  
  group <- interaction(
    participant_bins[c("dataset", "model")],
    drop = TRUE,
    lex.order = TRUE
  )
  indices_by_group <- split(seq_len(nrow(participant_bins)), group)
  
  with_local_seed(seed, {
    summaries <- lapply(indices_by_group, function(indices) {
      rows <- participant_bins[indices, , drop = FALSE]
      participant_ids <- unique(rows$participant_id)
      bin_ids <- sort(unique(rows$bin_id))
      values <- matrix(
        NA_real_,
        nrow = length(participant_ids),
        ncol = length(bin_ids),
        dimnames = list(participant_ids, as.character(bin_ids))
      )
      for (i in seq_len(nrow(rows))) {
        values[
          rows$participant_id[[i]],
          as.character(rows$bin_id[[i]])
        ] <- rows$participant_mean[[i]]
      }
      
      observed_means <- colMeans(values, na.rm = TRUE)
      observed_means[!is.finite(observed_means)] <- NA_real_
      bootstrap_means <- vapply(
        seq_len(as.integer(n_boot)),
        function(iteration) {
          sampled <- sample.int(
            n = nrow(values),
            size = nrow(values),
            replace = TRUE
          )
          means <- colMeans(values[sampled, , drop = FALSE], na.rm = TRUE)
          means[!is.finite(means)] <- NA_real_
          means
        },
        numeric(length(bin_ids))
      )
      if (is.null(dim(bootstrap_means))) {
        bootstrap_means <- matrix(
          bootstrap_means,
          nrow = length(bin_ids)
        )
      }
      
      intervals <- lapply(seq_along(bin_ids), function(j) {
        valid_bootstrap_means <- bootstrap_means[j, ]
        valid_bootstrap_means <- valid_bootstrap_means[
          is.finite(valid_bootstrap_means)
        ]
        if (length(valid_bootstrap_means) == 0L) {
          return(c(lower = NA_real_, upper = NA_real_))
        }
        stats::quantile(
          valid_bootstrap_means,
          probs = c(0.025, 0.975),
          names = FALSE
        )
      })
      intervals <- do.call(rbind, intervals)
      
      data.frame(
        dataset = rows$dataset[[1]],
        model = rows$model[[1]],
        bin_id = unname(bin_ids),
        bin_start = vapply(
          bin_ids,
          function(bin_id) rows$bin_start[match(bin_id, rows$bin_id)],
          numeric(1)
        ),
        bin_end = vapply(
          bin_ids,
          function(bin_id) rows$bin_end[match(bin_id, rows$bin_id)],
          numeric(1)
        ),
        bin_center = vapply(
          bin_ids,
          function(bin_id) rows$bin_center[match(bin_id, rows$bin_id)],
          numeric(1)
        ),
        mean_residual = unname(observed_means),
        lower = intervals[, 1],
        upper = intervals[, 2],
        n_participants = colSums(is.finite(values)),
        valid_bootstrap_replicates = rowSums(is.finite(bootstrap_means)),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    })
    result <- do.call(rbind, summaries)
    result <- result[
      order(result$dataset, result$model, result$bin_id),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
    result
  })
}


summarize_participant_residual_slopes <- function(participant_residuals,
                                                  min_trials = 3L) {
  required_columns <- c(
    "dataset", "model", "participant_id", "trial_index", "residual"
  )
  missing_columns <- setdiff(required_columns, names(participant_residuals))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing residual column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (length(min_trials) != 1L ||
      !is.finite(min_trials) ||
      min_trials < 2 ||
      min_trials != as.integer(min_trials)) {
    stop("`min_trials` must be a whole number of at least two.")
  }
  
  group <- interaction(
    participant_residuals[c("dataset", "model", "participant_id")],
    drop = TRUE,
    lex.order = TRUE
  )
  indices_by_group <- split(seq_len(nrow(participant_residuals)), group)
  slopes <- lapply(indices_by_group, function(indices) {
    rows <- participant_residuals[indices, , drop = FALSE]
    valid <- is.finite(rows$trial_index) & is.finite(rows$residual)
    rows <- rows[valid, , drop = FALSE]
    if (length(unique(rows$trial_index)) < min_trials) {
      return(NULL)
    }
    
    fit <- stats::lm(residual ~ trial_index, data = rows)
    coefficients <- stats::coef(fit)
    data.frame(
      dataset = rows$dataset[[1]],
      model = rows$model[[1]],
      participant_id = rows$participant_id[[1]],
      n_trials = nrow(rows),
      intercept = unname(coefficients[["(Intercept)"]]),
      slope = unname(coefficients[["trial_index"]]),
      stringsAsFactors = FALSE
    )
  })
  
  slopes <- Filter(Negate(is.null), slopes)
  if (length(slopes) == 0L) {
    stop("No participants had enough finite trials to estimate a slope.")
  }
  result <- do.call(rbind, slopes)
  result <- result[
    order(result$dataset, result$model, result$participant_id),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}


summarize_slope_distributions <- function(participant_slopes) {
  required_columns <- c("dataset", "model", "slope")
  missing_columns <- setdiff(required_columns, names(participant_slopes))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing slope column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (nrow(participant_slopes) == 0L) {
    stop("No participant slopes were provided.")
  }
  
  group <- interaction(
    participant_slopes[c("dataset", "model")],
    drop = TRUE,
    lex.order = TRUE
  )
  indices_by_group <- split(seq_len(nrow(participant_slopes)), group)
  summaries <- lapply(indices_by_group, function(indices) {
    rows <- participant_slopes[indices, , drop = FALSE]
    slopes <- rows$slope[is.finite(rows$slope)]
    if (length(slopes) == 0L) {
      return(NULL)
    }
    data.frame(
      dataset = rows$dataset[[1]],
      model = rows$model[[1]],
      n_participants = length(slopes),
      mean_slope = mean(slopes),
      median_slope = stats::median(slopes),
      sd_slope = stats::sd(slopes),
      percent_negative = 100 * mean(slopes < 0),
      stringsAsFactors = FALSE
    )
  })
  summaries <- Filter(Negate(is.null), summaries)
  if (length(summaries) == 0L) {
    stop("No finite participant slopes were available to summarize.")
  }
  
  result <- do.call(rbind, summaries)
  result <- result[order(result$dataset, result$model), , drop = FALSE]
  rownames(result) <- NULL
  result
}


summarize_participant_grand_means <- function(participant_residuals) {
  required_columns <- c(
    "dataset", "model", "participant_id", "residual"
  )
  missing_columns <- setdiff(required_columns, names(participant_residuals))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing residual column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (nrow(participant_residuals) == 0L) {
    stop("No participant residuals were provided.")
  }
  
  group <- interaction(
    participant_residuals[c("dataset", "model", "participant_id")],
    drop = TRUE,
    lex.order = TRUE
  )
  indices_by_group <- split(seq_len(nrow(participant_residuals)), group)
  grand_means <- lapply(indices_by_group, function(indices) {
    rows <- participant_residuals[indices, , drop = FALSE]
    valid <- is.finite(rows$residual)
    if (!any(valid)) {
      return(NULL)
    }
    
    data.frame(
      dataset = rows$dataset[[1]],
      model = rows$model[[1]],
      participant_id = rows$participant_id[[1]],
      n_trials = sum(valid),
      grand_mean = mean(rows$residual[valid]),
      stringsAsFactors = FALSE
    )
  })
  
  grand_means <- Filter(Negate(is.null), grand_means)
  if (length(grand_means) == 0L) {
    stop("No finite participant residuals were available to average.")
  }
  result <- do.call(rbind, grand_means)
  result <- result[
    order(result$dataset, result$model, result$participant_id),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}


one_sample_t_test_row <- function(values,
                                  test_family,
                                  measure,
                                  dataset,
                                  model,
                                  bin_id = NA_integer_,
                                  bin_start = NA_integer_,
                                  bin_end = NA_integer_) {
  values <- values[is.finite(values)]
  n <- length(values)
  estimate <- if (n > 0L) mean(values) else NA_real_
  standard_error <- if (n > 1L) stats::sd(values) / sqrt(n) else NA_real_
  t_statistic <- NA_real_
  degrees_freedom <- NA_real_
  p_value <- NA_real_
  ci_lower <- NA_real_
  ci_upper <- NA_real_
  status <- "insufficient_participants"
  
  if (n > 1L) {
    if (stats::sd(values) == 0) {
      status <- "zero_variance"
    } else {
      test <- stats::t.test(
        values,
        mu = 0,
        alternative = "two.sided",
        conf.level = 0.95
      )
      t_statistic <- unname(test$statistic)
      degrees_freedom <- unname(test$parameter)
      p_value <- test$p.value
      ci_lower <- test$conf.int[[1]]
      ci_upper <- test$conf.int[[2]]
      status <- "ok"
    }
  }
  
  data.frame(
    test_family = test_family,
    test_method = "one_sample_t",
    alternative = "two.sided",
    measure = measure,
    dataset = dataset,
    model = model,
    bin_id = bin_id,
    bin_start = bin_start,
    bin_end = bin_end,
    n_participants = n,
    estimate = estimate,
    standard_error = standard_error,
    t_statistic = t_statistic,
    degrees_freedom = degrees_freedom,
    p_value = p_value,
    p_adjusted = NA_real_,
    p_adjust_method = "Holm within test_family",
    ci_level = 0.95,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    status = status,
    stringsAsFactors = FALSE
  )
}


test_participant_residual_means <- function(participant_residuals,
                                            participant_bins,
                                            participant_slopes) {
  bin_group <- interaction(
    participant_bins[c("dataset", "model", "bin_id")],
    drop = TRUE,
    lex.order = TRUE
  )
  bin_test_rows <- lapply(split(
    seq_len(nrow(participant_bins)),
    bin_group
  ), function(indices) {
    rows <- participant_bins[indices, , drop = FALSE]
    one_sample_t_test_row(
      values = rows$participant_mean,
      test_family = "per_bin",
      measure = "mean_residual",
      dataset = rows$dataset[[1]],
      model = rows$model[[1]],
      bin_id = rows$bin_id[[1]],
      bin_start = rows$bin_start[[1]],
      bin_end = rows$bin_end[[1]]
    )
  })
  
  slope_group <- interaction(
    participant_slopes[c("dataset", "model")],
    drop = TRUE,
    lex.order = TRUE
  )
  slope_test_rows <- lapply(split(
    seq_len(nrow(participant_slopes)),
    slope_group
  ), function(indices) {
    rows <- participant_slopes[indices, , drop = FALSE]
    one_sample_t_test_row(
      values = rows$slope,
      test_family = "slope",
      measure = "mean_slope",
      dataset = rows$dataset[[1]],
      model = rows$model[[1]]
    )
  })
  
  participant_grand_means <- summarize_participant_grand_means(
    participant_residuals
  )
  grand_mean_group <- interaction(
    participant_grand_means[c("dataset", "model")],
    drop = TRUE,
    lex.order = TRUE
  )
  grand_mean_test_rows <- lapply(split(
    seq_len(nrow(participant_grand_means)),
    grand_mean_group
  ), function(indices) {
    rows <- participant_grand_means[indices, , drop = FALSE]
    one_sample_t_test_row(
      values = rows$grand_mean,
      test_family = "grand_mean",
      measure = "mean_residual",
      dataset = rows$dataset[[1]],
      model = rows$model[[1]]
    )
  })
  
  result <- do.call(
    rbind,
    c(bin_test_rows, slope_test_rows, grand_mean_test_rows)
  )
  for (family in unique(result$test_family)) {
    indices <- which(
      result$test_family == family &
        is.finite(result$p_value)
    )
    if (length(indices) > 0L) {
      result$p_adjusted[indices] <- stats::p.adjust(
        result$p_value[indices],
        method = "holm"
      )
    }
  }
  
  result <- result[
    order(result$test_family, result$model, result$bin_id),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}


plot_vanha_2024_1_residual_diagnostics <- function(n_boot = 1000L,
                                                   seed = 20261124L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is required to create residual diagnostic plots.")
  }
  
  dataset_name <- "VANHASBROECK_2024_1"
  participant_residuals <- load_participant_residuals(dataset_name)
  participant_bins <- bin_participant_residuals(participant_residuals)
  binned_summary <- bootstrap_binned_residual_means(
    participant_bins,
    n_boot = n_boot,
    seed = seed
  )
  participant_slopes <- summarize_participant_residual_slopes(
    participant_residuals
  )
  slope_summary <- summarize_slope_distributions(participant_slopes)
  test_results <- test_participant_residual_means(
    participant_residuals,
    participant_bins,
    participant_slopes
  )
  
  results_dir <- file.path(PATHS$results, "residual_time_scale")
  figures_dir <- file.path(PATHS$figures, "residual_time_scale")
  ensure_dir(results_dir)
  ensure_dir(figures_dir)
  
  utils::write.csv(
    binned_summary,
    file.path(
      results_dir,
      paste0(dataset_name, "_binned_residual_means.csv")
    ),
    row.names = FALSE
  )
  utils::write.csv(
    participant_slopes,
    file.path(
      results_dir,
      paste0(dataset_name, "_participant_residual_slopes.csv")
    ),
    row.names = FALSE
  )
  utils::write.csv(
    slope_summary,
    file.path(
      results_dir,
      paste0(dataset_name, "_participant_slope_summary.csv")
    ),
    row.names = FALSE
  )
  utils::write.csv(
    test_results,
    file.path(
      results_dir,
      paste0(dataset_name, "_residual_mean_tests.csv")
    ),
    row.names = FALSE
  )
  
  model_labels <- c(
    exponential = "Exponential",
    quasi_hyperbolic = "Quasi-hyperbolic",
    double_exponential = "Double exponential"
  )
  binned_summary$model_label <- factor(
    unname(model_labels[binned_summary$model]),
    levels = unname(model_labels[MODEL_TYPES])
  )
  participant_slopes$model_label <- factor(
    unname(model_labels[participant_slopes$model]),
    levels = unname(model_labels[MODEL_TYPES])
  )
  
  mean_plot <- ggplot2::ggplot(
    binned_summary,
    ggplot2::aes(x = bin_center, y = mean_residual)
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray45"
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      fill = "#1F6FEB",
      alpha = 0.18,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(color = "#1F6FEB", linewidth = 0.6) +
    ggplot2::geom_point(color = "#1F6FEB", size = 1.6) +
    ggplot2::facet_wrap(
      ggplot2::vars(model_label),
      nrow = 1
    ) +
    ggplot2::labs(
      x = "Trial index (10-trial bin center)",
      y = "Mean residual (observed - predicted)",
      title = paste(dataset_name, "binned residual trend"),
      subtitle = paste0(
        "Pointwise 95% percentile intervals from ",
        n_boot,
        " whole-participant bootstrap resamples"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  slope_plot <- ggplot2::ggplot(
    participant_slopes,
    ggplot2::aes(x = slope)
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray45"
    ) +
    ggplot2::geom_histogram(
      bins = 30,
      fill = "#1F6FEB",
      color = "white"
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(model_label),
      nrow = 1,
      scales = "free_y"
    ) +
    ggplot2::labs(
      x = "Participant-specific residual slope per trial",
      y = "Number of participants",
      title = paste(dataset_name, "participant residual slopes"),
      subtitle = "Negative values indicate residuals tend to decrease over trials"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  mean_plot_path <- file.path(
    figures_dir,
    paste0(dataset_name, "_binned_residual_trend.jpeg")
  )
  slope_plot_path <- file.path(
    figures_dir,
    paste0(dataset_name, "_participant_slope_distribution.jpeg")
  )
  ggplot2::ggsave(
    filename = mean_plot_path,
    plot = mean_plot,
    width = 12,
    height = 4.5,
    units = "in",
    dpi = 300
  )
  ggplot2::ggsave(
    filename = slope_plot_path,
    plot = slope_plot,
    width = 12,
    height = 4.5,
    units = "in",
    dpi = 300
  )
  
  message("Saved residual trend plot to: ", mean_plot_path)
  message("Saved participant slope plot to: ", slope_plot_path)
  invisible(list(
    binned_summary = binned_summary,
    participant_slopes = participant_slopes,
    slope_summary = slope_summary,
    test_results = test_results
  ))
}


load_discounting_functions <- function() {
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(PROJECT_ROOT)
    return(invisible(NULL))
  }
  
  source_files <- c("models.R", "dataset.R", "utils.R", "predict.R")
  for (source_file in source_files) {
    sys.source(
      file.path(PROJECT_ROOT, "R", source_file),
      envir = .GlobalEnv
    )
  }
  invisible(NULL)
}


run_vanha_2024_1_residual_diagnostics <- function() {
  load_discounting_functions()
  plot_vanha_2024_1_residual_diagnostics()
}

if (sys.nframe() == 0L) {
  run_vanha_2024_1_residual_diagnostics()
}

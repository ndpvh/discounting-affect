

################################################################################
# PURPOSE:
#
# Summarize both distinguishability studies without re-estimating any models.
# For each study, report AIC/BIC model-identification accuracy, selection
# frequencies, criterion differences, and failures. Also summarize absolute
# goodness of fit (SSE, RMSE, MAE, residual bias and autocorrelation).
#
# Read raw simulations from the separate a priori and a posteriori directories.
# Save CSV tables under each study's summary/ directory and a JPEG confusion
# plot under analysis/figures/distinguishability/<study>/.
# The "pilot" argument reads only the separate pilot results.
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)

# A unique winner is required; missing fits and ties remain explicit.
select_best_model <- function(values, models) {
  if (length(values) != 3 || length(models) != 3 ||
      anyDuplicated(models) || any(!is.finite(values))) {
    return(NA_character_)
  }

  selected <- models[values == min(values)]
  if (length(selected) > 1) return("tie")
  selected
}


# Summarize one or both studies using a unique ID for each simulated dataset.
summarize_distinguishability <- function(fits) {
  required <- c(
    "study", "dataset", "d", "k", "generating_model", "fitted_model",
    "simulation_id", "aic", "bic", "objective_sse", "rmse", "mae",
    "bias", "autocorrelation", "error"
  )
  if (!all(required %in% names(fits))) {
    stop("Distinguishability results have missing columns.")
  }

  keys <- c("study", "dataset", "d", "k", "generating_model")
  simulation_keys <- c(keys, "simulation_id")

  winners <- fits |>
    group_by(study, dataset, d, k, generating_model, simulation_id) |>
    summarise(
      best_aic = select_best_model(aic, fitted_model),
      best_bic = select_best_model(bic, fitted_model),
      .groups = "drop"
    )

  selections <- winners |>
    pivot_longer(
      cols = c(best_aic, best_bic),
      names_to = "criterion", values_to = "selected_model"
    ) |>
    mutate(criterion = sub("^best_", "", criterion))

  accuracy <- selections |>
    group_by(study, dataset, d, k, generating_model, criterion) |>
    summarise(
      n_simulations = n(),
      n_complete = sum(!is.na(selected_model)),
      n_correct = sum(selected_model == generating_model, na.rm = TRUE),
      n_ties = sum(selected_model == "tie", na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      n_incomplete = n_simulations - n_complete,
      accuracy_pct = ifelse(
        n_complete > 0, 100 * n_correct / n_complete, NA_real_
      ),
      accuracy_all_pct = 100 * n_correct / n_simulations
    )

  confusion <- selections |>
    filter(!is.na(selected_model)) |>
    count(study, dataset, d, k, generating_model,
          criterion, selected_model, name = "n") |>
    left_join(
      accuracy |>
        transmute(study, dataset, d, k, generating_model,
                  criterion, n_complete),
      by = c(keys, "criterion")
    ) |>
    mutate(pct = 100 * n / n_complete)

  true_fit <- fits |>
    filter(fitted_model == generating_model) |>
    transmute(
      study, dataset, d, k, generating_model, simulation_id,
      true_aic = aic, true_bic = bic
    )

  differences <- fits |>
    left_join(true_fit, by = simulation_keys) |>
    mutate(
      delta_aic = aic - true_aic,
      delta_bic = bic - true_bic
    ) |>
    pivot_longer(
      cols = c(delta_aic, delta_bic),
      names_to = "criterion", values_to = "difference"
    ) |>
    mutate(criterion = sub("^delta_", "", criterion)) |>
    group_by(study, dataset, d, k, generating_model,
             fitted_model, criterion) |>
    summarise(
      n_pairs = sum(is.finite(difference)),
      mean_difference = if (n_pairs > 0) {
        mean(difference[is.finite(difference)])
      } else {
        NA_real_
      },
      median_difference = if (n_pairs > 0) {
        median(difference[is.finite(difference)])
      } else {
        NA_real_
      },
      .groups = "drop"
    )

  failures <- fits |>
    group_by(study, dataset, d, k, generating_model, fitted_model) |>
    summarise(
      n_fits = n(),
      n_failed = sum(!is.finite(aic) | !is.finite(bic) | !is.na(error)),
      .groups = "drop"
    )

  # These statistics are absolute fit measures, not model-selection criteria.
  # NaN from an entirely failed group is reported as NA in the CSV.
  goodness_of_fit <- fits |>
    group_by(study, dataset, d, k, generating_model, fitted_model) |>
    summarise(
      n_fits = n(),
      n_valid = sum(is.finite(rmse)),
      sse_mean = mean(objective_sse, na.rm = TRUE),
      sse_median = median(objective_sse, na.rm = TRUE),
      rmse_mean = mean(rmse, na.rm = TRUE),
      rmse_median = median(rmse, na.rm = TRUE),
      rmse_sd = sd(rmse, na.rm = TRUE),
      mae_mean = mean(mae, na.rm = TRUE),
      mae_median = median(mae, na.rm = TRUE),
      bias_mean = mean(bias, na.rm = TRUE),
      abs_bias_mean = mean(abs(bias), na.rm = TRUE),
      autocorrelation_mean = mean(autocorrelation, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(across(
      where(is.numeric), ~ replace(.x, is.nan(.x), NA_real_)
    ))

  list(
    accuracy = accuracy,
    confusion = confusion,
    differences = differences,
    failures = failures,
    goodness_of_fit = goodness_of_fit
  )
}


# Write a summary and confusion plot for each study separately.
run_distinguishability_summary <- function(pilot = FALSE, study = "both") {
  config_file <- if (file.exists(file.path("analysis", "_config.R"))) {
    file.path("analysis", "_config.R")
  } else if (file.exists("_config.R")) {
    "_config.R"
  } else {
    stop("Could not find analysis/_config.R. Run from the repository root.")
  }
  source(config_file, local = TRUE)
  source(file.path(PATHS$analysis, "_helpers.R"), local = TRUE)

  studies <- if (study == "both") {
    c("apriori", "aposteriori")
  } else {
    study
  }
  if (!all(studies %in% c("apriori", "aposteriori"))) {
    stop('Choose "both", "apriori", or "aposteriori".')
  }

  all_summaries <- list()
  for (name in studies) {
    input_dir <- file.path(PATHS$results, "distinguishability", name)
    figure_dir <- file.path(PATHS$figures, "distinguishability", name)
    if (pilot) {
      input_dir <- file.path(input_dir, "pilot")
      figure_dir <- file.path(figure_dir, "pilot")
    }

    files <- sort(list.files(
      input_dir, pattern = "\\.Rds$", full.names = TRUE
    ))
    n_expected <- if (name == "apriori") 12 else 6
    if (length(files) != n_expected) {
      stop("Expected ", n_expected, " ", name, " files in ", input_dir,
           ", found ", length(files), ".")
    }

    results <- lapply(files, readRDS)
    if (any(!vapply(results, function(x) {
      identical(x$settings$study, name) &&
        identical(x$settings$pilot, pilot)
    }, logical(1)))) {
      stop("Some result files belong to another study or run mode.")
    }
    if (pilot && any(vapply(results, function(x) {
      nrow(x$fits) != 6
    }, logical(1)))) {
      stop("The pilot did not complete two simulations per scenario.")
    }

    fits <- bind_rows(lapply(results, `[[`, "fits"))
    summaries <- summarize_distinguishability(fits)
    all_summaries[[name]] <- summaries

    if (pilot && any(summaries$failures$n_failed > 0)) {
      warning("The ", name, " pilot contains failed fits. ",
              "Inspect the raw results before running the full study.")
    }

    output_dir <- file.path(input_dir, "summary")
    ensure_dir(output_dir)
    ensure_dir(figure_dir)

    for (table_name in names(summaries)) {
      output_file <- paste0("distinguishability_", table_name, ".csv")
      write.csv(
        summaries[[table_name]],
        file.path(output_dir, output_file),
        row.names = FALSE
      )
    }

    confusion_plot <- summaries$confusion |>
      mutate(
        condition = paste0("d = ", d, ", k = ", k),
        selected_model = factor(
          selected_model,
          levels = c(
            "exponential", "quasi_hyperbolic", "double_exponential", "tie"
          )
        )
      ) |>
      group_by(condition, criterion, generating_model) |>
      complete(selected_model, fill = list(n = 0, pct = 0)) |>
      ungroup()

    if (nrow(confusion_plot) > 0) {
      plot <- ggplot(
        confusion_plot,
        aes(x = selected_model, y = generating_model, fill = pct)
      ) +
        geom_tile(color = "white") +
        geom_text(
          aes(label = paste0(round(pct), "%")), size = 3
        ) +
        facet_grid(criterion ~ condition) +
        scale_fill_gradient(limits = c(0, 100)) +
        labs(
          x = "Selected model", y = "Generating model",
          fill = "Selected (%)"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )

      ggsave(
        file.path(figure_dir, "confusion.jpeg"), plot,
        width = 15, height = 7, dpi = 300, bg = "white"
      )
    }

    message("Saved ", name, " summaries in ", output_dir)
  }

  invisible(all_summaries)
}


if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  allowed <- c("pilot", "apriori", "aposteriori")
  if (length(args) > 2 || any(!args %in% allowed) ||
      sum(args == "pilot") > 1 ||
      sum(args %in% c("apriori", "aposteriori")) > 1) {
    stop("Usage: Rscript analysis/15_summarize_distinguishability.R ",
         "[pilot] [apriori|aposteriori]")
  }
  study <- intersect(args, c("apriori", "aposteriori"))
  if (length(study) == 0) study <- "both"
  run_distinguishability_summary(pilot = "pilot" %in% args, study = study)
}
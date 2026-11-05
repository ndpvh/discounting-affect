################################################################################
# PURPOSE:
#
# Summarize the simulations saved by 14_run_distinguishability.R without
# repeating model estimation. Report how often AIC and BIC identify the
# generating model, which alternative models are selected, and the differences
# in information criteria between each candidate and the generating model.
#
# Save CSV summaries in analysis/results/distinguishability/summary/ and a
# model-selection heatmap in analysis/figures/distinguishability/.
# Pass the argument "pilot" to summarize the separate pilot results.
################################################################################

# Return the unique best-fitting model, or a tie / missing result.
select_best_model <- function(values, models) {
  if (length(values) != 3L || length(models) != 3L ||
      anyDuplicated(models) ||
      any(!is.finite(values))) {
    return(NA_character_)
  }

  selected <- models[values == min(values)]
  if (length(selected) > 1L) {
    return("tie")
  }

  selected
}


# Compare fitted models within each simulation and aggregate the results.
summarize_distinguishability <- function(fits) {
  required <- c(
    "iteration", "generating_model", "fitted_model", "d", "k",
    "aic", "bic", "error"
  )
  if (!all(required %in% names(fits))) {
    stop("Distinguishability results have missing columns.")
  }

  winners <- fits |>
    dplyr::group_by(d, k, generating_model, iteration) |>
    dplyr::summarise(
      best_aic = select_best_model(aic, fitted_model),
      best_bic = select_best_model(bic, fitted_model),
      .groups = "drop"
    )

  selections <- winners |>
    tidyr::pivot_longer(
      cols = c(best_aic, best_bic),
      names_to = "criterion",
      values_to = "selected_model"
    ) |>
    dplyr::mutate(criterion = sub("^best_", "", criterion))

  accuracy <- selections |>
    dplyr::group_by(d, k, generating_model, criterion) |>
    dplyr::summarise(
      n_simulations = dplyr::n(),
      n_complete = sum(!is.na(selected_model)),
      n_correct = sum(selected_model == generating_model, na.rm = TRUE),
      n_ties = sum(selected_model == "tie", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      n_incomplete = n_simulations - n_complete,
      accuracy_pct = ifelse(
        n_complete > 0, 100 * n_correct / n_complete, NA_real_
      )
    )

  confusion <- selections |>
    dplyr::filter(!is.na(selected_model)) |>
    dplyr::count(
      d, k, generating_model, criterion, selected_model, name = "n"
    ) |>
    dplyr::left_join(
      accuracy |>
        dplyr::select(d, k, generating_model, criterion, n_complete),
      by = c("d", "k", "generating_model", "criterion")
    ) |>
    dplyr::mutate(pct = 100 * n / n_complete)

  true_fit <- fits |>
    dplyr::filter(fitted_model == generating_model) |>
    dplyr::select(
      d, k, generating_model, iteration,
      true_aic = aic, true_bic = bic
    )

  differences <- fits |>
    dplyr::left_join(
      true_fit,
      by = c("d", "k", "generating_model", "iteration")
    ) |>
    dplyr::mutate(
      delta_aic = aic - true_aic,
      delta_bic = bic - true_bic
    ) |>
    tidyr::pivot_longer(
      cols = c(delta_aic, delta_bic),
      names_to = "criterion",
      values_to = "difference"
    ) |>
    dplyr::mutate(criterion = sub("^delta_", "", criterion)) |>
    dplyr::group_by(d, k, generating_model, fitted_model, criterion) |>
    dplyr::summarise(
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
    dplyr::group_by(d, k, generating_model, fitted_model) |>
    dplyr::summarise(
      n_fits = dplyr::n(),
      n_failed = sum(!is.finite(aic) | !is.finite(bic) | !is.na(error)),
      .groups = "drop"
    )

  list(
    accuracy = accuracy,
    confusion = confusion,
    differences = differences,
    failures = failures
  )
}


run_distinguishability_summary <- function(pilot = FALSE) {
  config_file <- if (file.exists(file.path("analysis", "_config.R"))) {
    file.path("analysis", "_config.R")
  } else if (file.exists("_config.R")) {
    "_config.R"
  } else {
    stop("Could not find analysis/_config.R. Run from the repository root.")
  }

  source(config_file, local = TRUE)
  source(file.path(PATHS$analysis, "_helpers.R"), local = TRUE)

  input_dir <- file.path(PATHS$results, "distinguishability")
  figure_dir <- file.path(PATHS$figures, "distinguishability")
  if (pilot) {
    input_dir <- file.path(input_dir, "pilot")
    figure_dir <- file.path(figure_dir, "pilot")
  }

  files <- list.files(input_dir, pattern = "\\.Rds$", full.names = TRUE)
  n_expected <- if (pilot) 3L else 12L
  if (length(files) != n_expected) {
    stop("Expected ", n_expected, " simulation files in ", input_dir,
         ", found ", length(files), ".")
  }

  fits <- dplyr::bind_rows(lapply(files, function(path) {
    readRDS(path)$fits
  }))
  summaries <- summarize_distinguishability(fits)

  output_dir <- file.path(input_dir, "summary")
  ensure_dir(output_dir)
  ensure_dir(figure_dir)

  for (name in names(summaries)) {
    write.csv(
      summaries[[name]],
      file.path(output_dir, paste0("distinguishability_", name, ".csv")),
      row.names = FALSE
    )
  }

  confusion_plot <- summaries$confusion |>
    dplyr::mutate(
      condition = paste0("d = ", d, ", k = ", k),
      selected_model = factor(
        selected_model,
        levels = c(
          "exponential", "quasi_hyperbolic", "double_exponential", "tie"
        )
      )
    ) |>
    dplyr::group_by(condition, criterion, generating_model) |>
    tidyr::complete(
      selected_model,
      fill = list(n = 0L, pct = 0)
    ) |>
    dplyr::ungroup()

  plot <- ggplot2::ggplot(
    confusion_plot,
    ggplot2::aes(x = selected_model, y = generating_model, fill = pct)
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(pct), "%")),
      size = 3
    ) +
    ggplot2::facet_grid(criterion ~ condition) +
    ggplot2::scale_fill_gradient(limits = c(0, 100)) +
    ggplot2::labs(
      x = "Selected model",
      y = "Generating model",
      fill = "Selected (%)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  ggplot2::ggsave(
    file.path(figure_dir, "distinguishability_confusion.jpeg"),
    plot,
    width = 15,
    height = 7,
    dpi = 300,
    bg = "white"
  )

  message("Distinguishability summaries saved in ", output_dir)
  invisible(summaries)
}


if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 1L || (length(args) == 1L && args != "pilot")) {
    stop('Usage: Rscript analysis/15_summarize_distinguishability.R [pilot]')
  }
  run_distinguishability_summary(pilot = identical(args, "pilot"))
}

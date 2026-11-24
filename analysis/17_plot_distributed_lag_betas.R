################################################################################
# PURPOSE:
#
# Visualize the estimated slopes (beta) of the distributed lag model as a
# function of the lag, using the estimation results from 02_estimate_models.R.
#
# In the distributed lag model, the slopes are estimated freely for every lag
# up to DISTRIBUTED_LAG_LAG (see _config.R). Plotting these slopes against the
# lag therefore shows how the effect of each independent variable on affect
# evolves over time, without imposing a particular discounting shape. This
# offers a model-free view on the shape of the discounting function that can be
# compared to the shapes implied by the discounting models.
#
# For every dataset, one figure is created with:
#   - one row per dependent variable (e.g., positive and negative affect)
#   - one column per independent variable (e.g., outcome)
#   - the lag on the x-axis and the value of beta on the y-axis
#   - thin grey lines for the individual participants
#   - the median across participants (line) and interquartile range (ribbon)
#
# The column names of the estimation results follow the convention of
# `parameter_names`, where "beta_ij_l" denotes the slope of independent
# variable j on dependent variable i at lag l.
#
# Outputs are written to:
#   analysis/results/distributed_lag_betas/   (long-format betas and summary)
#   analysis/figures/distributed_lag_betas/   (one figure per dataset)
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

library(ggplot2)


################################################################################
# SETTINGS
################################################################################

input_dir <- PATHS$estimation
output_dir <- file.path(PATHS$results, "distributed_lag_betas")
figure_dir <- file.path(PATHS$figures, "distributed_lag_betas")

require_paths(input_dir, path_type = "dir", context = "estimation")
ensure_dir(output_dir)
ensure_dir(figure_dir)

# Display names of the datasets
dataset_labels <- c(
  VANHASBROECK_2021   = "Vanhasbroeck 2021",
  VANHASBROECK_2022   = "Vanhasbroeck 2022",
  VANHASBROECK_2024_1 = "Vanhasbroeck 2024 (Valence)",
  VANHASBROECK_2024_2 = "Vanhasbroeck 2024 (PA/NA)",
  NIEMEIJER_2022      = "Niemeijer 2022"
)

# Names of the dependent (y) and independent (x) variables per dataset, in the
# order in which they are stored in the datasets (see 01_process_data.R)
variable_labels <- list(
  VANHASBROECK_2021 = list(
    y = c("Happiness"),
    x = c("Certain reward", "Expected value", "Reward prediction error")
  ),
  VANHASBROECK_2022 = list(
    y = c("Positive affect", "Negative affect"),
    x = c("Outcome")
  ),
  VANHASBROECK_2024_1 = list(
    y = c("Valence"),
    x = c("Outcome")
  ),
  VANHASBROECK_2024_2 = list(
    y = c("Positive affect", "Negative affect"),
    x = c("Outcome")
  ),
  NIEMEIJER_2022 = list(
    y = c("Positive affect", "Negative affect"),
    x = c("Positive context", "Negative context")
  )
)


################################################################################
# LOAD AND RESHAPE THE ESTIMATES
################################################################################

estimates <- list()

for (dataset_name in ESTIMATION_DATASETS) {

  path <- file.path(input_dir, paste0(dataset_name, "_distributed_lag.csv"))
  if (!file.exists(path)) {
    warning("No distributed lag estimates found for ", dataset_name, ": ", path)
    next
  }

  df <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)

  # Select the beta columns and decompose their names into the dependent
  # variable (i), independent variable (j), and lag (l)
  beta_cols <- grep("^beta_[0-9][0-9]_[0-9]+$", names(df), value = TRUE)
  if (length(beta_cols) == 0L) {
    warning("No beta columns found for ", dataset_name)
    next
  }

  parts <- regmatches(beta_cols, regexec("^beta_([0-9])([0-9])_([0-9]+)$", beta_cols))

  long <- do.call(
    rbind,
    lapply(
      seq_along(beta_cols),
      function(b) {
        data.frame(
          dataset = dataset_name,
          participant_id = df$participant_id,
          y = as.integer(parts[[b]][2]),
          x = as.integer(parts[[b]][3]),
          lag = as.integer(parts[[b]][4]),
          beta = df[[beta_cols[b]]],
          stringsAsFactors = FALSE
        )
      }
    )
  )

  # Add readable labels for the variables. Fall back to the indices when no
  # labels are defined or when the number of variables does not match.
  labels <- variable_labels[[dataset_name]]
  long$y_label <- if (!is.null(labels) && max(long$y) <= length(labels$y)) {
    labels$y[long$y]
  } else {
    paste0("Y", long$y)
  }
  long$x_label <- if (!is.null(labels) && max(long$x) <= length(labels$x)) {
    labels$x[long$x]
  } else {
    paste0("X", long$x)
  }

  estimates[[dataset_name]] <- long
}

if (length(estimates) == 0L) {
  stop(
    "No distributed lag estimates found in ", input_dir, ". ",
    "Run 02_estimate_models.R first."
  )
}

estimates <- do.call(rbind, estimates)
rownames(estimates) <- NULL

# Remove failed estimations (rows with NA)
estimates <- estimates[!is.na(estimates$beta), , drop = FALSE]


################################################################################
# SUMMARIZE
################################################################################

summary_betas <- do.call(
  rbind,
  lapply(
    split(
      estimates,
      list(estimates$dataset, estimates$y, estimates$x, estimates$lag),
      drop = TRUE
    ),
    function(df) {
      data.frame(
        dataset = df$dataset[1],
        y = df$y[1],
        x = df$x[1],
        y_label = df$y_label[1],
        x_label = df$x_label[1],
        lag = df$lag[1],
        n = nrow(df),
        mean = mean(df$beta),
        sd = sd(df$beta),
        median = median(df$beta),
        q25 = unname(quantile(df$beta, 0.25)),
        q75 = unname(quantile(df$beta, 0.75)),
        stringsAsFactors = FALSE
      )
    }
  )
)

summary_betas <- summary_betas[
  order(
    match(summary_betas$dataset, ESTIMATION_DATASETS),
    summary_betas$y,
    summary_betas$x,
    summary_betas$lag
  ),
]
rownames(summary_betas) <- NULL

write.csv(
  estimates,
  file.path(output_dir, "distributed_lag_betas.csv"),
  row.names = FALSE
)
write.csv(
  summary_betas,
  file.path(output_dir, "distributed_lag_betas_summary.csv"),
  row.names = FALSE
)


################################################################################
# PLOTS
################################################################################

for (dataset_name in unique(estimates$dataset)) {

  df <- estimates[estimates$dataset == dataset_name, , drop = FALSE]
  sm <- summary_betas[summary_betas$dataset == dataset_name, , drop = FALSE]

  # Keep the order of the variables as they appear in the data
  y_levels <- unique(df$y_label[order(df$y)])
  x_levels <- unique(df$x_label[order(df$x)])

  df$y_label <- factor(df$y_label, levels = y_levels)
  df$x_label <- factor(df$x_label, levels = x_levels)
  sm$y_label <- factor(sm$y_label, levels = y_levels)
  sm$x_label <- factor(sm$x_label, levels = x_levels)

  n_y <- length(y_levels)
  n_x <- length(x_levels)
  max_lag <- max(df$lag)

  plt <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_line(
      data = df,
      aes(x = lag, y = beta, group = participant_id),
      color = "grey60",
      alpha = 0.3,
      linewidth = 0.3
    ) +
    geom_ribbon(
      data = sm,
      aes(x = lag, ymin = q25, ymax = q75),
      fill = "#4C72B0",
      alpha = 0.3
    ) +
    geom_line(
      data = sm,
      aes(x = lag, y = median),
      color = "#4C72B0",
      linewidth = 1
    ) +
    geom_point(
      data = sm,
      aes(x = lag, y = median),
      color = "#4C72B0",
      size = 1.5
    ) +
    facet_grid(
      y_label ~ x_label,
      scales = "free_y",
      switch = "y"
    ) +
    scale_x_continuous(breaks = 0:max_lag) +
    labs(
      title = dataset_labels[[dataset_name]],
      subtitle = paste0(
        "Distributed lag model (lag = ", max_lag, "), ",
        length(unique(df$participant_id)), " participants. ",
        "Line and band: median and interquartile range across participants."
      ),
      x = "Lag",
      y = expression(beta)
    ) +
    theme_bw() +
    theme(
      strip.placement = "outside",
      panel.grid.minor = element_blank(),
      plot.subtitle = element_text(size = 8)
    )

  ggsave(
    file.path(figure_dir, paste0(dataset_name, "_distributed_lag_betas.jpeg")),
    plt,
    width = 600 + n_x * 1100,
    height = 500 + n_y * 900,
    units = "px",
    dpi = 300
  )
}

message("Results written to ", output_dir)
message("Figures written to ", figure_dir)

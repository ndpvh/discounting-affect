################################################################################
# PURPOSE:
#
# Visualize the results of the discounting model estimation and comparison.
# This script produces four groups of plots:
#
#   1. PARAMETER HISTOGRAMS
#      Distribution of estimated parameter values per model and dataset.
#
#   2. BEST MODEL BAR PLOTS
#      Percentage of participants best described by each model (based on AIC
#      and BIC). Uses the same "best model" logic as comparative_analysis.R.
#
#   3. PAIRWISE COMPARISON HISTOGRAMS
#      For each pair of models (exponential vs quasi-hyperbolic, etc.), shows
#      the distribution of AIC differences across participants. A difference
#      close to zero means the two models fit equally well; a large positive
#      difference means model B fits better; large negative means model A fits
#      better. This is the plot style used in the 2022 paper.
#
#   4. SSE DISTRIBUTION PLOTS
#      Distribution of the raw sum of squared errors (SSE) per model and
#      dataset. Lower SSE = better fit to the data. Comparing these
#      distributions across models shows which model consistently achieves
#      lower prediction error.
#
#
# All plots are saved as PNG files to scripts/figures/visualization/.
################################################################################

library(ggplot2)
library(tidyr) 
library(dplyr)

# Create the output folders for figures if they do not already exist.
# The main folder contains four subfolders, one for each plot group.
# I found this better for organizing the output than dumping everything into one folder.
figure_dir <- file.path("scripts", "figures", "visualization")

figure_subdirs <- list(
  parameters  = file.path(figure_dir, "01_parameter_histograms"),
  best_model  = file.path(figure_dir, "02_best_model_barplots"),
  pairwise    = file.path(figure_dir, "03_pairwise_comparisons"),
  sse         = file.path(figure_dir, "04_sse_distributions")
)

invisible(lapply(figure_subdirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# Small helper for clean file names
safe_filename <- function(x) {
  gsub("[^A-Za-z0-9_-]+", "_", x)
}


################################################################################
# LOAD DATA
#
# We load every CSV produced by the estimation script and combine them into
# one large data.frame called `all_results`. Each row is one participant from
# one dataset fitted by one model. The columns are:
#   participant_id, model, dataset, <parameters>, aic, bic,
#   autocorrelation, bias, objective_sse
#
# We also recreate the `long` and `best` objects from comparative_analysis.R
# so that we can reuse that logic for the bar plots and pairwise comparisons.
################################################################################

input_dir <- file.path("scripts", "results", "estimation")

# The five datasets and three models — same as comparative_analysis.R
datasets <- c(
  "VANHASBROECK_2021",
  "VANHASBROECK_2022",
  "VANHASBROECK_2024_1",
  "VANHASBROECK_2024_2",
  "NIEMEIJER_2022"
)

models <- c("exponential", "quasi_hyperbolic", "double_exponential")
metrics <- c("aic", "bic")

# Statistics columns that are NOT parameters — we need to separate these
# when drawing parameter histograms
stat_cols <- c("aic", "bic", "autocorrelation", "bias", "objective_sse")

# ── Load all CSVs into one combined data.frame ────────────────────────────────
# bind_rows() is used instead of rbind() because different models/datasets can
# have different parameter columns. Missing columns are filled with NA.
all_results <- bind_rows(lapply(datasets, function(ds) {
  bind_rows(lapply(models, function(m) {
    path <- file.path(input_dir, paste0(ds, "_", m, ".csv"))

    # If the file doesn't exist, skip it with a warning
    if (!file.exists(path)) {
      warning("File not found, skipping: ", path)
      return(NULL)
    }

    df <- read.csv(path, check.names = FALSE)
    df$participant_id <- as.character(df$participant_id)
    df$dataset <- ds
    df$model <- m
    return(df)
  }))
}))

cat("Total rows loaded:", nrow(all_results), "\n")
cat("Datasets:         ", paste(unique(all_results$dataset), collapse = ", "), "\n")
cat("Models:           ", paste(unique(all_results$model),   collapse = ", "), "\n")

# ── Recreate `long` from comparative_analysis.R ───────────────────────────────
# `long` is a named list: one entry per dataset, each containing all three
# models stacked with columns: participant_id, model, aic, bic
long <- lapply(datasets, function(ds) {
  bind_rows(lapply(models, function(m) {
    path <- file.path(input_dir, paste0(ds, "_", m, ".csv"))
    if (!file.exists(path)) {
      warning("File not found, skipping: ", path)
      return(NULL)
    }

    df <- read.csv(path, check.names = FALSE)
    df$participant_id <- as.character(df$participant_id)
    df$model <- m

    return(df[, c("participant_id", "model", metrics)])
  }))
}) |> `names<-`(datasets)

# Remove datasets that did not load correctly
long <- long[!vapply(long, is.null, logical(1))]

# ── Recreate `best` from comparative_analysis.R ───────────────────────────────
# `best` tells us, per metric and dataset, which model each participant
# was best described by (lowest AIC or BIC)
best <- lapply(metrics, function(x) {
  lapply(names(long), function(y) {
    do.call(rbind, lapply(
      split(long[[y]], long[[y]]$participant_id),
      function(df) df[which.min(df[[x]]), ]
    ))
  }) |> `names<-`(names(long))
}) |> `names<-`(metrics)

# ── Recreate `counts` from comparative_analysis.R ─────────────────────────────
# `counts` tells us for each metric + dataset what % of participants each
# model won
counts <- lapply(metrics, function(x) {
  do.call(rbind, lapply(names(long), function(y) {
    count <- table(best[[x]][[y]]$model)
    data.frame(
      metric          = x,
      dataset         = y,
      model           = names(count),
      n_best          = as.numeric(count),
      percentage_best = round(100 * as.numeric(count) / sum(count), 1)
    )
  }))
})
counts <- do.call(rbind, counts)

# ── run_pairwise() — identical to comparative_analysis.R ─────────────────────
run_pairwise <- function(long_df, model_a, model_b, metric = "aic") {
  sub_df  <- long_df[long_df$model %in% c(model_a, model_b), ]
  wide_df <- reshape(sub_df[, c("participant_id", "model", metric)],
                     idvar     = "participant_id",
                     timevar   = "model",
                     direction = "wide")
  names(wide_df) <- gsub(paste0(metric, "."), "", names(wide_df), fixed = TRUE)
  wide_df$winner <- ifelse(wide_df[[model_a]] < wide_df[[model_b]],
                           model_a, model_b)
  wide_df$difference <- wide_df[[model_a]] - wide_df[[model_b]]
  list(per_person = wide_df,
       summary    = data.frame(
         model      = names(table(wide_df$winner)),
         n_best     = as.numeric(table(wide_df$winner)),
         percentage = round(100 * as.numeric(table(wide_df$winner)) /
                              nrow(wide_df), 1)
       ))
}

# ── Nicer display names for plots ─────────────────────────────────────────────
# These replace underscores with spaces and make things easier to read on plots
model_labels <- c(
  exponential        = "Exponential",
  quasi_hyperbolic   = "Quasi-Hyperbolic",
  double_exponential = "Double-Exponential"
)

dataset_labels <- c(
  VANHASBROECK_2021   = "Vanhasbroeck 2021",
  VANHASBROECK_2022   = "Vanhasbroeck 2022",
  VANHASBROECK_2024_1 = "Vanhasbroeck 2024 (PA/NA)",
  VANHASBROECK_2024_2 = "Vanhasbroeck 2024 (Valence)",
  NIEMEIJER_2022      = "Niemeijer 2022"
)

# A consistent colour palette — one colour per model, used in every plot
model_colours <- c(
  exponential        = "#4C72B0",   # blue
  quasi_hyperbolic   = "#DD8452",   # orange
  double_exponential = "#55A868"    # green
)

plot_colours <- setNames(
  unname(model_colours),
  unname(model_labels[names(model_colours)])
)


################################################################################
# PARAMETER HISTOGRAMS
#
# For each model, we identify which columns are parameters (i.e., not
# participant_id, dataset, model, or one of the five stat_cols) and draw
# one histogram per parameter, split into panels by dataset.
#
# Description:
#   Each bar shows how many participants had an estimated parameter value
#   in that range. A narrow tall peak means most participants have similar
#   values; a wide spread means participants vary a lot.
#
# pivot_longer() is used to reshape the data from wide format (one column
# per parameter) to long format (one row per parameter value), which is
# what ggplot2 needs for facet_wrap().
################################################################################

cat("\nDrawing separate parameter histograms...\n")

for (m in models) {

  # Subset to just this model
  df_model <- all_results[all_results$model == m, ]

  if (nrow(df_model) == 0) {
    warning("No rows found for model: ", m)
    next
  }

  # Identify parameter columns by excluding everything that is not a parameter
  non_param_cols <- c("participant_id", "model", "dataset", stat_cols)

  candidate_param_cols <- setdiff(names(df_model), non_param_cols)

  param_cols <- candidate_param_cols[
    vapply(df_model[candidate_param_cols], function(x) any(!is.na(x)), logical(1))
  ]

  cat("  Model:", m, "| Parameters:", paste(param_cols, collapse = ", "), "\n")

  # Draw one graph per parameter.
  # This creates files such as:
  #   parameters_exponential_alpha.png
  #   parameters_quasi_hyperbolic_beta.png
  for (param in param_cols) {

    df_param <- df_model[, c("participant_id", "dataset", "model", param)]
    names(df_param)[names(df_param) == param] <- "value"

    # Remove rows where the value is NA (failed participants or absent parameter)
    df_param <- df_param[!is.na(df_param$value), ]

    if (nrow(df_param) == 0) {
      warning("No non-NA values for model ", m, ", parameter ", param)
      next
    }

    df_param$dataset_label <- dataset_labels[df_param$dataset]

    p <- ggplot(df_param, aes(x = value)) +

      geom_histogram(
        bins   = 30,
        colour = "white",
        fill   = model_colours[[m]],
        alpha  = 0.85
      ) +

      # One panel per dataset only. The parameter and model are now separated
      # at the file level, which makes each graph easier to read.
      facet_wrap(~ dataset_label, scales = "free", nrow = 2) +

      labs(
        title    = paste0("Parameter distribution — ", param),
        subtitle = paste0("Model: ", model_labels[[m]]),
        x        = "Estimated value",
        y        = "Number of participants"
      ) +

      theme_bw() +
      theme(
        strip.text  = element_text(size = 9),
        plot.title  = element_text(face = "bold")
      )

    out_path <- file.path(
      figure_subdirs$parameters,
      paste0("parameters_", safe_filename(m), "_", safe_filename(param), ".png")
    )

    ggsave(out_path, p, width = 10, height = 7, dpi = 150)
    cat("  Saved:", out_path, "\n")
  }
}


################################################################################
# BEST MODEL BAR PLOTS
#
# For each metric (AIC and BIC), one bar plot per dataset showing what
# percentage of participants were best described by each of the three models.
#
# Description:
#   Each bar represents one model. The height is the percentage of participants
#   for whom that model had the lowest AIC (or BIC). If one bar is much taller
#   than the others, that model consistently fits the best for that dataset.
#
# We draw one figure that uses facet_wrap to show all datasets side by side,
# once for AIC and once for BIC.
################################################################################

cat("\nDrawing best model bar plots...\n")

for (metric in metrics) {

  df_metric <- counts[counts$metric == metric, ]

  # Replace internal names with nicer labels for the plot
  df_metric$model_label   <- model_labels[df_metric$model]
  df_metric$dataset_label <- dataset_labels[df_metric$dataset]

  p <- ggplot(df_metric,
              aes(x    = model_label,
                  y    = percentage_best,
                  fill = model_label)) +

    geom_bar(stat = "identity", colour = "white", width = 0.7) +

    # Add the percentage as a text label on top of each bar
    geom_text(aes(label = paste0(percentage_best, "%")),
              vjust = -0.4, size = 3.5) +

    # One panel per dataset
    facet_wrap(~ dataset_label, nrow = 1) +

    scale_fill_manual(values = setNames(model_colours, model_labels)) +

    # Set y axis to go from 0 to 100 with some headroom for the labels
    scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +

    labs(
      title    = paste("Best model per participant —", toupper(metric)),
      subtitle = "Percentage of participants best described by each model",
      x        = NULL,
      y        = paste0("% best by ", toupper(metric)),
      fill     = "Model"
    ) +

    theme_bw() +
    theme(
      axis.text.x     = element_blank(),   # remove x tick labels (redundant with legend)
      axis.ticks.x    = element_blank(),
      legend.position = "bottom",
      plot.title      = element_text(face = "bold"),
      strip.text      = element_text(size = 9)
    )

  out_path <- file.path(figure_subdirs$best_model, paste0("best_model_", metric, ".png"))
  ggsave(out_path, p, width = 14, height = 6, dpi = 150)
  cat("  Saved:", out_path, "\n")
}


################################################################################
# PAIRWISE COMPARISON HISTOGRAMS
#
# For each pair of models and each dataset, we compute the AIC difference
# per participant:
#     difference = AIC(model_a) - AIC(model_b)
#
# This difference is then shown as a histogram. Description:
#   - Bars to the LEFT  (negative difference): model A fits better
#   - Bars to the RIGHT (positive difference): model B fits better
#   - Bars near ZERO: the two models fit equally well
#   - A vertical red dashed line marks zero for reference
#
# We do this for both AIC and BIC, and for all three pairs of models.
# One figure is produced per pair × metric combination.
################################################################################

cat("\nDrawing pairwise comparison histograms...\n")

# All three pairs
pairs <- list(
  list(a = "exponential",      b = "quasi_hyperbolic"),
  list(a = "quasi_hyperbolic", b = "double_exponential"),
  list(a = "exponential",      b = "double_exponential")
)

for (metric in metrics) {
  for (pair in pairs) {

    model_a <- pair$a
    model_b <- pair$b

    # Run pairwise comparison for every dataset and stack into one data.frame
    pw_all <- do.call(rbind, lapply(names(long), function(ds) {
      if (is.null(long[[ds]])) return(NULL)
      pw      <- run_pairwise(long[[ds]], model_a, model_b, metric)
      df      <- pw$per_person
      df$dataset <- ds
      return(df)
    }))

    pw_all <- pw_all[!is.na(pw_all$difference), ]
    pw_all$dataset_label <- dataset_labels[pw_all$dataset]

    # Label for the x axis: "AIC(Exponential) - AIC(Quasi-Hyperbolic)"
    x_label <- paste0(
      toupper(metric), "(", model_labels[model_a], ") - ",
      toupper(metric), "(", model_labels[model_b], ")"
    )

    p <- ggplot(pw_all, aes(x = difference)) +

      geom_histogram(bins = 40, fill = "#4C72B0", colour = "white", alpha = 0.8) +

      # Vertical line at zero: left of this = model A better, right = model B better
      geom_vline(xintercept = 0, colour = "red", linetype = "dashed", linewidth = 0.8) +

      # One panel per dataset
      facet_wrap(~ dataset_label, scales = "free_y", nrow = 1) +

      labs(
        title    = paste0("Pairwise comparison: ",
                          model_labels[model_a], " vs ", model_labels[model_b]),
        subtitle = paste0("Metric: ", toupper(metric),
                          " | Left of red line = ", model_labels[model_a],
                          " better | Right = ", model_labels[model_b], " better"),
        x        = x_label,
        y        = "Number of participants"
      ) +

      theme_bw() +
      theme(
        plot.title  = element_text(face = "bold"),
        strip.text  = element_text(size = 9)
      )

    pair_name <- paste0(model_a, "_vs_", model_b)
    out_path  <- file.path(figure_subdirs$pairwise,
                           paste0("pairwise_", pair_name, "_", metric, ".png"))
    ggsave(out_path, p, width = 16, height = 5, dpi = 150)
    cat("  Saved:", out_path, "\n")
  }
}


################################################################################
# SSE DISTRIBUTION PLOTS
#
# The SSE (sum of squared errors) measures how far the model's predictions
# are from the observed data — lower is better.
#
# We produce two types of SSE plots:
#
#   5a. OVERLAPPING DENSITY PLOTS per dataset
#       All three models shown on the same panel for each dataset.
#       Useful for seeing which model achieves lower error overall.
#       geom_density draws a smooth curve rather than bars, which makes it
#       easier to compare three distributions at once.
#
#   5b. BOXPLOTS per model and dataset
#       The box shows the middle 50% of participants (the interquartile range).
#       The line inside the box is the median. Dots outside the whiskers are
#       outliers. This makes it easy to compare typical SSE values and spread.
################################################################################

cat("\nDrawing SSE distribution plots...\n")

# Subset to only what we need and add nice labels
df_sse <- all_results[, c("participant_id", "dataset", "model", "objective_sse")]
df_sse <- df_sse[!is.na(df_sse$objective_sse), ]
df_sse$model_label   <- model_labels[df_sse$model]
df_sse$dataset_label <- dataset_labels[df_sse$dataset]

# ── 5a: Overlapping density curves ───────────────────────────────────────────
p_density <- ggplot(df_sse,
                    aes(x     = objective_sse,
                        colour = model_label,
                        fill   = model_label)) +

  # geom_density draws a smooth estimated distribution curve
  # alpha makes the fill semi-transparent so overlapping curves are visible
  geom_density(alpha = 0.2, linewidth = 0.8) +

  # One panel per dataset
  facet_wrap(~ dataset_label, scales = "free", nrow = 2) +

  
  scale_colour_manual(values = plot_colours)
  scale_fill_manual(values = plot_colours)

  labs(
    title    = "SSE distribution by model and dataset",
    subtitle = "Lower SSE = better fit | Curves shifted left = better fitting model",
    x        = "Objective SSE",
    y        = "Density",
    colour   = "Model",
    fill     = "Model"
  ) +

  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold"),
    strip.text      = element_text(size = 9)
  )

out_path <- file.path(figure_subdirs$sse, "sse_density.png")
ggsave(out_path, p_density, width = 14, height = 8, dpi = 150)
cat("  Saved:", out_path, "\n")

# ── 5b: Boxplots ──────────────────────────────────────────────────────────────
p_box <- ggplot(df_sse,
                aes(x    = model_label,
                    y    = objective_sse,
                    fill = model_label)) +

  # geom_boxplot draws the box-and-whisker plot
  # outlier.alpha makes the outlier dots semi-transparent to reduce clutter
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.8) +

  # One panel per dataset
  facet_wrap(~ dataset_label, scales = "free_y", nrow = 2) +

  scale_fill_manual(values = plot_colours, labels = model_labels) +

  labs(
    title    = "SSE distribution by model and dataset",
    subtitle = "Lower box = better fit | Narrower box = more consistent fit",
    x        = NULL,
    y        = "Objective SSE",
    fill     = "Model"
  ) +

  theme_bw() +
  theme(
    axis.text.x     = element_blank(),
    axis.ticks.x    = element_blank(),
    legend.position = "bottom",
    plot.title      = element_text(face = "bold"),
    strip.text      = element_text(size = 9)
  )

out_path <- file.path(figure_subdirs$sse, "sse_boxplot.png")
ggsave(out_path, p_box, width = 14, height = 8, dpi = 150)
cat("  Saved:", out_path, "\n")

cat("\nAll figures saved inside:", figure_dir, "\n")
print(figure_subdirs)


#################################################################################
# BASE VISUALIZATION EXAMPLES OF EACH MODEL
################################################################################


library(ggplot2)

# Create folder
dir.create("figures", showWarnings = FALSE)

# Lag values: j = how many time steps in the past
lags <- 0:20

# Common theme for all plots
theme_discount <- theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# Common axis scales
x_scale <- scale_x_continuous(
  breaks = seq(0, 20, 2),
  limits = c(0, 20)
)

y_scale <- scale_y_continuous(
  limits = c(0, 1),
  breaks = seq(0, 1, 0.25)
)

# Common save settings
plot_width <- 7
plot_height <- 5


# ============================================================
# 1. Exponential model
# Model definition:
# y_t = alpha + sum_{j=0}^t Gamma^j B x_{t-j} + epsilon_t
#
# Scalar visualisation:
# w_j = gamma^j
# ============================================================

gamma_values <- c(0.30, 0.60, 0.90)

df_exp <- do.call(
  rbind,
  lapply(gamma_values, function(gamma) {
    data.frame(
      lag = lags,
      weight = gamma^lags,
      curve = paste0("gamma = ", gamma)
    )
  })
)

p_exp <- ggplot(df_exp, aes(x = lag, y = weight, colour = curve)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  x_scale +
  y_scale +
  labs(
    title = "Exponential discounting model",
    subtitle = expression(w[j] == gamma^j),
    x = "Lag j: time steps in the past",
    y = "Discount weight applied to past outcome",
    colour = expression(gamma)
  ) +
  scale_colour_discrete(
    labels = c(
      expression(gamma == 0.30),
      expression(gamma == 0.60),
      expression(gamma == 0.90)
    )
  ) +
  theme_discount

p_exp

ggsave(
  filename = "figures/exponential_discounting.pdf",
  plot = p_exp,
  width = plot_width,
  height = plot_height
)


# ============================================================
# 2. Quasi-hyperbolic model
# Model definition:
# y_t = alpha + sum_{j=0}^t N^j K^i(j) B x_{t-j} + epsilon_t
#
# where i(j) = 0 if j = 0, and i(j) = 1 otherwise.
#
# Scalar visualisation:
# w_j = nu^j * kappa^I(j > 0)
#
# This means:
# w_0 = 1
# w_j = kappa * nu^j, for j > 0
# ============================================================

nu_fixed <- 0.80
kappa_values <- c(0.25, 0.60, 0.90)

df_qh <- do.call(
  rbind,
  lapply(kappa_values, function(kappa) {
    data.frame(
      lag = lags,
      weight = ifelse(lags == 0, 1, kappa * nu_fixed^lags),
      curve = paste0("K = ", kappa)
    )
  })
)

p_qh <- ggplot(df_qh, aes(x = lag, y = weight, colour = curve)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  x_scale +
  y_scale +
  labs(
    title = "Quasi-hyperbolic discounting model",
    subtitle = expression(w[j] == nu^j * kappa^I(j > 0)),
    x = "Lag j: time steps in the past",
    y = "Discount weight applied to past outcome",
    colour = expression(kappa)
  ) +
  scale_colour_discrete(
    labels = c(
      expression(kappa == 0.25),
      expression(kappa == 0.60),
      expression(kappa == 0.90)
    )
  ) +
  theme_discount

p_qh

ggsave(
  filename = "figures/quasi_hyperbolic_discounting.pdf",
  plot = p_qh,
  width = plot_width,
  height = plot_height
)


# ============================================================
# 3. Double-exponential model
# Model definition:
# y_t = alpha + sum_{j=0}^t [omega Gamma^j + (1 - omega) N^j] B x_{t-j}
#       + epsilon_t
#
# Scalar visualisation:
# w_j = omega * gamma^j + (1 - omega) * nu^j
#
# In models.R, omega is constrained to [0, 0.5].
# Here, gamma is set to a faster decay process and nu to a slower one.
# ============================================================

gamma_fixed <- 0.30
nu_fixed_de <- 0.85
omega_values <- c(0.10, 0.30, 0.50)

df_de <- do.call(
  rbind,
  lapply(omega_values, function(omega) {
    data.frame(
      lag = lags,
      weight = omega * gamma_fixed^lags + (1 - omega) * nu_fixed_de^lags,
      curve = paste0("omega = ", omega)
    )
  })
)

p_de <- ggplot(df_de, aes(x = lag, y = weight, colour = curve)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  x_scale +
  y_scale +
  labs(
    title = "Double-exponential discounting model",
    subtitle = expression(w[j] == omega * gamma^j + (1 - omega) * nu^j),
    x = "Lag j: time steps in the past",
    y = "Discount weight applied to past outcome",
    colour = expression(omega)
  ) +
  scale_colour_discrete(
    labels = c(
      expression(omega == 0.10),
      expression(omega == 0.30),
      expression(omega == 0.50)
    )
  ) +
  theme_discount

p_de

ggsave(
  filename = "figures/double_exponential_discounting.pdf",
  plot = p_de,
  width = plot_width,
  height = plot_height
)


#################################################################################
# VISUALIZATION EXAMPLES OF CHANGE IN N
################################################################################

# ============================================================
# 1. Quasi-hyperbolic model: varying N (nu)
#
# Here, kappa is fixed and nu varies
# ============================================================

kappa_fixed <- 0.60
nu_values <- c(0.30, 0.60, 0.90)

df_qh_n <- do.call(
  rbind,
  lapply(nu_values, function(nu) {
    data.frame(
      lag = lags,
      weight = ifelse(lags == 0, 1, kappa_fixed * nu^lags),
      curve = paste0("N = ", nu)
    )
  })
)

p_qh_n <- ggplot(df_qh_n, aes(x = lag, y = weight, colour = curve)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  x_scale +
  y_scale +
  labs(
    title = "Quasi-hyperbolic discounting model",
    subtitle = expression(w[j] == nu^j * kappa^I(j > 0)),
    x = "Lag j: time steps in the past",
    y = "Discount weight applied to past outcome",
    colour = "N Value"
  ) +
  theme_discount

p_qh_n

ggsave(
  filename = "figures/quasi_hyperbolic_varying_N.pdf",
  plot = p_qh_n,
  width = plot_width,
  height = plot_height
)


# ============================================================
# 2. Double-exponential model: varying N (nu)
# Here, gamma and omega are fixed and nu varies
# ============================================================

gamma_fixed <- 0.30
omega_fixed <- 0.30
nu_values_de <- c(0.40, 0.65, 0.90)

df_de_n <- do.call(
  rbind,
  lapply(nu_values_de, function(nu) {
    data.frame(
      lag = lags,
      weight = omega_fixed * gamma_fixed^lags + (1 - omega_fixed) * nu^lags,
      curve = paste0("N = ", nu)
    )
  })
)

p_de_n <- ggplot(df_de_n, aes(x = lag, y = weight, colour = curve)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  x_scale +
  y_scale +
  labs(
    title = "Double-exponential discounting model",
    subtitle = expression(w[j] == omega * gamma^j + (1 - omega) * nu^j),
    x = "Lag j: time steps in the past",
    y = "Discount weight applied to past outcome",
    colour = "N value"
  ) +
  theme_discount

p_de_n

ggsave(
  filename = "figures/double_exponential_varying_N.pdf",
  plot = p_de_n,
  width = plot_width,
  height = plot_height
)
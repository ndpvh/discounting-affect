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
# All plots are saved as JPEG files to analysis/figures/ (PATHS$figures).
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
library(tidyr)
library(dplyr)
library(ggpubr)

# Create the output folders for figures if they do not already exist.
# The main folder contains four subfolders, one for each plot group.
# I found this better for organizing the output than dumping everything into one folder.
figure_dir <- file.path(PATHS$figures, "visualization")

figure_subdirs <- list(
  parameters  = file.path(figure_dir, "01_parameter_histograms"),
  best_model  = file.path(figure_dir, "02_best_model_barplots"),
  pairwise    = file.path(figure_dir, "03_pairwise_comparisons"),
  sse         = file.path(figure_dir, "04_sse_distributions")
)

invisible(lapply(figure_subdirs, ensure_dir))

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

input_dir <- PATHS$estimation

# The five datasets and three models — same as comparative_analysis.R
datasets <- ESTIMATION_DATASETS

models <- MODEL_TYPES
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

# ── Recreate `best` with explicit exact-tie handling ──────────────────────────
# `best` tells us, per metric and dataset, which model each participant was best
# described by (lowest AIC or BIC). If two or more models share the exact same
# minimum, the participant is labeled "tie" instead of being assigned according
# to whichever model happened to appear first.
best <- lapply(metrics, function(x) {
  lapply(names(long), function(y) {
    do.call(rbind, lapply(
      split(long[[y]], long[[y]]$participant_id),
      function(df) {
        valid <- !is.na(df[[x]])
        out <- df[1, , drop = FALSE]

        if (!any(valid)) {
          out$model <- NA_character_
          out[[x]] <- NA_real_
          return(out)
        }

        minimum <- min(df[[x]][valid])
        winners <- df[valid & df[[x]] == minimum, , drop = FALSE]
        out <- winners[1, , drop = FALSE]

        if (nrow(winners) > 1L) out$model <- "tie"
        out
      }
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
  wide_df$winner <- ifelse(
    wide_df[[model_a]] < wide_df[[model_b]],
    model_a,
    ifelse(
      wide_df[[model_a]] > wide_df[[model_b]],
      model_b,
      "tie"
    )
  )
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
  double_exponential = "Double-Exponential",
  tie                = "Tie"
)

dataset_labels <- c(
  VANHASBROECK_2021   = "Vanhasbroeck 2021",
  VANHASBROECK_2022   = "Vanhasbroeck 2022",
  VANHASBROECK_2024_1 = "Vanhasbroeck 2024 (Valence)",
  VANHASBROECK_2024_2 = "Vanhasbroeck 2024 (PA/NA)",
  NIEMEIJER_2022      = "Niemeijer 2022"
)

# A consistent colour palette — one colour per model, used in every plot
model_colours <- c(
  exponential        = "#4C72B0",   # blue
  quasi_hyperbolic   = "#DD8452",   # orange
  double_exponential = "#55A868",   # green
  tie                = "#7F7F7F"    # grey
)

plot_colours <- setNames(
  unname(model_colours),
  unname(model_labels[names(model_colours)])
)

# Point shapes provide a second visual cue for the line graphs, so the curves
# can be distinguished without relying only on colour.
curve_shapes <- c(16, 17, 15)  # circle, triangle, square


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
  #   parameters_exponential_alpha.jpeg
  #   parameters_quasi_hyperbolic_beta.jpeg
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
      paste0("parameters_", safe_filename(m), "_", safe_filename(param), ".jpeg")
    )

    ggsave(out_path, p, width = 10, height = 7, dpi = 300)
    cat("  Saved:", out_path, "\n")
  }
}


################################################################################
# BEST MODEL BAR PLOTS
#
# For each metric (AIC and BIC), one bar plot per dataset showing what
# percentage of participants were best described by each model; exact ties are
# shown explicitly as a separate "Tie" category.
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

  out_path <- file.path(figure_subdirs$best_model, paste0("best_model_", metric, ".jpeg"))
  ggsave(out_path, p, width = 14, height = 6, dpi = 300)
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

      coord_cartesian(xlim = c(-20, 20)) +

      theme_bw() +
      theme(
        plot.title  = element_text(face = "bold"),
        strip.text  = element_text(size = 9)
      )

    pair_name <- paste0(model_a, "_vs_", model_b)
    out_path  <- file.path(figure_subdirs$pairwise,
                           paste0("pairwise_", pair_name, "_", metric, ".jpeg"))
    ggsave(out_path, p, width = 16, height = 5, dpi = 300)
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

  
  scale_colour_manual(values = plot_colours) +
  scale_fill_manual(values = plot_colours) +

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

out_path <- file.path(figure_subdirs$sse, "sse_density.jpeg")
ggsave(out_path, p_density, width = 14, height = 8, dpi = 300)
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

out_path <- file.path(figure_subdirs$sse, "sse_boxplot.jpeg")
ggsave(out_path, p_box, width = 14, height = 8, dpi = 300)
cat("  Saved:", out_path, "\n")

cat("\nAll figures saved inside:", figure_dir, "\n")
print(figure_subdirs)

#################################################################################
# BASE VISUALIZATION EXAMPLES OF EACH MODEL
#################################################################################

library(ggplot2)
library(ggpubr)

# Make subdirectory for these figures if it does not already exist
base_fig_dir <- file.path(PATHS$figures, "base_model_figures")

if (!dir.exists(base_fig_dir)) {
  ensure_dir(base_fig_dir)
  cat("Created directory:", base_fig_dir, "\n")
} else {
  cat("Directory already exists:", base_fig_dir, "\n")
}


# ==============================================================================
# SHARED PLOTTING SETTINGS
# ==============================================================================

# Show discounting only up to lag 10
lags <- 0:10

# Point shapes provide a second visual cue in addition to colour
curve_shapes <- c(16, 17, 15)  # circle, triangle, square

# Common axis scales
x_scale <- scale_x_continuous(
  breaks = 0:10,
  limits = c(0, 10)
)

y_scale <- scale_y_continuous(
  limits = c(0, 1),
  breaks = seq(0, 1, 0.25)
)

# Theme for figures displayed individually
theme_discount <- theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",

    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 19
    ),

    axis.title = element_text(
      size = 17
    ),

    axis.text = element_text(
      size = 14
    ),

    legend.title = element_text(
      size = 15
    ),

    legend.text = element_text(
      size = 13
    ),

    panel.grid.minor = element_blank()
  )

# Theme specifically for plots that will appear side by side.
# The text is deliberately larger because each panel becomes smaller
# once two plots are combined into one manuscript figure.
theme_discount_paired <- theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",

    # Individual panel titles are removed because the combined figure
    # receives one centered model title.
    plot.title = element_blank(),

    axis.title = element_text(
      size = 18
    ),

    axis.text = element_text(
      size = 15
    ),

    legend.title = element_text(
      size = 16
    ),

    legend.text = element_text(
      size = 14
    ),

    panel.grid.minor = element_blank(),

    plot.margin = margin(
      t = 5,
      r = 10,
      b = 5,
      l = 10
    )
  )

# Common save settings for individual plots
plot_width <- 7
plot_height <- 5


# ==============================================================================
# RUTLEDGE ET AL. (2014) REFERENCE CURVE
# ==============================================================================

gamma_rutledge <- 0.61

df_rutledge <- data.frame(
  lag = lags,
  weight = gamma_rutledge^lags
)

p_rutledge <- ggplot(
  df_rutledge,
  aes(x = lag, y = weight)
) +
  geom_line(
    linewidth = 1.2,
    colour = "black"
  ) +
  geom_point(
    size = 2.8,
    shape = 16,
    colour = model_colours[["exponential"]]
  ) +
  x_scale +
  y_scale +
  labs(
    title = "Rutledge et al. (2014)",
    x = "Lag",
    y = "Weight"
  ) +
  theme_discount

p_rutledge

ggsave(
  filename = file.path(
    base_fig_dir,
    "rutledge_2014_discounting.jpeg"
  ),
  plot = p_rutledge,
  width = plot_width,
  height = plot_height,
  dpi = 300
)


# ==============================================================================
# 1. EXPONENTIAL
# ==============================================================================

gamma_values <- c(0.30, 0.60, 0.90)
gamma_labels <- sprintf("%.2f", gamma_values)

df_exp <- do.call(
  rbind,
  lapply(seq_along(gamma_values), function(i) {

    gamma <- gamma_values[i]

    data.frame(
      lag = lags,
      weight = gamma^lags,
      curve = factor(
        gamma_labels[i],
        levels = gamma_labels
      )
    )
  })
)

p_exp <- ggplot(
  df_exp,
  aes(
    x = lag,
    y = weight,
    colour = curve,
    shape = curve
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 2.8
  ) +
  x_scale +
  y_scale +
  labs(
    title = "Exponential",
    x = "Lag",
    y = "Weight",
    colour = expression(gamma),
    shape = expression(gamma)
  ) +
  scale_colour_discrete(
    labels = gamma_labels
  ) +
  scale_shape_manual(
    values = curve_shapes,
    labels = gamma_labels
  ) +
  theme_discount

p_exp

ggsave(
  filename = file.path(
    base_fig_dir,
    "exponential_discounting.jpeg"
  ),
  plot = p_exp,
  width = plot_width,
  height = plot_height,
  dpi = 300
)


# ==============================================================================
# 2. QUASI-HYPERBOLIC: VARYING KAPPA
# ==============================================================================

nu_fixed <- 0.80

kappa_values <- c(0.25, 0.60, 0.90)
kappa_labels <- sprintf("%.2f", kappa_values)

df_qh <- do.call(
  rbind,
  lapply(seq_along(kappa_values), function(i) {

    kappa <- kappa_values[i]

    data.frame(
      lag = lags,
      weight = ifelse(
        lags == 0,
        1,
        kappa * nu_fixed^lags
      ),
      curve = factor(
        kappa_labels[i],
        levels = kappa_labels
      )
    )
  })
)

p_qh <- ggplot(
  df_qh,
  aes(
    x = lag,
    y = weight,
    colour = curve,
    shape = curve
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 2.8
  ) +
  x_scale +
  y_scale +
  labs(
    title = "Quasi-hyperbolic",
    x = "Lag",
    y = "Weight",
    colour = expression(kappa),
    shape = expression(kappa)
  ) +
  scale_colour_discrete(
    labels = kappa_labels
  ) +
  scale_shape_manual(
    values = curve_shapes,
    labels = kappa_labels
  ) +
  theme_discount

p_qh


# ==============================================================================
# 3. QUASI-HYPERBOLIC: VARYING NU
# ==============================================================================

kappa_fixed <- 0.60

nu_values <- c(0.30, 0.60, 0.90)
nu_labels <- sprintf("%.2f", nu_values)

df_qh_n <- do.call(
  rbind,
  lapply(seq_along(nu_values), function(i) {

    nu <- nu_values[i]

    data.frame(
      lag = lags,
      weight = ifelse(
        lags == 0,
        1,
        kappa_fixed * nu^lags
      ),
      curve = factor(
        nu_labels[i],
        levels = nu_labels
      )
    )
  })
)

p_qh_n <- ggplot(
  df_qh_n,
  aes(
    x = lag,
    y = weight,
    colour = curve,
    shape = curve
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 2.8
  ) +
  x_scale +
  y_scale +
  labs(
    title = "Quasi-hyperbolic",
    x = "Lag",
    y = "Weight",
    colour = expression(nu),
    shape = expression(nu)
  ) +
  scale_colour_discrete(
    labels = nu_labels
  ) +
  scale_shape_manual(
    values = curve_shapes,
    labels = nu_labels
  ) +
  theme_discount

p_qh_n


# ==============================================================================
# COMBINED QUASI-HYPERBOLIC FIGURE
#
# Panel A varies kappa.
# Panel B varies nu.
#
# Each plot retains its own legend because the parameters differ.
# ==============================================================================

p_qh_pair_left <- p_qh +
  theme_discount_paired

p_qh_pair_right <- p_qh_n +
  theme_discount_paired

fig_qh_pair <- ggpubr::ggarrange(
  p_qh_pair_left,
  p_qh_pair_right,

  ncol = 2,
  nrow = 1,

  labels = c("A", "B"),
  font.label = list(
    size = 18,
    face = "bold"
  ),

  align = "hv",

  common.legend = FALSE
)

fig_qh_pair <- ggpubr::annotate_figure(
  fig_qh_pair,

  top = ggpubr::text_grob(
    "Quasi-hyperbolic",
    face = "bold",
    size = 20
  )
)

fig_qh_pair

ggsave(
  filename = file.path(
    base_fig_dir,
    "quasi_hyperbolic_paired.jpeg"
  ),
  plot = fig_qh_pair,
  width = 13,
  height = 6.5,
  dpi = 300
)


# ==============================================================================
# 4. DOUBLE-EXPONENTIAL: VARYING OMEGA
# ==============================================================================

gamma_fixed <- 0.30
nu_fixed_de <- 0.85

omega_values <- c(0.10, 0.30, 0.50)
omega_labels <- sprintf("%.2f", omega_values)

df_de <- do.call(
  rbind,
  lapply(seq_along(omega_values), function(i) {

    omega <- omega_values[i]

    data.frame(
      lag = lags,
      weight =
        omega * gamma_fixed^lags +
        (1 - omega) * nu_fixed_de^lags,

      curve = factor(
        omega_labels[i],
        levels = omega_labels
      )
    )
  })
)

p_de <- ggplot(
  df_de,
  aes(
    x = lag,
    y = weight,
    colour = curve,
    shape = curve
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 2.8
  ) +
  x_scale +
  y_scale +
  labs(
    title = "Double-exponential",
    x = "Lag",
    y = "Weight",
    colour = expression(omega),
    shape = expression(omega)
  ) +
  scale_colour_discrete(
    labels = omega_labels
  ) +
  scale_shape_manual(
    values = curve_shapes,
    labels = omega_labels
  ) +
  theme_discount

p_de


# ==============================================================================
# 5. DOUBLE-EXPONENTIAL: VARYING NU
# ==============================================================================

gamma_fixed <- 0.30
omega_fixed <- 0.30

nu_values_de <- c(0.40, 0.65, 0.90)
nu_labels_de <- sprintf("%.2f", nu_values_de)

df_de_n <- do.call(
  rbind,
  lapply(seq_along(nu_values_de), function(i) {

    nu <- nu_values_de[i]

    data.frame(
      lag = lags,
      weight =
        omega_fixed * gamma_fixed^lags +
        (1 - omega_fixed) * nu^lags,

      curve = factor(
        nu_labels_de[i],
        levels = nu_labels_de
      )
    )
  })
)

p_de_n <- ggplot(
  df_de_n,
  aes(
    x = lag,
    y = weight,
    colour = curve,
    shape = curve
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 2.8
  ) +
  x_scale +
  y_scale +
  labs(
    title = "Double-exponential",
    x = "Lag",
    y = "Weight",
    colour = expression(nu),
    shape = expression(nu)
  ) +
  scale_colour_discrete(
    labels = nu_labels_de
  ) +
  scale_shape_manual(
    values = curve_shapes,
    labels = nu_labels_de
  ) +
  theme_discount

p_de_n


# ==============================================================================
# COMBINED DOUBLE-EXPONENTIAL FIGURE
#
# Panel A varies omega.
# Panel B varies nu.
#
# Each panel keeps its own legend because the parameters differ.
# ==============================================================================

p_de_pair_left <- p_de +
  theme_discount_paired

p_de_pair_right <- p_de_n +
  theme_discount_paired

fig_de_pair <- ggpubr::ggarrange(
  p_de_pair_left,
  p_de_pair_right,

  ncol = 2,
  nrow = 1,

  labels = c("A", "B"),
  font.label = list(
    size = 18,
    face = "bold"
  ),

  align = "hv",

  common.legend = FALSE
)

fig_de_pair <- ggpubr::annotate_figure(
  fig_de_pair,

  top = ggpubr::text_grob(
    "Double-exponential",
    face = "bold",
    size = 20
  )
)

fig_de_pair

ggsave(
  filename = file.path(
    base_fig_dir,
    "double_exponential_paired.jpeg"
  ),
  plot = fig_de_pair,
  width = 13,
  height = 6.5,
  dpi = 300
)

##################################################################################
# PARAMETRIC BOOTSTRAP HEATMAPS
##################################################################################

# Paths ------------------------------------------------------------------------

bootstrap_file <- file.path(
  PATHS$parametric_bootstrap,
  "bootstrap_summary.Rds"
)

output_dir <- file.path(
  PATHS$figures,
  "bootstrap_analysis"
)

if (!file.exists(bootstrap_file)) {
  stop(
    "Bootstrap summary not found at: ",
    bootstrap_file,
    "\nRun parametric_bootstrap.R before creating this figure."
  )
}

ensure_dir(output_dir)


# Prepare data -----------------------------------------------------------------

# bootstrap_summary.Rds is a named list containing one data frame per model.
bootstrap_results <- readRDS(bootstrap_file) |>
  dplyr::bind_rows(.id = "model")

# Average coverage across the outcome/predictor variables belonging to each
# phenomenon. Coverage is expressed as a proportion between 0 and 1.
bootstrap_plot_data <- bootstrap_results |>
  dplyr::group_by(dataset, model, phenomenon) |>
  dplyr::summarise(
    coverage = mean(covered, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::filter(is.finite(coverage))

phenomenon_order <- c(
  "autocorrelation_1",
  "autocorrelation_2",
  "autocorrelation_3",
  "residual_autocorrelation_1",
  "residual_autocorrelation_2",
  "residual_autocorrelation_3",
  "outcome_correlation_0",
  "outcome_correlation_1",
  "outcome_correlation_2",
  "outcome_correlation_3",
  "outcome_correlation_4",
  "outcome_correlation_5",
  "moment_1",
  "moment_2",
  "moment_3",
  "moment_4",
  "bimodality_coefficient"
)

phenomenon_labels <- c(
  "Affect autocorrelation: lag 1",
  "Affect autocorrelation: lag 2",
  "Affect autocorrelation: lag 3",
  "Residual autocorrelation: lag 1",
  "Residual autocorrelation: lag 2",
  "Residual autocorrelation: lag 3",
  "Predictor–affect correlation: lag 0",
  "Predictor–affect correlation: lag 1",
  "Predictor–affect correlation: lag 2",
  "Predictor–affect correlation: lag 3",
  "Predictor–affect correlation: lag 4",
  "Predictor–affect correlation: lag 5",
  "Mean",
  "Variance",
  "Third central moment",
  "Fourth central moment",
  "Bimodality coefficient"
)

bootstrap_plot_data <- bootstrap_plot_data |>
  dplyr::mutate(
    dataset = factor(
      dataset,
      levels = c(
        "VANHASBROECK_2021",
        "VANHASBROECK_2022",
        "VANHASBROECK_2024_1",
        "VANHASBROECK_2024_2",
        "NIEMEIJER_2022"
      ),
      labels = c(
        "Vanhasbroeck et al. (2021)",
        "Vanhasbroeck et al. (2022)",
        "Vanhasbroeck et al. (2024): valence",
        "Vanhasbroeck et al. (2024): positive/negative affect",
        "Niemeijer et al. (2022)"
      )
    ),
    model = factor(
      model,
      levels = c(
        "exponential",
        "quasi_hyperbolic",
        "double_exponential"
      ),
      labels = c(
        "Exponential",
        "Quasi-\nhyperbolic",
        "Double-\nexponential"
      )
    ),
    phenomenon = factor(
      phenomenon,
      levels = rev(phenomenon_order),
      labels = rev(phenomenon_labels)
    ),
    coverage_label = sprintf("%.0f", 100 * coverage)
  )


# Plot -------------------------------------------------------------------------

bootstrap_coverage_plot <- ggplot(
  bootstrap_plot_data,
  aes(
    x = model,
    y = phenomenon,
    fill = coverage
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = coverage_label,
      colour = coverage >= 0.55
    ),
    size = 2.8
  ) +
  facet_wrap(
    ~ dataset,
    ncol = 3
  ) +
  scale_fill_gradient(
    low = "#f7fbff",
    high = "#08306b",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = scales::label_percent(accuracy = 1),
    oob = scales::squish
  ) +
  scale_colour_manual(
    values = c(
      "FALSE" = "black",
      "TRUE" = "white"
    ),
    guide = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Coverage"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      size = 9
    ),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    legend.position = "bottom",
    legend.key.width = grid::unit(2.5, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  )


# Save figure ------------------------------------------------------------------

ggsave(
  filename = file.path(
    output_dir,
    "bootstrap_coverage_heatmap.jpeg"
  ),
  plot = bootstrap_coverage_plot,
  width = 13,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

# =============================================================================
# FORGETTING FACTOR VISUALIZATION ACROSS DISCOUNTING MODELS
# =============================================================================
# PURPOSE:
#   This section visualizes the distribution of forgetting factors estimated
#   from three computational discounting models — Exponential, Double-Exponential,
#   and Quasi-Hyperbolic — across multiple datasets (2021, 2022, 2024_1, 2024_2,
#   and Niemeijer 2022).
#
#   Each model estimates one or more forgetting parameters:
#     - Exponential:         gamma
#     - Double-Exponential:  gamma, nu
#     - Quasi-Hyperbolic:    nu
#
#   Datasets with a single affect dimension (2021, 2024_1) produce one subscript
#   (_11), while datasets with two affect dimensions (2022, 2024_2, Niemeijer 2022)
#   produce two subscripts (_11 and _22), visualized via faceting.
#
# OUTPUT:
#   One boxplot per dataset, showing the distribution of forgetting factors
#   grouped by model/parameter combination.


# --- Color palette (consistent across all plots) ---
# Order matches factor levels: Exponential, Dexp-gamma, Dexp-nu, Qhyp-nu
MODEL_COLORS <- c(
  "Exponential"              = "#4C72B0",
  "Double-Exponential (y)"   = "#DD8452",
  "Double-Exponential (v)"   = "#DD8452",  # same color to visually group Dexp parameters
  "Quasi-Hyperbolic (v)"     = "#55A868"
)

# Factor levels and labels shared across all plots
MODEL_LEVELS <- names(MODEL_COLORS)


# =============================================================================
# DATA LOADING
# =============================================================================

# Load all CSVs for each dataset into named lists for clean access
load_dataset <- function(dataset_name) {
  list(
    exp  = read.csv(paste0(dataset_name, "_exponential.csv")),
    dexp = read.csv(paste0(dataset_name, "_double_exponential.csv")),
    qhyp = read.csv(paste0(dataset_name, "_quasi_hyperbolic.csv"))
  )
}

datasets <- list(
  "VANHASBROECK_2021"   = load_dataset("VANHASBROECK_2021"),
  "VANHASBROECK_2022"   = load_dataset("VANHASBROECK_2022"),
  "VANHASBROECK_2024_1" = load_dataset("VANHASBROECK_2024_1"),
  "VANHASBROECK_2024_2" = load_dataset("VANHASBROECK_2024_2"),
  "NIEMEIJER_2022"      = load_dataset("NIEMEIJER_2022")
)

# Datasets with two affect dimensions (subscripts _11 and _22)
MULTI_SUBSCRIPT <- c("VANHASBROECK_2022", "VANHASBROECK_2024_2", "NIEMEIJER_2022")


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Build a long-format data frame for a given dataset and subscript suffix
# (e.g., subscript = "11" extracts gamma_11, nu_11)
make_long_df <- function(data, subscript) {
  exp  <- data$exp
  dexp <- data$dexp
  qhyp <- data$qhyp

  data.frame(
    gamma = c(
      exp[[paste0("gamma_", subscript)]],
      dexp[[paste0("gamma_", subscript)]],
      dexp[[paste0("nu_",    subscript)]],
      qhyp[[paste0("nu_",    subscript)]]
    ),
    model = factor(
      rep(MODEL_LEVELS,
          times = c(nrow(exp), nrow(dexp), nrow(dexp), nrow(qhyp))),
      levels = MODEL_LEVELS
    ),
    subscript = subscript
  )
}

# Build the base ggplot boxplot (shared styling across all plots)
base_boxplot <- function(df, title) {
  ggplot(df, aes(x = model, y = gamma, fill = model)) +
    geom_boxplot(
      width         = 0.5,
      color         = "black",
      outlier.shape = 21,
      outlier.fill  = "white",
      outlier.size  = 2
    ) +
    scale_fill_manual(values = MODEL_COLORS) +
    labs(
      title = paste("Forgetting Factors —", title),
      x     = "Model",
      y     = "Forgetting Factor"
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "none",
      axis.text.x     = element_text(angle = 20, hjust = 1)
    )
}

# Add facet styling for multi-subscript plots
# Plain text labels "Subscript 11" and "Subscript 22" are used to avoid
# rendering issues with Unicode subscript characters across different systems
add_facets <- function(p) {
  p + facet_wrap(
    ~ subscript,
    labeller = labeller(subscript = c(
      "11" = "Possitive Affect",
      "22" = "Negative Affect"
    ))
  ) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text       = element_text(face = "bold")
  )
}


# =============================================================================
# PLOT GENERATION
# =============================================================================

# Iterate over all datasets and generate the appropriate plot for each
for (name in names(datasets)) {

  data <- datasets[[name]]

  if (name %in% MULTI_SUBSCRIPT) {
    # Two subscripts: combine _11 and _22 into one faceted plot
    combined_df <- rbind(make_long_df(data, "11"), make_long_df(data, "22"))
    p <- base_boxplot(combined_df, name)
    p <- add_facets(p)
  } else {
    # Single subscript: straightforward single plot
    combined_df <- make_long_df(data, "11")
    p <- base_boxplot(combined_df, name)
  }

  ggsave(paste0(name, ".jpeg"), plot = p, width = 10, height = 6, dpi = 300)
}

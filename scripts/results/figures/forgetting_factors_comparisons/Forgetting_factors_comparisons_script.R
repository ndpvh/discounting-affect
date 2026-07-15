# =============================================================================
# FORGETTING FACTOR VISUALIZATION ACROSS DISCOUNTING MODELS
# =============================================================================
#
# PURPOSE:
#   This script visualizes the distribution of forgetting factors estimated
#   from three computational discounting models — Exponential, Double-Exponential,
#   and Quasi-Hyperbolic — across multiple datasets (2021, 2022, 2024_1, 2024_2).
#
#   Each model estimates one or more forgetting parameters:
#     - Exponential:         gamma
#     - Double-Exponential:  gamma, nu
#     - Quasi-Hyperbolic:    nu, kappa
#
#   Datasets with a single affect dimension (2021, 2024_1) produce one subscript
#   (_11), while datasets with two affect dimensions (2022, 2024_2) produce two
#   subscripts (_11 and _22), visualized via faceting.
#
# OUTPUT:
#   One boxplot per dataset, showing the distribution of forgetting factors
#   grouped by model/parameter combination.
#
# DEPENDENCIES:
#   - ggplot2
#
# =============================================================================

library(ggplot2)

# --- Color palette (consistent across all plots) ---
# Order matches factor levels: Exponential, Dexp-gamma, Dexp-nu, Qhyp-nu, Qhyp-kappa
MODEL_COLORS <- c(
  "Exponential"            = "#4C72B0",
  "Double-Exponential (γ)" = "#DD8452",
  "Double-Exponential (ν)" = "#DD8452",  # same color to visually group Dexp parameters
  "Quasi-Hyperbolic (ν)"   = "#55A868",
  "Quasi-Hyperbolic (κ)"   = "#C44E52"
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
  "VANHASBROECK_2024_2" = load_dataset("VANHASBROECK_2024_2")
)

# Datasets with two affect dimensions (subscripts _11 and _22)
MULTI_SUBSCRIPT <- c("VANHASBROECK_2022", "VANHASBROECK_2024_2")


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Build a long-format data frame for a given dataset and subscript suffix
# (e.g., subscript = "11" extracts gamma_11, nu_11, kappa_11)
make_long_df <- function(data, subscript) {
  exp  <- data$exp
  dexp <- data$dexp
  qhyp <- data$qhyp
  
  data.frame(
    gamma = c(
      exp[[paste0("gamma_", subscript)]],
      dexp[[paste0("gamma_", subscript)]],
      dexp[[paste0("nu_",    subscript)]],
      qhyp[[paste0("nu_",    subscript)]],
      qhyp[[paste0("kappa_", subscript)]]
    ),
    model = factor(
      rep(MODEL_LEVELS,
          times = c(nrow(exp), nrow(dexp), nrow(dexp), nrow(qhyp), nrow(qhyp))),
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
      title = paste("Forgetting Factors ", title),
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
add_facets <- function(p) {
  p + facet_wrap(
    ~ subscript,
    labeller = labeller(subscript = c(
      "11" = "Subscript 11",
      "22" = "Subscript 22"
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
  
  ggsave(paste0(name, ".png"), plot = p, width = 10, height = 6, dpi = 300)
}

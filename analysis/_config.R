################################################################################
# analysis/_config.R
#
# Shared configuration for the research workflow in analysis/.
#
# This file contains NO statistical logic, no parameter settings, and no
# filesystem side effects. It only defines:
#
#   1. PROJECT_ROOT      – the repository root, resolved from disk.
#   2. PATHS             – a named list of canonical input/output directories.
#   3. Shared constants  – dataset and model identifiers used consistently
#                          across the active analysis scripts.
#
# Intended usage (inside each analysis script, before doing any work):
#
#   source("analysis/_config.R")          # or source("_config.R") from here
#   source("analysis/_helpers.R")
#
# Sourcing this file is safe to repeat: it has no side effects beyond
# defining variables in the environment it is sourced into.
################################################################################


################################################################################
# 1. PROJECT_ROOT
#
# The repository root is the directory that contains BOTH a DESCRIPTION file
# and an R/ subdirectory (this repository is an R package that also hosts the
# analysis workflow). We search upward from the working directory (or, as a
# fallback, from this file's own location) until we find such a marker.
#
# Base R only – no `here`, `rprojroot`, etc.
################################################################################

# Return TRUE if `dir` looks like the repository root.
is_project_root <- function(dir) {
  file.exists(file.path(dir, "DESCRIPTION")) &&
    dir.exists(file.path(dir, "R"))
}

# Walk upward from `start` (inclusive) until a project root is found.
find_project_root <- function(start) {
  current <- normalizePath(start, mustWork = FALSE)
  repeat {
    if (is_project_root(current)) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) {
      return(NA_character_)   # reached the filesystem root without a match
    }
    current <- parent
  }
}

# Resolve the directory containing this file.
#
# When R sources a file, it evaluates that file in an environment that
# carries `ofile` (the path used in the source() call). resolve_project_root()
# is called at the top level of _config.R, so the parent frame (-1) IS that
# sourcing environment. If it does not expose ofile (e.g. the file was
# loaded programmatically rather than via source()), we fall back to the
# current working directory only.
this_file_dir <- function() {
  ofile <- tryCatch(get0("ofile", envir = sys.frame(-1)), error = function(e) NULL)
  if (is.character(ofile) && nzchar(ofile)) {
    return(dirname(normalizePath(ofile, mustWork = FALSE)))
  }
  getwd()
}

resolve_project_root <- function() {
  # Candidate starting points (deduplicated, order preserved):
  #   1. The current working directory – covers running from the repo root
  #      or from analysis/.
  #   2. This file's own directory – covers running from elsewhere, since
  #      _config.R always lives in analysis/ and analysis/ always sits
  #      directly under the project root.
  candidates <- unique(c(getwd(), this_file_dir()))

  for (start in candidates) {
    root <- find_project_root(start)
    if (!is.na(root)) return(root)
  }

  stop(
    "Could not locate the project root while sourcing analysis/_config.R.\n",
    "The root is expected to contain a DESCRIPTION file and an R/ directory.\n",
    "Tried from: ", paste(candidates, collapse = ", "), "\n",
    "Run the analysis from the repository root or from the analysis/ directory."
  )
}

PROJECT_ROOT <- resolve_project_root()


################################################################################
# 2. PATHS
#
# Canonical input/output locations for the analysis workflow.
#
# Naming convention:
#   - INPUT paths  (where analysis reads from):
#       PATHS$raw_data, PATHS$processed_data
#   - OUTPUT paths (where analysis writes to):
#       PATHS$results (root), PATHS$estimation, PATHS$recovery,
#       PATHS$parametric_bootstrap, PATHS$model_comparison,
#       PATHS$forgetting_steps, PATHS$parameter_summary,
#       PATHS$non_parametric_bootstrap
#   - FIGURES (root of all generated figures):
#       PATHS$figures
#   - MISC:
#       PATHS$analysis  (the analysis/ directory itself),
#       PATHS$archive   (analysis/archive/, for legacy/work-in-progress code)
#
# NOTE: these are pure path strings. Creating the output directories is the
# job of _helpers.R (see init_output_dirs()), not of this file.
################################################################################

PATHS <- list(
  # Root locations
  project_root   = PROJECT_ROOT,
  analysis       = file.path(PROJECT_ROOT, "analysis"),

  # Input data
  raw_data       = file.path(PROJECT_ROOT, "analysis", "data", "raw"),
  processed_data = file.path(PROJECT_ROOT, "analysis", "data", "processed"),

  # Results (all under analysis/results/)
  results                = file.path(PROJECT_ROOT, "analysis", "results"),
  estimation             = file.path(PROJECT_ROOT, "analysis", "results", "estimation"),
  recovery               = file.path(PROJECT_ROOT, "analysis", "results", "recovery"),
  parametric_bootstrap   = file.path(PROJECT_ROOT, "analysis", "results", "parametric_bootstrap"),
  model_comparison       = file.path(PROJECT_ROOT, "analysis", "results", "model_comparison"),
  forgetting_steps       = file.path(PROJECT_ROOT, "analysis", "results", "forgetting_steps"),
  parameter_summary       = file.path(PROJECT_ROOT, "analysis", "results", "parameter_summary"),
  non_parametric_bootstrap = file.path(PROJECT_ROOT, "analysis", "results", "non_parametric_bootstrap"),

  # Figures
  figures        = file.path(PROJECT_ROOT, "analysis", "figures"),

  # Archive of legacy / work-in-progress scripts
  archive        = file.path(PROJECT_ROOT, "analysis", "archive")
)


################################################################################
# 3. SHARED CONSTANTS
#
# These reflect what is genuinely shared across the active scripts today.
# They do NOT reconcile any inconsistencies between scripts – they simply
# give a single, named definition for values that multiple scripts already
# agree on.
################################################################################

# --- Datasets ----------------------------------------------------------------
#
# RAW_DATASETS: the four source datasets as they appear in analysis/data/raw/
# and in the per-participant processed folders. Used by 01_process_data.R and
# 02_estimate_models.R.
RAW_DATASETS <- c(
  "VANHASBROECK_2021",
  "VANHASBROECK_2022",
  "VANHASBROECK_2024",
  "NIEMEIJER_2022"
)

# ESTIMATION_DATASETS: the dataset names as they appear in the estimation
# result CSV filenames (analysis/results/estimation/). The VANHASBROECK_2024
# dataset is split by response dimensionality during estimation:
#   _1 = d = 1 (valence-only participants)
#   _2 = d = 2 (positive/negative affect participants)
# This split is used by 04, 05, 07 and 11.
ESTIMATION_DATASETS <- c(
  "VANHASBROECK_2021",
  "VANHASBROECK_2022",
  "VANHASBROECK_2024_1",
  "VANHASBROECK_2024_2",
  "NIEMEIJER_2022"
)

# Map from a split estimation-result dataset name back to the raw dataset
# name. This is the same mapping already used inside 06_forgetting_steps.R
# (RAW_DATASET_MAP) and 05_model_comparison.R (dataset_name_map, in the
# forward direction).
ESTIMATION_TO_RAW_DATASET <- c(
  "VANHASBROECK_2021"  = "VANHASBROECK_2021",
  "VANHASBROECK_2022"  = "VANHASBROECK_2022",
  "VANHASBROECK_2024_1" = "VANHASBROECK_2024",
  "VANHASBROECK_2024_2" = "VANHASBROECK_2024",
  "NIEMEIJER_2022"     = "NIEMEIJER_2022"
)

# --- Models -------------------------------------------------------------------
#
# MODEL_TYPES: the three discounting model identifiers used across the whole
# workflow (estimation, recovery, bootstrap, comparison, forgetting steps).
# All active scripts use the same set; note that their ORDERING is not
# uniform across scripts (see _helpers.R / report) – treat this vector as an
# unordered SET, not as a canonical ordering.
MODEL_TYPES <- c("exponential", "quasi_hyperbolic", "double_exponential")

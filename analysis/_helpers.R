################################################################################
# analysis/_helpers.R
#
# Generic, non-statistical utility functions for the analysis workflow.
#
# This file is intended to be sourced AFTER analysis/_config.R:
#
#   source("analysis/_config.R")
#   source("analysis/_helpers.R")
#
# It contains only infrastructure helpers (directory handling, input checks,
# and a shared estimation-results loader that mirrors the existing
# implementation in 06_forgetting_steps.R). It contains NO statistical logic,
# no model definitions, and no tuning parameters.
################################################################################


################################################################################
# 0. CONFIG PRESENCE CHECK
#
# _helpers.R relies on the objects defined by _config.R (PROJECT_ROOT and
# PATHS). Fail early with a helpful message if those have not been sourced.
################################################################################

required_config_objects <- c("PROJECT_ROOT", "PATHS")
missing_config <- setdiff(required_config_objects, ls())
if (length(missing_config) > 0) {
  stop(
    "analysis/_helpers.R requires analysis/_config.R to be sourced first.\n",
    "Missing object(s): ", paste(missing_config, collapse = ", "), "\n",
    "Add before sourcing _helpers.R:\n",
    '    source("analysis/_config.R")'
  )
}


################################################################################
# 1. DIRECTORY HELPER
################################################################################

#' Ensure a directory exists (creating it, recursively, if needed).
#'
#' Generic replacement for the repeated idiom
#' `dir.create(dir, recursive = TRUE, showWarnings = FALSE)`.
#'
#' @param path Character scalar: the directory to ensure.
#' @return The (normalized) path, invisibly – so the call can be used as a
#'         drop-in value for a path variable.
#' @export
ensure_dir <- function(path) {
  path <- as.character(path)
  if (length(path) != 1 || !nzchar(path)) {
    stop("ensure_dir(): `path` must be a non-empty single string.")
  }
  if (!dir.exists(path)) {
    ok <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!ok || !dir.exists(path)) {
      stop("Could not create directory: ", path)
    }
  }
  invisible(normalizePath(path, mustWork = FALSE))
}


################################################################################
# 2. INPUT EXISTENCE HELPER
################################################################################

#' Check that a set of required files/directories all exist.
#'
#' Collects ALL missing items first and reports them together, so a call does
#' not fail on the first missing item and hide the rest.
#'
#' @param paths     Character vector of paths to check (files or directories).
#' @param path_type "any" (default), "file", or "dir" – what the paths are
#'                  expected to be. "any" accepts either.
#' @param context   Optional human-readable label included in the error
#'                  message (e.g. "estimation input data").
#' @return Invisible `TRUE` if all paths exist; never returns `FALSE`.
require_paths <- function(paths, path_type = c("any", "file", "dir"),
                          context = NULL) {
  path_type <- match.arg(path_type)
  paths <- as.character(paths)

  missing <- if (path_type == "file") {
    paths[!file.exists(paths)]
  } else if (path_type == "dir") {
    paths[!dir.exists(paths)]
  } else {
    paths[!(file.exists(paths) | dir.exists(paths))]
  }

  if (length(missing) > 0) {
    msg <- c(
      paste0(
        if (is.null(context)) "Required path(s) not found:"
        else paste0("Required ", context, " path(s) not found:")
      ),
      paste0("    - ", missing)
    )
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }

  invisible(TRUE)
}


################################################################################
# 3. ANALYSIS-OUTPUT DIRECTORY INITIALIZER
################################################################################

#' Create the standard generated-output directories defined in PATHS.
#'
#' Git does not store empty directories (e.g. analysis/results/recovery/), so
#' a fresh clone needs these created before the first run. This helper creates
#' ONLY the directories the analysis scripts are expected to write to; it
#' never creates files, and it never creates input-data directories
#' (raw/ and processed/ must be populated by the user or 01_process_data.R).
#'
#' Safe to call repeatedly: existing directories are left untouched.
#'
#' @return Invisible named character vector of the directories ensured.
init_output_dirs <- function() {
  # Directories the analysis scripts are expected to write results/figures to.
  output_dirs <- c(
    results                = PATHS$results,
    estimation             = PATHS$estimation,
    recovery               = PATHS$recovery,
    parametric_bootstrap   = PATHS$parametric_bootstrap,
    model_comparison       = PATHS$model_comparison,
    forgetting_steps       = PATHS$forgetting_steps,
    parameter_summary       = PATHS$parameter_summary,
    non_parametric_bootstrap = PATHS$non_parametric_bootstrap,
    figures                = PATHS$figures,
    processed_data         = PATHS$processed_data
  )

  for (d in output_dirs) {
    ensure_dir(d)
  }
  invisible(output_dirs)
}


################################################################################
# 4. SHARED ESTIMATION-RESULT LOADER
################################################################################

#' Read all parameter-estimation CSVs into a nested list.
#'
#' Returns data[[dataset]][[model_type]], where each element is the data
#' frame of per-participant estimates (plus AIC/BIC etc.) for that
#' dataset/model combination.
#'
#' Filename convention (already established by 02_estimate_models.R and
#' relied upon by 05, 06, 07, 08, and 11):
#'
#'   "<dataset>_<model_type>.csv"
#'
#' Note: model_type is identified by a SUFFIX match on the filename stem, so
#' a file named "VANHASBROECK_2024_1_double_exponential.csv" yields
#' dataset = "VANHASBROECK_2024_1", model_type = "double_exponential".
#'
#' This implementation reproduces the existing (complete) version in
#' 06_forgetting_steps.R line-for-line in behavior, so it can later replace
#' that copy. It does not change any dataset/model interpretation.
#'
#' @param dir          Directory containing the estimation CSVs
#'                     (e.g. PATHS$estimation).
#' @param model_types  Character vector of model identifiers, e.g. MODEL_TYPES.
#' @return Named nested list: data[[dataset]][[model_type]] -> data.frame.
load_estimation_data <- function(dir, model_types) {
  require_paths(dir, path_type = "dir", context = "estimation")

  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  data <- list()

  # Match longer model names first. This is essential because
  # "double_exponential" also ends with "exponential".
  model_types_by_specificity <- model_types[
    order(nchar(model_types), decreasing = TRUE)
  ]

  for (f in files) {
    file_name_no_csv <- sub("\\.csv$", "", basename(f))

    matches <- model_types_by_specificity[vapply(
      model_types_by_specificity,
      function(m) endsWith(file_name_no_csv, paste0("_", m)),
      logical(1)
    )]

    if (length(matches) == 0L) {
      warning(paste("Could not identify model type for file:", f))
      next
    }

    model_type <- matches[[1]]
    dataset <- sub(paste0("_", model_type, "$"), "", file_name_no_csv)

    df <- read.csv(f, stringsAsFactors = FALSE)

    if (is.null(data[[dataset]])) {
      data[[dataset]] <- list()
    }
    data[[dataset]][[model_type]] <- df
  }

  data
}

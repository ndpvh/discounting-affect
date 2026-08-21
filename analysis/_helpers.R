################################################################################
# analysis/_helpers.R
#
# Generic, non-statistical utility functions for the analysis workflow.
#
# This file is intended to be sourced AFTER analysis/_config.R:
#
#   From the repository root:
#     source("analysis/_config.R")
#     source("analysis/_helpers.R")
#
#   From inside the analysis/ directory:
#     source("_config.R")
#     source("_helpers.R")
#
# It contains only infrastructure helpers (directory handling, input checks,
# and a shared estimation-results loader). It contains NO statistical logic,
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
#' @param path Character scalar: the directory to ensure. Must be a single,
#'             non-NA, non-empty string.
#' @return The (normalized) path, invisibly – so the call can be used as a
#'         drop-in value for a path variable.
ensure_dir <- function(path) {
  if (!is.character(path)) {
    stop("ensure_dir(): `path` must be a single character string.")
  }
  if (length(path) != 1) {
    stop("ensure_dir(): `path` must be a single string, got ", length(path), ".")
  }
  if (is.na(path)) {
    stop("ensure_dir(): `path` must not be NA.")
  }
  if (!nzchar(path)) {
    stop("ensure_dir(): `path` must not be an empty string.")
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
#'                  expected to be. "file" requires a regular file (a directory
#'                  does NOT pass); "dir" requires a directory; "any" accepts
#'                  either.
#' @param context   Optional human-readable label included in the error
#'                  message (e.g. "estimation input data").
#' @return Invisible `TRUE` if all paths exist; never returns `FALSE`.
require_paths <- function(paths, path_type = c("any", "file", "dir"),
                          context = NULL) {
  path_type <- match.arg(path_type)
  paths <- as.character(paths)

  missing <- if (path_type == "file") {
    # A directory does not satisfy a "file" check.
    paths[!(file.exists(paths) & !dir.exists(paths))]
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
#' ONLY the directories the analysis scripts are expected to write to, and it
#' never creates files. Specifically:
#'   - raw_data is an EXISTING input location and is NOT created here;
#'   - processed_data is a GENERATED intermediate-data location (written by
#'     01_process_data.R, then read by estimation) and IS created here;
#'   - all results/* subdirectories and the figures/ root are also created.
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
    forgetting_factor_spread = PATHS$forgetting_factor_spread,
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
#' Model identification: the model type is found by matching a COMPLETE final
#' suffix of the form "_<model_type>" against the filename stem. This is done
#' deliberately to avoid the classic "double_exponential ends with
#' exponential" ambiguity, and the match does NOT depend on the order in which
#' `model_types` are supplied:
#'   - every model identifier that matches as a final "_<name>" suffix is
#'     collected;
#'   - if several match (because one name is a suffix of another), the LONGEST
#'     one wins, e.g. "VANHASBROECK_2024_1_double_exponential" ->
#'     model = "double_exponential", dataset = "VANHASBROECK_2024_1";
#'   - if none match, a warning is issued and the file is skipped (preserving
#'     the existing behavior);
#'   - the winning "_<model>" suffix is then removed to obtain the dataset name.
#'
#' It does not require every possible dataset/model combination to be present:
#' this repository is still a work in progress, so partial result sets are
#' expected and fine.
#'
#' @param dir          Directory containing the estimation CSVs
#'                     (e.g. PATHS$estimation).
#' @param model_types  Character vector of model identifiers, e.g. MODEL_TYPES.
#'                     Order does not matter.
#' @return Named nested list: data[[dataset]][[model_type]] -> data.frame.
load_estimation_data <- function(dir, model_types) {
  require_paths(dir, path_type = "dir", context = "estimation")

  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    stop(
      "The estimation directory exists but contains no .csv files: ", dir, "\n",
      "Run the estimation step first (analysis/02_estimate_models.R), or check ",
      "that PATHS$estimation points to the right location."
    )
  }

  data <- list()

  for (f in files) {
    file_name_no_csv <- sub("\\.csv$", "", basename(f))

    # Collect every model identifier that matches as a COMPLETE final suffix
    # (including its preceding underscore). Matching "_<name>" rather than a
    # bare name keeps "exponential" from matching "..._double_exponential".
    matched <- model_types[vapply(
      model_types,
      function(m) endsWith(file_name_no_csv, paste0("_", m)),
      logical(1)
    )]

    if (length(matched) == 0) {
      warning(paste("Could not identify model type for file:", f))
      next
    }

    # If more than one matched (one name is a suffix of another), keep the
    # LONGEST identifier so the result does not depend on vector ordering.
    model_type <- matched[which.max(nchar(matched))]

    # Remove exactly that "_<model>" suffix to recover the dataset name.
    dataset <- sub(paste0("_", model_type, "$"), "", file_name_no_csv)

    df <- read.csv(f, stringsAsFactors = FALSE)

    if (is.null(data[[dataset]])) {
      data[[dataset]] <- list()
    }
    data[[dataset]][[model_type]] <- df
  }

  data
}

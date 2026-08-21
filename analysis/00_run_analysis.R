# =============================================================================
# 00_run_analysis.R
# =============================================================================
# Reviewer-facing entry point for the discounting-affect analysis workflow.
#
# To run the workflow you only need to choose MODE below and run this script:
#
#     Rscript analysis/00_run_analysis.R   (from the repository root)
#   or
#     Rscript 00_run_analysis.R            (from inside analysis/)
#
# ---------------------------------------------------------------------------
# MODE
# ---------------------------------------------------------------------------
#
# "existing_results"   (DEFAULT — recommended for ordinary review)
#     Uses the computational results already stored in the repository.
#     Does NOT rerun model estimation (01-02), recovery generation (03), or
#     the parametric bootstrap (04). Reruns only the inexpensive downstream
#     summaries and visualizations. Some optional stages are skipped with a
#     clear message when their saved inputs are absent; this is not an error.
#
# "full_reproduction"
#     COMPUTATIONALLY EXPENSIVE. Reruns the full pipeline from data
#     processing (01) through the expensive recovery (03) and parametric
#     bootstrap (04), then the downstream summaries and visualizations.
#     Selecting this mode is itself the explicit intent to run the expensive
#     stages; there is no interactive confirmation prompt.
#
#     REPRODUCIBILITY NOTE: estimation (02) and recovery generation (03) use
#     stochastic optimization/simulation without a fixed global seed. Repeated
#     full-reproduction runs can therefore differ numerically. This behavior is
#     retained for now and should be discussed by the research group before any
#     seed policy is changed.
#
# =============================================================================


## --- 1. Choose the workflow mode (edit this one line) -----------------------
MODE <- "existing_results"


## --- 2. Locate and load the shared infrastructure ---------------------------
# The runner supports being launched from the repository root or from inside
# analysis/. We locate _config.R relative to the current working directory and
# source it, then source _helpers.R from the resolved analysis/ directory.
config_file <- if (file.exists(file.path("analysis", "_config.R"))) {
  file.path("analysis", "_config.R")
} else if (file.exists("_config.R")) {
  "_config.R"
} else {
  stop(
    "Could not find analysis/_config.R. ",
    "Run this script from the repository root or the analysis/ directory."
  )
}
source(config_file)
source(file.path(PATHS$analysis, "_helpers.R"))
rm(config_file)

# --- 3. Pin the working directory to the repository root ---------------------
# Numbered analysis scripts and the shared paths are anchored on PROJECT_ROOT,
# but a few of them also emit progress messages and auxiliary relative paths.
# Setting the working directory of THIS R process (only) to the repository
# root makes every subprocess behave identically regardless of whether the
# reviewer launched the runner from the repo root or from analysis/.
# This does NOT change the user's shell working directory outside the R process.
setwd(PROJECT_ROOT)


## --- 4. Validate MODE -------------------------------------------------------
valid_modes <- c("existing_results", "full_reproduction")
if (!is.character(MODE) || length(MODE) != 1 || !(MODE %in% valid_modes)) {
  stop(
    "Invalid MODE: ",
    format(MODE),
    ". Valid values are: ",
    paste0("\"", valid_modes, "\"", collapse = ", "),
    "."
  )
}


## --- 5. Locate the Rscript executable (base R only) -------------------------
rscript_bin <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
)
if (!file.exists(rscript_bin)) {
  stop("Could not find the Rscript executable at: ", rscript_bin)
}


## --- 6. Check analysis package dependencies ---------------------------------
# These packages are used directly by the active analysis scripts in addition
# to base/recommended R packages. The runner checks them up front so a reviewer
# gets one clear dependency message instead of failing partway through the
# workflow. Package installation remains the user's responsibility.
ANALYSIS_PACKAGES <- c(
  "MASS",
  "DEoptim",
  "nloptr",
  "devtools",
  "dplyr",
  "tidyr",
  "ggplot2",
  "ggpubr",
  "cowplot",
  "scales"
)

missing_packages <- ANALYSIS_PACKAGES[!vapply(
  ANALYSIS_PACKAGES,
  requireNamespace,
  FUN.VALUE = logical(1),
  quietly = TRUE
)]

if (length(missing_packages) > 0) {
  stop(
    "Missing R package(s) required by the analysis workflow: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall the missing package(s) before running the analysis. ",
    "The runner does not install packages automatically."
  )
}


## --- 7. Small helpers -------------------------------------------------------

# run_analysis_step(script, description)
#   Spawns a FRESH Rscript subprocess to execute `script`. The script path is
#   normalized and explicitly quoted so repository locations containing spaces
#   are safe. Waits for the subprocess to finish and stops the workflow if it
#   exits non-zero.
#   A fresh subprocess is required so that no objects or functions created by
#   one numbered script leak into another (fresh-session independence is a
#   property we deliberately preserved during the repository refactor).
run_analysis_step <- function(script, description) {
  script_path <- normalizePath(
    file.path(PATHS$analysis, script),
    mustWork = TRUE
  )
  cat(strrep("=", 79), "\n")
  cat("Running: ", script, "\n", sep = "")
  cat("Purpose: ", description, "\n", sep = "")
  cat(strrep("=", 79), "\n")

  # Explicitly quote the script path so repository locations containing spaces
  # are passed safely as a single argument to the fresh Rscript subprocess.
  status <- system2(
    command = rscript_bin,
    args = shQuote(script_path),
    wait = TRUE
  )
  if (status != 0L) {
    stop(
      "Analysis step failed: ", script,
      "\n  exit status: ", status,
      "\n  Stopping the workflow; later dependent stages were not run."
    )
  }
  invisible(status)
}

# file_count(dir, pattern)
#   Number of files directly under `dir` matching a regular expression.
file_count <- function(dir, pattern) {
  if (!dir.exists(dir)) return(0L)
  files <- list.files(dir, pattern = pattern, full.names = FALSE,
                      ignore.case = TRUE, no.. = TRUE)
  length(files)
}


## --- 8. Stage definitions ---------------------------------------------------
# The numerical script filenames are the authoritative ordering. Each stage
# records: the script filename and a short human-readable description.

stages_expensive <- list(
  list(script = "01_process_data.R",
       desc = "Process raw datasets into per-participant .rds files."),
  list(script = "02_estimate_models.R",
       desc = "Estimate discounting models on participant-level data."),
  list(script = "03_run_recovery.R",
       desc = "Run the (expensive) recovery procedure for each model."),
  list(script = "04_run_parametric_bootstrap.R",
       desc = "Run the (expensive) parametric bootstrap generation.")
)

stages_downstream <- list(
  list(script = "05_model_comparison.R",
       desc = "Compare fitted discounting models using saved estimation results."),
  list(script = "06_forgetting_steps.R",
       desc = "Compute forgetting-step quantities from saved estimation results."),
  list(script = "07_nonparametric_bootstrap_negativity_bias.R",
       desc = "Run the non-parametric bootstrap and negativity-bias analysis."),
  list(script = "08_forgetting_factor_spread.R",
       desc = "Summarize the spread of forgetting-factor estimates."),
  list(script = "09_summarize_recovery.R",
       desc = "Summarize saved recovery .Rds results into CSV reports."),
  list(script = "10_summarize_parametric_bootstrap.R",
       desc = "Summarize saved parametric-bootstrap CSVs (coverage, bias, RMSE)."),
  list(script = "11_visualization.R",
       desc = "Produce the reviewer-facing figures from saved analysis results.")
)


## --- 9. Startup banner ------------------------------------------------------
cat("\n")
cat("Discounting Affect — Analysis Workflow\n")
cat("Mode: ", MODE, "\n\n", sep = "")

if (MODE == "existing_results") {
  cat("Using saved computational results.\n")
  cat("Model estimation, recovery generation, and parametric bootstrap generation\n")
  cat("will NOT be rerun.\n\n")
} else {
  cat("WARNING: FULL REPRODUCTION MODE\n")
  cat(strrep("-", 79), "\n")
  cat("This workflow will rerun model estimation, recovery, and the parametric\n")
  cat("bootstrap. These stages are computationally intensive.\n")
  cat("Note: estimation and recovery are stochastic and are not globally seeded;\n")
  cat("repeated full runs may differ numerically.\n")
  cat(strrep("-", 79), "\n\n")
}


## --- 10. Initialize standard generated-output directories -------------------
# Creates empty output directories (results/*, figures/*, data/processed) so
# that later scripts do not need to individually ensure them. This does NOT
# create or overwrite any data or result files.
init_output_dirs()


## --- 11. Workflow execution -------------------------------------------------

completed <- character(0)
skipped   <- character(0)

# In existing-results mode, saved estimation CSVs are the core prerequisite for
# the downstream workflow. Without them, the analysis cannot proceed at all.
# Recovery and parametric-bootstrap result families remain optional because the
# repository is currently a work in progress.
if (MODE == "existing_results") {
  n_estimation_files <- file_count(PATHS$estimation, "\\.csv$")

  if (n_estimation_files == 0L) {
    stop(
      "Existing-results analysis cannot proceed because no saved estimation ",
      ".csv files were found under: ", PATHS$estimation, "\n",
      "Restore/provide the saved estimation results or set ",
      "MODE <- \"full_reproduction\" to regenerate them.\n",
      "The runner will not switch modes automatically."
    )
  }
}

# run_stage(stage, required)
#   Runs one downstream stage through a fresh Rscript subprocess. When
#   `required` is TRUE a failure stops the workflow. When `required` is FALSE
#   (used for optional stages in existing-results mode) an absent prerequisite
#   is reported as SKIPPED and the workflow continues.
#   The decision to skip is made by the caller (via the prerequisite checks
#   below); the stage itself, once attempted, must succeed.
run_stage <- function(stage, required = TRUE) {
  run_analysis_step(stage$script, stage$desc)
  completed <<- c(completed, stage$script)
  invisible(NULL)
}

# skip_stage(stage, reason)
#   Reports an intentionally-skipped optional stage and records it.
skip_stage <- function(stage, reason) {
  cat(strrep("=", 79), "\n")
  cat("SKIPPED: ", stage$script, "\n", sep = "")
  cat("Reason: ", reason, "\n", sep = "")
  cat(strrep("=", 79), "\n\n")
  skipped <<- c(skipped, stage$script)
  invisible(NULL)
}


# --- 11a. Expensive stages (full_reproduction only) -------------------------
if (MODE == "full_reproduction") {
  for (st in stages_expensive) {
    run_stage(st, required = TRUE)
  }
}


# --- 11b. Downstream stages (both modes) ------------------------------------
for (st in stages_downstream) {

  # Per-stage prerequisite gates. A stage that cannot be attempted (because a
  # required saved input is missing) is skipped ONLY in "existing_results"
  # mode. In "full_reproduction" mode the preceding expensive stages are
  # expected to have produced those files; absence there is a genuine failure
  # (surfaced as a normal subprocess failure, not a silent skip).

  if (st$script == "09_summarize_recovery.R") {

    n_rec <- file_count(PATHS$recovery, "\\.Rds$")
    if (n_rec == 0L) {
      if (MODE == "existing_results") {
        skip_stage(st, "No saved recovery .Rds files were found.")
        next
      }
      # full_reproduction: 03 should have produced them; let 09 fail naturally.
    }

  } else if (st$script == "10_summarize_parametric_bootstrap.R") {

    # 10 needs the raw bootstrap CSVs; bootstrap_summary.Rds is its output,
    # not an input, so it must NOT count toward this check.
    n_boot <- file_count(PATHS$parametric_bootstrap, "\\.csv$")
    if (n_boot == 0L) {
      if (MODE == "existing_results") {
        skip_stage(
          st,
          "No saved raw parametric-bootstrap .csv files were found."
        )
        next
      }
      # full_reproduction: 04 should have produced them; let 10 fail naturally.
    }

  } else if (st$script == "11_visualization.R") {

    # 11 depends on estimation CSVs AND on bootstrap_summary.Rds (which is the
    # output of 10). Both must be present for 11 to be meaningful.
    n_est  <- file_count(PATHS$estimation, "\\.csv$")
    bs_ok  <- file.exists(file.path(PATHS$parametric_bootstrap,
                                    "bootstrap_summary.Rds"))
    if (n_est == 0L || !bs_ok) {
      if (MODE == "existing_results") {
        missing <- c(
          if (n_est == 0L) "estimation .csv results" else NULL,
          if (!bs_ok)      "bootstrap_summary.Rds"    else NULL
        )
        skip_stage(
          st,
          paste0(
            "Required saved inputs are missing for visualization: ",
            paste(missing, collapse = " and "), "."
          )
        )
        next
      }
      # full_reproduction: preceding stages should have produced both; let 11
      # fail naturally so the reviewer sees the true error.
    }

  }

  # Stage passed its gate: run it in a fresh Rscript subprocess.
  run_stage(st, required = TRUE)
}


## --- 12. Final workflow summary --------------------------------------------
cat("\n")
cat(strrep("=", 79), "\n")
cat("Workflow complete\n")
cat(strrep("=", 79), "\n")
cat("Mode:      ", MODE, "\n", sep = "")
cat("Completed: ", length(completed), " stage(s)\n", sep = "")
for (s in completed) cat("               - ", s, "\n", sep = "")
if (length(skipped) > 0L) {
  cat("Skipped:   ", length(skipped), " stage(s)\n", sep = "")
  for (s in skipped) cat("               - ", s, "\n", sep = "")
} else {
  cat("Skipped:   (none)\n")
}
cat(strrep("=", 79), "\n\n")

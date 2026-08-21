################################################################################
# PURPOSE:
#
# Run the computationally expensive recovery study for the discounting models
# defined in this package and save the raw recovery result objects as .Rds files.
# The recovery study also requests fit statistics such as AIC and BIC, together
# with checks of the residual structure, so these diagnostics are stored in the
# saved recovery objects for later analysis.
#
# This script performs recovery GENERATION ONLY. The inexpensive downstream
# recovery summaries and simulated-vs-estimated recovery figures are generated
# from the saved .Rds files by analysis/09_summarize_recovery.R.
################################################################################
################################################################################
# SOURCE SHARED INFRASTRUCTURE
#
# Works from either the repository root or the analysis/ directory.
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

################################################################################
# PRELIMINARIES

devtools::load_all(PROJECT_ROOT)

# Define the number of recoveries `iterations` and the number of datapoints `N`
iterations <- 1000
N <- 140

# Define the models to use for the recovery study
fx <- list(
    "exponential" = exponential, 
    "quasi_hyperbolic" = quasi_hyperbolic,
    "double_exponential" = double_exponential
)
dims <- expand.grid(1:2, 1:2)

models <- list()
for(i in seq_len(nrow(dims))) {
    for(j in seq_along(fx)) {
        id <- paste0(
            names(fx)[j], 
            "_",
            dims$Var1[i],
            dims$Var2[i]
        )

        models[[id]] <- fx[[j]](d = dims$Var1[i], k = dims$Var2[i])
    }
}

# Define functions for generating the values of X in the simulation. The index
# in the list communicates the number of dimensions the X should take (i.e., 
# how many predictors there are)
x_function <- list(
    \(x) runif(x, min = -2, max = 2),
    list(
        \(x) runif(x, min = -2, max = 2),
        \(x) runif(x, min = -2, max = 2)
    )
)

# Define the different functions that you want to perform on the fitobj
fx <- list(
    "aic" = aic,
    "bic" = bic,
    "autocorrelation" = autocorrelation,
    "bias" = bias
)

# Define a function that will make use of multiple nloptr optimizers as specified 
# by the user. These can then be varied so that different combinations are tried
# out, typically combining a global optimizer with a local one.
#
# Assumption: We use the same control parameters for both estimation procedures.
optimizer <- function(obj, 
                      lower, 
                      upper,
                      algorithm = "NLOPT_LN_BOBYQA", 
                      maxeval = 1e3, 
                      ftol_abs = 1e-15, 
                      xtol_abs = 1e-15, 
                      print_level = 0,
                      ...) {
    
    # Perform estimation using DEoptim. This will serve as the global 
    # optimization procedure, allowing us to get in the ballpark of where it 
    # should be right.
    result <- DEoptim::DEoptim(
        obj, 
        lower,
        upper,
        control = DEoptim::DEoptim.control(
            ...
        )
    )

    # Extract the result of this estimation procedure and save it as an 
    # initial condition
    x0 <- result$optim$bestmem

    # Perform an additional estimation procedure with nloptr. Ideally, this is 
    # just through a local optimizer, but in theory, it can be another global one 
    # as well
    result <- nloptr::nloptr(
        x0,
        obj,
        lb = lower, 
        ub = upper, 
        opts = list(
            algorithm = algorithm, 
            maxeval = maxeval, 
            ftol_abs = ftol_abs, 
            xtol_abs = xtol_abs, 
            print_level = print_level
        )
    )
    
    # Return the results in a named list, as required by the fit function
    return(
        list(
            "parameters" = result$solution, 
            "objective" = result$objective
        )
    )
}



################################################################################
# PERFORM THE RECOVERY

# Ensure the recovery-result directory exists before anything is written.
# Done once in the parent process rather than inside the parallel worker.
ensure_dir(PATHS$recovery)

# A full recovery rerun should never mix new and stale model results. Remove the
# expected result files first; if a worker later fails, the script stops and the
# missing file makes the incomplete rerun visible.
expected_recovery_files <- file.path(
    PATHS$recovery, paste0(names(models), ".Rds")
)
unlink(expected_recovery_files[file.exists(expected_recovery_files)])

# Numeric summaries and recovery figures are derived from the raw recovery RDS
# files. Invalidate those downstream artifacts when a fresh generation starts so
# an interrupted recovery run cannot leave old summaries/figures looking current.
recovery_summary_dir <- file.path(PATHS$recovery, "summary")
if (dir.exists(recovery_summary_dir)) {
    old_summary_files <- list.files(recovery_summary_dir, full.names = TRUE)
    if (length(old_summary_files) > 0) unlink(old_summary_files, recursive = TRUE)
}
recovery_figure_dir <- file.path(PATHS$figures, "recovery")
if (dir.exists(recovery_figure_dir)) {
    old_recovery_figures <- list.files(
        recovery_figure_dir,
        pattern = "\\.(jpeg|jpg|png)$",
        full.names = TRUE,
        ignore.case = TRUE
    )
    if (length(old_recovery_figures) > 0) unlink(old_recovery_figures)
}

# Use all but one available core on Unix-like systems, but always leave at least
# one worker available. Windows uses serial execution because mclapply forks are
# not available there.
detected_cores <- parallel::detectCores()
if (is.na(detected_cores)) detected_cores <- 1L
recovery_cores <- if (Sys.info()["sysname"] == "Windows") {
    1L
} else {
    max(1L, as.integer(detected_cores - 1L))
}

# Loop over the models
empty <- parallel::mclapply(
    seq_along(models),
    function(j) {
        # Extract the model of choice
        my_model <- models[[j]]

        # Perform the recovery for the specified combination of functions,
        # providing them to the optimizer
        result <- recovery(
            my_model,
            iterations = iterations,
            fx = fx,

            # Simulation characteristics
            Xfun = x_function[[as.integer(my_model@k)]],
            N = N,

            # Model characteristics
            dynamics = "isotropic",
            covariance = "symmetric",

            # Additional stuff
            print_iteration = TRUE,
            print_content = paste(
                names(models)[j], 
                ": isotropic - symmetric",
                sep = ""
            ),

            # Optimization characteristics
            optimizer = function(obj, lower, upper, ...) optimizer(
                obj, 
                lower,
                upper,
                ...
            ),
                
            # DEoptim arguments
            itermax = 1e3,
            NP = 150,
            CR = 0.75,
            strategy = 6, 
            p = 0.8,
            reltol = 1e-15, 
            steptol = 100,
            trace = FALSE,

            # nloptr arguments
            maxeval = 1e5,
            xtol_abs = 1e-20,
            ftol_abs = 1e-20,
            print_level = 0
        )

        # Save the result
        saveRDS(
            result,
            file.path(
                PATHS$recovery,
                paste0(
                    names(models)[j], 
                    ".Rds"
                )
            )
        )

        cat("\n")

        return(NULL)
    },
    mc.cores = recovery_cores
)

worker_failures <- which(vapply(
    empty, function(x) inherits(x, "try-error"), logical(1)
))
if (length(worker_failures) > 0) {
    stop(
        "Recovery worker failure(s) for model index/indices: ",
        paste(worker_failures, collapse = ", "),
        ". The recovery result set is incomplete."
    )
}

cat("\n")

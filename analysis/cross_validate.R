

#### cut the series into blocks
# returns the trials associated with each fold
make_folds <- function(n, k){
  edges <- round(seq(0, n, length.out = k + 1))
  
  folds_list <- vector("list", length = k)
  
  for(i in 1:k){
    start_index <- edges[i] + 1
    end_index <- edges[i + 1]
    
    folds_list[[i]] <- start_index:end_index
  }
  
  return(folds_list)
}

##### turns estimated parameters into predicted affect for every row
predict_affect <- function(model_empty, parameters, dataset) {
  # model_empty = empty model, e.g. exponential(d = 1, k = 4)
  # parameters  = the estimated parameter vector (fitobj$parameters)
  # dataset     = a participant's processed data object
  
  # Put the estimated parameter values into the empty model
  # (settings copied from your parametric bootstrap script)
  model_filled <- fill(
    model_empty,
    parameters,
    dynamics = "isotropic",
    covariance = "symmetric",
    parameters_only = FALSE,
    cholesky = FALSE
  )
  
  # Predicted affect for every row, based on the stimuli in dataset@X
  predicted <- predict(model_filled, dataset)@Y
  
  return(predicted)
}

########## function that hides one block, fits, predicts and computes RMSE ########
cv_one_fold <- function(data, model_empty, test_rows, optimizer,
                        dynamics = "isotropic", covariance = "symmetric", ...){
  # data = one participant's processed data object (has @Y and @X)
  # model_empty = empty model of the right size, e.g. exponential(d = 1, k = 4)
  # test_rows = the rows of the block that is held out in this fold
  # optimizer = optimizer() function (DEoptim, then BOBYQA)
  # ... = DEoptim / nloptr settings, passed on to fit() unchanged
  
  # STEP 1: hide the ratings of the test block
  # stimuli in data@X stay complete, only affect values are removed
  data_masked <- data
  data_masked@Y[test_rows, ] <- NA
  
  # STEP 2: estimate the model on the ramining ratings
  # using fit() object 
  
  fitobj <- fit(##### turns estimated parameters into predicted affect for every row
    predict_affect <- function(model_empty, parameters, dataset) {
      # model_empty = empty model, e.g. exponential(d = 1, k = 4)
      # parameters  = the estimated parameter vector (fitobj$parameters)
      # dataset     = a participant's processed data object
      
      # Put the estimated parameter values into the empty model
      # (settings copied from your parametric bootstrap script)
      model_filled <- fill(
        model_empty,
        parameters,
        dynamics = "isotropic",
        covariance = "symmetric",
        parameters_only = FALSE,
        cholesky = FALSE
      )
      
      # Predicted affect for every row, based on the stimuli in dataset@X
      predicted <- predict(model_filled, dataset)@Y
      
      return(predicted)
    }
    model_empty, data_masked, dynamics = dynamics, covariance = covariance,
    optimizer = optimizer,
    ...
  )
  
  # STEP 3: predicted affect for ALL rows, using the estimated parameters
  predicted <- predict_affect(model_empty, fitobj$parameters, data)
  
  # STEP 4: compare predictions with the real ratings in the test block only.
  observed_test <- data@Y[test_rows, , drop = FALSE]
  predicted_test <- predicted[test_rows, , drop = FALSE]
  
  errors <- observed_test - predicted_test
  
  # RMSE per affect variable 
  rmse <- sqrt(colMeans(errors^2, na.rm = TRUE))
  n_ratings <- colSums(!is.na(errors))                # how many ratings were scored
  
  return(
    list(
      rmse = rmse,
      n_ratings = n_ratings,
      parameters = fitobj$parameters
    )
  )
}

### runs all the folds for one participant and one model, and averages the fold errors.
# for one participant and one model, the function goes through the blocks one by one
# each block is hidden in turn, and the modelestimated on the rest
# cv_ne_fold returns the RMSE on the hidden block
# the k RMSEs are collected in fold_rmse and mean_rmse is their average
# we then have one cross-validated RMSE per participant and model

cv_participant <- function(data,
                           model_empty,
                           optimizer,
                           k = 5,
                           folds = NULL,
                           ...) {
  # data = one participant's processed data object (has @Y and @X)
  # model_empty = empty model of the right size, e.g. exponential(d = 1, k = 4)
  # optimizer = your existing optimizer() function
  # k = number of blocks, used only if `folds` is not given
  # folds = optional list of row sets, one per fold. Leave NULL to cut the
  # series into k equal blocks. Supply it yourself when the blocks
  # must follow the data structure (e.g. whole days for Niemeijer)
  # ... = DEoptim / nloptr settings, passed on to cv_one_fold() and fit()
  
  # STEP 1: define the blocks (one block = one fold)
  if (is.null(folds)) {
    folds <- make_folds(nrow(data@Y), k)
  }
  n_folds <- length(folds)
  d <- ncol(data@Y) # number of affect variables 
  
  # STEP 2: empty containers to fill, one row per fold
  fold_rmse <- matrix(NA_real_, nrow = n_folds, ncol = d)
  n_ratings <- matrix(NA_real_, nrow = n_folds, ncol = d)
  colnames(fold_rmse) <- colnames(data@Y)
  colnames(n_ratings) <- colnames(data@Y)
  fold_parameters <- vector("list", n_folds)
  
  # STEP 3: run the folds one at a time
  for (j in 1:n_folds) {
    
    # tryCatch means that one failed fit does not stop the whole run
    # (same idea as in your fit_participant())
    result <- tryCatch(
      cv_one_fold(
        data = data,
        model_empty = model_empty,
        test_rows = folds[[j]],
        optimizer = optimizer,
        ...
      ),
      error = function(e) {
        message("  Fold ", j, " failed: ", conditionMessage(e))
        return(NULL)
      }
    )
    
    # Store the results if the fold worked; otherwise the row stays NA
    if (!is.null(result)) {
      fold_rmse[j, ] <- result$rmse
      n_ratings[j, ] <- result$n_ratings
      fold_parameters[[j]] <- result$parameters
    }
  }
  
  # STEP 4: first averaging step, across the folds of this participant.
  # na.rm = TRUE skips failed folds (and folds without any rating to score).
  mean_rmse <- colMeans(fold_rmse, na.rm = TRUE)
  
  # How many folds actually contributed, so failures stay visible
  n_valid_folds <- colSums(!is.na(fold_rmse))
  
  return(
    list(
      mean_rmse = mean_rmse, # this participant's cross-validated RMSE
      fold_rmse = fold_rmse, # the k separate RMSEs
      n_valid_folds = n_valid_folds,
      n_ratings = n_ratings,
      fold_parameters = fold_parameters # for the optional stability check
    )
  )
}

# Function that runs the cross-validation for every participant in one dataset folder
# and for all three models. It follows the same structurea s run_estimation()
# For each model, the function goes through every participant and run cv_participant
# It collects, per participant, the mean cross-validated RMSE for each affect variable 
# and the number of folds that worked, then writes one CSV per dataset and model
# participants whose whole cross-validation failed get a row of NA 
cv_dataset <- function(folder,
                       base_name,
                       models,
                       optimizer,
                       k = 5,
                       folds_function = NULL,
                       output_dir = file.path(PATHS$estimation, "cross_validation"),
                       ...) {
  # folder = folder with the per-participant RDS files
  # base_name = dataset name used in the output file names
  # models = list of model functions (exponential, quasi_hyperbolic, ...)
  # optimizer = your existing optimizer() function
  # k = number of blocks, used when folds_function is NULL
  # folds_function = optional function(data) that returns a list of row sets,
  #                  one per fold. Leave NULL for k equal blocks of rows.
  # output_dir = where the result CSVs are written
  # ... = DEoptim / nloptr settings (your optim_settings)
  
  # STEP 1: find the participant files
  rds_files <- list.files(
    folder,
    pattern = "\\.rds$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(rds_files) == 0) {
    stop("No RDS files found in: ", folder)
  }
  message("Found ", length(rds_files), " participants in ", basename(folder))
  
  ensure_dir(output_dir)
  
  # STEP 2: choose the number of workers (same strategy as run_estimation)
  detected_cores <- parallel::detectCores()
  if (is.na(detected_cores)) detected_cores <- 1L
  participant_cores <- if (Sys.info()["sysname"] == "Windows") {
    1L
  } else {
    max(1L, as.integer(round(detected_cores / 2) - 1L))
  }
  
  # STEP 3: loop over the models, in sequence
  for (model_name in names(models)) {
    message("\n  Cross-validating model: ", model_name)
    
    # STEP 4: loop over participants, in parallel
    rows <- parallel::mclapply(
      seq_along(rds_files),
      function(idx) {
        
        # Load this participant
        ds <- readRDS(rds_files[idx])
        participant_id <- tools::file_path_sans_ext(basename(rds_files[idx]))
        message("    Participant ", idx, " / ", length(rds_files),
                "  (", participant_id, ")")
        
        # Empty model with the dimensionality of this participant's data
        d <- ncol(ds@Y)
        model_empty <- models[[model_name]](d = d, k = ncol(ds@X))
        
        # Names for the output columns, one per affect variable
        y_names <- colnames(ds@Y)
        if (is.null(y_names)) y_names <- paste0("y", seq_len(d))
        
        # A row of NAs to fall back on if the cross-validation fails entirely
        values <- c(
          setNames(rep(NA_real_, d), paste0("rmse_", y_names)),
          setNames(rep(NA_real_, d), paste0("n_valid_folds_", y_names))
        )
        
        # Folds: default equal blocks, or the dataset-specific function
        folds <- NULL
        if (!is.null(folds_function)) {
          folds <- folds_function(ds)
        }
        
        # Run the cross-validation for this participant
        result <- tryCatch(
          cv_participant(
            data  = ds,
            model_empty = model_empty,
            optimizer = optimizer,
            k = k,
            folds = folds,
            ...
          ),
          error = function(e) {
            message("  Cross-validation failed: ", conditionMessage(e))
            return(NULL)
          }
        )
        
        # Fill in the results if it worked
        if (!is.null(result)) {
          values <- c(
            setNames(result$mean_rmse, paste0("rmse_", y_names)),
            setNames(result$n_valid_folds, paste0("n_valid_folds_", y_names))
          )
        }
        
        return(list(id = participant_id, d = d, values = values))
      },
      mc.cores = participant_cores
    )
    
    # STEP 5: stop if a parallel worker crashed
    worker_failures <- which(vapply(
      rows, function(x) inherits(x, "try-error"), logical(1)
    ))
    if (length(worker_failures) > 0) {
      stop(
        "Parallel worker failure(s) for ", base_name, " / ", model_name,
        " at participant index/indices: ",
        paste(worker_failures, collapse = ", "),
        ". No downstream result should be treated as current."
      )
    }
    
    # STEP 6: one data.frame row per participant
    df_rows <- lapply(
      rows,
      function(r) {
        data.frame(
          participant_id = r$id,
          t(r$values),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
    )
    
    # STEP 7: write the CSV. Participants with different numbers of affect
    # variables (VANHASBROECK_2024) get separate files, as in run_estimation.
    dims <- sapply(rows, function(r) r$d)
    
    for (d_value in sort(unique(dims))) {
      df <- do.call(rbind, df_rows[dims == d_value]) |>
        `rownames<-`(NULL)
      
      file_name <- if (length(unique(dims)) == 1) {
        paste0(base_name, "_", model_name, "_cv.csv")
      } else {
        paste0(base_name, "_", d_value, "_", model_name, "_cv.csv")
      }
      
      write.csv(df, file.path(output_dir, file_name), row.names = FALSE)
    }
  }
  
  return(NULL)
}

##### dataset specific folds
# Functions that cuts the series into consectuive blocks of whole days (Niemeijer)
# or into equal row blocks for the experimental datasets
make_block_folds <- function(n_rows, block_size, k) {
  # n_rows     = number of rows in this participant's data
  # block_size = number of rows that belong together (Niemeijer: 11 = one day)
  # k          = number of folds
  
  n_units <- ceiling(n_rows / block_size)     # e.g. number of days
  if (k > n_units) {
    stop("k = ", k, " is larger than the number of units (", n_units, ").")
  }
  
  # Group the consecutive units (days) into k consecutive folds
  unit_folds <- make_folds(n_units, k)
  
  # Translate each group of units back into rows
  folds <- list()
  for (j in 1:k) {
    first_row <- (min(unit_folds[[j]]) - 1) * block_size + 1
    last_row  <- min(max(unit_folds[[j]]) * block_size, n_rows)
    folds[[j]] <- first_row:last_row
  }
  
  return(folds)
}


get_folds_function <- function(dataset_name, k) {
  # Returns NULL (= k equal blocks of rows, the default) or a function(data)
  # that returns the folds for one participant.
  
  # Niemeijer: 10 beeps + 1 NA separator per day. Folds are whole days, so a
  # block never cuts a day in half (the same 11-row structure as in your
  # residual_block_size).
  if (dataset_name == "NIEMEIJER_2022") {
    return(
      function(data) make_block_folds(nrow(data@Y), block_size = 11, k = k)
    )
  }
  
  # All Vanhasbroeck datasets: equal blocks of rows. For the 2023 data
  # (140 trials = 5 repetitions of 28), k = 5 gives exactly one repetition
  # per fold.
  return(NULL)
}

###### aggregation and model comparison (second averaging step)
# reads the per-model CSVs, keeps only participants for whom every model has enough working folds.
# Reports mean RMSE per model with win percentages, pairwise win percentages and RMSE differences with bootstrap CIs
load_cv_results <- function(output_dir, base_name, model_names) {
  # Reads the CSVs written by cv_dataset(), one data.frame per model.
  # For the split 2024 data, pass base_name = "VANHASBROECK_2024_1" or "_2".
  results <- list()
  for (model_name in model_names) {
    file <- file.path(output_dir, paste0(base_name, "_", model_name, "_cv.csv"))
    if (!file.exists(file)) stop("Missing file: ", file)
    results[[model_name]] <- read.csv(file, stringsAsFactors = FALSE)
  }
  return(results)
}


build_rmse_matrix <- function(results, column, min_valid_folds = 3) {
  # One row per participant, one column per model, for ONE affect variable.
  # A participant is kept only if ALL models have at least min_valid_folds
  # working folds, so all models are compared on the same participants.
  
  folds_column <- sub("^rmse_", "n_valid_folds_", column)
  ids <- results[[1]]$participant_id
  
  rmse <- matrix(NA_real_, nrow = length(ids), ncol = length(results),
                 dimnames = list(ids, names(results)))
  enough_folds <- matrix(FALSE, nrow = length(ids), ncol = length(results))
  
  for (j in seq_along(results)) {
    df <- results[[j]][match(ids, results[[j]]$participant_id), ]
    rmse[, j] <- df[[column]]
    enough_folds[, j] <- !is.na(df[[folds_column]]) &
      df[[folds_column]] >= min_valid_folds
  }
  
  rmse[!enough_folds] <- NA
  rmse <- rmse[complete.cases(rmse), , drop = FALSE]
  return(rmse)
}


summarize_cv <- function(rmse_matrix) {
  # Second averaging step: average over participants, plus win percentages.
  winner <- colnames(rmse_matrix)[apply(rmse_matrix, 1, which.min)]
  wins   <- table(factor(winner, levels = colnames(rmse_matrix)))
  
  data.frame(
    model     = colnames(rmse_matrix),
    n         = nrow(rmse_matrix),
    mean_rmse = colMeans(rmse_matrix),
    sd_rmse   = apply(rmse_matrix, 2, sd),
    win_pct   = 100 * as.numeric(wins) / nrow(rmse_matrix),
    row.names = NULL
  )
}


pairwise_cv <- function(rmse_matrix, n_boot = 10000, seed = 1) {
  # Pairwise comparison in the format of your Table 5: win percentages and their
  # difference (delta_pp = first minus second; positive favors the first model,
  # because a LOWER RMSE wins), with percentile bootstrap CIs over participants.
  set.seed(seed)
  pairs <- combn(colnames(rmse_matrix), 2, simplify = FALSE)
  
  rows <- lapply(pairs, function(p) {
    a <- rmse_matrix[, p[1]]
    b <- rmse_matrix[, p[2]]
    n <- length(a)
    
    statistic <- function(idx) {
      c(delta_pp  = 100 * (mean(a[idx] < b[idx]) - mean(a[idx] > b[idx])),
        mean_diff = mean(a[idx] - b[idx]))
    }
    
    observed <- statistic(seq_len(n))
    boot <- t(replicate(n_boot, statistic(sample.int(n, replace = TRUE))))
    
    data.frame(
      model_1      = p[1],
      model_2      = p[2],
      wins_1       = 100 * mean(a < b),
      wins_2       = 100 * mean(a > b),
      delta_pp     = observed["delta_pp"],
      delta_pp_lo  = quantile(boot[, "delta_pp"], 0.025),
      delta_pp_hi  = quantile(boot[, "delta_pp"], 0.975),
      mean_diff    = observed["mean_diff"],
      mean_diff_lo = quantile(boot[, "mean_diff"], 0.025),
      mean_diff_hi = quantile(boot[, "mean_diff"], 0.975),
      row.names    = NULL
    )
  })
  
  return(do.call(rbind, rows))
}


summarize_cv_dataset <- function(output_dir, base_name, model_names,
                                 min_valid_folds = 3, n_boot = 10000) {
  # Runs everything for one dataset, for every affect variable it contains.
  results <- load_cv_results(output_dir, base_name, model_names)
  rmse_columns <- grep("^rmse_", names(results[[1]]), value = TRUE)
  
  out <- list()
  for (column in rmse_columns) {
    rmse_matrix <- build_rmse_matrix(results, column, min_valid_folds)
    out[[column]] <- list(
      overview = summarize_cv(rmse_matrix),
      pairwise = pairwise_cv(rmse_matrix, n_boot)
    )
  }
  return(out)
}


# ##############################################################################
# # PURPOSE: blocked k-fold cross-validation of the three discounting models for
# # every dataset. Writes one CSV per dataset and model to
# # PATHS$estimation/cross_validation. Run PROCESSING and ESTIMATION first.
# ##############################################################################
#
# config_file <- if (file.exists(file.path("analysis", "_config.R"))) {
#   file.path("analysis", "_config.R")
# } else if (file.exists("_config.R")) {
#   "_config.R"
# } else {
#   stop("Could not find analysis/_config.R. Run from the repository root.")
# }
# source(config_file)
# source(file.path(PATHS$analysis, "_helpers.R"))
# source(file.path(PATHS$analysis, "_cv_functions.R"))
# rm(config_file)
#
# devtools::load_all(PROJECT_ROOT)
# library(DEoptim)
# library(nloptr)
#
# # The optimizer: paste here the SAME definition as in your estimation script
# optimizer <- function(obj, lower, upper,
#                       algorithm = "NLOPT_LN_BOBYQA", maxeval = 1e3,
#                       ftol_abs = 1e-15, xtol_abs = 1e-15, print_level = 0, ...) {
#   result_de <- DEoptim::DEoptim(obj, lower, upper,
#                                 control = DEoptim::DEoptim.control(...))
#   x0 <- result_de$optim$bestmem
#   result_nl <- nloptr::nloptr(
#     x0, obj, lb = lower, ub = upper,
#     opts = list(algorithm = algorithm, maxeval = maxeval, ftol_abs = ftol_abs,
#                 xtol_abs = xtol_abs, print_level = print_level)
#   )
#   list(parameters = result_nl$solution, objective = result_nl$objective)
# }
#
# # The SAME settings as in the estimation script
# optim_settings <- list(
#   dynamics = "isotropic", covariance = "symmetric",
#   itermax = 1e3, NP = 150, CR = 0.75, strategy = 6, p = 0.8,
#   reltol = 1e-15, steptol = 100, trace = FALSE,
#   maxeval = 1e5, xtol_abs = 1e-20, ftol_abs = 1e-20
# )
#
# models <- list(
#   exponential        = exponential,
#   quasi_hyperbolic   = quasi_hyperbolic,
#   double_exponential = double_exponential
# )
#
# # Number of folds per dataset. Defaults to 5.
# # NIEMEIJER_2022: 21 days -> 7 folds of 3 days.
# # VANHASBROECK_2021: PLACEHOLDER (k = 3). Blocked k-fold is doubtful for this
# # dataset (60 trials, forgetting steps near 30): decide with the team first.
# k_per_dataset <- list(
#   NIEMEIJER_2022    = 7,
#   VANHASBROECK_2021 = 3
# )
#
# datasets <- RAW_DATASETS
#
# for (i in seq_along(datasets)) {
#   message("\n========== ", datasets[i], " ==========")
#
#   k <- if (datasets[i] %in% names(k_per_dataset)) k_per_dataset[[datasets[i]]] else 5
#
#   do.call(
#     cv_dataset,
#     c(
#       list(
#         folder         = file.path(PATHS$processed_data, datasets[i]),
#         base_name      = datasets[i],
#         models         = models,
#         optimizer      = optimizer,
#         k              = k,
#         folds_function = get_folds_function(datasets[i], k)
#       ),
#       optim_settings
#     )
#   )
# }
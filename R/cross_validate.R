# Blocked cross-validation of affect discounting models.
# Model fitting excludes held-out ratings; prediction uses the known predictors.
# This evaluates out-of-sample fit, not strictly forward-time forecasting.


# Fold construction ----------------------------------------------------------

# Split a time series into approximately equal consecutive folds.
make_folds <- function(n, k) {
  if (k < 2 || k > n) {
    stop("k must be between 2 and n.")
  }

  edges <- round(seq(0, n, length.out = k + 1))

  lapply(seq_len(k), function(i) {
    seq.int(edges[i] + 1, edges[i + 1])
  })
}


# Construct consecutive folds without splitting fixed-size blocks.
make_block_folds <- function(n_rows, block_size, k) {
  n_units <- ceiling(n_rows / block_size)
  unit_folds <- make_folds(n_units, k)

  lapply(unit_folds, function(units) {
    first_row <- (min(units) - 1L) * block_size + 1L
    last_row <- min(max(units) * block_size, n_rows)

    seq.int(first_row, last_row)
  })
}


#' Select fold construction for a dataset
#'
#' Niemeijer folds preserve complete 11-row days (10 beeps and a separator).
#' Other datasets use approximately equal consecutive rows.
#'
#' @param dataset_name Dataset identifier.
#' @param k Number of folds.
#' @return A fold-generating function for Niemeijer, or `NULL` otherwise.
#' @export
get_folds_function <- function(dataset_name, k) {
  if (dataset_name == "NIEMEIJER_2022") {
    return(function(data) {
      make_block_folds(nrow(data@Y), block_size = 11L, k = k)
    })
  }

  NULL
}


# Fitting and prediction -----------------------------------------------------

# Predictions depend on predictors and parameters, never observed affect.
predict_affect <- function(model_empty, parameters, data,
                           dynamics = "isotropic", covariance = "symmetric") {
  model <- fill(
    model_empty,
    parameters,
    dynamics = dynamics,
    covariance = covariance,
    parameters_only = FALSE,
    cholesky = FALSE
  )

  predict(model, data)@Y
}


# Mask held-out affect ratings during fitting and prediction.
cv_one_fold <- function(data, model_empty, test_rows, optimizer,
                        dynamics = "isotropic", covariance = "symmetric", ...) {

  data_masked <- data
  data_masked@Y[test_rows, ] <- NA_real_

  fitobj <- fit(
    model_empty,
    data_masked,
    dynamics = dynamics,
    covariance = covariance,
    optimizer = optimizer,
    ...
  )

  predicted <- predict_affect(
    model_empty,
    fitobj$parameters,
    data_masked,
    dynamics = dynamics,
    covariance = covariance
  )

  errors <- data@Y[test_rows, , drop = FALSE] -
    predicted[test_rows, , drop = FALSE]

  list(
    rmse = sqrt(colMeans(errors^2, na.rm = TRUE)),
    n_ratings = colSums(!is.na(errors)),
    parameters = fitobj$parameters
  )
}


#' Cross-validate one model for one participant
#'
#' Fits the model with each consecutive fold of affect ratings withheld in
#' turn. The returned overall RMSE pools squared errors across all held-out
#' observations, rather than averaging fold-level RMSE values.
#'
#' @param data A `dataset` object for one participant.
#' @param model_empty An unfilled model with matching outcome and predictor
#'   dimensions.
#' @param optimizer Optimization function passed to `fit()`.
#' @param k Number of folds when `folds` is `NULL`.
#' @param folds Optional list of row indices for each held-out fold.
#' @param ... Additional arguments passed through to `fit()`.
#' @return A list with pooled RMSE, per-fold RMSE, valid-fold counts, numbers
#'   of held-out ratings, and estimated parameters for each fold.
#' @export
cv_participant <- function(data, model_empty, optimizer, k = 5,
                           folds = NULL, ...) {

  if (is.null(folds)) {
    folds <- make_folds(nrow(data@Y), k)
  }

  n_folds <- length(folds)
  n_outcomes <- ncol(data@Y)

  fold_rmse <- matrix(NA_real_, n_folds, n_outcomes)
  n_ratings <- matrix(NA_real_, n_folds, n_outcomes)
  fold_parameters <- vector("list", n_folds)

  colnames(fold_rmse) <- colnames(data@Y)
  colnames(n_ratings) <- colnames(data@Y)

  for (i in seq_along(folds)) {
    result <- tryCatch(
      cv_one_fold(
        data = data,
        model_empty = model_empty,
        test_rows = folds[[i]],
        optimizer = optimizer,
        ...
      ),
      error = function(e) {
        message("Fold ", i, " failed: ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(result)) {
      next
    }

    fold_rmse[i, ] <- result$rmse
    n_ratings[i, ] <- result$n_ratings
    fold_parameters[[i]] <- result$parameters
  }

  # Weight fold-level mean squared errors by the number of observed ratings.
  n_observed <- colSums(n_ratings, na.rm = TRUE)

  pooled_rmse <- sqrt(
    colSums(fold_rmse^2 * n_ratings, na.rm = TRUE) / n_observed
  )
  pooled_rmse[n_observed == 0] <- NA_real_

  list(
    pooled_rmse = pooled_rmse,
    fold_rmse = fold_rmse,
    n_valid_folds = colSums(!is.na(fold_rmse)),
    n_ratings = n_ratings,
    fold_parameters = fold_parameters
  )
}

# Dataset execution ----------------------------------------------------------

#' Cross-validate models across participants in a dataset
#'
#' Runs blocked cross-validation for each participant and model, then writes
#' participant-level RMSE and valid-fold counts to one CSV per model. Datasets
#' containing different numbers of affect outcomes are saved separately.
#'
#' @param folder Directory containing participant-level RDS datasets.
#' @param base_name Identifier used to name the output CSV files.
#' @param models Named list of model constructor functions.
#' @param optimizer Optimization function passed to `fit()`.
#' @param output_dir Directory in which to save cross-validation results.
#' @param k Number of consecutive folds.
#' @param folds_function Optional function accepting a dataset and returning
#'   a list of held-out row indices. Defaults to equal consecutive folds.
#' @param ... Additional arguments passed through to `fit()`.
#' @return Invisibly returns `NULL` after writing CSV files.
#' @export
cv_dataset <- function(folder, base_name, models, optimizer, output_dir,
                       k = 5, folds_function = NULL, ...) {

  rds_files <- list.files(
    folder,
    pattern = "\\.rds$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(rds_files) == 0L) {
    stop("No RDS files found in: ", folder)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Reserve some CPU capacity for the expensive participant-level fits.
  detected_cores <- parallel::detectCores()
  if (is.na(detected_cores)) {
    detected_cores <- 1L
  }

  participant_cores <- if (.Platform$OS.type == "windows") {
    1L
  } else {
    max(1L, as.integer(round(detected_cores / 2) - 1L))
  }

  message("Found ", length(rds_files), " participants in ", basename(folder))

  for (model_name in names(models)) {
    message("\nCross-validating model: ", model_name)

    rows <- parallel::mclapply(
      seq_along(rds_files),
      function(i) {
        data <- readRDS(rds_files[i])
        participant_id <- tools::file_path_sans_ext(basename(rds_files[i]))

        message("Participant ", i, "/", length(rds_files), ": ", participant_id)

        d <- ncol(data@Y)
        model_empty <- models[[model_name]](d = d, k = ncol(data@X))

        y_names <- colnames(data@Y)
        if (is.null(y_names)) {
          y_names <- paste0("y", seq_len(d))
        }

        folds <- if (is.null(folds_function)) {
          NULL
        } else {
          folds_function(data)
        }

        result <- tryCatch(
          cv_participant(
            data = data,
            model_empty = model_empty,
            optimizer = optimizer,
            k = k,
            folds = folds,
            ...
          ),
          error = function(e) {
            message("Cross-validation failed: ", conditionMessage(e))
            NULL
          }
        )

        # Preserve a result row even when all folds fail for a participant.
        if (is.null(result)) {
          rmse <- rep(NA_real_, d)
          valid_folds <- rep(NA_real_, d)
        } else {
          rmse <- result$pooled_rmse
          valid_folds <- result$n_valid_folds
        }

        values <- c(
          setNames(rmse, paste0("rmse_", y_names)),
          setNames(valid_folds, paste0("n_valid_folds_", y_names))
        )

        list(id = participant_id, d = d, values = values)
      },
      mc.cores = participant_cores
    )

    failed <- which(vapply(
      rows, inherits, logical(1), "try-error"
    ))

    if (length(failed) > 0L) {
      stop(
        "Parallel worker failure for ", base_name, " / ", model_name,
        " at participant indices: ", paste(failed, collapse = ", ")
      )
    }

    # Keep one- and two-outcome participants in separate result files.
    dims <- vapply(rows, function(x) x$d, integer(1))
    split_dimensions <- length(unique(dims)) > 1L

    for (d_value in sort(unique(dims))) {
      df <- do.call(
        rbind,
        lapply(rows[dims == d_value], function(x) {
          data.frame(
            participant_id = x$id,
            t(x$values),
            check.names = FALSE
          )
        })
      )

      rownames(df) <- NULL

      suffix <- if (split_dimensions) paste0("_", d_value) else ""
      file_name <- paste0(base_name, suffix, "_", model_name, "_cv.csv")

      write.csv(df, file.path(output_dir, file_name), row.names = FALSE)
    }
  }

  invisible(NULL)
}

# Model comparison -----------------------------------------------------------

# Load the participant-level results for all fitted models.
load_cv_results <- function(output_dir, base_name, model_names) {
  files <- file.path(
    output_dir,
    paste0(base_name, "_", model_names, "_cv.csv")
  )

  missing <- files[!file.exists(files)]
  if (length(missing) > 0L) {
    stop("Missing CV results: ", paste(missing, collapse = ", "))
  }

  setNames(lapply(files, read.csv), model_names)
}

# Keep participants with sufficient valid folds in every model.
build_rmse_matrix <- function(results, column, min_valid_folds = 3) {
  ids <- results[[1]]$participant_id
  folds_column <- sub("^rmse_", "n_valid_folds_", column)

  rmse <- matrix(
    NA_real_,
    nrow = length(ids),
    ncol = length(results),
    dimnames = list(ids, names(results))
  )

  for (model_name in names(results)) {
    df <- results[[model_name]]
    df <- df[match(ids, df$participant_id), , drop = FALSE]

    valid <- is.finite(df[[folds_column]]) &
      df[[folds_column]] >= min_valid_folds

    rmse[, model_name] <- ifelse(valid, df[[column]], NA_real_)
  }

  rmse[rowSums(is.finite(rmse)) == ncol(rmse), , drop = FALSE]
}


# Summarize model RMSE and unique wins; report tied winners separately.
summarize_cv <- function(rmse_matrix) {
  if (nrow(rmse_matrix) == 0L) {
    stop("No participants with valid RMSE values for all models.")
  }

  best_rmse <- apply(rmse_matrix, 1, min)
  winners <- sweep(rmse_matrix, 1, best_rmse, "==")

  ties <- rowSums(winners) > 1
  winners[ties, ] <- FALSE

  data.frame(
    model = colnames(rmse_matrix),
    n = nrow(rmse_matrix),
    mean_rmse = colMeans(rmse_matrix),
    sd_rmse = apply(rmse_matrix, 2, sd),
    win_pct = 100 * colMeans(winners),
    tie_pct = 100 * mean(ties),
    row.names = NULL
  )
}


# Compare model pairs using a paired bootstrap over participants.
# Positive win difference or negative RMSE difference favors the first model.
pairwise_cv <- function(rmse_matrix, n_boot = 10000, seed = 1) {
  if (nrow(rmse_matrix) == 0L) {
    stop("No participants with valid RMSE values for all models.")
  }

  set.seed(seed)

  pairs <- combn(colnames(rmse_matrix), 2, simplify = FALSE)

  do.call(rbind, lapply(pairs, function(models) {
    a <- rmse_matrix[, models[1]]
    b <- rmse_matrix[, models[2]]

    difference <- a - b
    wins <- sign(-difference)

    boot <- replicate(n_boot, {
      idx <- sample.int(length(difference), replace = TRUE)

      c(
        delta_pp = 100 * mean(wins[idx]),
        mean_diff = mean(difference[idx])
      )
    })

    data.frame(
      model_1 = models[1],
      model_2 = models[2],
      wins_1 = 100 * mean(a < b),
      wins_2 = 100 * mean(b < a),
      tie_pct = 100 * mean(a == b),
      delta_pp = 100 * mean(wins),
      delta_pp_lo = unname(quantile(boot["delta_pp", ], 0.025)),
      delta_pp_hi = unname(quantile(boot["delta_pp", ], 0.975)),
      mean_diff = mean(difference),
      mean_diff_lo = unname(quantile(boot["mean_diff", ], 0.025)),
      mean_diff_hi = unname(quantile(boot["mean_diff", ], 0.975))
    )
  }))
}

#' Summarize cross-validation results for one dataset
#'
#' Compares models using the same participants with sufficient valid folds.
#' Returns overall RMSE summaries and pairwise win rates and RMSE differences,
#' with participant-bootstrap confidence intervals for the latter.
#'
#' @param output_dir Directory containing the model-level CV result files.
#' @param base_name Dataset identifier used in the CSV filenames.
#' @param model_names Names of the models to compare.
#' @param min_valid_folds Minimum successful folds required per model and
#'   participant.
#' @param n_boot Number of participant-bootstrap samples for confidence
#'   intervals.
#' @return A named list for each affect outcome, containing an `overview`
#'   and a `pairwise` data frame.
#' @export
summarize_cv_dataset <- function(output_dir, base_name, model_names,
                                 min_valid_folds = 3, n_boot = 10000) {
  results <- load_cv_results(output_dir, base_name, model_names)
  rmse_columns <- grep("^rmse_", names(results[[1]]), value = TRUE)

  summaries <- lapply(rmse_columns, function(column) {
    rmse <- build_rmse_matrix(results, column, min_valid_folds)

    list(
      overview = summarize_cv(rmse),
      pairwise = pairwise_cv(rmse, n_boot = n_boot)
    )
  })

  setNames(summaries, rmse_columns)
}

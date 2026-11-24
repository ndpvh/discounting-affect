################################################################################
# PURPOSE:
#
# Reconstruct participant-level residuals from the existing parameter
# estimates, then plot the participant-averaged residual against observation
# order for each dataset/model combination. This does not refit models.
#
# Run analysis/01_process_data.R first to create participant RDS files, then
# run this script from the repository root. Optional arguments select datasets.
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


participant_mean_residuals <- function(residuals) {
  if (!is.numeric(residuals)) {
    stop("Residuals must be numeric.")
  }
  if (is.null(dim(residuals))) {
    residuals <- matrix(residuals, ncol = 1L)
  }
  if (length(dim(residuals)) != 2L) {
    stop("Residuals must be a vector or a two-dimensional matrix.")
  }
  
  means <- rowMeans(residuals, na.rm = TRUE)
  means[!is.finite(means)] <- NA_real_
  means
}


summarize_residuals_by_trial <- function(participant_residuals) {
  required_columns <- c(
    "dataset", "model", "participant_id", "trial_index", "residual"
  )
  missing_columns <- setdiff(required_columns, names(participant_residuals))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing residual column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (nrow(participant_residuals) == 0L) {
    stop("No participant residuals were provided.")
  }
  
  group <- interaction(
    participant_residuals[c("dataset", "model", "trial_index")],
    drop = TRUE,
    lex.order = TRUE
  )
  indices_by_group <- split(seq_len(nrow(participant_residuals)), group)
  
  summaries <- lapply(indices_by_group, function(indices) {
    rows <- participant_residuals[indices, , drop = FALSE]
    valid <- is.finite(rows$residual)
    if (!any(valid)) {
      return(NULL)
    }
    
    data.frame(
      dataset = rows$dataset[[1]],
      model = rows$model[[1]],
      trial_index = rows$trial_index[[1]],
      mean_residual = mean(rows$residual[valid]),
      n_participants = sum(valid),
      stringsAsFactors = FALSE
    )
  })
  
  summaries <- Filter(Negate(is.null), summaries)
  if (length(summaries) == 0L) {
    stop("No finite residuals were available to summarize.")
  }
  
  result <- do.call(rbind, summaries)
  result <- result[
    order(result$dataset, result$model, result$trial_index),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}


reconstruct_participant_residuals <- function(data,
                                              model_type,
                                              parameter_row,
                                              model_functions) {
  if (!methods::is(data, "dataset")) {
    stop("The processed participant file does not contain a dataset object.")
  }
  if (is.null(model_functions[[model_type]])) {
    stop("No model constructor is available for: ", model_type)
  }
  
  model <- model_functions[[model_type]](
    d = ncol(data@Y),
    k = ncol(data@X)
  )
  parameter_columns <- parameter_names(
    model,
    dynamics = "isotropic",
    parameters_only = TRUE
  )
  missing_columns <- setdiff(parameter_columns, names(parameter_row))
  if (length(missing_columns) > 0L) {
    stop(
      "Estimated parameter column(s) missing for ", model_type, ": ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  parameters <- vapply(
    parameter_columns,
    function(column) as.numeric(parameter_row[[column]]),
    numeric(1)
  )
  if (any(!is.finite(parameters))) {
    stop("Estimated parameters contain non-finite values.")
  }
  
  model <- fill(
    model,
    parameters,
    dynamics = "isotropic",
    parameters_only = TRUE
  )
  data@Y - predict(model, data)@Y
}


load_participant_residuals <- function(dataset_names) {
  estimation_data <- load_estimation_data(
    PATHS$estimation,
    MODEL_TYPES
  )
  model_functions <- list(
    exponential = exponential,
    quasi_hyperbolic = quasi_hyperbolic,
    double_exponential = double_exponential
  )
  residual_rows <- list()
  n_rows <- 0L
  
  for (dataset_name in dataset_names) {
    if (dataset_name %in% names(ESTIMATION_TO_RAW_DATASET)) {
      raw_dataset_name <- ESTIMATION_TO_RAW_DATASET[[dataset_name]]
    } else {
      raw_dataset_name <- dataset_name
    }
    participant_dir <- file.path(PATHS$processed_data, raw_dataset_name)
    require_paths(
      participant_dir,
      path_type = "dir",
      context = paste0("processed participant data for ", dataset_name)
    )
    
    participant_files <- list.files(
      participant_dir,
      pattern = "\\.[Rr][Dd][Ss]$",
      full.names = TRUE
    )
    participant_ids <- tools::file_path_sans_ext(basename(participant_files))
    if (anyDuplicated(participant_ids)) {
      stop("Duplicate participant IDs in: ", participant_dir)
    }
    participant_file_by_id <- setNames(participant_files, participant_ids)
    
    for (model_type in MODEL_TYPES) {
      estimates <- estimation_data[[dataset_name]][[model_type]]
      if (is.null(estimates)) {
        stop(
          "Missing estimates for ", dataset_name, " / ", model_type,
          " in ", PATHS$estimation
        )
      }
      if (!"participant_id" %in% names(estimates)) {
        stop(
          "Missing participant_id column for ", dataset_name, " / ",
          model_type
        )
      }
      if (anyDuplicated(estimates$participant_id)) {
        stop(
          "Duplicate participant IDs in estimates for ",
          dataset_name, " / ", model_type
        )
      }
      
      rows_before_model <- n_rows
      skipped <- 0L
      for (i in seq_len(nrow(estimates))) {
        participant_id <- as.character(estimates$participant_id[[i]])
        participant_file <- unname(participant_file_by_id[participant_id])
        if (length(participant_file) == 0L || is.na(participant_file)) {
          stop(
            "No processed data file found for participant ", participant_id,
            " in ", participant_dir
          )
        }
        
        participant_data <- readRDS(participant_file)
        if (!methods::is(participant_data, "dataset")) {
          stop(
            "Processed participant file does not contain a dataset object: ",
            participant_file
          )
        }
        parameter_columns <- parameter_names(
          model_functions[[model_type]](
            d = ncol(participant_data@Y),
            k = ncol(participant_data@X)
          ),
          dynamics = "isotropic",
          parameters_only = TRUE
        )
        missing_columns <- setdiff(parameter_columns, names(estimates))
        if (length(missing_columns) > 0L) {
          stop(
            "Estimated parameter column(s) missing for ", dataset_name,
            " / ", model_type, ": ", paste(missing_columns, collapse = ", ")
          )
        }
        parameter_values <- unlist(
          estimates[i, parameter_columns, drop = FALSE],
          use.names = FALSE
        )
        if (any(!is.finite(parameter_values))) {
          skipped <- skipped + 1L
          next
        }
        
        residual_matrix <- reconstruct_participant_residuals(
          participant_data,
          model_type,
          estimates[i, , drop = FALSE],
          model_functions
        )
        participant_residuals <- participant_mean_residuals(residual_matrix)
        n <- length(participant_residuals)
        n_rows <- n_rows + 1L
        residual_rows[[n_rows]] <- data.frame(
          dataset = dataset_name,
          model = model_type,
          participant_id = participant_id,
          trial_index = seq_len(n),
          residual = participant_residuals,
          stringsAsFactors = FALSE
        )
      }
      
      if (skipped > 0L) {
        warning(
          "Skipped ", skipped, " participant(s) with non-finite estimates for ",
          dataset_name, " / ", model_type,
          call. = FALSE
        )
      }
      if (n_rows == rows_before_model) {
        stop(
          "No usable participant residuals for ",
          dataset_name, " / ", model_type
        )
      }
    }
  }
  
  do.call(rbind, residual_rows)
}


plot_mean_residuals <- function(dataset_names) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is required to create the residual plot.")
  }
  
  participant_residuals <- load_participant_residuals(dataset_names)
  mean_residuals <- summarize_residuals_by_trial(participant_residuals)
  
  results_dir <- file.path(PATHS$results, "residual_time_scale")
  figures_dir <- file.path(PATHS$figures, "residual_time_scale")
  ensure_dir(results_dir)
  ensure_dir(figures_dir)
  
  utils::write.csv(
    mean_residuals,
    file.path(results_dir, "mean_residuals_by_trial.csv"),
    row.names = FALSE
  )
  
  model_labels <- c(
    exponential = "Exponential",
    quasi_hyperbolic = "Quasi-hyperbolic",
    double_exponential = "Double exponential"
  )
  mean_residuals$dataset <- factor(
    mean_residuals$dataset,
    levels = dataset_names
  )
  mean_residuals$model_label <- factor(
    unname(model_labels[mean_residuals$model]),
    levels = unname(model_labels[MODEL_TYPES])
  )
  
  plot <- ggplot2::ggplot(
    mean_residuals,
    ggplot2::aes(x = trial_index, y = mean_residual)
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray45"
    ) +
    ggplot2::geom_point(color = "#1F6FEB", alpha = 0.8, size = 1.2) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(dataset),
      cols = ggplot2::vars(model_label),
      scales = "free_x"
    ) +
    ggplot2::labs(
      x = "Trial index",
      y = "Mean residual (observed - predicted)",
      title = "Mean residuals over trial order",
      subtitle = "Each point averages participant-level residuals across outcome dimensions"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  ggplot2::ggsave(
    filename = file.path(figures_dir, "mean_residuals_by_trial.jpeg"),
    plot = plot,
    width = 15,
    height = max(6, 2.2 * length(dataset_names)),
    units = "in",
    dpi = 300
  )
  message(
    "Saved mean residual plot to: ",
    file.path(figures_dir, "mean_residuals_by_trial.jpeg")
  )
  invisible(mean_residuals)
}


run_residual_analysis <- function() {
  dataset_names <- commandArgs(trailingOnly = TRUE)
  if (length(dataset_names) == 0L) {
    dataset_names <- ESTIMATION_DATASETS
  }
  unknown_datasets <- setdiff(dataset_names, ESTIMATION_DATASETS)
  if (length(unknown_datasets) > 0L) {
    stop(
      "Unknown estimation dataset(s): ",
      paste(unknown_datasets, collapse = ", "),
      "\nAvailable datasets: ",
      paste(ESTIMATION_DATASETS, collapse = ", ")
    )
  }
  
  devtools::load_all(PROJECT_ROOT)
  plot_mean_residuals(dataset_names)
}

if (sys.nframe() == 0L) {
  run_residual_analysis()
}

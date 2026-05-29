################################################################################
# PURPOSE: 
# 
# Perform a recovery study for the different discounting models defined within 
# this package. The recovery study will also include typical fit statistics 
# such as AIC and BIC, as well as some checks for the residual structure to 
# ensure everything is going well.
################################################################################
################################################################################
# PRELIMINARIES

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
                "scripts", 
                "results", 
                "recovery",
                paste0(
                    names(models)[j], 
                    ".Rds"
                )
            )
        )

        # Create recovery plots
        plt <- lapply(
            colnames(result$simulate),
            function(col) {
                # Create a dataframe of simulated parameters values (x) and 
                # estimated ones (y)
                plot_data <- cbind(
                    result$simulate[, col],
                    result$fit[, col]
                ) |>
                    as.data.frame() |>
                    `colnames<-` (c("x", "y"))

                # Get the range of values
                limits <- range(c(plot_data$x, plot_data$y))

                # Create a plot for the recovery
                plt <- ggplot2::ggplot(
                    data = plot_data, 
                    ggplot2::aes(
                        x = x, 
                        y = y
                    )
                ) +
                    ggplot2::geom_abline(
                        intercept = 0, 
                        slope = 1, 
                        linewidth = 2, 
                        color = "black"
                    ) +
                    ggplot2::geom_point(
                        shape = 21,
                        alpha = 0.25, 
                        color = "black",
                        fill = "cornflowerblue",
                        size = 5
                    ) +
                    ggplot2::annotate(
                        "text",
                        x = limits[1] + 0.05 * diff(limits),
                        y = limits[1] + 0.95 * diff(limits),
                        label = format(
                            round(
                                cor(
                                    plot_data$x, 
                                    plot_data$y
                                ),
                                digits = 2
                            ),
                            nsmall = 2
                        ),
                        size = 5,
                        hjust = 0
                    ) +
                    ggplot2::lims(
                        x = limits,
                        y = limits
                    ) +
                    ggplot2::labs(
                        title = col,
                        x = "Simulated",
                        y = "Estimated"
                    ) +
                    ggplot2::theme(
                        panel.background = ggplot2::element_rect(
                            fill = "white"
                        ),
                        panel.border = ggplot2::element_rect(
                            fill = NA, 
                            color = "black",
                            linewidth = 1
                        ),
                        axis.title = ggplot2::element_text(size = 15),
                        plot.title = ggplot2::element_text(size = 20)
                    )
                
                return(plt)
            }
        )

        if(length(plt) %% 2 == 1) {
            plt[[length(plt) + 1]] <- ggplot2::ggplot() +
                ggplot2::theme_void()
        }

        plt <- cowplot::plot_grid(
            plotlist = plt,
            nrow = my_model@d,
            ncol = round(length(plt) / my_model@d),
            byrow = FALSE
        )

        ggplot2::ggsave(
            file.path(
                "scripts",
                "figures",
                "recovery",
                paste0(
                    names(models)[j], 
                    ".png"
                )
            ),
            plt,
            width = ceiling(ncol(result$simulate) * 1500 / my_model@d), 
            height = my_model@d * 1550,
            unit = "px",
            limitsize = FALSE
        )

        cat("\n")

        return(NULL)
    },
    mc.cores = ifelse(
        Sys.info()["sysname"] == "Windows",
        1,
        parallel::detectCores() - 1
    )
)

cat("\n")

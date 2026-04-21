################################################################################
# PURPOSE: 
# 
# Perform a recovery study for the different discounting models defined within 
# this package. The recovery study will also include typical fit statistics 
# such as AIC and BIC, as well as some checks for the residual structure to 
# ensure everything is going well.
################################################################################

devtools::load_all()



################################################################################
# PRELIMINARIES

# Define the number of recoveries `iterations` and the number of datapoints `N`
iterations <- 20
N <- 1400

# Define the models to use for the recovery study
models <- list(
    "exponential_21" = exponential(d = 2, k = 1),
    "quasi_hyperbolic_21" = quasi_hyperbolic(d = 2, k = 1),
    "double_exponential_21" = double_exponential(d = 2, k = 1)
)

# Define functions for generating the values of X in the simulation
x_function <- \(x) runif(x, min = -2, max = 2)

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
                      algorithms,
                      ...) {
    
    # Generate an initial condition for the first optimization procedure
    x0 <- runif(
        length(lower), 
        min = lower, 
        max = upper
    )

    # Loop over all algorithms that should be tried and tested. Update the 
    # initial condition after every estimation round.
    for(i in seq_along(algorithms)) {
        # Perform the estimation
        result <- nloptr::nloptr(
            x0,
            obj,
            lb = lower, 
            ub = upper, 
            opts = list(
                algorithm = algorithms[i],
                ...
            )
        )

        # Update the initial condition
        x0 <- result$solution
    }

    # Return the results in a named list, as required by the fit function
    return(
        list(
            "parameters" = result$solution, 
            "objective" = result$objective
        )
    )
}

# Define the different optimizers to try out and provide them with an informative
# name
optim_configs <- expand.grid(
    c("NLOPT_GN_DIRECT", "NLOPT_GN_DIRECT_L", "NLOPT_GN_CRS2_LM", "NLOPT_GN_ESCH"),
    c("NLOPT_LN_COBYLA", "NLOPT_LN_BOBYQA", "NLOPT_LN_NELDERMEAD", "NLOPT_LD_LBFGS")
) |>
    dplyr::mutate(
        truncated_1 = Var1 |>
            as.character() |>
            substring(10),
        truncated_2 = Var2 |>
            as.character() |>
            substring(10),
        name = paste(truncated_1, truncated_2, sep = "-"),
        name = tolower(name)
    ) |>
    dplyr::select(Var1, Var2, name)



################################################################################
# PERFORM THE RECOVERY

# Loop over the optimizers and over the different models, performing model 
# estimation in parallel.
empty <- lapply(
    seq_along(optim_configs),
    function(i) {
        # Provide some information on the optimizer used
        cat(optim_configs$name[i], "\n")

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
                    Xfun = x_function,
                    N = N,

                    # Model characteristics
                    dynamics = "isotropic",
                    covariance = "symmetric",

                    # Additional stuff
                    print_iteration = TRUE,
                    print_content = paste(
                        names(models)[j], 
                        ": anisotropic - symmetric",
                        sep = ""
                    ),

                    # Optimization characteristics
                    optimizer = function(obj, lower, upper, ...) optimizer(
                        obj, 
                        lower,
                        upper,
                        algorithms = optim_configs[i, c("Var1", "Var2")],
                        ...
                    ),
                    maxeval = 1e5,
                    xtol_abs = 1e-20,
                    ftol_abs = 1e-20,
                    restarts = 3
                )

                # Save the result
                saveRDS(
                    result,
                    file.path(
                        "scripts", 
                        "results", 
                        "niels",
                        paste0(
                            names(models)[j], 
                            "__",  
                            optim_configs$name[i], 
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
                        "niels",
                        paste0(
                            names(models)[j], 
                            "__", 
                            optim_configs$name[i], 
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

        return(NULL)
    }
)

cat("\n")

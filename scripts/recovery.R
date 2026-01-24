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
iterations <- 100
N <- 50

# Define the models to use for the recovery study
models <- list(
    "exponential_11" = exponential(d = 1, k = 1),
    "exponential_12" = exponential(d = 1, k = 2),
    "exponential_13" = exponential(d = 1, k = 3),
    "exponential_21" = exponential(d = 2, k = 1),
    "exponential_22" = exponential(d = 2, k = 2),
    "exponential_23" = exponential(d = 2, k = 3),

    "quasi_hyperbolic_11" = quasi_hyperbolic(d = 1, k = 1),
    "quasi_hyperbolic_12" = quasi_hyperbolic(d = 1, k = 2),
    "quasi_hyperbolic_13" = quasi_hyperbolic(d = 1, k = 3),
    "quasi_hyperbolic_21" = quasi_hyperbolic(d = 2, k = 1),
    "quasi_hyperbolic_22" = quasi_hyperbolic(d = 2, k = 2),
    "quasi_hyperbolic_23" = quasi_hyperbolic(d = 2, k = 3),

    "double_exponential_11" = double_exponential(d = 1, k = 1),
    "double_exponential_12" = double_exponential(d = 1, k = 2),
    "double_exponential_13" = double_exponential(d = 1, k = 3),
    "double_exponential_21" = double_exponential(d = 2, k = 1),
    "double_exponential_22" = double_exponential(d = 2, k = 2),
    "double_exponential_23" = double_exponential(d = 2, k = 3)
)

# Define the dynamical and covariance characteristics of these models
characteristics <- cbind(
    rep(c("anisotropic", "isotropic", "symmetric"), each = 2),
    rep(c("isotropic", "symmetric"), times = 3)
)

# Define functions for generating the values of X in the simulation
x_functions <- list(
    \(x) runif(x, min = -2, max = 2),
    \(x) cbind(
        runif(x, min = -2, max = 2),
        runif(x, min = -2, max = 2)
    ),
    \(x) cbind(
        runif(x, min = -2, max = 2),
        runif(x, min = -2, max = 2),
        runif(x, min = -2, max = 2)
    )
)

# Define the different functions that you want to perform on the fitobj
fx <- list(
    "aic" = aic,
    "bic" = bic,
    "autocorrelation" = autocorrelation,
    "bias" = bias
)



################################################################################
# PERFORM THE RECOVERY

# Loop over the different models. Perform this in parallel on Mac and Ubuntu
# and in sequence on Windows
empty <- parallel::mclapply(
    seq_along(models),
    function(i) {
        # Extract the model of choice
        my_model <- models[[i]]

        # Determine which of the functions for X will serve as the one in this 
        # recovery
        Xfun <- x_functions[[as.integer(my_model@k)]]

        # Loop over the different characteristics of the models
        for(j in seq_len(nrow(characteristics))) {
            # Perform the recovery study
            result <- recovery(
                my_model,
                iterations = iterations,
                fx = fx,

                # Simulation characteristics
                Xfun = Xfun,
                N = N,

                # Model characteristics
                dynamics = characteristics[j, 1],
                covariance = characteristics[j, 2],

                # DEoptim characteristics
                itermax = 1000,
                strategy = 6,
                p = 0.5, 
                NP = 200,
                trace = FALSE,

                # Additional stuff
                print_iteration = TRUE,
                print_content = paste(
                    names(models)[i], 
                    ": ",
                    characteristics[j, 1], 
                    " - ",
                    characteristics[j, 2],
                    ": ",
                    sep = ""
                )
            )

            # Save the result
            saveRDS(
                result,
                file.path(
                    "scripts", 
                    "results", 
                    "recovery",
                    paste0(
                        names(models)[i], 
                        "__", 
                        characteristics[j, 1], 
                        "_",
                        characteristics[j, 2], 
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
                        names(models)[i], 
                        "__", 
                        characteristics[j, 1], 
                        "_",
                        characteristics[j, 2], 
                        ".png"
                    )
                ),
                plt,
                width = ceiling(ncol(result$simulate) * 1500 / my_model@d), 
                height = my_model@d * 1550,
                unit = "px",
                limitsize = FALSE
            )
        }

        return(NULL)
    },
    mc.cores = ifelse(
        Sys.info()["sysname"] == "Windows",
        1,
        parallel::detectCores() - 1
    )
)

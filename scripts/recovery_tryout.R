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

# Define the different optimizers to try out and provide them with an informative
# name
optim_configs <- list(
    "neldermead" = list(
        optimizer = "optim",
        method = "Nelder-Mead",
        trace = 0,
        maxit = 1e5,
        abstol = 1e-15,
        reltol = 1e-15
    ),
    "bfgs" = list(
        optimizer = "optim",
        method = "L-BFGS-B",
        trace = 0,
        maxit = 1e5,
        abstol = 1e-15,
        reltol = 1e-15
    ),
    "bobyqa" = list(
        optimizer = "nloptr",
        algorithm = "NLOPT_LN_BOBYQA",
        xtol_abs  = 1e-15,  
        maxeval   = 1e5,   
        ftol_abs  = 1e-15,
        restarts  = 5     
    ),
    "direct" = list(
        optimizer = "nloptr",
        algorithm = "NLOPT_GN_DIRECT",
        xtol_abs  = 1e-15,  
        maxeval   = 1e5,   
        ftol_abs  = 1e-15,
        restarts  = 5      
    ),
    "esch" = list(
        optimizer = "nloptr",
        algorithm = "NLOPT_GN_ESCH",
        xtol_abs  = 1e-15,  
        maxeval   = 1e5,   
        ftol_abs  = 1e-15,
        restarts  = 5      
    ),
    "differential" = list(
        optimizer = "DEoptim",
        itermax = 2500,
        strategy = 6,
        p = 0.5,
        NP = 300,
        CR = 0.6,
        reltol = 1e-5,
        steptol = 200,
        trace = FALSE
    )
)



################################################################################
# PERFORM THE RECOVERY

# Loop over the optimizers and over the different models, performing model 
# estimation in parallel.
empty <- lapply(
    seq_along(optim_configs),
    function(i) {
        # Provide some information on the optimizer used
        cat(names(optim_configs)[i], "\n")

        # Loop over the models
        empty <- parallel::mclapply(
            seq_along(models),
            function(j) {
                # Extract the model of choice
                my_model <- models[[j]]

                # Create a shell function for the recovery, allowing us to 
                # dispatch on the different optimizers. Unfortunately, 
                # unpacking arguments from a list in R is not possible in 
                # a straightforward way, hence this hack.
                shell <- function(...) {
                    return(
                        recovery(
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
                            ...
                        )
                    )
                }

                # Dispatch on the optimizer and run the simulation studies 
                # for that particular optimizer
                if(optim_configs[[i]]$optimizer == "DEoptim") {
                    result <- shell(
                        optimizer = optim_configs[[i]]$optimizer,
                        itermax = optim_configs[[i]]$itermax,
                        strategy = optim_configs[[i]]$strategy,
                        p = optim_configs[[i]]$p,
                        NP = optim_configs[[i]]$NP,
                        CR = optim_configs[[i]]$CR,
                        reltol = optim_configs[[i]]$reltol,
                        steptol = optim_configs[[i]]$steptol,
                        trace = optim_configs[[i]]$trace
                    )
                } else if(optim_configs[[i]]$optimizer == "nloptr") {
                    result <- shell(
                        optimizer = optim_configs[[i]]$optimizer,
                        algorithm = optim_configs[[i]]$algorithm,
                        xtol_abs = optim_configs[[i]]$xtol_abs,  
                        maxeval = optim_configs[[i]]$maxeval,   
                        ftol_abs = optim_configs[[i]]$ftol_abs,
                        restarts = optim_configs[[i]]$restarts
                    )
                } else if(optim_configs[[i]]$optimize == "optim") {
                    result <- shell(
                        optimizer = optim_configs[[i]]$optimizer, 
                        method = optim_configs[[i]]$method,
                        trace = optim_configs[[i]]$trace,
                        maxit = optim_configs[[i]]$maxit,
                        abstol = optim_configs[[i]]$abstol,
                        reltol = optim_configs[[i]]$reltol
                    )
                }

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
                            names(optim_configs)[i], 
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
                            names(optim_configs)[i], 
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

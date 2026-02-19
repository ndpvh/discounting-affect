# Simulate and estimate a model

This function takes in one or two models of a same dimensionality and
simulates data with the first and estimates the second model on those
data. This allows users to perform recovery studies – the topic of which
is to find out how accurately the parameters a model can be estimated –
and to perform distinguishability studies – the topic of which is to
find out how well we can distinguish between one or the other model
based on their fit or some statistics.

## Usage

``` r
recovery(sim_model, ...)

# S4 method for class 'model'
recovery(
  sim_model,
  fit_model = NULL,
  iterations = 100,
  fx = list(),
  X = NULL,
  Xfun = NULL,
  N = NULL,
  dynamics = "isotropic",
  covariance = "symmetric",
  sim_dynamics = dynamics,
  sim_covariance = covariance,
  fit_dynamics = dynamics,
  fit_covariance = covariance,
  print_iteration = TRUE,
  print_content = "",
  ...
)
```

## Arguments

- sim_model:

  An instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)
  with which the data will be generated/simulated. Note that this model
  can be empty: Parameters will be simulated automatically within this
  function.

- ...:

  Arguments passed on to
  [`fit`](https://github.com/ndpvh/discounting-affect/reference/fit.md).

- fit_model:

  An instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)
  that will be estimated on the simulated data. Note that this model can
  be empty, but should have a same dimensionality (that is, `d` and `k`)
  as `sim_model`. By default, `fit_model` is equal to `sim_model`

- iterations:

  Integer denoting the number of iterations to run for the
  recovery/distinguishability study. Defaults to `100`.

- fx:

  Named list of functions that take the output of the estimation
  procedure and summarize it in a way. The function should only take in
  a single argument, it being the output
  of[`fit`](https://github.com/ndpvh/discounting-affect/reference/fit.md),
  and should return a single value. Examples of functions that may be
  useful to include are estimates of \\AIC\\ and \\BIC\\. Defaults to an
  empty list.

- X:

  Numeric vector or numeric matrix containing the values of the
  predictor variable(s). If a numeric vector, the dimension `k` of the
  predictor variables within `object` should be equal to `1`. If a
  numeric matrix, the dimension `k` of the predictor variables within
  `object` should be equal to the number of columns provided to this
  argument. The length of the provided vector or the number of rows of
  the provided matrix will determine the number of simulated datapoints.
  Defaults to `NULL`. If provided, it overrides the value of `Xfun`.

- Xfun:

  Function or a list of functions that define how the values of the
  predictor variable(s) should be generated. The functions should take
  in a single argument `N` defining the number of values generated for
  this predictor variable, and hence the number of datapoints that
  should be generated for the simulation. If a list, then the result of
  each function in the list will be bound together in a matrix. Defaults
  to `NULL`.

- N:

  Number of datapoints to simulate. Passed on to `Xfun` when defined.
  Defaults to `NULL`.

- dynamics, sim_dynamics, fit_dynamics:

  Character denoting the structure of the dynamic parameters of the
  models. See
  [`fill`](https://github.com/ndpvh/discounting-affect/reference/fill.md)
  for guidance on their potential values. By default, both the
  simulation and fitting model are `"isotropic"`.

- covariance, sim_covariance, fit_covariance:

  Character denoting the structure of the covariance matrix for the
  models. See
  [`fill`](https://github.com/ndpvh/discounting-affect/reference/fill.md)
  for guidance on their potential values. By default, both the
  simulation and fitting model are `"symmetric"`.

- print_iteration:

  Logical denoting whether to print the iteration of the recovery at
  this moment. Defaults to `TRUE`.

- print_content:

  Character containing information that you would wish to print
  alongside the iteration. Gets printed before the iteration itself.
  Defaults to an empty string.

## Value

Named list containing a `data.frame` with simulated parameters (under
`"simulate"`) and a `data.frame` with estimated parameters, the result
of the objective function, and other statistics that users can compute
based on the output of
[`fit`](https://github.com/ndpvh/discounting-affect/reference/fit.md).

## Examples

``` r
# Create a study in which we simulate from a slightly more complicated model
# and estimate a simpler model, both based on the exponential discounting 
# model
recovery(
  exponential(d = 1, k = 1),

  # Specifications of the recovery
  iterations = 10,
  fx = list(
    "aic" = function(x) 
      (length(x$residuals) * log(x$objective) / length(x$residuals) + 2 * length(x$parameters))
  ),

  # Arguments for simulate
  Xfun = \(x) rnorm(x),
  N = 25, 
  sim_dynamics = "symmetric",
  sim_covariance = "symmetric",

  # Arguments for fit
  fit_dynamics = "isotropic",
  fit_covariance = "isotropic",
  itermax = 25,
  trace = FALSE
)
#>   1                      2                      3                      4                      5                      6                      7                      8                      9                      10                    
#> $simulate
#>       alpha_1    beta_11  gamma_11  sigma_11
#> 1   0.8760734  3.8307686 0.3581289 0.2581185
#> 2  -0.5281600  0.8142939 0.4048979 0.9424444
#> 3  -0.5172888 -0.6439639 0.3906836 0.4817135
#> 4  -0.8763094  0.6438051 0.6301719 0.5038121
#> 5   0.3940185  0.9882011 0.8252085 0.7519971
#> 6   0.2080263  4.0861255 0.3155878 0.4659547
#> 7  -0.4150512  4.3005771 0.9098218 0.2431121
#> 8  -0.8342010 -1.0503619 0.5048869 0.4122052
#> 9  -0.1309133  2.1095289 0.8906557 0.1098664
#> 10  0.2521955  2.4983026 0.7896063 0.4678484
#> 
#> $fit
#>       alpha_1    beta_11     gamma_11   sigma_11  objective       aic
#> 1   0.9382257  3.8556543 0.3738391030 0.05889204  1.4185421  8.349630
#> 2  -0.2652526  0.9690627 0.0001170678 0.57225718 13.7360627 10.620025
#> 3  -0.3736402 -0.6179490 0.2524011815 0.22819428  5.4769759  9.700553
#> 4  -0.9018636  0.5241077 0.6963197922 0.17301950  4.1526249  9.423741
#> 5   0.2379365  0.8568246 0.8611713276 0.50366316 12.1419237 10.496664
#> 6   0.2155970  4.2670229 0.2746007011 0.17350409  4.1654603  9.426827
#> 7  -0.4595417  4.3025335 0.9034839578 0.04574329  1.1117774  8.105960
#> 8  -0.7968895 -1.0641660 0.5042218516 0.09117591  2.1896802  8.783755
#> 9  -0.2455765  2.0864679 0.8807711501 0.01366580  0.3279812  6.885201
#> 10  0.5227139  2.4568096 0.8043898443 0.22579513  5.4200274  9.690101
#> 
```

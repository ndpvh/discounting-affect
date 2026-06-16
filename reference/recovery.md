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
#>   1                    
#> Warning: unknown names in control: itermax
#>   2                    
#> Warning: unknown names in control: itermax
#>   3                    
#> Warning: unknown names in control: itermax
#>   4                    
#> Warning: unknown names in control: itermax
#>   5                    
#> Warning: unknown names in control: itermax
#>   6                    
#> Warning: unknown names in control: itermax
#>   7                    
#> Warning: unknown names in control: itermax
#>   8                    
#> Warning: unknown names in control: itermax
#>   9                    
#> Warning: unknown names in control: itermax
#>   10                    
#> Warning: unknown names in control: itermax
#> $simulate
#>        alpha_1    beta_11   gamma_11    sigma_11
#> 1   0.24504259 -4.5293000 0.88412701 0.097578675
#> 2  -0.04115010  2.3078453 0.06877967 0.110370136
#> 3   0.11051237 -0.6077674 0.47360821 0.372355005
#> 4   0.77387758 -3.5436484 0.32568980 0.578440698
#> 5  -0.64474701 -4.7579667 0.48087282 0.100249275
#> 6   0.05417916  3.2076547 0.30601955 0.467287591
#> 7  -0.91684856 -1.0161143 0.72917722 0.089710025
#> 8   0.14873823  0.5490381 0.94520573 0.003740364
#> 9   0.58508225  2.3261442 0.42377234 0.064595671
#> 10 -0.89136071  1.8740881 0.61532261 0.689122244
#> 
#> $fit
#>       alpha_1    beta_11  gamma_11     sigma_11    objective       aic
#> 1   0.9999971 -1.3311063 1.0000000 12.226183908 353.48895435 13.867852
#> 2  -0.9999996 -0.3183056 1.0000000  6.097348245 147.01806184 12.990555
#> 3   0.1450451 -0.6877502 0.5863918  0.262179160   6.29229987  9.839327
#> 4   0.9711924 -3.6113141 0.3586619  0.422365176  10.13676423 10.316169
#> 5  -0.4754435 -4.8501902 0.4980926  0.070943992   1.70265756  8.532190
#> 6  -0.1245574  3.0965876 0.3462607  0.582538128  13.98092087 10.637694
#> 7  -0.9203495 -1.0792367 0.7196410  0.115101136   2.76242777  9.016110
#> 8   0.1591878  0.5372447 0.9480032  0.001345411   0.03229047  4.567017
#> 9   0.5738375  2.3408674 0.3955073  0.064092024   1.53820886  8.430619
#> 10 -0.8216340  1.9955956 0.5982129  0.760980921  18.26354225 10.904907
#> 
```

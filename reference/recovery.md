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
#>        alpha_1   beta_11   gamma_11   sigma_11
#> 1   0.04718214  1.991339 0.95672699 0.01756332
#> 2  -0.81088930  1.426539 0.55630113 0.67094691
#> 3  -0.69590675 -4.515063 0.68234734 0.92588219
#> 4  -0.57706106  1.546639 0.02898173 0.25566434
#> 5   0.88520977  4.223072 0.59879785 0.69701228
#> 6   0.03722637  1.040132 0.90861255 0.31559462
#> 7  -0.41505119  4.300577 0.90982185 0.24311205
#> 8  -0.83420104 -1.050362 0.50488693 0.41220518
#> 9  -0.13091330  2.109529 0.89065568 0.10986635
#> 10  0.25219549  2.498303 0.78960627 0.46784837
#> 
#> $fit
#>        alpha_1   beta_11   gamma_11    sigma_11  objective       aic
#> 1   0.03802146  2.032352 0.95288325 0.006493631  0.2192525  6.482469
#> 2  -0.98023331  1.507981 0.20281709 0.192164784  4.6219609  9.530819
#> 3  -0.48831415 -4.353223 0.64153970 0.856432308 20.5545752 11.023084
#> 4  -0.54609764  1.560876 0.06682918 0.063965546  1.5352623  8.428701
#> 5   0.83323455  4.294115 0.60032750 0.427434362 10.2595347 10.328207
#> 6  -0.02916657  1.002506 0.92813084 0.197082742  4.7319374  9.554335
#> 7  -0.45954171  4.302533 0.90348396 0.045743294  1.1117774  8.105960
#> 8  -0.79688947 -1.064166 0.50422185 0.091175908  2.1896802  8.783755
#> 9  -0.24557647  2.086468 0.88077115 0.013665804  0.3279812  6.885201
#> 10  0.52271390  2.456810 0.80438984 0.225795128  5.4200274  9.690101
#> 
```

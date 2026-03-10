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
#>       alpha_1    beta_11   gamma_11    sigma_11
#> 1   0.4715527  3.1861283 0.70342856 0.451959106
#> 2   0.4706135 -4.0485906 0.50323421 0.199733363
#> 3  -0.1586548  3.0108573 0.30630835 0.108226817
#> 4   0.2347165 -0.1734592 0.65826279 0.203764741
#> 5  -0.4012301 -0.1754471 0.25102123 0.088960862
#> 6   0.3600013  4.8036742 0.09574083 0.332384532
#> 7   0.7962199 -0.1401581 0.12251734 0.511899278
#> 8  -0.2499864 -0.9137039 0.92181620 0.033673933
#> 9   0.3364183 -2.8222423 0.76708603 0.160654964
#> 10 -0.1335895 -3.3804679 0.97187678 0.001209376
#> 
#> $fit
#>        alpha_1     beta_11   gamma_11    sigma_11  objective       aic
#> 1   0.20487963  3.11348199 0.68338876 0.479640517 11.5137507 10.443542
#> 2   0.43132126 -4.03493187 0.52273957 0.117928463  2.8303376  9.040396
#> 3  -0.18672973  3.09815172 0.28191586 0.059214629  1.4212760  8.351555
#> 4   0.08981146 -0.18551867 0.08095869 0.198867773  4.7730269  9.562981
#> 5  -0.45244164 -0.09927568 0.53203206 0.087006474  2.0881825  8.736294
#> 6   0.31620870  4.80718607 0.07900896 0.267055858  6.4138304  9.858457
#> 7   0.47667511 -0.44879921 0.83048167 0.201094445  4.8263266  9.574086
#> 8  -0.18304491 -0.93973351 0.92324106 0.018648143  0.4475790  7.196098
#> 9   0.33160033 -2.92209502 0.78008448 0.077054960  1.8502874  8.615341
#> 10 -0.22157813 -3.43229871 0.97225359 0.006102538  0.1777408  6.272571
#> 
```

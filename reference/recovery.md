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
#>        alpha_1     beta_11  gamma_11     sigma_11
#> 1   0.39961673  3.89279018 0.9083681 0.7384021622
#> 2  -0.22535176  0.95170925 0.2108602 0.0002859343
#> 3   0.50682420  0.15316330 0.6139433 0.0028895751
#> 4  -0.24704935  2.02464690 0.1540385 0.2440936929
#> 5  -0.05358182  0.01299776 0.8932511 0.5602410513
#> 6   0.46037303 -0.88649520 0.9310864 0.0099222712
#> 7  -0.97101975 -0.48783158 0.4508022 0.2223753060
#> 8   0.75541625  1.10746058 0.5687281 0.0005180080
#> 9  -0.09714792 -4.30053638 0.5374790 0.2116217283
#> 10 -0.88885888  3.29686953 0.3124058 0.0646705223
#> 
#> $fit
#>       alpha_1    beta_11     gamma_11     sigma_11    objective       aic
#> 1  -0.4387179  3.8862313 9.155605e-01 1.1652261906 29.971532790 11.400248
#> 2  -0.2278729  0.9509414 2.114238e-01 0.0001408057  0.003381222  2.310482
#> 3   0.4869767  0.1662712 6.283858e-01 0.0023418754  0.056207809  5.121300
#> 4  -0.2491267  1.9200839 1.674713e-01 0.1703549824  4.088519661  9.408183
#> 5  -0.4108731  0.1012967 3.487454e-07 0.4486237141 10.766969301 10.376483
#> 6   0.4712374 -0.8798346 9.324555e-01 0.0099327395  0.238385807  6.566135
#> 7  -0.9513746 -0.4495895 5.340918e-01 0.2064402293  4.954568388  9.600310
#> 8   0.7521321  1.1068054 5.705154e-01 0.0003682879  0.008841252  3.271673
#> 9  -0.1382401 -4.2046340 5.481744e-01 0.2008153568  4.819568812  9.572684
#> 10 -0.8444252  3.3719362 3.314985e-01 0.0525338001  1.260811285  8.231755
#> 
```

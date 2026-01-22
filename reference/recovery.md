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
  sim_dynamics = "isotropic",
  sim_covariance = "symmetric",
  fit_dynamics = "isotropic",
  fit_covariance = "symmetric",
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

- sim_dynamics, fit_dynamics:

  Character denoting the structure of the dynamic parameters of the
  models. See
  [`fill`](https://github.com/ndpvh/discounting-affect/reference/fill.md)
  for guidance on their potential values.

- sim_covariance, fit_covariance:

  Character denoting the structure of the covariance matrix for the
  models. See
  [`fill`](https://github.com/ndpvh/discounting-affect/reference/fill.md)
  for guidance on their potential values.

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
#> $simulate
#>       alpha_1    beta_11  gamma_11  sigma_11
#> 1   0.8569972 -3.4713281 0.3856933 0.7613589
#> 2   0.6521350 -2.1040805 0.3951991 0.7686621
#> 3   0.9726840 -1.0776891 0.1876451 0.9299054
#> 4  -0.8299362 -2.6235053 0.3315630 0.4635610
#> 5  -0.2605241 -2.2082261 0.3500239 0.3059435
#> 6   0.3009353 -0.6631639 0.3738693 0.2184910
#> 7   0.0882654 -1.9570082 0.5534691 0.2898066
#> 8  -0.1595766 -4.3435898 0.9757551 0.9275211
#> 9  -0.5922077 -3.0689218 0.8292778 0.0121203
#> 10  0.3658308  2.5180615 0.4250825 0.9416419
#> 
#> $fit
#>        alpha_1    beta_11  gamma_11   sigma_11 objective       aic
#> 1   0.82515333 -3.1871739 0.4273836 0.61662205 14.798933 10.694555
#> 2   0.39358489 -2.0797859 0.4903053 0.63099627 15.149367 10.717959
#> 3   0.99811203 -1.1704834 0.4362487 1.32169186 32.214797 11.472426
#> 4  -0.94982943 -2.3933690 0.2308495 0.63052586 15.132648 10.716855
#> 5  -0.06182098 -2.1407326 0.3784976 0.28475363  6.836606  9.922291
#> 6   0.34663569 -0.6688359 0.1688776 0.20502626  4.922150  9.593745
#> 7   0.07885944 -1.9596535 0.5435221 0.24347692  5.843476  9.765326
#> 8  -0.49383948 -4.4309722 0.9735560 1.48424461 35.989756 11.583234
#> 9  -0.60246529 -3.0956377 0.8290827 0.01698464  0.415764  7.122363
#> 10  0.17673488  2.5706500 0.4448343 1.15802805 27.793059 11.324786
#> 
```

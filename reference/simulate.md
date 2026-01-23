# Method for simulating from a model

Takes in an instance of the
[`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)
and simulates data according to this model and the specifications of the
user. Depends heavily on the trajectories created by
[`predict`](https://rdrr.io/r/stats/predict.html).

## Usage

``` r
# S4 method for class 'model'
simulate(object, X = NULL, Xfun = NULL, N = NULL)
```

## Arguments

- object:

  An instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)
  defining the model

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

## Value

An instance of the
[`dataset-class`](https://github.com/ndpvh/discounting-affect/reference/dataset-class.md)
containing the simulated data.

## Examples

``` r
# Define the model
my_model <- exponential(
  parameters = list(
    "alpha" = numeric(2), 
    "beta" = matrix(1:2, ncol = 1),
    "gamma" = diag(2) * 0.75
  ),
  covariance = diag(2)
)

# Simulate data for this model using a function for generating X
simulate(
  my_model,
  Xfun = \(x) rnorm(x, mean = 0, sd = 1),
  N = 10
)
#> An object of class "dataset"
#> 
#> Slot "Y": 10x2matrix
#>             [,1]       [,2]
#> [1,] -1.11909204 -1.3169212
#> [2,]  0.08248530  1.7871303
#> [3,]  0.77508338  1.6217721
#> [4,] -0.08796889  2.2511720
#> [5,]  2.41179640  3.8473506
#> [6,]  0.69904290  0.5444769
#> 
#> Slot "X": 10x1matrix
#>            [,1]
#> [1,] -0.8524557
#> [2,]  1.3995634
#> [3,]  0.3742660
#> [4,]  0.3344005
#> [5,]  0.8339270
#> [6,] -0.5364249

# Simulate data for this model using predefined values of X
simulate(
  my_model,
  X = rnorm(10, mean = 0, sd = 1)
)
#> An object of class "dataset"
#> 
#> Slot "Y": 10x2matrix
#>            [,1]         [,2]
#> [1,] -0.1471925 -0.569195354
#> [2,] -1.4441886 -2.636290577
#> [3,] -2.6968305 -1.207971628
#> [4,] -1.3871766 -1.057587021
#> [5,] -1.2953789 -0.381788406
#> [6,] -0.6191894 -0.002896544
#> 
#> Slot "X": 10x1matrix
#>             [,1]
#> [1,]  0.03244136
#> [2,] -1.36883151
#> [3,] -0.44671143
#> [4,]  0.65788949
#> [5,] -0.10401469
#> [6,] -0.60792126
```

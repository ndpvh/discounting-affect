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
#>            [,1]       [,2]
#> [1,] -1.0599854  0.8274374
#> [2,] -1.0493135 -1.2536097
#> [3,]  0.5486790 -1.6941302
#> [4,] -0.8114646  0.5033577
#> [5,]  0.7658832  0.6928226
#> [6,]  0.1819412  0.8833535
#> 
#> Slot "X": 10x1matrix
#>             [,1]
#> [1,]  0.04020439
#> [2,]  0.12430107
#> [3,] -0.99843255
#> [4,]  1.23339006
#> [5,]  0.34042449
#> [6,] -0.47270248

# Simulate data for this model using predefined values of X
simulate(
  my_model,
  X = rnorm(10, mean = 0, sd = 1)
)
#> An object of class "dataset"
#> 
#> Slot "Y": 10x2matrix
#>            [,1]       [,2]
#> [1,] -1.0757810 -2.3975857
#> [2,] -1.7592490 -3.5061445
#> [3,]  0.2127395 -0.4650887
#> [4,] -2.5408781 -3.4284209
#> [5,] -2.3663328  1.4782080
#> [6,] -1.3454502 -2.2395971
#> 
#> Slot "X": 10x1matrix
#>            [,1]
#> [1,] -1.1936412
#> [2,] -0.7517233
#> [3,]  1.4558414
#> [4,] -0.8286035
#> [5,]  0.2897745
#> [6,] -0.4800535
```

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
#> [1,] -1.2540513 -1.0351236
#> [2,]  1.3694621  2.0786340
#> [3,]  3.2795182  3.6112186
#> [4,] -0.5659642  0.3919145
#> [5,]  1.0802777  6.4057205
#> [6,]  1.0892322  0.8138160
#> 
#> Slot "X": 10x1matrix
#>            [,1]
#> [1,] -0.8728982
#> [2,]  1.3062695
#> [3,]  1.5826983
#> [4,] -1.0490193
#> [5,]  2.1506223
#> [6,] -1.1632103

# Simulate data for this model using predefined values of X
simulate(
  my_model,
  X = rnorm(10, mean = 0, sd = 1)
)
#> An object of class "dataset"
#> 
#> Slot "Y": 10x2matrix
#>            [,1]      [,2]
#> [1,] -1.5638218 -2.187796
#> [2,]  0.2202227 -4.641437
#> [3,] -0.3778339 -1.901815
#> [4,]  0.3780508 -4.299934
#> [5,] -2.6263028 -4.035466
#> [6,] -1.9244089 -2.465406
#> 
#> Slot "X": 10x1matrix
#>            [,1]
#> [1,] -0.9649521
#> [2,]  0.2424992
#> [3,] -0.8151621
#> [4,] -0.5942290
#> [5,] -0.8008680
#> [6,] -0.6248165
```

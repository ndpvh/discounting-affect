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
#>            [,1]     [,2]
#> [1,]  0.2378517 1.614348
#> [2,]  1.0884055 4.369936
#> [3,]  0.7749635 2.595879
#> [4,]  4.3489004 6.716216
#> [5,]  2.5457267 2.972424
#> [6,] -1.5108045 1.611866
#> 
#> Slot "X": 10x1matrix
#>            [,1]
#> [1,]  0.8275447
#> [2,]  1.1349904
#> [3,]  0.5622791
#> [4,]  1.7901876
#> [5,] -0.3068802
#> [6,] -0.8414098

# Simulate data for this model using predefined values of X
simulate(
  my_model,
  X = rnorm(10, mean = 0, sd = 1)
)
#> An object of class "dataset"
#> 
#> Slot "Y": 10x2matrix
#>           [,1]      [,2]
#> [1,] 2.0292070 0.3530041
#> [2,] 2.0084995 0.6669948
#> [3,] 3.2295838 0.6473454
#> [4,] 0.8492488 2.7828088
#> [5,] 1.2279241 3.1579585
#> [6,] 1.5664292 3.6794258
#> 
#> Slot "X": 10x1matrix
#>            [,1]
#> [1,] 0.12718068
#> [2,] 0.16085201
#> [3,] 0.56157138
#> [4,] 0.07577947
#> [5,] 1.22380198
#> [6,] 0.57734598
```

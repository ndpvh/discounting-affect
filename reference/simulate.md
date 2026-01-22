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
#> [1,] -0.6704978  0.8727471
#> [2,] -2.8427188 -0.6215433
#> [3,] -2.3079674 -3.8811488
#> [4,] -1.6268003 -1.4854638
#> [5,] -0.2316777  0.4952942
#> [6,]  1.2507985  2.4861084
#> 
#> Slot "X": 10x1matrix
#>            [,1]
#> [1,] -0.0788349
#> [2,] -0.4993008
#> [3,] -1.0351772
#> [4,]  0.4794212
#> [5,]  0.8699730
#> [6,]  0.2334889

# Simulate data for this model using predefined values of X
simulate(
  my_model,
  X = rnorm(10, mean = 0, sd = 1)
)
#> An object of class "dataset"
#> 
#> Slot "Y": 10x2matrix
#>             [,1]       [,2]
#> [1,] -0.49097764 -0.8420040
#> [2,] -1.10957639 -2.1943937
#> [3,] -1.99275953 -0.8371423
#> [4,] -0.25561840 -1.2050228
#> [5,] -0.09080193  0.2087370
#> [6,] -0.96077198 -2.0107322
#> 
#> Slot "X": 10x1matrix
#>            [,1]
#> [1,] -0.3770563
#> [2,] -0.8097119
#> [3,] -0.6330054
#> [4,]  0.3203433
#> [5,]  0.5960723
#> [6,] -1.3950491
```

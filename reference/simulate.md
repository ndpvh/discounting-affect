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
#>            [,1]      [,2]
#> [1,] -0.9669476 3.4667576
#> [2,]  0.1536131 1.2783633
#> [3,]  0.5433868 1.5715708
#> [4,]  0.6926003 2.0545968
#> [5,] -0.3176507 0.9168740
#> [6,]  0.4277141 0.2522899
#> 
#> Slot "X": 10x1matrix
#>            [,1]
#> [1,]  1.4377441
#> [2,]  0.1788962
#> [3,] -0.9261600
#> [4,]  1.1496064
#> [5,] -0.8341172
#> [6,]  0.2418911

# Simulate data for this model using predefined values of X
simulate(
  my_model,
  X = rnorm(10, mean = 0, sd = 1)
)
#> An object of class "dataset"
#> 
#> Slot "Y": 10x2matrix
#>            [,1]     [,2]
#> [1,]  1.9408751 2.978394
#> [2,]  2.6127720 4.503331
#> [3,]  4.5100836 3.829914
#> [4,]  2.1522162 4.515396
#> [5,]  1.0570680 1.014076
#> [6,] -0.2310961 1.438478
#> 
#> Slot "X": 10x1matrix
#>            [,1]
#> [1,]  1.6130170
#> [2,]  0.9966790
#> [3,]  0.9731979
#> [4,]  0.2221123
#> [5,] -0.8977707
#> [6,]  0.3158777
```

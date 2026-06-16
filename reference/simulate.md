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
#> [1,] -0.4183891  1.6218753
#> [2,]  0.6062376  1.3758322
#> [3,]  0.3225780 -1.7048300
#> [4,] -0.5562047  2.0146343
#> [5,]  1.7295471 -0.8439367
#> [6,] -1.2382489 -4.1150333
#> 
#> Slot "X": 10x1matrix
#>             [,1]
#> [1,]  0.39709527
#> [2,]  0.52502528
#> [3,] -0.45748143
#> [4,] -0.04519345
#> [5,] -0.77066745
#> [6,] -1.02764037

# Simulate data for this model using predefined values of X
simulate(
  my_model,
  X = rnorm(10, mean = 0, sd = 1)
)
#> An object of class "dataset"
#> 
#> Slot "Y": 10x2matrix
#>            [,1]        [,2]
#> [1,]  0.6366360 -0.06775621
#> [2,] -0.3994301 -3.09453328
#> [3,] -1.0169735 -4.61215726
#> [4,] -3.3056081 -8.84031121
#> [5,] -2.4797973 -4.67011527
#> [6,] -2.5326032 -1.26968669
#> 
#> Slot "X": 10x1matrix
#>            [,1]
#> [1,]  0.1770301
#> [2,] -1.2027317
#> [3,] -0.9962892
#> [4,] -2.4418848
#> [5,]  0.3166672
#> [6,]  0.4130898
```

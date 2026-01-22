# Compute the model expectations

Within this package, the `predict` method computes the expectations of a
particular model provided to `object` using the data provided to `data`.
It plays a pivotal role in both the estimation and simulation of the
models provided in this package.

## Usage

``` r
# S4 method for class 'exponential'
predict(object, data)

# S4 method for class 'quasi_hyperbolic'
predict(object, data)

# S4 method for class 'double_exponential'
predict(object, data)
```

## Arguments

- object:

  Instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)

- data:

  Instance of the
  [`dataset-class`](https://github.com/ndpvh/discounting-affect/reference/dataset-class.md)

## Value

Instance of the
[`dataset-class`](https://github.com/ndpvh/discounting-affect/reference/dataset-class.md)
where the slot `y` contains the predictions of the model

## Examples

``` r
# Create an exponential discounting model
my_model <- exponential(
  parameters = list(
    "alpha" = 0,
    "beta" = 1,
    "gamma" = 0.5
  ),
  covariance = 1
)
#> Warning: The parameter "gamma" should be a matrix: Changing type.
#> Warning: The parameter "beta" should be a matrix: Changing type assuming a single independent variable.
#> Warning: The argument "covariance" should be a matrix: Changing type.

# Create an instance of the dataset with only predictor values. It will throw
# a warning because Y is implied to be empty, but we capture this warning 
# with suppressWarnings()
data <- dataset(
  X = matrix(
    rnorm(10),
    nrow = 10,
    ncol = 1
  )
) |>
  suppressWarnings()

# Compute the values of Y as expected by the exponential discounting model 
# defined in my_model
predict(
  my_model,
  data
)
#> An object of class "dataset"
#> 
#> Slot "Y": 10x1matrix
#>             [,1]
#> [1,] -1.15339839
#> [2,] -0.91733707
#> [3,]  0.32769404
#> [4,] -1.10666609
#> [5,] -0.01119150
#> [6,]  0.06951015
#> 
#> Slot "X": 10x1matrix
#>            [,1]
#> [1,] -1.1533984
#> [2,] -0.3406379
#> [3,]  0.7863626
#> [4,] -1.2705131
#> [5,]  0.5421415
#> [6,]  0.0751059
```

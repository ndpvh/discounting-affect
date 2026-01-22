# Constructor for the [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)

Constructor for the
[`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)

## Usage

``` r
model(
  d = 1,
  k = 0,
  n = NA,
  parameters = list(),
  covariance = matrix(0, nrow = d, ncol = d)
)
```

## Arguments

- d:

  Integer denoting the number of dimensions of the model. Defaults to
  `1`.

- k:

  Integer denoting the number of independent variables. Defaults to `0`.

- n:

  Integer denoting the number of parameters in the model. Defaults to
  `NA`, in which case the number of parameters in the model will be
  counted.

- parameters:

  List containing the parameters relevant to a particular model
  instance. Defaults to an empty list, meaning the model cannot be
  properly used.

- covariance:

  Numeric matrix denoting the residual covariance of the model. Default
  to a matrix of `0`s with the dimensionality implied by `d`

## Value

Instance of
[`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)

## See also

[`exponential-class`](https://github.com/ndpvh/discounting-affect/reference/exponential-class.md)

## Examples

``` r
model(
  d = 2,
  k = 2,
  parameters = list(
    "delta" = numeric(2),
    "theta" = diag(2)
  ),
  covariance = diag(2)
)
#> Model of class "model":
#> 
#> Dimension: 2
#> Number of predictors: 2
#> Number of parameters: 9
#> 
#> Parameters:
#>   delta: |  0.00  |
#>          |  0.00  |
#> 
#>   theta: | 1.00  0.00 |
#>          | 0.00  1.00 |
#> 
#> 
#> Covariance: | 1.00  0.00 |
#>             | 0.00  1.00 |
```

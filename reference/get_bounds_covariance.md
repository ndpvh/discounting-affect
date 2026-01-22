# Get bounds for the parameters in the covariance matrix

Get bounds for the parameters in the covariance matrix

## Usage

``` r
get_bounds_covariance(d, lower, upper, covariance = "symmetric")
```

## Arguments

- d:

  Integer denoting the dimensionality of the model.

- lower, upper:

  Numeric value containing the value of the lower and upper bounds for
  the covariance matrix.

- covariance:

  Character denoting the structure of the covariance matrix. Can either
  be `"symmetric"` (symmetric around the diagonal) or `"isotropic"`
  (diagonal). Defaults to `"symmetric"`.

## Value

List containing two numeric vectors of lower and upper bounds for the
covariance matrix in that order.

## Examples

``` r
get_bounds_covariance(
  2, 
  0,
  1,
  covariance = "symmetric"
)
#> [[1]]
#> [1] 0 0 0
#> 
#> [[2]]
#> [1] 1 1 1
#> 

get_bounds_covariance(
  2,
  0, 
  1,
  covariance = "isotropic"
)
#> [[1]]
#> [1] 0 0
#> 
#> [[2]]
#> [1] 1 1
#> 
```

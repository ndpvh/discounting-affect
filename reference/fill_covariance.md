# Fill the covariance matrix with values

Fill the covariance matrix with values

## Usage

``` r
fill_covariance(d, parameters, covariance = "symmetric")
```

## Arguments

- d:

  Integer denoting the dimensionality of the model.

- parameters:

  Numeric vector containing the values of the parameters that should be
  assigned to the model. Importantly, the number of parameters within
  this vector should correspond exactly to the number of parameters
  needed by the model. Ideally, you let functions internal to this
  package handle this for you, rather than you defining these parameters
  manually.

- covariance:

  Character denoting the structure of the covariance matrix. Can either
  be `"symmetric"` (symmetric around the diagonal) or `"isotropic"`
  (diagonal). Defaults to `"symmetric"`.

## Value

Matrix filled with the specified values.

## Examples

``` r
fill_covariance(
  2,
  1:3,
  covariance = "symmetric"
)
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    2    3

fill_covariance(
  2,
  1:2,
  covariance = "isotropic"
)
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    2
```

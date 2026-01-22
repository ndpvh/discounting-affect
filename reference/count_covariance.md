# Count the number of parameters in the covariance matrix

Count the number of parameters in the covariance matrix

## Usage

``` r
count_covariance(d, covariance = "symmetric")
```

## Arguments

- d:

  Integer denoting the dimensionality of the model.

- covariance:

  Character denoting the structure of the covariance matrix. Can either
  be `"symmetric"` (symmetric around the diagonal) or `"isotropic"`
  (diagonal). Defaults to `"symmetric"`.

## Value

Integer denoting the number of parameters in the covariance matrix under
the specified conditions.

## Examples

``` r
count_covariance(2, "symmetric")
#> [1] 3
count_covariance(2, "isotropic")
#> [1] 2
```

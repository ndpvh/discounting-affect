# Get the names for the parameters in the covariance matrix

Get the names for the parameters in the covariance matrix

## Usage

``` r
parameter_names_covariance(d, covariance = "symmetric")
```

## Arguments

- d:

  Integer denoting the dimensionality of the model.

- covariance:

  Character denoting the structure of the covariance matrix. Can either
  be `"symmetric"` (symmetric around the diagonal) or `"isotropic"`
  (diagonal). Defaults to `"symmetric"`.

## Value

Character vector containing the names for all parameters in the
covariance matrix. The order corresponds to the assumptions of
[`get_bounds`](https://github.com/ndpvh/discounting-affect/reference/get_bounds.md)
and
[`fill`](https://github.com/ndpvh/discounting-affect/reference/fill.md).

## Examples

``` r
parameter_names_covariance(2, covariance = "symmetric")
#> [1] "sigma_11" "sigma_21" "sigma_22"

parameter_names_covariance(2, covariance = "isotropic")
#> [1] "sigma_11" "sigma_22"
```

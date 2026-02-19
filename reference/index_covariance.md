# Index the covariance matrix

Index the covariance matrix

## Usage

``` r
index_covariance(d, start, covariance = "symmetric", cholesky = TRUE)
```

## Arguments

- d:

  Integer denoting the dimensionality of the model.

- start:

  Integer denoting the index at which to start the indexing of the
  covariance matrix, so that it is the first value of the index.

- covariance:

  Character denoting the structure of the covariance matrix. Can either
  be `"symmetric"` (symmetric around the diagonal) or `"isotropic"`
  (diagonal). Defaults to `"symmetric"`.

- cholesky:

  Logical denoting whether the idea is to use the Cholesky decomposition
  to create the values of the covariance matrix. In this case, the
  indices should only span the lower-triangular of the matrix. Defaults
  to `TRUE`.

## Value

Indexed covariance matrix.

## Examples

``` r
index_covariance(
  2,
  6,
  covariance = "symmetric",
  cholesky = FALSE
)
#>      [,1] [,2]
#> [1,]    6    7
#> [2,]    7    8

index_covariance(
  2,
  6,
  covariance = "isotropic",
  cholesky = FALSE
)
#>      [,1] [,2]
#> [1,]    6    0
#> [2,]    0    7
```

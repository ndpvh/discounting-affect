# Index model parameters

Fill the model parameters with indices denoting which values to use for
a particular model.

## Usage

``` r
index(model, ...)

# S4 method for class 'exponential'
index(
  model,
  dynamics = "isotropic",
  parameters_only = TRUE,
  full = TRUE,
  cholesky = TRUE,
  ...
)

# S4 method for class 'quasi_hyperbolic'
index(
  model,
  dynamics = "isotropic",
  parameters_only = TRUE,
  full = TRUE,
  cholesky = TRUE,
  ...
)

# S4 method for class 'double_exponential'
index(
  model,
  dynamics = "isotropic",
  parameters_only = TRUE,
  full = TRUE,
  cholesky = TRUE,
  ...
)
```

## Arguments

- model:

  Instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)

- ...:

  Additional arguments passed on to the methods.

- dynamics:

  Character denoting the structure of the dynamical matrices. Can either
  be `"anisotropic"` (completely free), `"symmetric"` (symmetric around
  the diagonal), and `"isotropic"` (diagonal). Note that this influences
  different parameters for different models, namely \\\Gamma\\ for the
  exponential discounting model, \\N\\ and \\K\\ for the
  quasi-hyperbolic discounting model, and \\\Gamma\\ and \\N\\ for the
  double-exponential discounting model. Defaults to `"isotropic"`.

- parameters_only:

  Logical denoting whether to only fill the parameters in de `parameter`
  slot of the model (`TRUE`), or to fill the covariance matrix as well
  (`FALSE`). Defaults to `TRUE`.

- full:

  Logical denoting whether to provide the full matrices or whether a
  partial fill is sufficient. Makes the distinction between a symmetric
  matrix being lower-triangular or fully filled. Defaults to `TRUE` as
  for most purposes, you would want the complete matrix. An example of
  where this is `FALSE` can be found in
  [`parameters`](https://github.com/ndpvh/discounting-affect/reference/parameters.md).
  Note that if `full = FALSE` and `parameters_only = FALSE`, this will
  automatically let the `cholesky` argument become `TRUE`, ensuring only
  the lower-triangular of the covariance matrix is indexed (as required
  by `full`).

- cholesky:

  Logical denoting whether the idea is to use the Cholesky decomposition
  to create the values of the covariance matrix. In this case, the
  indices should only span the lower-triangular of the matrix. Defaults
  to `TRUE`.

## Value

Instance of the
[`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)
with the parameters being indexed

## Examples

``` r
index(
  double_exponential(d = 2, k = 3),
  full = TRUE
)
#> Model of class "double_exponential":
#> 
#> Dimension: 2
#> Number of predictors: 3
#> Number of parameters: 20
#> 
#> Parameters:
#>   alpha: |  1.00  |
#>          |  2.00  |
#> 
#>   beta: | 3.00  5.00  7.00 |
#>         | 4.00  6.00  8.00 |
#> 
#>   omega: |  9.00  |
#> 
#>   gamma: | 10.00  0.00 |
#>          | 0.00  11.00 |
#> 
#>   nu: | 12.00  0.00 |
#>       | 0.00  13.00 |
#> 
#> 
#> Covariance: | 0.00  0.00 |
#>             | 0.00  0.00 |

index(
  double_exponential(d = 2, k = 3),
  full = FALSE
)
#> Model of class "double_exponential":
#> 
#> Dimension: 2
#> Number of predictors: 3
#> Number of parameters: 20
#> 
#> Parameters:
#>   alpha: |  1.00  |
#>          |  2.00  |
#> 
#>   beta: | 3.00  5.00  7.00 |
#>         | 4.00  6.00  8.00 |
#> 
#>   omega: |  9.00  |
#> 
#>   gamma: | 10.00  0.00 |
#>          | 0.00  11.00 |
#> 
#>   nu: | 12.00  0.00 |
#>       | 0.00  13.00 |
#> 
#> 
#> Covariance: | 0.00  0.00 |
#>             | 0.00  0.00 |
```

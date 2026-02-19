# Extract model parameters

Extract the parameters of a particular model. This includes the
covariances, so not only the deterministic model parameters.

## Usage

``` r
parameters(model, ...)

# S4 method for class 'model'
parameters(model, vector = TRUE)
```

## Arguments

- model:

  Instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)

- vector:

  Logical denoting whether to output the parameters in a vector (`TRUE`)
  or a list (`FALSE`). Defaults to `TRUE`.

## Value

Numeric vector containing all parameters of the model.

## Examples

``` r
parameters(
  double_exponential(d = 2, k = 3),
  vector = TRUE
)
#> Error in index_covariance(d, max(params[["nu"]]) + 1, cholesky = !full |     cholesky, ...): unused argument (vector = TRUE)

parameters(
  double_exponential(d = 2, k = 3),
  vector = FALSE
)
#> Error in index_covariance(d, max(params[["nu"]]) + 1, cholesky = !full |     cholesky, ...): unused argument (vector = FALSE)
```

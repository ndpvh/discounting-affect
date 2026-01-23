# Compute the mean of the residuals

Compute the mean of the residuals

## Usage

``` r
bias(fitobj)
```

## Arguments

- fitobj:

  Output of the
  [`fit`](https://github.com/ndpvh/discounting-affect/reference/fit.md)
  function.

## Value

Numeric denoting the mean of the residuals. If multiple dimensions exist
in the residuals, then the mean will be computed for each dimension
separately and the average of them will be returned.

## Examples

``` r
# Generate a dataset
data <- dataset(Y = rnorm(100), X = rnorm(100)) |>
  suppressWarnings()

# Estimate the exponential discounting model on these data
fitobj <- fit(
  exponential(),
  data,
  itermax = 50,
  trace = FALSE
)

# Compute the autocorrelation based on this fitobj
bias(fitobj)
#> [1] -5.459092e-05
```

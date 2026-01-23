# Compute the autocorrelation of the residuals

Compute the autocorrelation of the residuals

## Usage

``` r
autocorrelation(fitobj, lag = 1)
```

## Arguments

- fitobj:

  Output of the
  [`fit`](https://github.com/ndpvh/discounting-affect/reference/fit.md)
  function.

- lag:

  The lag of the autocorrelation to compute. Defaults to `1`.

## Value

Numeric denoting the autocorrelation of the residuals. If multiple
dimensions exist in the residuals, then the autocorrelation will be
computed for each dimension separately and a mean of them will be
returned.

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
autocorrelation(fitobj)
#> [1] -0.1600752
```

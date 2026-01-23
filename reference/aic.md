# Compute the \\AIC\\ based on \\SSE\\

Compute the \\AIC\\ based on \\SSE\\

## Usage

``` r
aic(fitobj)
```

## Arguments

- fitobj:

  Output of the
  [`fit`](https://github.com/ndpvh/discounting-affect/reference/fit.md)
  function.

## Value

Numeric denoting the \\AIC\\

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

# Compute the AIC based on this fitobj
aic(fitobj)
#> [1] 12.32387
```

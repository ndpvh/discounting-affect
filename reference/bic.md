# Compute the \\BIC\\ based on \\SSE\\

Compute the \\BIC\\ based on \\SSE\\

## Usage

``` r
bic(fitobj)
```

## Arguments

- fitobj:

  Output of the
  [`fit`](https://github.com/ndpvh/discounting-affect/reference/fit.md)
  function.

## Value

Numeric denoting the \\BIC\\

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

# Compute the BIC based on this fitobj
bic(fitobj)
#> [1] -14.20758
```

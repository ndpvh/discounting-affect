# Extract model parameters

Extract the parameters of a particular model.

## Usage

``` r
parameters(model, ...)

# S4 method for class 'model'
parameters(model, parameters_only = FALSE, ...)
```

## Arguments

- model:

  Instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)

- ...:

  Additional arguments passed on to
  [`index`](https://github.com/ndpvh/discounting-affect/reference/index.md)

- parameters_only:

  Logical denoting whether to only extract the determinstic parameters
  of the model (`TRUE`) or also include the covariance parameters
  (`FALSE`). Defaults to `FALSE`.

## Value

Numeric vector containing all parameters of the model.

## Examples

``` r
parameters(double_exponential(d = 2, k = 3))
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

parameters(double_exponential(d = 2, k = 3))
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```

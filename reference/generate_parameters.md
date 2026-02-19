# Generate model parameters

Generation of model parameters within this function always occurs
according to a uniform distribution within the bounds defined for the
models.

## Usage

``` r
generate_parameters(model, ...)

# S4 method for class 'model'
generate_parameters(model, ...)
```

## Arguments

- model:

  Instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)

- ...:

  Additional arguments passed on to
  [`get_bounds`](https://github.com/ndpvh/discounting-affect/reference/get_bounds.md)

## Value

Numeric vector of parameters for the model.

## Examples

``` r
generate_parameters(
  double_exponential(d = 2, k = 3),
  dynamics = "isotropic",
  covariance = "isotropic",
  parameters_only = FALSE
)
#>  [1] -0.816669493  0.985575447 -4.532399578  3.484483110 -1.641173423
#>  [6] -3.507795951  2.250178752  3.332840479  0.208431715  0.001893545
#> [11]  0.590804011  0.504328924  0.335171416  0.923226315  0.943496200
```

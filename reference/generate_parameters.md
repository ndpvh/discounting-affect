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
#>  [1]  0.56538360  0.88648120 -1.99229498 -3.74534505 -4.70354282 -1.18990346
#>  [7]  4.97506781 -3.89318031  0.45052567  0.21496655  0.37779710  0.05602967
#> [13]  0.84524970  0.27382625  0.57399488
```

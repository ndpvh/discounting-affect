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
#>  [1]  0.79083034 -0.83833089 -2.59586157 -3.84708706 -3.68565217  0.76244891
#>  [7]  3.05063219  1.54377358  0.06382314  0.60785261  0.72614169  0.45804841
#> [13]  0.12668422  0.10807363  0.88886352
```

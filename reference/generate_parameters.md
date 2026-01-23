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
#>  [1] -0.9064799  0.6968966 -1.6411734 -3.5077960  2.2501788  3.3328405
#>  [7] -0.8313657 -4.9810645  0.2954020  0.5043289  0.3351714  0.9232255
#> [13]  0.9434956  0.6228657  0.1989700
```

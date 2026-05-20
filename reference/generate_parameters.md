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
#>  [1]  0.48757753  0.73021898  0.07736419  4.58084214  2.56870109 -0.46173776
#>  [7]  4.77724536 -0.29173161  0.23864062  0.61872997  0.59499592  0.55824141
#> [13]  0.49126778  0.93387570  0.20304897
```

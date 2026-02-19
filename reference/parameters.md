# Extract model parameters

Extract the parameters of a particular model. This includes the
covariances, so not only the deterministic model parameters.

## Usage

``` r
parameters(model, ...)

# S4 method for class 'model'
parameters(model, vector = TRUE)
```

## Arguments

- model:

  Instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)

- vector:

  Logical denoting whether to output the parameters in a vector (`TRUE`)
  or a list (`FALSE`). Defaults to `TRUE`.

## Value

Numeric vector containing all parameters of the model.

## Examples

``` r
parameters(
  double_exponential(d = 2, k = 3),
  vector = TRUE
)
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

parameters(
  double_exponential(d = 2, k = 3),
  vector = FALSE
)
#> [[1]]
#> [1] 0 0
#> 
#> [[2]]
#> [1] 0 0 0 0 0 0
#> 
#> [[3]]
#> [1] 0
#> 
#> [[4]]
#> [1] 0 0 0 0
#> 
#> [[5]]
#> [1] 0 0 0 0
#> 
#> [[6]]
#> [1] 0
#> 
#> [[7]]
#> [1] 0
#> 
#> [[8]]
#> [1] 0
#> 
```

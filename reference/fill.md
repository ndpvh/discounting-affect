# Change model parameters

Fill the model parameters of a particular instance of the
[`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)
with the ones provided in a numeric vector. Used in
[`objective_function`](https://github.com/ndpvh/discounting-affect/reference/objective_function.md)
to allow for predictions with the model while cohering to the input
typical numerical optimizers such as `DEoptim` and `nloptr` provide
within the objective function.

## Usage

``` r
fill(model, parameters, ...)

# S4 method for class 'exponential'
fill(
  model,
  parameters,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = TRUE
)

# S4 method for class 'quasi_hyperbolic'
fill(
  model,
  parameters,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = TRUE
)

# S4 method for class 'double_exponential'
fill(
  model,
  parameters,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = TRUE
)
```

## Arguments

- model:

  Instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)

- parameters:

  Numeric vector containing the values of the parameters that should be
  assigned to the model. Importantly, the number of parameters within
  this vector should correspond exactly to the number of parameters
  needed by the model. Ideally, you let functions internal to this
  package handle this for you, rather than you defining these parameters
  manually.

- ...:

  Additional arguments passed on to the methods.

- dynamics:

  Character denoting the structure of the dynamical matrices. Can either
  be `"anisotropic"` (completely free), `"symmetric"` (symmetric around
  the diagonal), and `"isotropic"` (diagonal). Note that this influences
  different parameters for different models, namely \\\Gamma\\ for the
  exponential discounting model, \\N\\ and \\K\\ for the
  quasi-hyperbolic discounting model, and \\\Gamma\\ and \\N\\ for the
  double-exponential discounting model. Defaults to `"isotropic"`.

- covariance:

  Character denoting the structure of the covariance matrix. Can either
  be `"symmetric"` (symmetric around the diagonal) or `"isotropic"`
  (diagonal). Defaults to `"symmetric"`.

- parameters_only:

  Logical denoting whether to only fill the parameters in de `parameter`
  slot of the model (`TRUE`), or to fill the covariance matrix as well
  (`FALSE`). Defaults to `TRUE`.

## Value

Instance of the
[`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)
containing the values `parameters` in its `parameter`-slot.

## Examples

``` r
# Create an empty instance of the exponential discounting model
my_model <- exponential()

# Assign the parameters of a vector to this exponential function
fill(
  my_model,
  c(1, 2, 0.5)
)
#> Model of class "exponential":
#> 
#> Dimension: 1
#> Number of predictors: 1
#> Number of parameters: 4
#> 
#> Parameters:
#>   alpha: |  1.00  |
#> 
#>   beta: | 2.00 |
#> 
#>   gamma: | 0.50 |
#> 
#> 
#> Covariance: | 0.00 |
```

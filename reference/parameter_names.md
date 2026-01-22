# Name the relevant parameters of the model

Name the relevant parameters of the model

## Usage

``` r
parameter_names(model, ...)

# S4 method for class 'exponential'
parameter_names(
  model,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = TRUE
)

# S4 method for class 'quasi_hyperbolic'
parameter_names(
  model,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = TRUE
)

# S4 method for class 'double_exponential'
parameter_names(
  model,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = TRUE
)
```

## Arguments

- model:

  Instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md).

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

  Logical denoting whether to only name the parameters in de `parameter`
  slot of the model (`TRUE`), or to fill the covariance matrix as well
  (`FALSE`). Defaults to `TRUE`.

## Value

Character vector containing the names for the parameters in the model.
The order corresponds to the assumptions of
[`get_bounds`](https://github.com/ndpvh/discounting-affect/reference/get_bounds.md)
and
[`fill`](https://github.com/ndpvh/discounting-affect/reference/fill.md).

## Examples

``` r
# Create an empty instance of the exponential discounting model
my_model <- exponential()

# Get the names for the model, once with and once without the covariances 
# included
parameter_names(
  my_model,
  dynamics = "anisotropic",
  covariance = "isotropic",
  parameters_only = FALSE
)
#> [1] "alpha_1"  "beta_11"  "gamma_11" "sigma_11"

parameter_names(
  my_model,
  dynamics = "anisotropic",
  covariance = "isotropic",
  parameters_only = TRUE
)
#> [1] "alpha_1"  "beta_11"  "gamma_11"
```

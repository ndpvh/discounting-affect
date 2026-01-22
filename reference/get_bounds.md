# Get bounds of the parameters of a model

Get bounds of the parameters of a model

## Usage

``` r
get_bounds(model, ...)

# S4 method for class 'exponential'
get_bounds(
  model,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = TRUE,
  lower = NULL,
  upper = NULL
)

# S4 method for class 'quasi_hyperbolic'
get_bounds(
  model,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = TRUE,
  lower = NULL,
  upper = NULL
)

# S4 method for class 'double_exponential'
get_bounds(
  model,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = TRUE,
  lower = NULL,
  upper = NULL
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

  Logical denoting whether to only get bounds for the parameters in de
  `parameter` slot of the model (`TRUE`), or to fill the covariance
  matrix as well (`FALSE`). Defaults to `TRUE`.

- lower:

  Numeric vector of the same length as the substantive parameters in the
  model, denoting the lower bounds for each one. For the exponential
  discounting model, for example, a value of \`c(0, 0, 0, 0)\` would
  denote a minimal value of 0 for all values in \\\bm{\alpha}\\, \\B\\,
  \\\Gamma\\, and \\\Sigma\\. Defaults depend on the model itself and
  have been chosen while accounting for natural bounds in the data used
  in this project. Note that if `parameters_only = TRUE`, there is no
  need to specify a bound for the covariances.

- upper:

  Numeric vector that functions the same as `lower`, but specifying the
  upper bounds instead.

## Value

Named list containing numeric vectors denoting the lower and upper
bounds for the parameters (under `"lower"` and `"upper"` resp.). The
order maintained in this

## Examples

``` r
# Create an empty instance of the exponential discounting model
my_model <- exponential()

# Get the bounds for the model
get_bounds(
  my_model,
  dynamics = "anisotropic",
  covariance = "isotropic",
  parameters_only = FALSE
)
#> $lower
#> [1] -1e+00 -5e+00  0e+00  1e-05
#> 
#> $upper
#> [1] 1 5 1 1
#> 

# Get the bounds for the model when specifying your personal lower and upper
# bounds
get_bounds(
  my_model,
  dynamics = "anisotropic",
  covariance = "isotropic",
  parameters_only = FALSE,
  lower = c(0, -100, 0, 0.01),
  upper = c(1, 100, 0.5, 1)
)
#> $lower
#> [1]  0e+00 -1e+02  0e+00  1e-02
#> 
#> $upper
#> [1]   1.0 100.0   0.5   1.0
#> 
```

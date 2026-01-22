# Count number of parameters for a model

Count number of parameters for a model

## Usage

``` r
count_parameters(model, ...)

# S4 method for class 'exponential'
count_parameters(
  model,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = FALSE
)

# S4 method for class 'quasi_hyperbolic'
count_parameters(
  model,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = FALSE
)

# S4 method for class 'double_exponential'
count_parameters(
  model,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = FALSE
)
```

## Arguments

- model:

  An instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)

- ...:

  Arguments passed on to the methods.

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

  Logical denoting whether to only count the number of parameters in de
  `parameter` slot of the model (`TRUE`), or to count the number of
  parameters in the covariance matrix as well (`FALSE`). Defaults to
  `FALSE`.

## Value

Integer denoting the number of parameters the model contains within the
current specifications.

## Examples

``` r
# Define a model with a particular dimensionality
my_model <- exponential(
  parameters = list(
    "alpha" = numeric(2),
    "beta" = matrix(0, nrow = 2, ncol = 5),
    "gamma" = matrix(0, nrow = 2, ncol = 2)
  ),
  covariance = matrix(0, nrow = 2, ncol = 2)
)

# Get the number of parameters for this model under no restrictions (i.e., 
# anisotropic forgetting factors and symmetric covariances)
count_parameters(
  my_model, 
  dynamics = "anisotropic",
  covariance = "symmetric"
)
#> [1] 19

# Get the number of parameters for this model in the most limited case 
# (i.e., isotropic forgetting factors and covariances)
count_parameters(
  my_model,
  dynamics = "isotropic",
  covariance = "isotropic"
)
#> [1] 16
```

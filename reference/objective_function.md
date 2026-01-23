# Objective function

This method defines how the objective function of the estimation
procedure looks like. The result of this objective function is the sum
squared error, meaning the estimation routine makes use of a
least-squares procedure.

## Usage

``` r
objective_function(model, data, parameters, ...)

# S4 method for class 'model,dataset'
objective_function(model, data, parameters, dynamics = "isotropic")
```

## Arguments

- model:

  Instance of the
  [`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md),
  defining the model to evaluate the objective function for.

- data:

  Instance of the
  [`dataset-class`](https://github.com/ndpvh/discounting-affect/reference/dataset-class.md)
  containing the data to fit the model to.

- parameters:

  Numeric vector containing the parameters of the model to compute the
  \\SSE\\ for.

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

## Value

The sum of squared error or \\SSE\\, quantifying misfit of the model to
the data.

## Details

For the purposes of the package, the objective function defines how the
estimation routine should go about, therefore defining the procedure
with which the fit of the model to the data under a particular set of
parameters is evaluated. Within this package, we use the least-squares
procedure, therefore minimizing the sum of squared error (\\SSE\\),
defined as:

\$\$SSE = Y - \hat{Y}\$\$

where \\Y\\ represents the observed data and \\\hat{Y}\\ the predictions
of the model under the proposed parameter set.

The procedure as used here relies heavily on the implementation of the
[`predict`](https://rdrr.io/r/stats/predict.html) method to get the
predictions \\\hat{Y}\\. If a recovery does not go well, it may be
useful to take a look at how this method is defined for your particular
model.

Note that the objective function is only useful when using a
non-analytic approach to estimation, as the one used in this package.

## Examples

``` r
# Simulate data to use for this example
data <- simulate(
  quasi_hyperbolic(
    parameters = list(
      "alpha" = 1, 
      "beta" = as.matrix(2),
      "nu" = as.matrix(0.75),
      "kappa" = as.matrix(0.5)
    ),
    covariance = as.matrix(1)
  ),
  X = rnorm(100)
)

# Evaluate the objective function for an exponential model with a particular
# set of parameters
objective_function(
  exponential(),
  data,
  c(1, 2, 0.75)
)
#> [1] 202.9634

# Evaluate the objective function when using exactly the model and the 
# parameters that generated the data
objective_function(
  quasi_hyperbolic(),
  data,
  c(1, 2, 0.75, 0.5)
)
#> [1] 113.6952
```

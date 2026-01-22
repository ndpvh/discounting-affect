# Constructor for the [`exponential-class`](https://github.com/ndpvh/discounting-affect/reference/exponential-class.md)

Defines an instance of the
[`exponential-class`](https://github.com/ndpvh/discounting-affect/reference/exponential-class.md),
that is the class defining the exponential discounting model. For the
mathematical equations and how to specify them in this constructor,
users can look at the details. For more information on the model itself,
see the vignette on the model definitions.

## Usage

``` r
exponential(
  d = NA,
  k = NA,
  parameters = NULL,
  covariance = NULL,
  cholesky = FALSE
)
```

## Arguments

- d:

  Integer denoting the number of dimensions of the model. Defaults to
  `NA`, in which case this dimensionality is inferred from the
  parameters. Note that if `d` and `k` are both defined while
  `parameters` is left unspecified, then the model will automatically
  create an empty model of the specified dimensionality.

- k:

  Integer denoting the number of independent variables. Defaults to
  `NA`, in which case this dimensionality is inferred from the
  parameters.

- parameters:

  List containing the parameters relevant to a particular model
  instance. Defaults to `NULL`, creating an empty model with \\d = 1\\
  and \\k = 1\\ (unless `d` and `k` are specified).

- covariance:

  Numeric matrix denoting the residual covariance of the model. Default
  to `NULL`, creating a matrix of `0`s with the dimensionality implied
  by `d`.

- cholesky:

  Logical denoting whether `covariance` is a lower-triangular
  decomposition matrix instead of an actual covariance matrix. Defaults
  to `FALSE`.

## Value

Instance of
[`exponential-class`](https://github.com/ndpvh/discounting-affect/reference/exponential-class.md)

## Details

The exponential discounting model assumes that the effect of stimuli on
affect fades away at an exponential rate and can be defined as:

\$\$\boldsymbol{y}\_t = \boldsymbol{\alpha} + \sum\_{j = 0}^t \Gamma^j B
\boldsymbol{x}\_{t - j} + \boldsymbol{\epsilon}\_t\$\$

where \\\boldsymbol{\alpha}\\ is a \\d\\-dimensional vector representing
the mean of the process \\\boldsymbol{y}\\, \\\Gamma\\ is a \\d \times
d\\ matrix containing the forgetting factors, determining how long the
effect of the independent variables \\\boldsymbol{x}\\ on the process
\\\boldsymbol{y}\\ lingers on and therefore determining the dynamics of
the system, \\B\\ is a \\d \times k\\matrix containing the slopes of the
independent variables \\\boldsymbol{x}\\, and \\\boldsymbol{\epsilon}\\
represents the residuals of the system. Within this package, we assume
that:

\$\$\boldsymbol{\epsilon} \overset{iid}{\sim} N(\boldsymbol{0},
\Sigma)\$\$

where \\\Sigma\\ is a \\d \times d\\ matrix representing the residual
covariance matrix.

The exponential discounting model can also be written as an instance of
the \\VARMAX(1, 1)\\, so that:

\$\$\boldsymbol{y}\_t = (I_d - \Gamma) \boldsymbol{\alpha} + B
\boldsymbol{x}\_t + \Gamma \boldsymbol{y}\_{t - 1} - \Gamma
\boldsymbol{\epsilon}\_{t - 1} + \boldsymbol{\epsilon}\_t\$\$

where \\I_d\\ is a \\d \times d\\ identity matrix.

To define the model, one should minimally define the parameters of the
model through `parameters` and `covariance`. In `covariance`, one either
provides the \\d \times d\\ residual covariance matrix \\\Sigma\\
(`cholesky = FALSE`) or a lower-triangular \\d \times d\\ matrix \\G\\
that represents part of the decomposition of this matrix
(`cholesky = TRUE`), so that we can retrieve the covariance matrix as:

\$\$\Sigma = G G^T\$\$

The latter option ensures that the covariance matrix \\\Sigma\\ defined
within the model is positive-definite, but may require some additional
thinking on the side of the user.

In `parameters`, one should provide a named list with instances
`"alpha"`, `"beta"`, and `"gamma"`, each defining the respective
parameters \\\boldsymbol{\alpha}\\, \\B\\, and \\\Gamma\\ of the model.
Note that the dimensionalities of these parameters should match up and
are actively checked when constructing this class.

There are some important restrictions to take into account when defining
the model:

- The eigenvalues \\\lambda_i\\ of \\\Gamma\\ should all lie between 0
  and 1, or \\\lambda_i \in \[0, 1)\\, a restriction that imposes
  exponential decay rather than sawtooth or more complex dynamics

- The covariance matrix \\\Sigma\\ should be positive definite, as
  explained above

## See also

[`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)
[`exponential-class`](https://github.com/ndpvh/discounting-affect/reference/exponential-class.md)

## Examples

``` r
exponential(
  d = 2,
  k = 2,
  parameters = list(
    "alpha" = numeric(2),
    "beta" = diag(2) * 2,
    "gamma" = diag(2) * 0.5
  ),
  covariance = diag(2)
)
#> Model of class "exponential":
#> 
#> Dimension: 2
#> Number of predictors: 2
#> Number of parameters: 13
#> 
#> Parameters:
#>   alpha: |  0.00  |
#>          |  0.00  |
#> 
#>   beta: | 2.00  0.00 |
#>         | 0.00  2.00 |
#> 
#>   gamma: | 0.50  0.00 |
#>          | 0.00  0.50 |
#> 
#> 
#> Covariance: | 1.00  0.00 |
#>             | 0.00  1.00 |

exponential(
  d = 2,
  k = 2
)
#> Model of class "exponential":
#> 
#> Dimension: 2
#> Number of predictors: 2
#> Number of parameters: 13
#> 
#> Parameters:
#>   alpha: |  0.00  |
#>          |  0.00  |
#> 
#>   beta: | 0.00  0.00 |
#>         | 0.00  0.00 |
#> 
#>   gamma: | 0.00  0.00 |
#>          | 0.00  0.00 |
#> 
#> 
#> Covariance: | 0.00  0.00 |
#>             | 0.00  0.00 |
```

# Constructor for the [`double_exponential-class`](https://github.com/ndpvh/discounting-affect/reference/double_exponential-class.md)

Defines an instance of the
[`double_exponential-class`](https://github.com/ndpvh/discounting-affect/reference/double_exponential-class.md),
that is the class defining the double-exponential discounting model. For
the mathematical equations and how to specify them in this constructor,
users can look at the details. For more information on the model itself,
see the vignette on the model definitions.

## Usage

``` r
double_exponential(
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
[`double_exponential-class`](https://github.com/ndpvh/discounting-affect/reference/double_exponential-class.md)

## Details

The double-exponential discounting model assumes that the effect of
stimuli on affect fades away at an exponential rate as constructed
through two processes, one representing longer and the other slower
discounting rates, where each process' effect is accounted for through a
weighted sum. The model can be defined as:

\$\$\boldsymbol{y}\_t = \boldsymbol{\alpha} + \sum\_{j = 0}^t \left(
\omega \Gamma^j + (1 - \omega) N^j \right) B \boldsymbol{x}\_{t - j} +
\boldsymbol{\epsilon}\_t\$\$

where \\\boldsymbol{\alpha}\\ is a \\d\\-dimensional vector representing
the mean of the process \\\boldsymbol{y}\\, \\\Gamma\\ and \\N\\ are \\d
\times d\\ matrices containing the forgetting factors, determining how
long the effect of the independent variables \\\boldsymbol{x}\\ on the
process \\\boldsymbol{y}\\ lingers on and therefore determining the
dynamics of the system, \\\omega \in \[0, 0.5\]\\ determines the
strength with which \\\Gamma\\ and \\N\\ influence the discounting
process, \\B\\ is a \\d \times k\\matrix containing the slopes of the
independent variables \\\boldsymbol{x}\\, and \\\boldsymbol{\epsilon}\\
represents the residuals of the system. Within this package, we assume
that:

\$\$\boldsymbol{\epsilon} \overset{iid}{\sim} N(\boldsymbol{0},
\Sigma)\$\$

where \\\Sigma\\ is a \\d \times d\\ matrix representing the residual
covariance matrix.

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
`"alpha"`, `"beta"`, `"omega"`, `"gamma"`, and `"nu"`, each defining the
respective parameters \\\boldsymbol{\alpha}\\, \\B\\, \\\omega\\,
\\\Gamma\\ and \\N\\ of the model. Note that the dimensionalities of
these parameters should match up and are actively checked when
constructing this class.

There are some important restrictions to take into account when defining
the model:

- The eigenvalues \\\lambda_i\\ of \\\Gamma\\ and \\N\\ should all lie
  between 0 and 1, or \\\lambda_i \in \[0, 1)\\, a restriction that
  imposes exponential decay rather than sawtooth or more complex
  dynamics

- The weight \\\omega\\ should lie between 0 and 0.5, or \\\omega \in
  \[0, 0.5\]\\, a restriction that is imposed to ensure recoverability
  of the model

- The covariance matrix \\\Sigma\\ should be positive definite, as
  explained above

## See also

[`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)
[`exponential-class`](https://github.com/ndpvh/discounting-affect/reference/exponential-class.md)
[`quasi_hyperbolic-class`](https://github.com/ndpvh/discounting-affect/reference/quasi_hyperbolic-class.md)
[`double_exponential-class`](https://github.com/ndpvh/discounting-affect/reference/double_exponential-class.md)

## Examples

``` r
double_exponential(
  d = 2,
  k = 2,
  parameters = list(
    "alpha" = numeric(2),
    "beta" = diag(2) * 2,
    "omega" = 0.25,
    "gamma" = diag(2) * 0.75,
    "nu" = diag(2) * 0.5
  ),
  covariance = diag(2)
)
#> Model of class "double_exponential":
#> 
#> Dimension: 2
#> Number of predictors: 2
#> Number of parameters: 18
#> 
#> Parameters:
#>   alpha: |  0.00  |
#>          |  0.00  |
#> 
#>   beta: | 2.00  0.00 |
#>         | 0.00  2.00 |
#> 
#>   omega: |  0.25  |
#> 
#>   gamma: | 0.75  0.00 |
#>          | 0.00  0.75 |
#> 
#>   nu: | 0.50  0.00 |
#>       | 0.00  0.50 |
#> 
#> 
#> Covariance: | 1.00  0.00 |
#>             | 0.00  1.00 |

double_exponential(
  d = 2,
  k = 2
)
#> Model of class "double_exponential":
#> 
#> Dimension: 2
#> Number of predictors: 2
#> Number of parameters: 18
#> 
#> Parameters:
#>   alpha: |  0.00  |
#>          |  0.00  |
#> 
#>   beta: | 0.00  0.00 |
#>         | 0.00  0.00 |
#> 
#>   omega: |  0.00  |
#> 
#>   gamma: | 0.00  0.00 |
#>          | 0.00  0.00 |
#> 
#>   nu: | 0.00  0.00 |
#>       | 0.00  0.00 |
#> 
#> 
#> Covariance: | 0.00  0.00 |
#>             | 0.00  0.00 |
```

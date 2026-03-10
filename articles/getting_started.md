# Getting Started

This project revolves around the comparison of several temporal
discounting models when applied to affect, following up on the call for
such a comparison mentioned in Vanhasbroeck et al. (2024). These models
can generally be defined as follows:

$$\mathbf{y}_{t} = {\mathbf{α}} + \sum\limits_{j = 0}^{t}F(\mathbf{\Theta},t,j)B\mathbf{x}_{t - j} + {\mathbf{ϵ}}_{t}$$
where $\mathbf{y}_{t}$ is a $d \times 1$ vector containing the dependent
variables at time $t$, $\mathbf{x}_{t}$ is a $k \times 1$ vector
containing the predictor variables at time $t$, $\mathbf{α}$ is a
$d \times 1$ vector representing the mean of the dependent variables,
$F{()}$ is a function that outputs a $d \times d$ matrix defining the
dynamics of the system, $B$ is a $d \times k$ matrix of slopes
connecting the predictor variables to the dependent variables, and
${\mathbf{ϵ}}_{t}$ represents the residuals of the system, where we
assume that:

$${\mathbf{ϵ}} \sim N(\mathbf{0},\Sigma)$$ where $\Sigma$ is the
residual covariance matrix.

Importantly, the dynamics defined by the function $F$ requires the
specification of some dynamic parameters $\mathbf{\Theta}$ and the lag
between the current time $t$ and the time $j$ at which the predictor was
observed. In other words, these models assume that the influence of the
predictor variables on the dependent variables are *forgotten* or
*regulated* over time (Rutledge et al., 2014). Using them to model
affect dynamics therefore represents an interesting opportunity.

On this page, we explain the basic structure of the project and how to
use the functions inherent to it. For more information on the
discounting models themselves, we refer the interested reader to the
dedicated
[vignette](https://ndpvh.github.io/discounting-affect/articles/models.html).

## Structure

This project combines an R package with analysis scripts, providing its
own ecosystem in which functions that are defined in the package are
immediately used for our research purposes. Using a package structure
has several advantages, most notably providing a clear structure for
other researchers who stumble upon the project, allowing for integrated
tests of the code written for this project, and including documentation
detailing our reasoning. While in-principle the `discounting` package
can be used by others, it is not necessarily meant to and may,
therefore, sometimes feel user-unfriendly.

The combination of a package structure with analysis scripts implies a
particular folder structure in which the code are contained.
Specifically, you can find the following scripts in the following
folders:

- `R`: Definitions of the generics, methods, classes, and functions used
  within this project;
- `tests`: Tests defined for the content of the package;
- `man`: Documentation regarding the content of the package;
- `scripts`: Analysis scripts that make use of the content of the
  package.

From a research perspective, the latter are the most interesting to
examine. If you want to figure out what’s happening under the hood, one
may also want to take a look at the content of the `R` folder.

## Using the Package

The package has been developed in an object-oriented way, specifically
leveraging on the functionality of the S4 class in R (for more
information, see Wickham, 2019). Using recently proposed standard, we
define our models as instances of particular classes
([`exponential()`](https://ndpvh.github.io/discounting-affect/reference/exponential.html),
[`quasi_hyperbolic()`](https://ndpvh.github.io/discounting-affect/reference/quasi_hyperbolic.html),
[`double_exponential()`](https://ndpvh.github.io/discounting-affect/reference/double_exponential.html))
on which we can then apply some methods (e.g.,
[`predict()`](https://ndpvh.github.io/discounting-affect/reference/predict.html),
[`simulate()`](https://ndpvh.github.io/discounting-affect/reference/simulate.html),
[`fit()`](https://ndpvh.github.io/discounting-affect/reference/fit.html);
Evers & Vanhasbroeck, 2026). In this section, we will provide an
introduction to the package’s main functionalities, namely defining a
model, simulating from a model, fitting a model, and performing a
recovery study.

### Defining a Model

To define a particular model, we use the constructor functions for the
exponential, quasi-hyperbolic, or double-exponential discounting models,
aptly called
[`exponential()`](https://ndpvh.github.io/discounting-affect/reference/exponential.html),
[`quasi_hyperbolic()`](https://ndpvh.github.io/discounting-affect/reference/quasi_hyperbolic.html),
and
[`double_exponential()`](https://ndpvh.github.io/discounting-affect/reference/double_exponential.html).
At minimum, what these functions need are the number of indepedendent
variables `d` and the number of predictor variables `k`. For example,
the following command will create an empty model with the specified
dimensionality:

``` r
my_model <- exponential(d = 2, k = 3)
my_model
#> Model of class "exponential":
#> 
#> Dimension: 2
#> Number of predictors: 3
#> Number of parameters: 15
#> 
#> Parameters:
#>   alpha: |  0.00  |
#>          |  0.00  |
#> 
#>   beta: | 0.00  0.00  0.00 |
#>         | 0.00  0.00  0.00 |
#> 
#>   gamma: | 0.00  0.00 |
#>          | 0.00  0.00 |
#> 
#> 
#> Covariance: | 0.00  0.00 |
#>             | 0.00  0.00 |
```

If you have a set of parameters that you would like to provide to the
model, you can alternatively specify the `parameters` and `covariance`
arguments, for example:

``` r
# Create some parameters to be provided to the constructor
params <- list(
  "alpha" = c(-1, 1), 
  "beta" = matrix(1:6, nrow = 2, ncol = 3),
  "gamma" = diag(2) * 0.5
)
cov <- diag(2)

# Create and print the resulting model
my_model <- exponential(
  parameters = params, 
  covariance = cov
)
my_model
#> Model of class "exponential":
#> 
#> Dimension: 2
#> Number of predictors: 3
#> Number of parameters: 15
#> 
#> Parameters:
#>   alpha: |  -1.00  |
#>          |  1.00  |
#> 
#>   beta: | 1  3  5 |
#>         | 2  4  6 |
#> 
#>   gamma: | 0.50  0.00 |
#>          | 0.00  0.50 |
#> 
#> 
#> Covariance: | 1.00  0.00 |
#>             | 0.00  1.00 |
```

#### Troubleshooting

There are several things that should be kept in mind when defining a
model, all of which will be handled here.

##### Mind the Dimensionality

Using the constructor, one can specify the arguments `d`, `k`,
`parameters`, and `covariance` simultaneously, for example:

``` r
# Create some parameters to be provided to the constructor
params <- list(
  "alpha" = c(-1, 1), 
  "beta" = matrix(1:6, nrow = 2, ncol = 3),
  "gamma" = diag(2) * 0.5
)
cov <- diag(2)

# Create and print the resulting model
my_model <- exponential(
  d = 2, 
  k = 3,
  parameters = params, 
  covariance = cov
)
my_model
#> Model of class "exponential":
#> 
#> Dimension: 2
#> Number of predictors: 3
#> Number of parameters: 15
#> 
#> Parameters:
#>   alpha: |  -1.00  |
#>          |  1.00  |
#> 
#>   beta: | 1  3  5 |
#>         | 2  4  6 |
#> 
#>   gamma: | 0.50  0.00 |
#>          | 0.00  0.50 |
#> 
#> 
#> Covariance: | 1.00  0.00 |
#>             | 0.00  1.00 |
```

However, in this case, it is important that the dimensionality of all
arguments match. In this example, we specified parameters and
covariances for the case where `d = 2` and `k = 3`. Providing different
values in the constructor will lead to an error:

``` r
# Misspecify the dimensionality of d and k
my_model <- exponential(
  d = 1, 
  k = 4,
  parameters = params, 
  covariance = cov
)
#> Error in `exponential()`:
#> ! Dimensionality d is not in accord with the dimensionality of "alpha".
```

Similarly, the dimensionality of all parameters should add up. For
example, specifying two-dimensional deterministic parameters (in this
case, `"alpha"`, `"beta"`, and `"gamma"`) but a one-dimensional
covariance will also lead to errors:

``` r
# Redefine the covariance
cov <- as.matrix(1)

# Create the misspecified model
my_model <- exponential(
  parameters = params, 
  covariance = cov
)
#> Error in `model()`:
#> ! Provided dimensionality d does not match the covariance matrix.
```

This is also true for misspecified determinstic parameters. For example:

``` r
# Redefine the beta parameter so that it doesn't match anymore
params[["beta"]] <- matrix(1:3, nrow = 1, ncol = 3)

# Create the misspecified model
my_model <- exponential(
  parameters = params,
  covariance = cov
)
#> Error in `exponential()`:
#> ! Dimensionalities d and/or k are not in accord with the dimensionalilty of "beta".
```

##### Mind the Parameter Names

Each of the omdels provided in the package have their own
characteristics set of parameters. These are `"alpha"`, `"beta"`, and
`"gamma"` for the exponential discounting model, `"alpha"`, `"beta"`,
`"nu"`, and `"kappa"` for the quasi-hyperbolic discounting model,
`"alpha"`, `"beta"`, `"omega"`, `"gamma"`, and `"nu"` for the
double-exponential discounting model. When using the `parameters`
argument to specify these argument, the constructors assume that all
relevant parameters are specified. If this isn’t the case, an error will
be thrown:

``` r
my_model <- exponential(
  parameters = list(),
  covariance = cov
)
#> Error in `exponential()`:
#> ! Not all parameters are defined in the list. Ensure that the list contains slots "alpha", "beta", and "gamma".
```

To find out which parameters you are lacking, you can look at the
expected content for the `parameters` argument within the error or,
alternatively, create an empty model and inspect its output to see the
characteristic parameters of the discounting model, for example:

``` r
quasi_hyperbolic()
#> Model of class "quasi_hyperbolic":
#> 
#> Dimension: 1
#> Number of predictors: 1
#> Number of parameters: 5
#> 
#> Parameters:
#>   alpha: |  0.00  |
#> 
#>   beta: | 0.00 |
#> 
#>   nu: | 0.00 |
#> 
#>   kappa: | 0.00 |
#> 
#> 
#> Covariance: | 0.00 |
```

Note that if you specify too many parameters in the list, that they will
be discarded while constructing the model. For example:

``` r
# Add a useless parameter to the list
params[["test"]] <- matrix(0, nrow = 2, ncol = 2)

# Construct the exponential discounting model
my_model <- exponential(
  parameters = params, 
  covariance = cov
)
#> Warning in exponential(parameters = params, covariance = cov): Too many
#> different parameters provided to `parameters`. Deleting the redundant ones.
my_model
#> Model of class "exponential":
#> 
#> Dimension: 2
#> Number of predictors: 3
#> Number of parameters: 15
#> 
#> Parameters:
#>   alpha: |  -1.00  |
#>          |  1.00  |
#> 
#>   beta: | 1  3  5 |
#>         | 2  4  6 |
#> 
#>   gamma: | 0.50  0.00 |
#>          | 0.00  0.50 |
#> 
#> 
#> Covariance: | 1.00  0.00 |
#>             | 0.00  1.00 |
```

##### Mind the Parameter Bounds

Some of the parameters of the models have natural bounds that they must
comply with. Specifically, the eigenvalues of the dynamic parameters of
all three models (`"gamma"`, `"nu"`, `"kappa"`) should lie between 0 and
1, and for the double-exponential model, the weighting parameter
`"omega"` should lie between 0 and 0.5. If the parameters provided to
the constructor do not comply to these bounds, an error will be thrown:

``` r
# Create parameters with out-of-bounds values
params <- list(
  "alpha" = c(-1, 1),
  "beta" = matrix(1:6, nrow = 2, ncol = 3),
  "gamma" = diag(2) * 1.5
)

# Create the misspecified model
my_model <- exponential(
  parameters = params,
  covariance = cov
)
#> Error in `exponential()`:
#> ! The eigenvalues of "gamma" should lie between 0 and 1.
```

##### Mind the Parameter Types

Much of the methods that can be applied to the models rely on matrix
algebra, which implies that the parameters that we define for these
models are often matrices. This is enforced when using the constructor,
which may lead to either a warning when it is possible to transform the
parameters to a matrix, or to an error otherwise. For example:

``` r
# Create a parameters set for which it is possible to transform the parameters 
# to a matrix
params <- list(
  "alpha" = c(-1, 1),
  "beta" = 1:2, 
  "gamma" = diag(2) * 0.5
)

# Throws a warning, stating that beta has been transformed to a matrix
my_model <- exponential(
  parameters = params, 
  covariance = cov
)
#> Warning in exponential(parameters = params, covariance = cov): The parameter
#> "beta" should be a matrix: Changing type assuming a single independent
#> variable.

# Create a parameters set for which it is impossible to transform the parameters 
# to a matrix without additional information
params <- list(
  "alpha" = c(-1, 1),
  "beta" = 1:6, 
  "gamma" = diag(2) * 0.5
)

# Throws an error, stating that beta cannot be transformed to a matrix of the 
# correct dimensionality
my_model <- exponential(
  parameters = params,
  covariance = cov
)
#> Warning in exponential(parameters = params, covariance = cov): The parameter
#> "beta" should be a matrix: Changing type assuming a single independent
#> variable.
#> Error in `exponential()`:
#> ! Dimensionalities d and/or k are not in accord with the dimensionalilty of "beta".
```

#### Accessing Model Characteristics

Each model is defined by a particular set of characteristics or
*attributes* that can be accessed by the user. As no getters/setters are
defined within the package, one can only access these attributes through
the `@`, specifically using the format `<object>@<attribute>`. For
example, accessing the parameters for `my_model` can be achieved by
calling:

``` r
my_model@parameters
#> $alpha
#> [1] -1  1
#> 
#> $beta
#>      [,1]
#> [1,]    1
#> [2,]    2
#> 
#> $gamma
#>      [,1] [,2]
#> [1,]  0.5  0.0
#> [2,]  0.0  0.5
```

The attributes that are defined for the models are the following:

- `d`: Number of dependent variables;
- `k`: Number of indepedent variables;
- `n`: Number of parameters in the model (see
  [`count_parameters()`](https://ndpvh.github.io/discounting-affect/reference/count_parameters.html));
- `parameters`: Named list of parameters of the model;
- `covariance`: Numeric matrix containing the covariance of the model.

To change these attributes, you can assign a new value to the slot
through the assignment operator `<-`:

``` r
my_model@d <- 3
my_model
#> Model of class "exponential":
#> 
#> Dimension: 3
#> Number of predictors: 1
#> Number of parameters: 11
#> 
#> Parameters:
#>   alpha: |  -1.00  |
#>          |  1.00  |
#> 
#>   beta: | 1 |
#>         | 2 |
#> 
#>   gamma: | 0.50  0.00 |
#>          | 0.00  0.50 |
#> 
#> 
#> Covariance: | 1.00  0.00 |
#>             | 0.00  1.00 |
```

Note, however, that no additional checks are run to ensure that all
information in the model is correct. *Changing an attribute in this way
is thus at your own risk!*

#### Non-Manual Model Creation

Typically, you do not want to create a model and specify its parameters
manually, but rather create such parameters quasi-randomly instead.
Within the `discounting` package, we allow for such creation through the
following workflow.

First, one has to specify the kind of model they wish to use and its
dimensionality, leaving the parameters unspecified. For example:

``` r
my_model <- double_exponential(d = 3, k = 2)
my_model
#> Model of class "double_exponential":
#> 
#> Dimension: 3
#> Number of predictors: 2
#> Number of parameters: 34
#> 
#> Parameters:
#>   alpha: |  0.00  |
#>          |  0.00  |
#>          |  0.00  |
#> 
#>   beta: | 0.00  0.00 |
#>         | 0.00  0.00 |
#>         | 0.00  0.00 |
#> 
#>   omega: |  0.00  |
#> 
#>   gamma: | 0.00  0.00  0.00 |
#>          | 0.00  0.00  0.00 |
#>          | 0.00  0.00  0.00 |
#> 
#>   nu: | 0.00  0.00  0.00 |
#>       | 0.00  0.00  0.00 |
#>       | 0.00  0.00  0.00 |
#> 
#> 
#> Covariance: | 0.00  0.00  0.00 |
#>             | 0.00  0.00  0.00 |
#>             | 0.00  0.00  0.00 |
```

Then, we can use the
[`generate_parameters()`](https://ndpvh.github.io/discounting-affect/reference/generate_parameters.html)
function to generate a random set of parameters for a model with this
dimensionality. This function takes in several arguments that allow one
to specify the particulars of the model, specifically:

- `dynamics`: Character denoting the structure of dynamic matrices.
  Either `"isotropic"` (diagonal matrix), `"symmetric"` (symmetric
  matrix), or `"anisotropic"` (completely free matrix);
- `covariance`: Character denoting the structure of the covariance
  matrix. Either `"isotropic"` or `"symmetric"`;
- `parameters_only`: Logical denoting whether to only generate the
  determinstic parameters (`TRUE`) or to include the covariance matrix
  as well;
- `lower`, `upper`: Bounds placed on each type of parameter for the
  model. For the double exponential, one value should be provided for
  `"alpha"`, `"beta"`, `"omega"`, `"gamma"`, `"nu"`, and (if
  `parameters_only = FALSE`) `covariance`.

For example, creating a double exponential model with diagonal dynamic
matrices and a symmetric covariance matrix can be achieved as follows:

``` r
params <- generate_parameters(
  my_model,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = FALSE,
  lower = c(-100, -5, 0, 0, 0, 1e-2),
  upper = c(100, 5, 0.25, 1, 1, 10)
)
params
#>  [1] -46.89826737 -25.57522007  14.57067267   4.08207790  -2.98318069
#>  [6]   3.98389685   4.44675269   1.60797792   1.29114044   0.01544657
#> [11]   0.20597457   0.17655675   0.68702285   0.38410372   0.76984142
#> [16]   0.49769924   7.17900890   9.91914189   3.80655144   7.77667776
#> [21]   9.34770526   2.12930379
```

As one can see, the output of
[`generate_parameters()`](https://ndpvh.github.io/discounting-affect/reference/generate_parameters.html)
is a numerical vector containing different randomly generated values for
all specified parameters. What’s left is to assign these values to the
parameters of the model. This can be achieved with the function
[`fill()`](https://ndpvh.github.io/discounting-affect/reference/fill.html),
using the same specifications as previously:

``` r
my_model <- fill(
  my_model,
  params,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = FALSE
)
my_model
#> Model of class "double_exponential":
#> 
#> Dimension: 3
#> Number of predictors: 2
#> Number of parameters: 34
#> 
#> Parameters:
#>   alpha: |  -46.89827  |
#>          |  -25.57522  |
#>          |  14.57067  |
#> 
#>   beta: | 4.082078  4.446753 |
#>         | -2.983181  1.607978 |
#>         | 3.983897  1.29114 |
#> 
#>   omega: |  0.01544657  |
#> 
#>   gamma: | 0.2059746  0.00  0.00 |
#>          | 0.00  0.1765568  0.00 |
#>          | 0.00  0.00  0.6870228 |
#> 
#>   nu: | 0.3841037  0.00  0.00 |
#>       | 0.00  0.7698414  0.00 |
#>       | 0.00  0.00  0.4976992 |
#> 
#> 
#> Covariance: | 51.53817  71.20961  27.32727 |
#>             | 71.20961  185.769  57.66183 |
#>             | 27.32727  57.66183  33.5136 |
```

Note that the attribute `n` of the model has not been updated up to now,
providing us with the false information that our model contains 34
parameters instead of the 22 non-zero ones. To update this information,
one can use the
[`count_parameters()`](https://ndpvh.github.io/discounting-affect/reference/count_parameters.html)
function:

``` r
my_model@n <- count_parameters(
  my_model,
  dynamics = "isotropic",
  covariance = "symmetric",
  parameters_only = FALSE
)
```

By going through each step, we now have a model with randomly generated
parameters that conforms to a structure we are interested in. Within the
package, this same procedure is used in the
[`recovery()`](https://ndpvh.github.io/discounting-affect/reference/recovery.html)
function, which we’ll discuss later on.

### Simulating from a Model

Once a model has been defined, one can simulate data from the model
through the
[`predict()`](https://ndpvh.github.io/discounting-affect/reference/predict.html)
and
[`simulate()`](https://ndpvh.github.io/discounting-affect/reference/simulate.html)
functions, the former of which provides deterministic predictions
according to the model parameters and the latter of which adds
stochastic noise to these predictions. Critical to both functions is the
prespecification of the values for the predictor variable, as we detail
below.

#### Using `simulate()`

For
[`simulate()`](https://ndpvh.github.io/discounting-affect/reference/simulate.html),
the prespecification of the predictor variable(s) can be achieved
through specifying the `X` argument, for example:

``` r
data <- simulate(
  my_model,
  X = matrix(1:20, nrow = 10, ncol = 2)
)
data
#> An object of class "dataset"
#> 
#> Slot "Y": 10x3matrix
#>           [,1]       [,2]     [,3]
#> [1,]  7.033362 -17.708057 31.51762
#> [2,] 38.278309   6.914416 51.36866
#> [3,] 59.539690  39.133381 67.36761
#> [4,] 67.949514  -7.156479 62.12882
#> [5,] 85.550601  10.694502 80.99778
#> [6,] 99.084633  10.103330 91.65352
#> 
#> Slot "X": 10x2matrix
#>      [,1] [,2]
#> [1,]    1   11
#> [2,]    2   12
#> [3,]    3   13
#> [4,]    4   14
#> [5,]    5   15
#> [6,]    6   16
```

A few things are of note here. First, notice that `X` needs to be a
$N \times k$ matrix, where $N$ represents the number of time-points you
want to simulate and $k$ represents the number of independent variables.
Second, notice that the output is of a class
[`dataset`](https://ndpvh.github.io/discounting-affect/reference/dataset.html),
containing the (generated) values for the dependent variables in a slot
`Y` and the (provided) values for the independent variables in a slot
`X`. This class was introduced to ensure streamlined analyses within
this project. Similar to the models, one can access these slots through
the `@` operator:

``` r
data@Y 
#>             [,1]       [,2]      [,3]
#>  [1,]   7.033362 -17.708057  31.51762
#>  [2,]  38.278309   6.914416  51.36866
#>  [3,]  59.539690  39.133381  67.36761
#>  [4,]  67.949514  -7.156479  62.12882
#>  [5,]  85.550601  10.694502  80.99778
#>  [6,]  99.084633  10.103330  91.65352
#>  [7,] 101.764923  -1.897299  97.54509
#>  [8,] 118.775729  -5.663254 113.74452
#>  [9,] 137.892720  -7.700075 125.33525
#> [10,] 154.689899 -17.497790 132.46116
data@X 
#>       [,1] [,2]
#>  [1,]    1   11
#>  [2,]    2   12
#>  [3,]    3   13
#>  [4,]    4   14
#>  [5,]    5   15
#>  [6,]    6   16
#>  [7,]    7   17
#>  [8,]    8   18
#>  [9,]    9   19
#> [10,]   10   20
```

Alternatively, one can simulate data through a specification of a set of
generative functions for `X` through `Xfun` and the number of
observations through `N`. A function should be provided for each
predictor variable separately, providing these functions in a list.
Imagine, for example, the case where one predictor variable has only
positive values and the other negative values, then we may simulate data
as follows:

``` r
data <- simulate(
  my_model,
  Xfun = list(
    function(x) runif(x, min = -5, max = 0),
    function(x) runif(x, min = 0, max = 5)
  ),
  N = 10
)
data
#> An object of class "dataset"
#> 
#> Slot "Y": 10x3matrix
#>           [,1]        [,2]       [,3]
#> [1,] -40.53025  -2.7338175   7.191823
#> [2,] -40.46132  -4.1317609   4.121692
#> [3,] -24.14702  21.2414217  11.050690
#> [4,] -49.51990  -0.4008086 -10.616902
#> [5,] -51.17896 -12.5521233  -2.999879
#> [6,] -57.46489  30.6995599  -6.954664
#> 
#> Slot "X": 10x2matrix
#>           [,1]     [,2]
#> [1,] -3.000028 3.211441
#> [2,] -3.373239 4.381346
#> [3,] -1.214564 3.894573
#> [4,] -3.986539 3.986544
#> [5,] -1.444394 2.276372
#> [6,] -4.391540 2.050420
```

Notice that functions should be provided within the `Xfun` list.

#### Using `predict()`

For
[`predict()`](https://ndpvh.github.io/discounting-affect/reference/predict.html),
the prespecification of the predictor variable(s) is achieved through
providing an instance of the
[`dataset`](https://ndpvh.github.io/discounting-affect/reference/dataset.html)
class, owing to its use in the estimation functions
[`objective_function()`](https://ndpvh.github.io/discounting-affect/reference/objective_function.html)
and
[`fit()`](https://ndpvh.github.io/discounting-affect/reference/fit.html).
One should therefore first specify a
[`dataset`](https://ndpvh.github.io/discounting-affect/reference/dataset.html)
and then use
[`predict()`](https://ndpvh.github.io/discounting-affect/reference/predict.html)
to get the deterministic predictions from a particular model. For
example:

``` r
# Create an instance of the dataset class with predefined dimensionality
# corresponding to the model's
data <- dataset(
  Y = matrix(0, nrow = 10, ncol = my_model@d),
  X = cbind(
    runif(10, min = -5, max = 0),
    runif(10, min = 0, max = 5)
  )
)

# Provide the dataset to predict
data <- predict(
  my_model,
  data
)
data
#> An object of class "dataset"
#> 
#> Slot "Y": 10x3matrix
#>           [,1]       [,2]      [,3]
#> [1,] -39.16579 -10.432116  9.066897
#> [2,] -39.73867  -7.627996  9.959264
#> [3,] -38.14101  -7.002311 12.337440
#> [4,] -35.62368   4.616960  7.538564
#> [5,] -47.37604  16.655712 -3.443050
#> [6,] -44.15995  22.602799 -2.906789
#> 
#> Slot "X": 10x2matrix
#>            [,1]     [,2]
#> [1,] -2.7688234 4.280658
#> [2,] -1.1000756 1.956796
#> [3,] -0.5969048 1.902469
#> [4,] -2.9343790 4.477227
#> [5,] -4.6809576 3.221579
#> [6,] -3.3225625 3.705393
```

#### Troubleshooting

Both
[`predict()`](https://ndpvh.github.io/discounting-affect/reference/predict.html)
and
[`simulate()`](https://ndpvh.github.io/discounting-affect/reference/simulate.html)
are sensitive to problems in dimensions. For
[`simulate()`](https://ndpvh.github.io/discounting-affect/reference/simulate.html),
this is limited to the number of predictors not corresponding to the
model-specified number of predictors `k`, for example:

``` r
# Check the number of predictors assumed by the model
my_model@k
#> [1] 2

# Providing too few predictors through X or Xfun
simulate(
  my_model,
  X = numeric(10)
)
#> Error in `.local()`:
#> ! Dimensionality of "X" is smaller than the one required for the model. Provide a value for "X" with 2 columns to proceed.
simulate(
  my_model,
  Xfun = list(function(x) runif(x)),
  N = 10
)
#> Error in `.local()`:
#> ! Dimensionality of "X" is smaller than the one required for the model. Provide a value for "X" with 2 columns to proceed.
```

Note that the function will make dimensionalities match up when it has
sufficient information to do so:

``` r
# Providing too many predictors through X or Xfun
data <- simulate(
  my_model,
  X = cbind(
    rep(1, 10),
    rep(2, 10),
    rep(3, 10)
  )
)
#> Warning in .local(object, ...): Dimensionality of "X" is greater than
#> dimensionality provided in the model. Only using the first 2 columns in "X" for
#> the simulation.
data 
#> An object of class "dataset"
#> 
#> Slot "Y": 10x3matrix
#>           [,1]       [,2]     [,3]
#> [1,] -23.20606  -9.498049 26.78450
#> [2,] -21.66651 -19.214942 27.18717
#> [3,] -19.01776  -9.653805 33.79410
#> [4,] -16.71352 -22.008136 30.42871
#> [5,] -24.30921 -14.862184 27.77791
#> [6,] -29.74816 -25.152297 31.25597
#> 
#> Slot "X": 10x2matrix
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    1    2
#> [3,]    1    2
#> [4,]    1    2
#> [5,]    1    2
#> [6,]    1    2

data <- simulate(
  my_model,
  Xfun = list(
    function(x) rep(1, x),
    function(x) rep(2, x),
    function(x) rep(3, x)
  ),
  N = 10
)
#> Warning in .local(object, ...): Dimensionality of "X" is greater than
#> dimensionality provided in the model. Only using the first 2 columns in "X" for
#> the simulation.
data
#> An object of class "dataset"
#> 
#> Slot "Y": 10x3matrix
#>           [,1]      [,2]     [,3]
#> [1,] -37.33505 -24.92719 28.03760
#> [2,] -30.86217 -23.55292 24.04884
#> [3,] -24.02625 -13.73564 24.21014
#> [4,] -22.59340 -19.49103 22.90323
#> [5,] -34.56169 -21.86961 26.33807
#> [6,] -17.85003 -19.33573 31.53847
#> 
#> Slot "X": 10x2matrix
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    1    2
#> [3,]    1    2
#> [4,]    1    2
#> [5,]    1    2
#> [6,]    1    2
```

For
[`predict()`](https://ndpvh.github.io/discounting-affect/reference/simulate.html),
both the number of dependent variables `d` and the number of predictors
`k` can have a mismatch with the provided model, but that only the
latter will lead to an error. For example:

``` r
# Check the number of dependent and independent variables need by the model
my_model@d 
#> [1] 3
my_model@k 
#> [1] 2

# Mismatch regarding dimensions d: No error
data <- dataset(
  Y = matrix(0, nrow = 10, ncol = 1),
  X = matrix(0, nrow = 10, ncol = 2)
)
predict(
  my_model, 
  data
)
#> An object of class "dataset"
#> 
#> Slot "Y": 10x3matrix
#>           [,1]      [,2]     [,3]
#> [1,] -46.89827 -25.57522 14.57067
#> [2,] -46.89827 -25.57522 14.57067
#> [3,] -46.89827 -25.57522 14.57067
#> [4,] -46.89827 -25.57522 14.57067
#> [5,] -46.89827 -25.57522 14.57067
#> [6,] -46.89827 -25.57522 14.57067
#> 
#> Slot "X": 10x2matrix
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    0    0
#> [3,]    0    0
#> [4,]    0    0
#> [5,]    0    0
#> [6,]    0    0

# Mismatch regarding dimensions k: Error
data <- dataset(
  Y = matrix(0, nrow = 10, ncol = 3),
  X = matrix(0, nrow = 10, ncol = 1)
)
predict(
  my_model, 
  data
)
#> Error in `params[["beta"]] %*% X[i, ]`:
#> ! non-conformable arguments
```

Furthermore note that in the creation of the
[`dataset`](https://ndpvh.github.io/discounting-affect/reference/dataset.html),
the number of rows in `X` and `Y` may also mismatch:

``` r
dataset(
  Y = matrix(0, nrow = 10, ncol = 2),
  X = matrix(0, nrow = 20, ncol = 2)
)
#> Error in `dataset()`:
#> ! Number of rows in "Y" and "X" are not the same.
```

### Estimating a Model

Imagine that you have the following data saved in a variable `data`:

``` r
dim(data)
#> [1] 100   3
head(data)
#>   positive_affect negative_affect outcome
#> 1            0.38            0.56   -4.69
#> 2            0.33            0.57   -2.56
#> 3            0.41            0.55    1.46
#> 4            0.62            0.43    8.16
#> 5            0.45            0.53   -5.97
#> 6            0.66            0.43    7.97
```

Then one needs to use a particular workflow to estimate a particular
discounting model.

The first step involves transforming a `data.frame` to a
[`dataset`](https://ndpvh.github.io/discounting-affect/reference/dataset.html),
which can be achieved through using its constructor:

``` r
data <- dataset(
  data, 
  y_cols = c("positive_affect", "negative_affect"),
  x_cols = "outcome"
)
data
#> An object of class "dataset"
#> 
#> Slot "Y": 100x2matrix
#>      positive_affect negative_affect
#> [1,]            0.38            0.56
#> [2,]            0.33            0.57
#> [3,]            0.41            0.55
#> [4,]            0.62            0.43
#> [5,]            0.45            0.53
#> [6,]            0.66            0.43
#> 
#> Slot "X": 100x1matrix
#>      outcome
#> [1,]   -4.69
#> [2,]   -2.56
#> [3,]    1.46
#> [4,]    8.16
#> [5,]   -5.97
#> [6,]    7.97
```

Next, one should specify the model to be estimated. In this example, we
will estimate a quasi-hyperbolic discounting model, specifying an empty
model with dimensions $d = 2$ and $k = 1$:

``` r
my_model <- quasi_hyperbolic(
  d = 2, 
  k = 1
)
```

Finally, we can use the
[`fit()`](https://ndpvh.github.io/discounting-affect/reference/fit.html)
function to estimate the specified model on the data, further specifying
the structure of the dynamics and the covariance matrix:

``` r
result <- fit(
  my_model,
  data, 
  dynamics = "isotropic",
  covariance = "symmetric",
  trace = FALSE
)
```

Under the hood, we use least-squares estimation as specified through the
[`objective_function()`](https://ndpvh.github.io/discounting-affect/reference/objective_function.html)
method. The result of the estimation is a named list containing the
following information:

- `"model"`: Model with the estimated parameters;
- `"fit"`: Result of the optimization procedure as provided by the
  specified optimizer;
- `"objective"`: Value of the least-squares at the end of the
  optimization procedure;
- `"residuals"`: Residuals of the model;
- `"parameters"`: Named vector of parameter values.

The estimated model can thus be accessed through calling:

``` r
result$model
#> Model of class "quasi_hyperbolic":
#> 
#> Dimension: 2
#> Number of predictors: 1
#> Number of parameters: 11
#> 
#> Parameters:
#>   alpha: |  0.5026342  |
#>          |  0.4935457  |
#> 
#>   beta: | 0.02530127 |
#>         | -0.0124281 |
#> 
#>   nu: | 0.8784477  0.00 |
#>       | 0.00  0.872398 |
#> 
#>   kappa: | 0.8600623  0.00 |
#>          | 0.00  0.892485 |
#> 
#> 
#> Covariance: | 8.092268e-05  -2.067372e-05 |
#>             | -2.067372e-05  6.060043e-05 |
```

#### Optimizer

One may have noticed the `trace = FALSE` argument specified in the code
above. This argument is not native to `discounting`, but stems from one
of the optimizers that is used to fit the models to data, specifically
from the package `DEoptim`.

Within this package, we allow users to use one of two packages to
estimate their models, them being `DEoptim` (a package around the
Differential Evolution algorithm; Mullen et al., 2011) and `nloptr` (an
R interface around a set of algorithms provided by NLopt, see
<https://nlopt.readthedocs.io/en/latest/>; Johnson, 2008), specifying
the package they want to use through the `optimizer` argument (either
`"DEoptim"` or `"nloptr"`).

To allow some flexibility to users, they can provide additional
package-specific arguments controlling the estimation procedure
belonging to either `DEoptim.control` for `DEoptim` or `opts` for
`nloptr`. For example, we can estimate the same model through the BOBYQA
algorithm in `nloptr` in the following way:

``` r
result <- fit(
  my_model,
  data, 

  # Define the structure of the model
  dynamics = "isotropic",
  covariance = "symmetric",

  # Define lower and upper bounds for the estimation
  lower = c(0, -1, 0, 0),
  upper = c(1, 1, 1, 1),

  # Define the optimizer and its parameters
  optimizer = "nloptr",
  algorithm = "NLOPT_LN_BOBYQA",
  xtol_rel = 1e-10, 
  ftol_rel = 1e-10,
  maxeval = 2500
)
result$model
#> Model of class "quasi_hyperbolic":
#> 
#> Dimension: 2
#> Number of predictors: 1
#> Number of parameters: 11
#> 
#> Parameters:
#>   alpha: |  0.5033122  |
#>          |  0.4944639  |
#> 
#>   beta: | 0.02529582 |
#>         | -0.01247399 |
#> 
#>   nu: | 0.8785277  0.00 |
#>       | 0.00  0.8772852 |
#> 
#>   kappa: | 0.8604844  0.00 |
#>          | 0.00  0.8818714 |
#> 
#> 
#> Covariance: | 8.091217e-05  -2.078007e-05 |
#>             | -2.078007e-05  5.880502e-05 |
```

Note that in this version, we specified lower and upper bounds for the
parameters via the `lower` and `upper` arguments. These are specific to
the `discounting` package and are the same as the ones previously used
by
[`generate_parameters()`](https://ndpvh.github.io/discounting-affect/reference/generate_parameters.html)
(see also
[`get_bounds()`](https://ndpvh.github.io/discounting-affect/reference/get_bounds.html)).

### Performing a Recovery Study

Before estimating models on data, one first needs to check whether one
is able to estimate these parameters reliably. One aay of doing this is
through performing a *recovery study*, a repeated simulation-estimation
procedure for random sets of parameters. For this, the `discounting`
package provides the
[`recovery()`](https://ndpvh.github.io/discounting-affect/reference/recover.html)
method, using the same function arguments as
[`simulate()`](https://ndpvh.github.io/discounting-affect/reference/simulate.html)
and
[`fit()`](https://ndpvh.github.io/discounting-affect/reference/fit.html):

``` r
set.seed(1)

# Define the model for which to run the recovery study
my_model <- exponential(d = 2, k = 1)

# Complete a recovery study with 100 repetitions
result <- recovery(
  my_model, 
  iterations = 100, 

  # Specifics for the model
  dynamics = "isotropic",
  covariance = "isotropic",

  # Specifics for the simulation
  Xfun = function(x) rnorm(x),
  N = 200,

  # Specifics for the estimation
  optimizer = "nloptr",
  algorithm = "NLOPT_LN_BOBYQA",
  xtol_rel = 1e-10, 
  ftol_rel = 1e-10,
  maxeval = 2500
)
```

The result of the recovery study is a named list with the following
slots:

- `"simulate"`: Dataframe containing the simulated parameters;
- `"fit"`: Dataframe containing the estimated parameters.

Comparing both dataframes can then lead to insight in how well the model
can be recovered, for example by creating a scatterplot showing
simulated vs estimated values of the parameters:

``` r
# Define the window for the plots
par(mfrow = c(1, 2))

# Loop over all columns in the simulated parameters
for(i in seq_len(ncol(result$simulate))) {
  # Extract the simulated and estimated parameter values for this column
  x <- result$simulate[, i]
  y <- result$fit[, i]

  # Create a plot displaying their relationship
  plot(
    x, 
    y,
    xlab = "Simulated Value",
    ylab = "Estimated Value",
    main = colnames(result$simulate)[i],
    type = "n",
    xlim = range(c(x, y)),
    ylim = range(c(x, y)),
    cex.main = 2.5,
    cex.lab = 2
  )

  abline(
    a = 0, 
    b = 1,
    lwd = 4,
    col = "black"
  )

  points(
    x, 
    y,
    pch = 21,
    bg = "cornflowerblue",
    col = "black",
    cex = 3,
    lwd = 2
  )

  text(
    x = min(c(x, y)) + 0.025 * diff(range(c(x, y))),
    y = max(c(x, y)) - 0.05 * diff(range(c(x, y))),
    label = paste0(
      "r = ", 
      format(round(cor(x, y), 2), nsmall = 2)
    ),
    cex = 2,
    adj = 0
  )
}
```

![One sees a scatterplot displaying the relationship between the values
of the simulated parameters vs those of the estimated
parameters.](getting_started_files/figure-html/unnamed-chunk-39-1.png)![One
sees a scatterplot displaying the relationship between the values of the
simulated parameters vs those of the estimated
parameters.](getting_started_files/figure-html/unnamed-chunk-39-2.png)![One
sees a scatterplot displaying the relationship between the values of the
simulated parameters vs those of the estimated
parameters.](getting_started_files/figure-html/unnamed-chunk-39-3.png)![One
sees a scatterplot displaying the relationship between the values of the
simulated parameters vs those of the estimated
parameters.](getting_started_files/figure-html/unnamed-chunk-39-4.png)

One may want to examine how well two models are distinguishable from
each other, tackling model mimicry (Navarro et al., 2004; Wagenmakers et
al., 2004). For this, one can specify the optional `fit_model` argument:

``` r
set.seed(1)

# Define the model for which to run the recovery study
my_model <- exponential(d = 2, k = 1)
other_model <- quasi_hyperbolic(d = 2, k = 1)

# Complete a recovery study with 100 repetitions
result <- recovery(
  my_model, 
  fit_model = other_model,
  iterations = 100, 

  # Specifics for the simulated model
  sim_dynamics = "isotropic",
  sim_covariance = "isotropic",

  # Specifics for the fitted model
  fit_dynamics = "isotropic",
  fit_covariance = "isotropic",

  # Specifics for the simulation
  Xfun = function(x) rnorm(x),
  N = 200,

  # Specifics for the estimation
  optimizer = "nloptr",
  algorithm = "NLOPT_LN_BOBYQA",
  xtol_rel = 1e-10, 
  ftol_rel = 1e-10,
  maxeval = 2500,

  # Specification of statistics interesting for distinguishability purposes
  fx = list("aic" = aic)
)
```

The output of this function is the same as previously, with the sole
difference being that the `data.frame` in the `"simulate"` slot contains
the parameters of the simulation model whereas the `data.frame` in the
`"fit"` slot contains the parameters of the fitted model.

Several things are of note here. First of all, note that you can specify
different structures of the dynamic parameters and the covariances for
the simulated models (`sim_dynamics` and `sim_covariance`) and for the
model to be estimated (`fit_dynamics` and `fit_covariance`).
Additionally note that within a distinguishability study, one is often
interested in examining four scenarios regarding `<simulated model>` -
`<fitted model>`: `my_model` - `my_model`, `my_model` - `other_model`,
`other_model` - `my_model`, and `other_model` - `other_model`. This
allows one to make graphs displaying the model’s distinguishability when
one or the other model are the “true” model (see Navarro et al., 2004).
Finally, note that within a distinguishability study, it is typically
not straightforward to compare values of parameters for the
simulated/estimated models in a meaningful way. Instead, it is useful to
provide a list of aggregating functions to the argument `fx`, allowing
one to examine the distinguishability of the models with regard to
summary statistics quantifying the fit or theoretically meaningful
constructs.

## Scripts

The `scripts` directory contains the scripts that we used for our
analyses, as well as any results (in `scripts/results`), figures (in
`scripts/figures`), and data that we used (in `scripts/data`). The
purpose of each script can be summarized as follows:

- `recovery.R`: Contains the recovery study for the models of interest,
  each varying in dimensionalities $d$ and $k$ ranging from 1 to 3.

For the results of our analyses, we refer the interested reader to the
article published alongside this code.

## References

Evers, K., C. & Vanhasbroeck, N. (2026). *Sharing computational models
as reproducible and user-friendly packages: A tutorial in R.* Manuscript
in preparation.

Johnson, S. G. (2008). *The NLopt nonlinear-optimization package.*
<http://github.com/stevengj/nlopt>

Mullen, K. M., Ardia, D., Gil, D. L., Windover, D., & Cline, J. (2011).
DEoptim: An R package for global optimization by Differential Evolution.
*Journal of Statistical Software, 40*(6), 1–26. doi:
[10.18637/jss.v040.i06](https://doi.org/10.18637/jss.v040.i06)

Navarro, D. J., Pitt, M. A., & Myung, J. (2004). Assessing the
distinguishability of models and the informativeness of data. *Cognitive
Psychology, 49*(1), 47-84. doi:
[10.1016/j.cogpsych.2003.11.001](https://doi.org/10.1016/j.cogpsych.2003.11.001)

Powell, M. J. D. (2009). *The BOBYQA algorithm for bound constrained
optimization without derivatives.* Technical Report NA2009/06,
Department of Applied Mathematics and Theoretical Physics, Cambridge
England.

Rutledge, R. B., Skandali, N., Dayan, P., & Dolan, R. J. (2014). A
computational and neural model of momentary subjective well-being.
*Proceedings of the National Academy of Sciences of the United States of
America, 111*(33), 12252–12257. doi:
[10.1073/pnas.1407535111](https://doi.org/10.1073/pnas.1407535111)

Vanhasbroeck, N., Loossens, T., & Tuerlinckx, F. (2024). Two peas in a
pod: Discounting models as a special case of the VARMAX. *Journal of
Mathematical Psychology*, Article 102856. doi:
[10.1016/j.jmp.2024.102856](https://doi.org/10.1016/j.jmp.2024.102856)

Wagenmakers, E.-J., Ratcliff, R., Gomez, P., & Iverson, G. J. (2004).
Assessing model mimicry using the parametric bootstrap. *Journal of
Mathematical Psychology, 48*(1), 28-50. doi:
[10.1016/j.jmp.2003.11.004](https://doi.org/10.1016/j.jmp.2003.11.004)

Wickham, H. (2019). *Advanced R, 2nd edition.* CRC Press. Available
online at <https://adv-r.hadley.nz/>

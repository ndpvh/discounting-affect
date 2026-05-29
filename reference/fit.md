# Fit a model to data

This method enables the user to fit a model (defined in `model`) on a
particular dataset (defined in `data`). The estimation procedure makes
use of numerical optimization using either
[`DEoptim`](https://rdrr.io/pkg/DEoptim/man/DEoptim.html) or
[`nloptr`](https://astamm.github.io/nloptr/reference/nloptr.html), as
specified by the user. Estimation proceeds through an optimization
according to the output of the objective function of the provided model,
as defined through
[`objective_function`](https://github.com/ndpvh/discounting-affect/reference/objective_function.md),
thus using least-squares as optimization standard.

## Usage

``` r
fit(model, data, ...)

# S4 method for class 'model,dataset'
fit(
  model,
  data,
  dynamics = "isotropic",
  covariance = "symmetric",
  optimizer = "DEoptim",
  lower = NULL,
  upper = NULL,
  ...
)
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

- ...:

  Arguments passed on to the control parameters of the optimizer, either
  to
  [`DEoptim.control`](https://rdrr.io/pkg/DEoptim/man/DEoptim.control.html)
  or the `opts` argument of
  [`nloptr`](https://astamm.github.io/nloptr/reference/nloptr.html).

- dynamics:

  Character denoting the structure of the dynamical matrices. Can either
  be `"anisotropic"` (completely free), `"symmetric"` (symmetric around
  the diagonal), and `"isotropic"` (diagonal). Note that this influences
  different parameters for different models, namely \\\Gamma\\ for the
  exponential discounting model, \\N\\ and \\K\\ for the
  quasi-hyperbolic discounting model, and \\\Gamma\\ and \\N\\ for the
  double-exponential discounting model. Defaults to `"isotropic"`.

- covariance:

  Character denoting the structure of covariance matrix. Can either by
  `"symmetric"` (symmetric around the diagonal) and `"isotropic"`
  (diagonal). Defaults to `"symmetric"`.

- optimizer:

  Character denoting the optimizer to use for the estimation. Can either
  be `"DEoptim"` for the differential evolution algorithm in
  [`DEoptim`](https://rdrr.io/pkg/DEoptim/man/DEoptim.html) or
  `"nloptr"` for the library implemented in
  [`nloptr`](https://astamm.github.io/nloptr/reference/nloptr.html).
  Defaults to `"DEoptim"`.

- lower, upper:

  Numeric vector containing lower and upper bounds for the parameters in
  the estimation routine. Uses the same defaults as
  [`get_bounds`](https://github.com/ndpvh/discounting-affect/reference/get_bounds.md).

## Value

An named list containing an instance of the
[`model-class`](https://github.com/ndpvh/discounting-affect/reference/model-class.md)
with the estimated parameters (`"model"`), the results of the
optimization procedure (`"fit"`), the value of the objective after
ending the optimization procedure (`"objective"`), the residuals of the
model (`"residuals"`), and a named vector containing the values of the
estimated parameters linked to a character vector explaining their
content (`"parameters"`).

## Details

Note that currently, least-squares estimation is assumed, meaning that
the covariance matrix is left out of the objective function. If
maximum-likelihood were needed instead, then this function would need to
change.

## Examples

``` r
# Simulate data to use for this example
data <- simulate(
  quasi_hyperbolic(
    parameters = list(
      "alpha" = c(1, -1) ,
      "beta" = matrix(2, nrow = 2, ncol = 2),
      "nu" = diag(2) * 0.75,
      "kappa" = diag(2) * 0.5
    ),
    covariance = matrix(c(1, 0.25, 0.25, 1), nrow = 2, ncol = 2)
  ),
  X = matrix(rnorm(200), nrow = 100, ncol = 2)
)

# Evaluate the objective function for an exponential model with a particular
# set of parameters
fit(
  exponential(d = 2, k = 2),
  data,
  dynamics = "isotropic",
  covariance = "isotropic",
  itermax = 50,
  trace = FALSE
)
#> Warning: unknown names in control: itermax
#> $model
#> Model of class "exponential":
#> 
#> Dimension: 2
#> Number of predictors: 2
#> Number of parameters: 10
#> 
#> Parameters:
#>   alpha: |  0.9993453  |
#>          |  -0.9988963  |
#> 
#>   beta: | 1.615939  1.897179 |
#>         | 1.560964  1.731107 |
#> 
#>   gamma: | 0.5614686  0.00 |
#>          | 0.00  0.4757407 |
#> 
#> 
#> Covariance: | 1.351938  0.00 |
#>             | 0.00  1.847991 |
#> 
#> $fit
#> $fit$par
#> [1]  0.9993453 -0.9988963  1.6159386  1.5609643  1.8971790  1.7311073  0.5614686
#> [8]  0.4757407
#> 
#> $fit$value
#> [1] 323.2299
#> 
#> $fit$counts
#> function gradient 
#>      501       NA 
#> 
#> $fit$convergence
#> [1] 1
#> 
#> $fit$message
#> NULL
#> 
#> 
#> $objective
#> [1] 323.2299
#> 
#> $residuals
#>               [,1]        [,2]
#>   [1,]  1.12084873 -1.29182154
#>   [2,] -0.80096404 -0.93853756
#>   [3,]  1.24567854  1.42123293
#>   [4,] -1.05710617 -0.11921886
#>   [5,] -2.24462103 -0.13645207
#>   [6,]  1.49885212  1.49706934
#>   [7,]  0.47171510  2.03607510
#>   [8,]  0.71236496  0.72012323
#>   [9,]  3.19016826  2.85340187
#>  [10,]  0.65589020  0.14373049
#>  [11,] -0.71869543 -1.62186832
#>  [12,]  1.26559846  1.08285740
#>  [13,] -1.01917993 -1.33553449
#>  [14,]  0.02008725 -1.33342945
#>  [15,] -0.27628149 -1.75604105
#>  [16,] -1.23321199  2.18032344
#>  [17,] -0.72765863 -1.60023765
#>  [18,] -0.50402473 -0.07066836
#>  [19,]  0.77485981  0.69698250
#>  [20,]  1.76919399  1.37357146
#>  [21,]  2.37886028  3.04907584
#>  [22,] -0.91995348 -0.48454894
#>  [23,] -1.76781301 -3.57563896
#>  [24,]  1.54883169  0.56452132
#>  [25,]  1.95619156  0.71992454
#>  [26,]  0.22761307 -2.16273753
#>  [27,] -1.54468755 -0.41568980
#>  [28,] -1.98225337 -0.74707982
#>  [29,]  0.44895140  0.05261328
#>  [30,] -1.26881734 -1.76893099
#>  [31,]  0.66102649 -1.27327799
#>  [32,] -1.04834471 -2.40638529
#>  [33,]  0.53714415  1.99957802
#>  [34,] -0.12844582 -1.37077320
#>  [35,]  0.33669958 -1.16609447
#>  [36,]  0.78468382  1.91128209
#>  [37,] -1.26354370 -0.01879470
#>  [38,]  0.22223182 -0.61872588
#>  [39,]  0.13225641  1.19518889
#>  [40,]  0.40253407 -0.38901038
#>  [41,] -0.27872074  0.82511230
#>  [42,] -0.24605372  0.45635097
#>  [43,]  0.51901432 -0.50888722
#>  [44,] -0.85432158 -1.26991585
#>  [45,]  2.05107417 -0.51355682
#>  [46,]  1.01446593  1.05744352
#>  [47,] -1.31282952  0.91286276
#>  [48,]  1.47548513  0.46560484
#>  [49,] -1.87660953  0.73825540
#>  [50,] -0.78477267 -0.99366902
#>  [51,]  0.91646800  1.26119388
#>  [52,]  0.18357289  1.29256979
#>  [53,] -0.24175351 -0.24282942
#>  [54,]  0.61421390  0.82618898
#>  [55,]  1.34060886  1.47314335
#>  [56,] -0.24258178  0.79406723
#>  [57,]  1.42087805  1.25906281
#>  [58,] -1.41369686  1.39744050
#>  [59,] -2.47763926 -1.28649050
#>  [60,] -1.02316497 -0.75478827
#>  [61,] -1.07754435 -3.60597888
#>  [62,] -0.17495051 -0.93910628
#>  [63,]  0.71391448 -0.76663563
#>  [64,] -1.84045134 -0.60122866
#>  [65,]  0.27241383  0.20358605
#>  [66,]  1.62080290  1.26023377
#>  [67,] -0.35186286 -1.13795324
#>  [68,]  0.51172208 -1.41041615
#>  [69,] -1.88299371 -0.23550807
#>  [70,]  0.08648769 -0.77397418
#>  [71,] -0.99567204 -1.90854472
#>  [72,] -0.95196489 -0.29741586
#>  [73,]  0.32641312 -1.84989928
#>  [74,] -2.35407468 -3.22968424
#>  [75,] -0.87514474 -1.78980918
#>  [76,]  0.93600692 -1.07311199
#>  [77,] -0.50311996 -0.37458225
#>  [78,]  0.09914650  0.00922252
#>  [79,] -0.07771611  0.06112484
#>  [80,] -0.84560801 -0.09887305
#>  [81,] -0.38339100 -0.91240594
#>  [82,] -0.22213204 -0.15256514
#>  [83,] -1.02758891 -2.13368329
#>  [84,]  1.23953874 -1.51113882
#>  [85,] -1.55491306 -1.80174771
#>  [86,]  1.72691964  3.06556974
#>  [87,]  1.07867225  0.12463686
#>  [88,]  1.03296793  0.50912453
#>  [89,]  0.91734435 -0.26385199
#>  [90,]  0.02554548 -1.90955885
#>  [91,] -0.42962179 -0.80777127
#>  [92,] -1.90855183 -0.17132230
#>  [93,] -0.23137246  0.71215998
#>  [94,]  0.37649249 -1.49656670
#>  [95,] -0.48262961 -2.07702586
#>  [96,] -2.55111634  1.06527601
#>  [97,]  0.39417331 -0.65340717
#>  [98,] -0.66395736 -1.25014774
#>  [99,]  0.18160020 -0.18138904
#> [100,] -0.45371715  0.13196903
#> 
#> $parameters
#>    alpha_1    alpha_2    beta_11    beta_21    beta_12    beta_22   gamma_11 
#>  0.9993453 -0.9988963  1.6159386  1.5609643  1.8971790  1.7311073  0.5614686 
#>   gamma_22   sigma_11   sigma_22 
#>  0.4757407  1.3519384  1.8479910 
#> 
```

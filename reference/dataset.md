# Constructor for the [`dataset-class`](https://github.com/ndpvh/discounting-affect/reference/dataset-class.md)

Takes in a `data.frame` and, based on the value of the arguments,
constructs an instance of the
[`dataset-class`](https://github.com/ndpvh/discounting-affect/reference/dataset-class.md).
Through this type of class definition, it is ensured that the
estimation, simulation, cross-validation, and recovery routines can all
make use of the same data structure, making their definition easier
within this package. Note that this class is only defined on the
within-subject level, meaning that the data provided to this constructor
should be on this level.

## Usage

``` r
dataset(
  data = NULL,
  y_cols = NULL,
  x_cols = NULL,
  Y = NULL,
  X = NULL,
  sorting_variable = NULL
)
```

## Arguments

- data:

  A `data.frame` containing the dependent and independent variables of
  interest for a single person. Defaults to `NULL`, in which case an
  empty instance of the
  [`dataset-class`](https://github.com/ndpvh/discounting-affect/reference/dataset-class.md)
  will be created.

- y_cols:

  A character vector denoting the names of the columns for the dependent
  variables. Defaults to `NULL`, in which case an error will be thrown
  asking for more information.

- x_cols:

  A character vector denoting the names of the columns for the
  independent variables. Defaults to `NULL`, in which case an error will
  be thrown asking for more information.

- Y:

  Numeric vector or numeric matrix containing the values for the
  dependent variable(s). Defaults to `NULL`, in which case it will
  become an empty matrix of length \\N x 1\\. Ignored if `data` is
  specified instead.

- X:

  Numeric vector or numeric matrix containing the values for the
  independent variable(s). Defaults to `NULL`, in which case the `data`
  should be specified instead.

- sorting_variable:

  Character denoting the columnn name of a variable in the `data`
  according to which the values for `Y` and `X` should be sorted.
  Defaults to `NULL`, meaning that no sorting should happen.

## Value

Instance of the
[`dataset-class`](https://github.com/ndpvh/discounting-affect/reference/dataset-class.md).

## See also

[`dataset-class`](https://github.com/ndpvh/discounting-affect/reference/dataset-class.md)

## Examples

``` r
# Create a data.frame for reference
my_data <- data.frame(
  DV_1 = rep(1, each = 10),
  DV_2 = rep(2, each = 10),
  IV_1 = rep(3, each = 10),
  IV_2 = rep(4, each = 10),
  IV_3 = rep(5, each = 10)
)

# Create a dataset with only a single DV and IV
dataset(
  data = my_data, 
  y_cols = "DV_1",
  x_cols = "IV_1"
)
#> An object of class "dataset"
#> 
#> Slot "Y": 10x1matrix
#>      DV_1
#> [1,]    1
#> [2,]    1
#> [3,]    1
#> [4,]    1
#> [5,]    1
#> [6,]    1
#> 
#> Slot "X": 10x1matrix
#>      IV_1
#> [1,]    3
#> [2,]    3
#> [3,]    3
#> [4,]    3
#> [5,]    3
#> [6,]    3

# Create a dataset with multiple DV and IV
dataset(
  data = my_data,
  y_cols = c("DV_1", "DV_2"),
  x_cols = c("IV_1", "IV_2", "IV_3")
)
#> An object of class "dataset"
#> 
#> Slot "Y": 10x2matrix
#>      DV_1 DV_2
#> [1,]    1    2
#> [2,]    1    2
#> [3,]    1    2
#> [4,]    1    2
#> [5,]    1    2
#> [6,]    1    2
#> 
#> Slot "X": 10x3matrix
#>      IV_1 IV_2 IV_3
#> [1,]    3    4    5
#> [2,]    3    4    5
#> [3,]    3    4    5
#> [4,]    3    4    5
#> [5,]    3    4    5
#> [6,]    3    4    5
 
```

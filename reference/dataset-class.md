# Dataset Class

This class contains the standard format for the dataset to be used
within this project, ensuring a standardized approach for all
computations.

## Slots

- `Y`:

  Numeric matrix of observations to be fitted, that is the dependent
  variables

- `X`:

  Numeric matrix of the stimuli that should be related to `Y`, that is
  the independent variables

- `N`:

  Integer denoting the number of datapoints contained in `Y` and `X`.
  This value is automatically generated in the constructor of the
  dataset (see
  [`dataset`](https://github.com/ndpvh/discounting-affect/reference/dataset.md)).

## See also

[`dataset`](https://github.com/ndpvh/discounting-affect/reference/dataset.md)

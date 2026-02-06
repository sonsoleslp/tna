# Import Wide Format Sequence Data as Long Format Sequence Data

This function transforms wide format data where features are in separate
columns into a long format suitable for sequence analysis. It creates
windows of data based on row order and generates sequence order within
these windows.

## Usage

``` r
import_data(data, cols, id_cols, window_size = 1, replace_zeros = TRUE)
```

## Arguments

- data:

  A `data.frame` in wide format.

- cols:

  An `expression` giving a tidy selection of column names to be
  transformed into long format (actions). This can be a vector of column
  names (e.g., `c(feature1, feature2)`) or a range specified as
  `feature1:feature6` (without quotes) to include all columns from
  'feature1' to 'feature6' in the order they appear in the data frame.
  For more information on tidy selections, see
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).

- id_cols:

  An `expression` giving a tidy selection of column names that uniquely
  identify each observation (IDs).

- window_size:

  An `integer` specifying the size of the window for sequence grouping.
  Default is 1 (each row is a separate window).

- replace_zeros:

  A `logical` value indicating whether to replace 0s in `cols` with
  `NA`. The default is `TRUE`.

## Value

A `data.frame` in long format with added columns for window and sequence
order.

## See also

Other data:
[`import_onehot()`](http://sonsoles.me/tna/reference/import_onehot.md),
[`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md),
[`print.tna_data()`](http://sonsoles.me/tna/reference/print.tna_data.md),
[`simulate.tna()`](http://sonsoles.me/tna/reference/simulate.tna.md)

## Examples

``` r
data <- data.frame(
  ID = c("A", "A", "B", "B"),
  Time = c(1, 2, 1, 2),
  feature1 = c(10, 0, 15, 20),
  feature2 = c(5, 8, 0, 12),
  feature3 = c(2, 4, 6, 8),
  other_col = c("X", "Y", "Z", "W")
)

# Using a vector
long_data1 <- import_data(
  data = data,
  cols = c(feature1, feature2),
  id_cols = c("ID", "Time"),
  window_size = 2,
  replace_zeros = TRUE
)

# Using a column range
long_data2 <- import_data(
  data = data,
  cols = feature1:feature3,
  id_cols = c("ID", "Time"),
  window_size = 2,
  replace_zeros = TRUE
)
```

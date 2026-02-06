# Import One-Hot Data and Create a Co-Occurrence Network Model

Import One-Hot Data and Create a Co-Occurrence Network Model

## Usage

``` r
import_onehot(data, cols, window = 1L)
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

- window:

  An `integer` specifying the size of the window for sequence grouping.
  Default is 1 (each row is a separate window). Can also be a
  `character` string giving a name of the column in `data` whose levels
  define the windows.

## Value

A `tna` object for the co-occurrence model.

## See also

Other data:
[`import_data()`](http://sonsoles.me/tna/reference/import_data.md),
[`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md),
[`print.tna_data()`](http://sonsoles.me/tna/reference/print.tna_data.md),
[`simulate.tna()`](http://sonsoles.me/tna/reference/simulate.tna.md)

## Examples

``` r
d <- data.frame(
  window = gl(100, 5),
  feature1 = rbinom(500, 1, prob = 0.33),
  feature2 = rbinom(500, 1, prob = 0.25),
  feature3 = rbinom(500, 1, prob = 0.50)
)
model <- import_onehot(d, feature1:feature3, window = "window")
```

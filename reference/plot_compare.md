# Plot the Difference Network Between Two Models

Plots the difference network between model `x` and model `y`. The edges
are computed from subtracting the two models. The pie chart is the
difference in initial probabilities between model `x` and model `y`.
Green color indicates that `x`is greater than `y`and red indicates
otherwise.

## Usage

``` r
plot_compare(x, ...)

# S3 method for class 'tna'
plot_compare(x, y, posCol = "#009900", negCol = "red", ...)
```

## Arguments

- x:

  A `tna` object. This is the the principal model.

- ...:

  Additional arguments passed to
  [`cograph::splot()`](http://sonsoles.me/cograph/reference/splot.md).

- y:

  A `tna` object. This is the model subtracted from the principal model.

- posCol:

  Color for plotting edges and pie when the first group has a higher
  value. See
  [`cograph::splot()`](http://sonsoles.me/cograph/reference/splot.md).

- negCol:

  Color for plotting edges and pie when the second group has a higher
  value. See
  [`cograph::splot()`](http://sonsoles.me/cograph/reference/splot.md).

## Value

A `cograph_network` object displaying the difference network between the
two models.

## See also

Model comparison functions
[`compare()`](http://sonsoles.me/tna/reference/compare.md),
[`compare.group_tna()`](http://sonsoles.me/tna/reference/compare.group_tna.md),
[`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_comparison()`](http://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare.group_tna()`](http://sonsoles.me/tna/reference/plot_compare.group_tna.md),
[`print.tna_comparison()`](http://sonsoles.me/tna/reference/print.tna_comparison.md),
[`print.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)

## Examples

``` r
model_x <- tna(group_regulation[group_regulation[, 1] == "plan", ])
model_y <- tna(group_regulation[group_regulation[, 1] != "plan", ])
plot_compare(model_x, model_y)

```

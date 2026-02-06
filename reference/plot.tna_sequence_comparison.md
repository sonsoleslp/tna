# Plot a Sequence Comparison

Plot a Sequence Comparison

## Usage

``` r
# S3 method for class 'tna_sequence_comparison'
plot(
  x,
  n = 10,
  legend = TRUE,
  cells = TRUE,
  text_color = "white",
  digits = 2L,
  ...
)
```

## Arguments

- x:

  A `tna_sequence_comparison` object.

- n:

  An `integer` giving the number of patterns to plot. The default is
  `10`.

- legend:

  A `logical` value indicating whether to show the color scale legend.
  The default is `TRUE`.

- cells:

  A `logical` value indicating whether to display the numeric values in
  each cell. The default is `TRUE`.

- text_color:

  A `character` string specifying the text color to use for the cell
  values. The default is `"white"`.

- digits:

  An `integer` specifying the number of digits for the cell values.

- ...:

  Not used.

## Value

A `ggplot` object.

## See also

Model comparison functions
[`compare()`](http://sonsoles.me/tna/reference/compare.md),
[`compare.group_tna()`](http://sonsoles.me/tna/reference/compare.group_tna.md),
[`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_comparison()`](http://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md),
[`plot_compare.group_tna()`](http://sonsoles.me/tna/reference/plot_compare.group_tna.md),
[`print.tna_comparison()`](http://sonsoles.me/tna/reference/print.tna_comparison.md),
[`print.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)

## Examples

``` r
group <- c(rep("High", 1000), rep("Low", 1000))
comp <- compare_sequences(group_regulation, group)
plot(comp)

```

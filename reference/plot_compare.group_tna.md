# Plot the Difference Network Between Two Groups

Plot the Difference Network Between Two Groups

## Usage

``` r
# S3 method for class 'group_tna'
plot_compare(x, i = 1L, j = 2L, ...)
```

## Arguments

- x:

  A `group_tna` object.

- i:

  An `integer` index or the name of the principal cluster as a
  `character` string.

- j:

  An `integer` index or the name of the secondary cluster as a
  `character` string.

- ...:

  Additional arguments passed to
  [`plot_compare.tna()`](http://sonsoles.me/tna/reference/plot_compare.md).

## Value

A `qgraph` object displaying the difference network between the two
clusters

## See also

Model comparison functions
[`compare()`](http://sonsoles.me/tna/reference/compare.md),
[`compare.group_tna()`](http://sonsoles.me/tna/reference/compare.group_tna.md),
[`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_comparison()`](http://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md),
[`print.tna_comparison()`](http://sonsoles.me/tna/reference/print.tna_comparison.md),
[`print.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)

## Examples

``` r
model <- group_model(engagement_mmm)
plot_compare(model)

```

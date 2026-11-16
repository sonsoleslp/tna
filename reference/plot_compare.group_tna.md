# Plot the Difference Network Between Two Groups

Plot the Difference Network Between Two Groups

## Usage

``` r
# S3 method for class 'group_tna'
plot_compare(x, i = NULL, j = NULL, ...)
```

## Arguments

- x:

  A `group_tna` object.

- i:

  An `integer` index or the name of the principal cluster as a
  `character` string. When `i` and `j` are both `NULL` (the default),
  every pair of clusters is compared.

- j:

  An `integer` index or the name of the secondary cluster as a
  `character` string. See `i`.

- ...:

  Additional arguments passed to
  [`cograph::plot_compare()`](https://sonsoles.me/cograph/reference/plot_compare.html).

## Value

The difference networks are drawn, and returned invisibly: for one pair
of clusters (two clusters, or `i` and `j` given), a `list` with the
elements `weights` and `inits` (the differences, see
[`plot_compare.tna()`](https://sonsoles.me/tna/reference/plot_compare.md));
otherwise a named `list` with such a `list` for each pair of clusters.

## See also

Model comparison functions
[`compare()`](https://sonsoles.me/tna/reference/compare.md),
[`compare.group_tna()`](https://sonsoles.me/tna/reference/compare.group_tna.md),
[`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_comparison()`](https://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot.tna_sequence_comparison()`](https://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare()`](https://sonsoles.me/tna/reference/plot_compare.md),
[`print.tna_comparison()`](https://sonsoles.me/tna/reference/print.tna_comparison.md),
[`print.tna_sequence_comparison()`](https://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)

## Examples

``` r
model <- group_model(engagement_mmm)
plot_compare(model)

```

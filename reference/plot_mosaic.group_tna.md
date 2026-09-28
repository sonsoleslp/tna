# Plot State Frequencies as a Mosaic Between Two Groups

Plot State Frequencies as a Mosaic Between Two Groups

## Usage

``` r
# S3 method for class 'group_tna'
plot_mosaic(x, label, ...)
```

## Arguments

- x:

  A `group_tna` object.

- label:

  An optional `character` string that can be provided to specify the
  grouping factor name if `x` was not constructed using a column name of
  the original data.

- ...:

  Ignored.

## Value

A `ggplot` object.

## See also

Basic functions
[`build_model()`](https://sonsoles.me/tna/reference/build_model.md),
[`hist.group_tna()`](https://sonsoles.me/tna/reference/hist.group_tna.md),
[`hist.tna()`](https://sonsoles.me/tna/reference/hist.tna.md),
[`plot.group_tna()`](https://sonsoles.me/tna/reference/plot.group_tna.md),
[`plot.tna()`](https://sonsoles.me/tna/reference/plot.tna.md),
[`plot_frequencies()`](https://sonsoles.me/tna/reference/plot_frequencies.md),
[`plot_frequencies.group_tna()`](https://sonsoles.me/tna/reference/plot_frequencies.group_tna.md),
[`plot_mosaic()`](https://sonsoles.me/tna/reference/plot_mosaic.md),
[`plot_mosaic.tna_data()`](https://sonsoles.me/tna/reference/plot_mosaic.tna_data.md),
[`print.group_tna()`](https://sonsoles.me/tna/reference/print.group_tna.md),
[`print.summary.group_tna()`](https://sonsoles.me/tna/reference/print.summary.group_tna.md),
[`print.summary.tna()`](https://sonsoles.me/tna/reference/print.summary.tna.md),
[`print.tna()`](https://sonsoles.me/tna/reference/print.tna.md),
[`summary.group_tna()`](https://sonsoles.me/tna/reference/summary.group_tna.md),
[`summary.tna()`](https://sonsoles.me/tna/reference/summary.tna.md),
[`tna-package`](https://sonsoles.me/tna/reference/tna-package.md)

## Examples

``` r
model <- group_model(engagement, group = rep(1:3, length.out = 1000))
plot_mosaic(model)

```

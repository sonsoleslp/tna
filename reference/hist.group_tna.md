# Plot a Histogram of Edge Weights for a `group_tna` Object.

Plot a Histogram of Edge Weights for a `group_tna` Object.

## Usage

``` r
# S3 method for class 'group_tna'
hist(x, ...)
```

## Arguments

- x:

  A `group_tna` object.

- ...:

  Additional arguments passed to
  [`graphics::hist()`](https://rdrr.io/r/graphics/hist.html).

## Value

A `list` (invisibly) of `histogram` objects of the edge weights of each
cluster.

## See also

Basic functions
[`build_model()`](https://sonsoles.me/tna/reference/build_model.md),
[`hist.tna()`](https://sonsoles.me/tna/reference/hist.tna.md),
[`plot.group_tna()`](https://sonsoles.me/tna/reference/plot.group_tna.md),
[`plot.tna()`](https://sonsoles.me/tna/reference/plot.tna.md),
[`plot_frequencies()`](https://sonsoles.me/tna/reference/plot_frequencies.md),
[`plot_frequencies.group_tna()`](https://sonsoles.me/tna/reference/plot_frequencies.group_tna.md),
[`plot_mosaic()`](https://sonsoles.me/tna/reference/plot_mosaic.md),
[`plot_mosaic.group_tna()`](https://sonsoles.me/tna/reference/plot_mosaic.group_tna.md),
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
model <- group_model(engagement_mmm)
hist(model)



```

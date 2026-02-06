# Plot a Histogram of Edge Weights in the Network

Plot a Histogram of Edge Weights in the Network

## Usage

``` r
# S3 method for class 'tna'
hist(x, breaks, col = "lightblue", main, xlab, border = "white", ...)
```

## Arguments

- x:

  a vector of values for which the histogram is desired.

- breaks:

  one of:

  - a vector giving the breakpoints between histogram cells,

  - a function to compute the vector of breakpoints,

  - a single number giving the number of cells for the histogram,

  - a character string naming an algorithm to compute the number of
    cells (see ‘Details’),

  - a function to compute the number of cells.

  In the last three cases the number is a suggestion only; as the
  breakpoints will be set to
  [`pretty`](https://rdrr.io/r/base/pretty.html) values, the number is
  limited to `1e6` (with a warning if it was larger). If `breaks` is a
  function, the `x` vector is supplied to it as the only argument (and
  the number of breaks is only limited by the amount of available
  memory).

- col:

  a colour to be used to fill the bars.

- main:

  A `character` string defining the title of the plot.

- xlab:

  A `character` string defining the vertical axis label.

- border:

  the color of the border around the bars. The default is to use the
  standard foreground color.

- ...:

  Additional arguments passed to
  [`graphics::hist()`](https://rdrr.io/r/graphics/hist.html).

## Value

A `histogram` object of edge weights.

## See also

Basic functions
[`build_model()`](http://sonsoles.me/tna/reference/build_model.md),
[`hist.group_tna()`](http://sonsoles.me/tna/reference/hist.group_tna.md),
[`plot.group_tna()`](http://sonsoles.me/tna/reference/plot.group_tna.md),
[`plot.tna()`](http://sonsoles.me/tna/reference/plot.tna.md),
[`plot_frequencies()`](http://sonsoles.me/tna/reference/plot_frequencies.md),
[`plot_frequencies.group_tna()`](http://sonsoles.me/tna/reference/plot_frequencies.group_tna.md),
[`plot_mosaic()`](http://sonsoles.me/tna/reference/plot_mosaic.md),
[`plot_mosaic.group_tna()`](http://sonsoles.me/tna/reference/plot_mosaic.group_tna.md),
[`plot_mosaic.tna_data()`](http://sonsoles.me/tna/reference/plot_mosaic.tna_data.md),
[`print.group_tna()`](http://sonsoles.me/tna/reference/print.group_tna.md),
[`print.summary.group_tna()`](http://sonsoles.me/tna/reference/print.summary.group_tna.md),
[`print.summary.tna()`](http://sonsoles.me/tna/reference/print.summary.tna.md),
[`print.tna()`](http://sonsoles.me/tna/reference/print.tna.md),
[`summary.group_tna()`](http://sonsoles.me/tna/reference/summary.group_tna.md),
[`summary.tna()`](http://sonsoles.me/tna/reference/summary.tna.md),
[`tna-package`](http://sonsoles.me/tna/reference/tna-package.md)

## Examples

``` r
model <- tna(group_regulation)
hist(model)

```

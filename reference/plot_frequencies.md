# Plot the Frequency Distribution of States

Plot the Frequency Distribution of States

## Usage

``` r
plot_frequencies(x, ...)

# S3 method for class 'tna'
plot_frequencies(x, width = 0.7, hjust = 1.2, show_label = TRUE, colors, ...)
```

## Arguments

- x:

  A `tna` object created from sequence data.

- ...:

  Ignored.

- width:

  A `numeric` value for the Width of the bars. Default is 0.7,

- hjust:

  A `numeric` value for the horizontal adjustment of the labels. Default
  is 1.2.

- show_label:

  A `logical` value indicating whether to show a label with the
  frequency counts. Default is `TRUE`.

- colors:

  A `character` vector of colors to be used in the plot (one per label)
  or a single color.

## Value

A `ggplot` object.

## See also

Basic functions
[`build_model()`](http://sonsoles.me/tna/reference/build_model.md),
[`hist.group_tna()`](http://sonsoles.me/tna/reference/hist.group_tna.md),
[`hist.tna()`](http://sonsoles.me/tna/reference/hist.tna.md),
[`plot.group_tna()`](http://sonsoles.me/tna/reference/plot.group_tna.md),
[`plot.tna()`](http://sonsoles.me/tna/reference/plot.tna.md),
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
plot_frequencies(model)

plot_frequencies(model, width =  0.5, colors = "pink")

```

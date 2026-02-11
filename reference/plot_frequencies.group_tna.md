# Plot the Frequency Distribution of States

Plot the Frequency Distribution of States

## Usage

``` r
# S3 method for class 'group_tna'
plot_frequencies(
  x,
  label,
  colors,
  width = 0.7,
  palette = "Set2",
  show_label = TRUE,
  position = "dodge",
  hjust = 1.2,
  ...
)
```

## Arguments

- x:

  A `group_tna` object.

- label:

  An optional `character` string that can be provided to specify the
  grouping factor name if `x` was not constructed using a column name of
  the original data.

- colors:

  A `character` vector of colors to be used in the plot (one per group).

- width:

  A `numeric` value for the width of the bars. The default is `0.7`.

- palette:

  A `character` string that specifies the palette to be used if `colors`
  are not passed.

- show_label:

  A `logical` value indicating whether to show a label with the
  frequency counts. Default is `TRUE`.

- position:

  Position of the bars:`"dodge"`, `"dodge2"`, `"fill"` or `"stack"`.

- hjust:

  A `numeric` value for the horizontal adjustment of the labels. The
  default is `1.2`.

- ...:

  Ignored.

## Value

A `ggplot` object.

## See also

Basic functions
[`build_model()`](http://sonsoles.me/tna/reference/build_model.md),
[`hist.group_tna()`](http://sonsoles.me/tna/reference/hist.group_tna.md),
[`hist.tna()`](http://sonsoles.me/tna/reference/hist.tna.md),
[`plot.group_tna()`](http://sonsoles.me/tna/reference/plot.group_tna.md),
[`plot.tna()`](http://sonsoles.me/tna/reference/plot.tna.md),
[`plot_frequencies()`](http://sonsoles.me/tna/reference/plot_frequencies.md),
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
model <- group_model(engagement_mmm)
# Default
plot_frequencies(model)

# Default labels outside and custom colors
plot_frequencies(
  model,
  width = 0.9,
  hjust = -0.3,
  colors = c("#218516", "#f9c22e", "#53b3cb")
)

# Stacked with no labels
plot_frequencies(model, position = "stack", show_label = FALSE)

# Fill
plot_frequencies(model, position = "fill", hjust = 1.1)

```

# Plot Centrality Measures

Plot Centrality Measures

## Usage

``` r
# S3 method for class 'group_tna_centralities'
plot(
  x,
  reorder = TRUE,
  ncol = 3,
  scales = c("free_x", "fixed"),
  colors,
  palette = "Set2",
  labels = TRUE,
  ...
)
```

## Arguments

- x:

  A `group_tna_centralities` object.

- reorder:

  A `logical` value indicating whether to reorder the values for each
  centrality in a descending order. The default is `TRUE`.

- ncol:

  Number of columns to use for the facets. The default is 3.

- scales:

  Either `"fixed"` or `"free_x"` (the default). If `"free_x"`, the
  horizontal axis is scaled individually in each facet. If `"fixed"`,
  the same values are used for all axes.

- colors:

  The colors for each node (default is the model colors if the `tna`
  model object is passed, otherwise `"black"`).

- palette:

  A color palette to be applied if `colors` is not specified.

- labels:

  A `logical` value indicating whether to show the centrality numeric
  values. The default is `TRUE`.

- ...:

  Ignored.

## Value

A `ggplot` object displaying a line chart for each centrality with one
line per cluster.

## See also

Centrality measure functions
[`betweenness_network()`](http://sonsoles.me/tna/reference/betweenness_network.md),
[`centralities()`](http://sonsoles.me/tna/reference/centralities.md),
[`plot.tna_centralities()`](http://sonsoles.me/tna/reference/plot.tna_centralities.md),
[`print.group_tna_centralities()`](http://sonsoles.me/tna/reference/print.group_tna_centralities.md),
[`print.tna_centralities()`](http://sonsoles.me/tna/reference/print.tna_centralities.md)

## Examples

``` r
model <- group_model(engagement_mmm)
cm <- centralities(model)
plot(cm)

```

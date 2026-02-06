# Plot Centrality Measures

Plots the centrality measures of a `tna_centralities` object as a
lollipop chart. The resulting plot includes facets for each centrality
measure, showing the values for each state. The returned plot is a
`ggplot2` object, so it can be easily modified and styled. See
[`centralities()`](http://sonsoles.me/tna/reference/centralities.md) for
details on the centrality measures.

## Usage

``` r
# S3 method for class 'tna_centralities'
plot(
  x,
  reorder = TRUE,
  ncol = 3,
  scales = c("free_x", "fixed"),
  colors,
  labels = TRUE,
  ...
)
```

## Arguments

- x:

  An object of class `tna_centralities`.

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

- labels:

  A `logical` value indicating whether to show the centrality numeric
  values. The default is `TRUE`.

- ...:

  Ignored.

## Value

A `ggplot` object displaying the lollipop charts for each centrality
measure.

## See also

Centrality measure functions
[`betweenness_network()`](http://sonsoles.me/tna/reference/betweenness_network.md),
[`centralities()`](http://sonsoles.me/tna/reference/centralities.md),
[`plot.group_tna_centralities()`](http://sonsoles.me/tna/reference/plot.group_tna_centralities.md),
[`print.group_tna_centralities()`](http://sonsoles.me/tna/reference/print.group_tna_centralities.md),
[`print.tna_centralities()`](http://sonsoles.me/tna/reference/print.tna_centralities.md)

## Examples

``` r
tna_model <- tna(group_regulation)
cm <- centralities(tna_model)
plot(cm, ncol = 3, reorder = TRUE)

```

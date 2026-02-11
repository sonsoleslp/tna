# Plot a Transition Network Analysis Model

This function plots a transition network analysis (TNA) model using the
`qgraph` package. The nodes in the graph represent states, with node
sizes corresponding to initial state probabilities. Edge labels
represent the edge weights of the network.

## Usage

``` r
# S3 method for class 'tna'
plot(
  x,
  node_list,
  use_list_order = TRUE,
  x_offset,
  labels,
  colors,
  pie,
  cut,
  show_pruned = TRUE,
  pruned_edge_color = "pink",
  edge.color = NA,
  edge.labels = TRUE,
  edge.label.position = 0.65,
  layout = "circle",
  layout_args = list(),
  scale_nodes,
  scaling_factor = 0.5,
  mar = rep(5, 4),
  theme = "colorblind",
  ...
)
```

## Arguments

- x:

  A `tna` object from
  [`tna()`](http://sonsoles.me/tna/reference/build_model.md).

- node_list:

  An optional `list` of two `character` vectors that define two mutually
  exclusive groups of node labels.

- use_list_order:

  A `logical` value. If `node_list` is provided, defines how the order
  of the nodes in the plot is defined. A `TRUE` value uses the order in
  `node_list`. Otherwise, the nodes are ranked based on edge weights and
  ordered according to the rank.

- x_offset:

  An optional `numeric` vector with the same number of elements as there
  are states. Defines a horizontal offset for each node in the plot when
  `node_list` is provided.

- labels:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- colors:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- pie:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- cut:

  Edge color and width emphasis cutoff value. The default is the median
  of the edge weights. See
  [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html) for
  details.

- show_pruned:

  A `logical` value indicating if pruned edges removed by
  [`prune()`](http://sonsoles.me/tna/reference/prune.md) should be shown
  in the plot. The default is `TRUE`, and the edges are drawn as dashed
  with a different color to distinguish them.

- pruned_edge_color:

  A `character` string for the color to use for pruned edges when
  `show_pruned = TRUE`. The default is `"pink"`.

- edge.color:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- edge.labels:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- edge.label.position:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- layout:

  One of the following:

  - A `character` string describing a `qgraph` layout (e.g., `"circle"`)
    or the name of a `igraph` layout function (e.g.,
    `"layout_on_grid"`).

  - A `matrix` of node positions to use, with a row for each node and
    `x` and `y` columns for the node positions.

  - A layout function from `igraph`.

- layout_args:

  A `list` of arguments to pass to the `igraph` layout function when
  `layout` is a function or a character string that specifies a function
  name.

- scale_nodes:

  A `character` string giving the name of a centrality measure to scale
  the node size by. See
  [`centralities()`](http://sonsoles.me/tna/reference/centralities.md)
  for valid names. If missing (the default), uses default
  [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html)
  scaling. The value of `vsize` provided via `...` is used as baseline
  size.

- scaling_factor:

  A `numeric` value specifying how strongly to scale the nodes when
  `scale_nodes` is provided. Values between 0 and 1 will result in
  smaller differences and values larger than 1 will result in greater
  differences. The default is `0.5`.

- mar:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- theme:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- ...:

  Additional arguments passed to
  [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

## Value

A `qgraph` plot of the transition network.

## See also

Basic functions
[`build_model()`](http://sonsoles.me/tna/reference/build_model.md),
[`hist.group_tna()`](http://sonsoles.me/tna/reference/hist.group_tna.md),
[`hist.tna()`](http://sonsoles.me/tna/reference/hist.tna.md),
[`plot.group_tna()`](http://sonsoles.me/tna/reference/plot.group_tna.md),
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
plot(model)

```

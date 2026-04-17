# Plot a Transition Network Analysis Model

This function plots a transition network analysis (TNA) model using the
`cograph` package. The nodes in the graph represent states, with node
sizes corresponding to initial state probabilities. Edge labels
represent the edge weights of the network. See
[`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
for details on how to further configure the plot.

## Usage

``` r
# S3 method for class 'tna'
plot(
  x,
  node_list,
  use_list_order = TRUE,
  scale_nodes,
  scaling_factor = 0.5,
  ...
)
```

## Arguments

- x:

  A `tna` object from
  [`build_model()`](http://sonsoles.me/tna/reference/build_model.md).

- node_list:

  An optional `list` of two `character` vectors that define two mutually
  exclusive groups of node labels.

- use_list_order:

  A `logical` value. If `node_list` is provided, defines how the order
  of the nodes in the plot is defined. A `TRUE` value uses the order in
  `node_list`. Otherwise, the nodes are ranked based on edge weights and
  ordered according to the rank.

- scale_nodes:

  A `character` string giving the name of a centrality measure to scale
  the node size by. See
  [`centralities()`](http://sonsoles.me/tna/reference/centralities.md)
  for valid names. If missing (the default), uses default
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
  scaling. The value of `node_size` provided via `...` is used as
  baseline size.

- scaling_factor:

  A `numeric` value specifying how strongly to scale the nodes when
  `scale_nodes` is provided. Values between 0 and 1 will result in
  smaller differences and values larger than 1 will result in greater
  differences. The default is `0.5`.

- ...:

  Additional arguments passed to
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
  or
  [`cograph::plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.html).

## Value

A `cograph_network` plot of the transition network.

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

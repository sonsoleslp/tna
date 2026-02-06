# Plot a Transition Network Model from a Matrix of Edge Weights

Plot a Transition Network Model from a Matrix of Edge Weights

## Usage

``` r
plot_model(
  x,
  labels,
  colors,
  cut,
  edge.labels = TRUE,
  edge.label.position = 0.65,
  layout = "circle",
  layout_args = list(),
  mar = rep(5, 4),
  theme = "colorblind",
  ...
)
```

## Arguments

- x:

  A square `matrix` of edge weights.

- labels:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- colors:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- cut:

  Edge color and width emphasis cutoff value. The default is the median
  of the edge weights. See
  [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html) for
  details.

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

- mar:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- theme:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- ...:

  Additional arguments passed to
  [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

## Value

See [`plot.tna()`](http://sonsoles.me/tna/reference/plot.tna.md).

## Examples

``` r
m <- matrix(rexp(25), 5, 5)
plot_model(m)

```

# Plot Cliques of a TNA Network

Plot Cliques of a TNA Network

## Usage

``` r
# S3 method for class 'tna_cliques'
plot(
  x,
  n = 6,
  first = 1,
  show_loops = FALSE,
  edge.labels = TRUE,
  edge.label.position = 0.65,
  minimum = 1e-05,
  mar = rep(5, 4),
  layout = "circle",
  layout_args = list(),
  cut = 0.01,
  normalize = TRUE,
  ask = TRUE,
  colors,
  theme = "colorblind",
  ...
)
```

## Arguments

- x:

  A `tna_cliques` object.

- n:

  An `integer` defining the maximum number of cliques to show. The
  defaults is `6`.

- first:

  An `integer` giving the index of the first clique to show. The default
  index is `1`.

- show_loops:

  A `logical` value indicating whether to include loops in the plots or
  not.

- edge.labels:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- edge.label.position:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- minimum:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- mar:

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

- cut:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- normalize:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- ask:

  A `logical` value. When `TRUE`, show plots one by one and asks to plot
  the next plot in interactive mode.

- colors:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- theme:

  See [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- ...:

  Ignored.

## Value

`NULL` (invisibly).

## See also

Clique-related functions
[`cliques()`](http://sonsoles.me/tna/reference/cliques.md),
[`plot.group_tna_cliques()`](http://sonsoles.me/tna/reference/plot.group_tna_cliques.md),
[`print.group_tna_cliques()`](http://sonsoles.me/tna/reference/print.group_tna_cliques.md),
[`print.tna_cliques()`](http://sonsoles.me/tna/reference/print.tna_cliques.md)

## Examples

``` r
model <- tna(group_regulation)
cliq <- cliques(model, size = 2)
plot(cliq, n = 1, ask = FALSE)

```

# Plot Cliques of a TNA Network

Plot Cliques of a TNA Network

## Usage

``` r
# S3 method for class 'tna_cliques'
plot(x, n = 6, first = 1, show_loops = FALSE, colors, ask = TRUE, ...)
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

- colors:

  Optional `character` vector of colors values to use for the nodes.

- ask:

  A `logical` value. When `TRUE`, show plots one by one and asks to plot
  the next plot in interactive mode.

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

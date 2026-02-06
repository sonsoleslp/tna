# Plot Found Cliques

Plot Found Cliques

## Usage

``` r
# S3 method for class 'group_tna_cliques'
plot(x, title, ...)
```

## Arguments

- x:

  A `group_tna_cliques` object.

- title:

  A `character` vector of titles to use for each plot.

- ...:

  Arguments passed to
  [`plot.tna_cliques()`](http://sonsoles.me/tna/reference/plot.tna_cliques.md).

## Value

A `list` (invisibly) with one element per cluster. Each element contains
a `qgraph` plot when only one clique is present per cluster, otherwise
the element is `NULL`.

## See also

Clique-related functions
[`cliques()`](http://sonsoles.me/tna/reference/cliques.md),
[`plot.tna_cliques()`](http://sonsoles.me/tna/reference/plot.tna_cliques.md),
[`print.group_tna_cliques()`](http://sonsoles.me/tna/reference/print.group_tna_cliques.md),
[`print.tna_cliques()`](http://sonsoles.me/tna/reference/print.tna_cliques.md)

## Examples

``` r
model <- group_model(engagement_mmm)
cliq <- cliques(model, size = 2)
plot(cliq, ask = FALSE)







```

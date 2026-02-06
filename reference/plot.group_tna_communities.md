# Plot Detected Communities

Plot Detected Communities

## Usage

``` r
# S3 method for class 'group_tna_communities'
plot(x, title, colors, ...)
```

## Arguments

- x:

  A `group_tna_communities` object.

- title:

  A `character` vector of titles to use for each plot.

- colors:

  A `character` vector of colors to use.

- ...:

  Arguments passed to
  [`plot.tna_communities()`](http://sonsoles.me/tna/reference/plot.tna_communities.md).

## Value

A `list` (invisibly) of `qgraph` objects in which the nodes are colored
by community for each cluster.

## See also

Community detection functions
[`communities()`](http://sonsoles.me/tna/reference/communities.md),
[`plot.tna_communities()`](http://sonsoles.me/tna/reference/plot.tna_communities.md),
[`print.group_tna_communities()`](http://sonsoles.me/tna/reference/print.group_tna_communities.md),
[`print.tna_communities()`](http://sonsoles.me/tna/reference/print.tna_communities.md)

## Examples

``` r
model <- group_model(engagement_mmm)
comm <- communities(model)
plot(comm)



```

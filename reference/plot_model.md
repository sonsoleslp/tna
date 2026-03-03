# Plot a Transition Network Model from a Matrix of Edge Weights

Plot a Transition Network Model from a Matrix of Edge Weights

## Usage

``` r
plot_model(x, labels, colors, ...)
```

## Arguments

- x:

  A square `matrix` of edge weights.

- labels:

  Optional `character` vector of node labels.

- colors:

  An optional `character` vector of node colors to use.

- ...:

  Additional arguments passed to
  [`cograph::splot()`](http://sonsoles.me/cograph/reference/splot.md) or
  [`cograph::plot_htna()`](http://sonsoles.me/cograph/reference/plot_htna.md).

## Value

See [`plot.tna()`](http://sonsoles.me/tna/reference/plot.tna.md).

## Examples

``` r
m <- matrix(rexp(25), 5, 5)
plot_model(m)

```

# Plot a Transition Network Model from a Matrix of Edge Weights

Plot a Transition Network Model from a Matrix of Edge Weights

## Usage

``` r
plot_model(x, labels, colors, cut, ...)
```

## Arguments

- x:

  A square `matrix` of edge weights.

- labels:

  Optional `character` vector of node labels. See
  [cograph::tplot](http://sonsoles.me/cograph/reference/plot_tna.md).

- colors:

  An optional `character` vector of node colors to use.

- cut:

  A `numeric` value for the edge emphasis threshold. See
  [cograph::tplot](http://sonsoles.me/cograph/reference/plot_tna.md).

- ...:

  Additional arguments passed to
  [`cograph::tplot()`](http://sonsoles.me/cograph/reference/plot_tna.md)
  or
  [`cograph::plot_htna()`](http://sonsoles.me/cograph/reference/plot_htna.md).

## Value

See [`plot.tna()`](http://sonsoles.me/tna/reference/plot.tna.md).

## Examples

``` r
m <- matrix(rexp(25), 5, 5)
plot_model(m)

```

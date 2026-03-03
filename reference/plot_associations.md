# Plot an Association Network

Plot an Association Network

## Usage

``` r
plot_associations(x, ...)

# S3 method for class 'tna'
plot_associations(x, edge_color, ...)
```

## Arguments

- x:

  A `tna` object.

- ...:

  Additional arguments passed to
  [`plot_model()`](http://sonsoles.me/tna/reference/plot_model.md).

- edge_color:

  An optional `character` vector of colors for the edges. By default,
  the colors are specified by the magnitude of the standardized
  residual.

## Value

A `cograph_network` object.

## Examples

``` r
model <- ftna(group_regulation)
plot_associations(model)

```

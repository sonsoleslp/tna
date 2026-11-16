# Build a Social Network Analysis Model

Build a Social Network Analysis Model

## Usage

``` r
sna(x, aggregate = sum, ...)
```

## Arguments

- x:

  A `data.frame` or a `matrix` with three columns: the first two
  representing the states and the third giving the weights.

- aggregate:

  A `function` to use for aggregating the weights. The default is
  [`sum()`](https://rdrr.io/r/base/sum.html).

- ...:

  Additional arguments passed to `aggregate`.

## Value

A `tna` object representing the model. Its `weights` element is a square
`matrix` with one row and column per state, holding the aggregated
weight of each pair of states (zero when the pair does not occur). The
`inits` and `data` elements are `NULL`.

## Examples

``` r
set.seed(123)
states <- c("A", "B", "C", "D")
d <- data.frame(
  from = sample(states, 100, replace = TRUE),
  to = sample(states, 100, replace = TRUE),
  weight = rexp(100)
)
model <- sna(d)
```

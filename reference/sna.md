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

A `tna` object representing the model.

## Examples

``` r
set.seed(123)
d <- data.frame(
  from = sample(LETTERS[1:4], 100, replace = TRUE),
  to = sample(LETTERS[1:4], 100, replace = TRUE),
  weight = rexp(100)
)
model <- sna(d)
```

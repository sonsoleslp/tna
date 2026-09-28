# Simulate Method (Re-export of [stats::simulate](https://rdrr.io/r/stats/simulate.html))

The [`stats::simulate()`](https://rdrr.io/r/stats/simulate.html) generic
is re-exported so that the
[`simulate.tna()`](https://sonsoles.me/tna/reference/simulate.tna.md)
and
[`simulate.group_tna()`](https://sonsoles.me/tna/reference/simulate.group_tna.md)
methods provided by this package are dispatched correctly when the user
only loads `tna`. See the method-specific help pages for argument and
return details.

## Usage

``` r
simulate(object, nsim = 1, seed = NULL, ...)
```

## Arguments

- object:

  A statistical model object. The `tna` package provides methods for
  objects of class `tna` and `group_tna`.

- nsim:

  An `integer` giving the number of sequences to simulate.

- seed:

  An `integer` random seed for reproducibility, or `NULL`.

- ...:

  Further arguments passed to the dispatched method.

## Value

The value returned by the dispatched method (a `data.frame` of simulated
sequences for `tna` and `group_tna` objects). See the method-specific
help pages for details.

## Examples

``` r
model <- tna(group_regulation)
sim <- simulate(model, nsim = 5, max_len = 10)
```

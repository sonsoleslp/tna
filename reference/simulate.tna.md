# Simulate Data from a Transition Network Analysis Model

Simulate Data from a Transition Network Analysis Model

## Usage

``` r
# S3 method for class 'tna'
simulate(
  object,
  nsim = 1,
  seed = NULL,
  max_len = 100L,
  na_range = c(0L, 0L),
  ...
)
```

## Arguments

- object:

  A `tna` object. The edge weights must be transition probabilities,
  i.e., the model must have `type = "relative"`.

- nsim:

  An `integer` giving the number of sequences to simulate. The default
  is 1.

- seed:

  Ignored. Please use [`set.seed()`](https://rdrr.io/r/base/Random.html)
  manually.

- max_len:

  An `integer` giving the maximum length of the simulated sequences.
  When no missing values are generated, this is the length of all
  simulated sequences.

- na_range:

  An `integer` vector of length 2 giving the minimum and maximum number
  of missing values to generate for each sequence. The number of missing
  values is drawn uniformly from this range. If both values are zero
  (the default), no missing values are generated.

- ...:

  Ignored.

## Value

A `data.frame` of the simulated sequence data with `nsim` rows and
`max_len` columns.

## See also

Other data:
[`import_data()`](http://sonsoles.me/tna/reference/import_data.md),
[`import_onehot()`](http://sonsoles.me/tna/reference/import_onehot.md),
[`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md),
[`print.tna_data()`](http://sonsoles.me/tna/reference/print.tna_data.md)

## Examples

``` r
model <- tna(group_regulation)
sim <- simulate(model, nsim = 10, max_len = 10)
```

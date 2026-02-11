# Simulate Data from a Group Transition Network Analysis Model

Simulate Data from a Group Transition Network Analysis Model

## Usage

``` r
# S3 method for class 'group_tna'
simulate(
  object,
  nsim = 1,
  seed = NULL,
  max_len = 100L,
  na_range = c(0L, 0L),
  zero_row = "self",
  format = "wide",
  ...
)
```

## Arguments

- object:

  A `group_tna` object. The edge weights must be transition
  probabilities or frequencies, i.e., the model must have
  `type = "relative"` or `type = "frequency"`.

- nsim:

  An `integer` vector giving the number of sequences to simulate per
  group. If a single integer is provided, the same number of sequences
  is generated per each group. The default is 1.

- seed:

  an object specifying if and how the random number generator should be
  initialized (‘seeded’).  
  For the `"lm"` method, either `NULL` or an integer that will be used
  in a call to `set.seed` before simulating the response vectors. If
  set, the value is saved as the `"seed"` attribute of the returned
  value. The default, `NULL` will not change the random generator state,
  and return [`.Random.seed`](https://rdrr.io/r/base/Random.html) as the
  `"seed"` attribute, see ‘Value’.

- max_len:

  An `integer` vector giving the maximum length of the simulated
  sequences per group. When no missing values are generated, this is the
  length of all simulated sequences. If a single integer is provided,
  the maximum length is the same for each group.

- na_range:

  An `integer` vector of length 2 giving the minimum and maximum number
  of missing values to generate for each sequence. The number of missing
  values is drawn uniformly from this range. If both values are zero
  (the default), no missing values are generated.

- zero_row:

  A `character` string describing how to process zero rows in the weight
  matrix. The option `"self"` (the default) assigns probability 1 to the
  corresponding state (self loop) and option `"uniform"` assigns a
  uniform distribution.

- format:

  A `character` string indicating whether the data should be returned in
  `wide` or `long` format.

- ...:

  Ignored.

## Value

A `data.frame` of the simulated sequence data.

## See also

Other data:
[`import_data()`](http://sonsoles.me/tna/reference/import_data.md),
[`import_onehot()`](http://sonsoles.me/tna/reference/import_onehot.md),
[`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md),
[`print.tna_data()`](http://sonsoles.me/tna/reference/print.tna_data.md),
[`simulate.tna()`](http://sonsoles.me/tna/reference/simulate.tna.md)

## Examples

``` r
model <- group_tna(
  group_regulation,
  group = rep(c("High", "Low"), each = 1000)
)
sim <- simulate(model, nsim = 10, max_len = 10)
```

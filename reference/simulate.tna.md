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
  zero_row = "self",
  format = "wide",
  ...
)
```

## Arguments

- object:

  A `tna` object. The edge weights must be transition probabilities or
  frequencies, i.e., the model must have `type = "relative"` or
  `type = "frequency"`.

- nsim:

  An `integer` giving the number of sequences to simulate. The default
  is 1.

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

  An `integer` giving the maximum length of the simulated sequences.
  When no missing values are generated, this is the length of all
  simulated sequences.

- na_range:

  An `integer` vector of length 2 giving the minimum and maximum number
  of missing values to generate for each sequence. The number of missing
  values is drawn uniformly from this range. If both values are zero
  (the default), no missing values are generated.

- zero_row:

  A `character` string describing how to process zero rows in the weight
  matrix, i.e., states that are never left, such as states that only end
  sequences. The option `"self"` (the default) assigns probability 1 to
  the corresponding state (self loop) and option `"uniform"` assigns a
  uniform distribution. Applies to both model types.

- format:

  A `character` string indicating whether the data should be returned in
  `wide` or `long` format.

- ...:

  Ignored.

## Value

A `data.frame` of the simulated sequence data. In `wide` format, one row
per sequence and one column per time point (`T1`, `T2`, ...). In `long`
format, one row per observation with the columns `id`, `time`, and
`state`.

## See also

Other data:
[`import_data()`](https://sonsoles.me/tna/reference/import_data.md),
[`import_onehot()`](https://sonsoles.me/tna/reference/import_onehot.md),
[`list_random_state_pools()`](https://sonsoles.me/tna/reference/list_random_state_pools.md),
[`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md),
[`print.tna_data()`](https://sonsoles.me/tna/reference/print.tna_data.md),
[`random_group_tna()`](https://sonsoles.me/tna/reference/random_group_tna.md),
[`random_tna()`](https://sonsoles.me/tna/reference/random_tna.md),
[`random_tna_mmm()`](https://sonsoles.me/tna/reference/random_tna_mmm.md),
[`simulate.group_tna()`](https://sonsoles.me/tna/reference/simulate.group_tna.md)

## Examples

``` r
model <- tna(group_regulation)
sim <- simulate(model, nsim = 10, max_len = 10, seed = 1)
print(sim)
#>           T1        T2         T3         T4         T5         T6        T7
#> 1       plan   emotion  consensus  consensus    emotion    discuss consensus
#> 2       plan      plan       plan    monitor    discuss    discuss synthesis
#> 3    discuss consensus       plan    emotion   cohesion  consensus consensus
#> 4   cohesion consensus      adapt  consensus    discuss   cohesion consensus
#> 5  consensus      plan  consensus coregulate       plan coregulate   emotion
#> 6   cohesion consensus       plan  consensus  consensus coregulate   discuss
#> 7   cohesion consensus       plan  consensus       plan  consensus synthesis
#> 8    emotion  cohesion       plan  consensus    discuss  synthesis   discuss
#> 9    emotion  cohesion coregulate  consensus coregulate    emotion consensus
#> 10 consensus   monitor    discuss    emotion   cohesion coregulate   monitor
#>            T8        T9       T10
#> 1        plan      plan      plan
#> 2     emotion  cohesion consensus
#> 3     discuss synthesis      plan
#> 4     discuss consensus   discuss
#> 5    cohesion consensus   emotion
#> 6     emotion consensus      plan
#> 7   consensus consensus      plan
#> 8   consensus   discuss synthesis
#> 9  coregulate   emotion consensus
#> 10       plan      plan   discuss
```

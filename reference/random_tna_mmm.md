# Build a Random Mixture Markov Model Object

Construct a synthetic `tna_mmm` object that mirrors the *structure* a
fitted seqHMM mixed Markov model exposes to
[tna](https://sonsoles.me/tna/reference/build_model.md) without
depending on seqHMM at runtime. The returned object can be passed to
[`group_model()`](https://sonsoles.me/tna/reference/group_model.md)
(dispatching via `group_model.tna_mmm`) and to
[`mmm_stats()`](https://sonsoles.me/tna/reference/mmm_stats.md)
(dispatching via `mmm_stats.tna_mmm`).

Real seqHMM `mhmm` objects continue to dispatch via the original
`*.mhmm` methods.

## Usage

``` r
random_tna_mmm(
  n_clusters = NULL,
  n_states = NULL,
  states = NULL,
  category = NULL,
  alpha = NULL,
  diag_boost = NULL,
  n_sequences = NULL,
  seq_length = NULL,
  n_covariates = 1L,
  seed = NULL
)
```

## Arguments

- n_clusters:

  An `integer` giving the number of mixture clusters. If `NULL` (the
  default), drawn from `2:4` on each call.

- n_states:

  An `integer` \>= 2 giving the number of states. If `NULL` (the
  default), a value is drawn from `7:11` on each call.

- states:

  An optional `character` vector of state labels of length at least
  `n_states`. The first `n_states` are used. If `NULL` (the default),
  labels are taken from `category` or auto-picked from a built-in pool
  that fits `n_states`.

- category:

  An optional `character` string naming a built-in label pool. Available
  pools are returned by
  [`list_random_state_pools()`](https://sonsoles.me/tna/reference/list_random_state_pools.md).
  When `NULL` (the default), a pool whose size is at least `n_states` is
  sampled at random. Ignored when `states` is supplied.

- alpha:

  A positive `numeric` Dirichlet concentration parameter. Small values
  (e.g. `0.3`) produce sparse, peaked transitions; large values (e.g.
  `5`) produce near-uniform transitions. If `NULL` (the default), a
  value is drawn from `Uniform(0.5, 1.0)` on each call.

- diag_boost:

  A non-negative `numeric` added to the diagonal of the transition
  matrix before re-normalising rows. Larger values make states
  "stickier" (more self-transitions). If `NULL` (the default), a value
  is drawn from `Uniform(1.5, 3.0)` on each call.

- n_sequences:

  An `integer` giving the number of sequences to simulate from the true
  parameters. If `NULL` (the default), a value is drawn from `500:800`
  on each call.

- seq_length:

  An `integer` giving the length of each simulated sequence. If `NULL`
  (the default), a value is drawn from `6:20` on each call.

- n_covariates:

  An `integer` \>= 1 giving the number of regression variables
  (including the intercept) used to predict cluster membership. Default
  is 1 (intercept only). When \> 1, additional rows are added to the
  coefficient matrix.

- seed:

  An `integer` random seed for reproducibility, or `NULL` (the default)
  for fresh randomness on every call.

## Value

An object of class `tna_mmm` containing fields `observations`,
`transition_probs`, `initial_probs`, `coefficients`, `vcov`,
`most_probable_cluster`, `cluster_names`, `state_names`, `n_clusters`,
`n_states`, `n_sequences`, `n_covariates`.

## See also

Other data:
[`import_data()`](https://sonsoles.me/tna/reference/import_data.md),
[`import_onehot()`](https://sonsoles.me/tna/reference/import_onehot.md),
[`list_random_state_pools()`](https://sonsoles.me/tna/reference/list_random_state_pools.md),
[`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md),
[`print.tna_data()`](https://sonsoles.me/tna/reference/print.tna_data.md),
[`random_group_tna()`](https://sonsoles.me/tna/reference/random_group_tna.md),
[`random_tna()`](https://sonsoles.me/tna/reference/random_tna.md),
[`simulate.group_tna()`](https://sonsoles.me/tna/reference/simulate.group_tna.md),
[`simulate.tna()`](https://sonsoles.me/tna/reference/simulate.tna.md)

## Examples

``` r
model <- random_tna_mmm(seed = 1)
mmm_stats(model)
#>     cluster    variable  estimate std_error   ci_lower ci_upper  z_value
#> 1 Cluster 2 (Intercept) 0.4825967 0.4679061 -0.4344823 1.399676 1.031397
#>     p_value
#> 1 0.3023549
grp <- group_model(model)
```

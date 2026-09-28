# Build a Random Group Transition Network Analysis Model

Construct a fully-functional `group_tna` object from synthetic
parameters. Each group receives its own randomly drawn transition matrix
and initial probabilities over a shared alphabet, so groups have
heterogeneous dynamics by default. Per-group overrides allow custom
group sizes, sparsity, stickiness, or hand-supplied transition matrices.

## Usage

``` r
random_group_tna(
  n_groups = NULL,
  group_names = NULL,
  n_states = NULL,
  states = NULL,
  category = NULL,
  alpha = NULL,
  diag_boost = NULL,
  n_sequences = NULL,
  seq_length = NULL,
  type = "relative",
  per_group = NULL,
  seed = NULL
)
```

## Arguments

- n_groups:

  An `integer` giving the number of groups. If `NULL` (the default), a
  value is drawn from `2:4` on each call.

- group_names:

  An optional `character` vector of group names of length `n_groups`.
  Defaults to `"Group 1"`, `"Group 2"`, ...

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

- type:

  A `character` string giving the model type. One of `"relative"` (the
  default), `"frequency"`, `"co-occurrence"`, `"attention"`.

- per_group:

  An optional `list` of length `n_groups`. Each element is itself a
  `list` of overrides applied to that group only. Recognised override
  names: `alpha`, `diag_boost`, `trans_matrix`, `init_probs`,
  `n_sequences`, `seq_length`. Unset entries fall back to the top-level
  defaults (which may themselves be drawn at random when `NULL`).

- seed:

  An `integer` random seed for reproducibility, or `NULL` (the default)
  for fresh randomness on every call.

## Value

A `group_tna` object.

## See also

Other data:
[`import_data()`](https://sonsoles.me/tna/reference/import_data.md),
[`import_onehot()`](https://sonsoles.me/tna/reference/import_onehot.md),
[`list_random_state_pools()`](https://sonsoles.me/tna/reference/list_random_state_pools.md),
[`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md),
[`print.tna_data()`](https://sonsoles.me/tna/reference/print.tna_data.md),
[`random_tna()`](https://sonsoles.me/tna/reference/random_tna.md),
[`random_tna_mmm()`](https://sonsoles.me/tna/reference/random_tna_mmm.md),
[`simulate.group_tna()`](https://sonsoles.me/tna/reference/simulate.group_tna.md),
[`simulate.tna()`](https://sonsoles.me/tna/reference/simulate.tna.md)

## Examples

``` r
# Fresh random group model on every call
model <- random_group_tna()

# Explicit two-group engagement demo with per-group differences
model <- random_group_tna(
  n_groups  = 2,
  n_states  = 3,
  category  = "engagement",
  per_group = list(
    list(n_sequences = 400, diag_boost = 3),
    list(n_sequences = 100, alpha = 0.3)
  ),
  seed = 42
)
```

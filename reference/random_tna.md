# Build a Random Transition Network Analysis Model

Construct a fully-functional `tna` object from synthetic parameters
without needing pre-existing sequence data. Random transition
probabilities are drawn from a Dirichlet distribution, sequences are
simulated from the resulting model, and a canonical `tna` object is
fitted on those sequences.

Calling `random_tna()` with no arguments returns a fresh, sticky network
with a coherent alphabet, drawn fresh on every call.

## Usage

``` r
random_tna(
  n_states = NULL,
  states = NULL,
  category = NULL,
  alpha = NULL,
  diag_boost = NULL,
  trans_matrix = NULL,
  init_probs = NULL,
  n_sequences = NULL,
  seq_length = NULL,
  type = "relative",
  return_params = FALSE,
  seed = NULL
)
```

## Arguments

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

- trans_matrix:

  An optional square `numeric` matrix of transition probabilities. When
  supplied, `alpha` and `diag_boost` are ignored for the transition
  matrix. Rows are renormalised to sum to one.

- init_probs:

  An optional `numeric` vector of initial state probabilities.
  Renormalised to sum to one. If `NULL`, drawn from a Dirichlet on the
  same alphabet.

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

- return_params:

  A `logical`. If `TRUE`, returns a `list` with the fitted model and the
  ground-truth parameters used to generate it. Default is `FALSE`.

- seed:

  An `integer` random seed for reproducibility, or `NULL` (the default)
  for fresh randomness on every call.

## Value

A `tna` object, or, when `return_params = TRUE`, a `list` with the
elements `model` (the `tna` object), `trans_matrix` (the true transition
`matrix`), `init_probs` (the true initial probabilities), `sequences`
(the simulated sequences as a wide `data.frame`), `labels` (the state
labels), and `category` (the label pool used).

## See also

Other data:
[`import_data()`](https://sonsoles.me/tna/reference/import_data.md),
[`import_onehot()`](https://sonsoles.me/tna/reference/import_onehot.md),
[`list_random_state_pools()`](https://sonsoles.me/tna/reference/list_random_state_pools.md),
[`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md),
[`print.tna_data()`](https://sonsoles.me/tna/reference/print.tna_data.md),
[`random_group_tna()`](https://sonsoles.me/tna/reference/random_group_tna.md),
[`random_tna_mmm()`](https://sonsoles.me/tna/reference/random_tna_mmm.md),
[`simulate.group_tna()`](https://sonsoles.me/tna/reference/simulate.group_tna.md),
[`simulate.tna()`](https://sonsoles.me/tna/reference/simulate.tna.md)

## Examples

``` r
# Fresh random model on every call
model <- random_tna()

# Explicit small-state demo using the engagement pool
model <- random_tna(n_states = 3, category = "engagement")

# Reproducible model
model <- random_tna(seed = 42)

# Recover the ground-truth parameters along with the model
out <- random_tna(seed = 7, return_params = TRUE)
str(out, max.level = 1)
#> List of 6
#>  $ model       :List of 4
#>   ..- attr(*, "type")= chr "relative"
#>   ..- attr(*, "scaling")= chr(0) 
#>   ..- attr(*, "params")=List of 1
#>   ..- attr(*, "class")= chr "tna"
#>  $ trans_matrix: num [1:8, 1:8] 0.65084 0.00658 0.06441 0.00231 0.09574 ...
#>   ..- attr(*, "dimnames")=List of 2
#>  $ init_probs  : Named num [1:8] 0.0868 0.1265 0.1334 0.1099 0.4211 ...
#>   ..- attr(*, "names")= chr [1:8] "Outline" "Test" "Note" "Cite" ...
#>  $ sequences   :'data.frame':    602 obs. of  7 variables:
#>  $ labels      : chr [1:8] "Outline" "Test" "Note" "Cite" ...
#>  $ category    : chr "behavioral"
```

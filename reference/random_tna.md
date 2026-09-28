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

A `tna` object, or a `list` with elements `model`, `trans_matrix`,
`init_probs`, `sequences`, `labels`, and `category` when
`return_params = TRUE`.

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

# Recover the ground-truth parameters
out <- random_tna(seed = 7, return_params = TRUE)
out$trans_matrix
#>               Outline        Test        Note        Cite    Research
#> Outline   0.650835298 0.043606039 0.028010411 0.111609915 0.056645413
#> Test      0.006583164 0.691942762 0.097029742 0.008455561 0.045670543
#> Note      0.064408166 0.004099030 0.655905813 0.014235598 0.103907112
#> Cite      0.002314978 0.025650260 0.068111669 0.631944647 0.167362401
#> Research  0.095737644 0.090456429 0.020780962 0.003247003 0.724659204
#> Draft     0.060127477 0.024646562 0.009819818 0.073839085 0.009743293
#> Highlight 0.012948049 0.005976364 0.046225233 0.131632139 0.127282132
#> Annotate  0.034780472 0.029400254 0.094943630 0.051292094 0.103632443
#>                Draft    Highlight    Annotate
#> Outline   0.05123559 0.0394980023 0.018559331
#> Test      0.05645087 0.0928047606 0.001062593
#> Note      0.05110080 0.0773399977 0.029003486
#> Cite      0.05251167 0.0255365930 0.026567783
#> Research  0.04831808 0.0002156296 0.016585048
#> Draft     0.76312071 0.0305544997 0.028148557
#> Highlight 0.00470706 0.6555567365 0.015672288
#> Annotate  0.01950503 0.0175106613 0.648935412
```

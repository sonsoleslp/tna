# Assess Model Reliability

Performs reliability analysis and outputs a concise summary of key
metrics. The results can also be visualized.

## Usage

``` r
reliability(x, ...)

# S3 method for class 'tna'
reliability(
  x,
  types = NULL,
  split = 0.5,
  iter = 1000,
  scaling = "none",
  actor = NULL,
  ...
)
```

## Arguments

- x:

  A `tna` object. Grouped models with scaled edge weights, and their
  networks, are not resampled (an error of class `tna_error_scaling`).

- ...:

  Ignored.

- types:

  A `character` vector giving the model types to fit. The default `NULL`
  uses the type of `x`. For every type, the transitions are counted from
  the sequence data of `x` as that type defines them, with the
  parameters (`params`) and scaling of `x`. See
  [`build_model()`](https://sonsoles.me/tna/reference/build_model.md)
  for the available types.

- split:

  A `numeric` value between `0` and `1` specifying the proportion of
  data for the split. The default is `0.5` for an even split.

- iter:

  An `integer` specifying number of iterations (splits). The default is
  `1000`.

- scaling:

  A `character` string naming the scaling applied to the weights of the
  two halves before they are compared. The default is `"none"`. See
  [`compare()`](https://sonsoles.me/tna/reference/compare.md) for the
  options.

- actor:

  An optional actor identifier for sequences nested in actors: a
  `character` string naming a non-sequence column of the data the model
  was built from, or a vector with one identifier per sequence. When
  given, the actors are split, so that all sequences of an actor fall in
  the same half, and `split` is the proportion of actors. The default
  `NULL` splits single sequences.

## Value

A `tna_reliability` object, which is a `list` with two tibbles:

- `metrics`: One row per split and comparison metric, with the columns
  `category`, `metric`, and `value` (and `model_type` when several
  `types` are given).

- `summary`: One row per metric with the `mean`, `sd`, `median`, `min`,
  `max`, and quartiles (`q25`, `q75`) of the metric over the splits.

[`summary.tna_reliability()`](https://sonsoles.me/tna/reference/summary.tna_reliability.md)
returns all metrics with their category as a tidy table.

## See also

Validation functions
[`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md),
[`deprune()`](https://sonsoles.me/tna/reference/deprune.md),
[`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
[`markov_order()`](https://sonsoles.me/tna/reference/markov_order.md),
[`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md),
[`permutation_test.group_tna()`](https://sonsoles.me/tna/reference/permutation_test.group_tna.md),
[`plot.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/plot.group_tna_bootstrap.md),
[`plot.group_tna_permutation()`](https://sonsoles.me/tna/reference/plot.group_tna_permutation.md),
[`plot.group_tna_stability()`](https://sonsoles.me/tna/reference/plot.group_tna_stability.md),
[`plot.tna_bootstrap()`](https://sonsoles.me/tna/reference/plot.tna_bootstrap.md),
[`plot.tna_markov_order()`](https://sonsoles.me/tna/reference/plot.tna_markov_order.md),
[`plot.tna_permutation()`](https://sonsoles.me/tna/reference/plot.tna_permutation.md),
[`plot.tna_reliability()`](https://sonsoles.me/tna/reference/plot.tna_reliability.md),
[`plot.tna_stability()`](https://sonsoles.me/tna/reference/plot.tna_stability.md),
[`print.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/print.group_tna_bootstrap.md),
[`print.group_tna_permutation()`](https://sonsoles.me/tna/reference/print.group_tna_permutation.md),
[`print.group_tna_stability()`](https://sonsoles.me/tna/reference/print.group_tna_stability.md),
[`print.summary.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/print.summary.group_tna_bootstrap.md),
[`print.summary.tna_bootstrap()`](https://sonsoles.me/tna/reference/print.summary.tna_bootstrap.md),
[`print.summary.tna_markov_order()`](https://sonsoles.me/tna/reference/print.summary.tna_markov_order.md),
[`print.tna_bootstrap()`](https://sonsoles.me/tna/reference/print.tna_bootstrap.md),
[`print.tna_markov_order()`](https://sonsoles.me/tna/reference/print.tna_markov_order.md),
[`print.tna_permutation()`](https://sonsoles.me/tna/reference/print.tna_permutation.md),
[`print.tna_reliability()`](https://sonsoles.me/tna/reference/print.tna_reliability.md),
[`print.tna_stability()`](https://sonsoles.me/tna/reference/print.tna_stability.md),
[`prune()`](https://sonsoles.me/tna/reference/prune.md),
[`pruning_details()`](https://sonsoles.me/tna/reference/pruning_details.md),
[`reprune()`](https://sonsoles.me/tna/reference/reprune.md),
[`summary.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md),
[`summary.tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.tna_bootstrap.md),
[`summary.tna_markov_order()`](https://sonsoles.me/tna/reference/summary.tna_markov_order.md),
[`summary.tna_permutation()`](https://sonsoles.me/tna/reference/summary.tna_permutation.md),
[`summary.tna_reliability()`](https://sonsoles.me/tna/reference/summary.tna_reliability.md),
[`summary.tna_stability()`](https://sonsoles.me/tna/reference/summary.tna_stability.md)

## Examples

``` r
# Small number of iterations for CRAN
model <- tna(engagement)
rel <- reliability(model, iter = 20)
```

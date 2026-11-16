# Compare Two Networks from Sequence Data using a Permutation Test

This function compares two networks built from sequence data using
permutation tests. The function builds Markov models for two sequence
objects, computes the transition probabilities, and compares them by
performing permutation tests. It returns the differences in transition
probabilities, effect sizes, estimated p-values, and confidence
intervals.

## Usage

``` r
permutation_test(x, ...)

# S3 method for class 'tna'
permutation_test(
  x,
  y,
  adjust = "none",
  iter = 1000,
  paired = FALSE,
  level = 0.05,
  measures = character(0),
  actor = NULL,
  ...
)
```

## Arguments

- x:

  A `tna` object containing sequence data for the first `tna` model.
  Grouped models with scaled edge weights, and their networks, are not
  resampled (an error of class `tna_error_scaling`). Two models with
  scaled edge weights are compared with a message (class
  `tna_message_scaling`): each is scaled on its own, so comparing them
  is not warranted.

- ...:

  Additional arguments passed to
  [`centralities()`](https://sonsoles.me/tna/reference/centralities.md).

- y:

  A `tna` object containing sequence data for the second `tna` model.

- adjust:

  A `character` string for the method to adjust p-values with for
  multiple comparisons. The default is `"none"` for no adjustment. See
  the `method` argument of
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
  details and available adjustment methods. The edge p-values are
  corrected over the edges tested: the transitions present in either
  network, each pair of a co-occurrence network counted once. A
  transition absent from both keeps its p-value of 1.

- iter:

  An `integer` giving the number of permutations to perform. The default
  is 1000.

- paired:

  A `logical` value. If `TRUE`, perform paired permutation tests; if
  `FALSE`, perform unpaired tests. The default is `FALSE`.

- level:

  A `numeric` value giving the significance level for the permutation
  tests. The default is 0.05.

- measures:

  A `character` vector of centrality measures to test. See
  [`centralities()`](https://sonsoles.me/tna/reference/centralities.md)
  for a list of available centrality measures.

- actor:

  An optional actor identifier for sequences nested in actors (for
  example, several sessions per student). Either a `character` string
  naming a non-sequence column of the data the models were built from
  (such as a column of `meta_data` from
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)),
  or a vector with one identifier per sequence (the sequences of `x`
  first, then those of `y`). When given, the permutation respects the
  nesting: actors whose sequences are all in one group are reassigned
  between the groups as whole units, and the sequences of actors present
  in both groups are shuffled within the actor (Good, 2005). A warning
  is given when the actors allow too few distinct permutations to reach
  `level`. Cannot be combined with `paired = TRUE`. The default `NULL`
  permutes single sequences.

## Value

A `tna_permutation` object which is a `list` with two elements: `edges`
and `centralities` (`NULL` when `measures` is empty). Both contain the
following elements:

- `stats`: A `data.frame` of original differences, effect sizes, and
  estimated p-values, with one row per edge (columns `edge_name`,
  `diff_true`, `effect_size`, `p_value`) or per state and centrality
  measure (columns `state`, `centrality`, `diff_true`, `effect_size`,
  `p_value`). The effect size is the observed difference divided by the
  standard deviation of the differences of the permuted samples, and
  `NA` when the permuted differences do not vary.

- `diffs_true`: The differences in the data: a `matrix` for the edges,
  and a `data.frame` with one row per state and one column per measure
  for the centralities.

- `diffs_sig`: The same as `diffs_true` with the non-significant
  differences set to zero.

Use
[`summary.tna_permutation()`](https://sonsoles.me/tna/reference/summary.tna_permutation.md)
for the results as a tidy table.

## References

Good, P. (2005). *Permutation, Parametric, and Bootstrap Tests of
Hypotheses* (3rd ed.). Springer.

## See also

Validation functions
[`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md),
[`deprune()`](https://sonsoles.me/tna/reference/deprune.md),
[`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
[`markov_order()`](https://sonsoles.me/tna/reference/markov_order.md),
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
[`reliability()`](https://sonsoles.me/tna/reference/reliability.md),
[`reprune()`](https://sonsoles.me/tna/reference/reprune.md),
[`summary.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md),
[`summary.tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.tna_bootstrap.md),
[`summary.tna_markov_order()`](https://sonsoles.me/tna/reference/summary.tna_markov_order.md),
[`summary.tna_permutation()`](https://sonsoles.me/tna/reference/summary.tna_permutation.md),
[`summary.tna_reliability()`](https://sonsoles.me/tna/reference/summary.tna_reliability.md),
[`summary.tna_stability()`](https://sonsoles.me/tna/reference/summary.tna_stability.md)

## Examples

``` r
model_x <- tna(head(group_regulation, 200))
model_y <- tna(tail(group_regulation, 200))
# Small number of iterations for CRAN
permutation_test(model_x, model_y, iter = 20)
#> # A tibble: 81 × 4
#>    edge_name           diff_true effect_size p_value
#>    <chr>                   <dbl>       <dbl>   <dbl>
#>  1 adapt -> adapt        0            NA      1     
#>  2 cohesion -> adapt     0.00541       0.912  0.857 
#>  3 consensus -> adapt    0.00106       0.329  0.952 
#>  4 coregulate -> adapt   0.0111        0.839  0.381 
#>  5 discuss -> adapt     -0.116        -6.90   0.0476
#>  6 emotion -> adapt      0.00275       0.314  0.905 
#>  7 monitor -> adapt     -0.0102       -1.10   0.524 
#>  8 plan -> adapt         0.00180       0.591  0.429 
#>  9 synthesis -> adapt   -0.0863       -1.44   0.190 
#> 10 adapt -> cohesion    -0.0361       -0.445  0.762 
#> # ℹ 71 more rows
```

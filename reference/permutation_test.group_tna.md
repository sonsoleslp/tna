# Compare Networks using a Permutation Test

Test edge weight differences between all pairs or a subset of pairs of a
`group_tna` object. See
[`permutation_test.tna()`](https://sonsoles.me/tna/reference/permutation_test.md)
for more details.

## Usage

``` r
# S3 method for class 'group_tna'
permutation_test(
  x,
  groups,
  adjust = "none",
  iter = 1000,
  paired = FALSE,
  level = 0.05,
  measures = character(0),
  consecutive = FALSE,
  actor = NULL,
  ...
)
```

## Arguments

- x:

  A `group_tna` object. Grouped models with scaled edge weights, and
  their networks, are not resampled (an error of class
  `tna_error_scaling`).

- groups:

  An `integer` vector or a `character` vector of group indices or names,
  respectively, defining which groups to compare. When not provided, all
  pairs are compared (the default).

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

- consecutive:

  A `logical` value. If `FALSE` (the default), all pairwise comparisons
  are performed in lexicographic order with respect to the order of the
  groups. If `TRUE`, only comparisons between consecutive pairs of
  groups are performed.

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

- ...:

  Additional arguments passed to
  [`centralities()`](https://sonsoles.me/tna/reference/centralities.md).

## Value

A `group_tna_permutation` object, which is a `list` of `tna_permutation`
objects (see
[`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)),
one per compared pair of groups, named after the pair.

## See also

Validation functions
[`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md),
[`deprune()`](https://sonsoles.me/tna/reference/deprune.md),
[`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
[`markov_order()`](https://sonsoles.me/tna/reference/markov_order.md),
[`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md),
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
model <- group_model(engagement_mmm)
# Small number of iterations for CRAN
permutation_test(model, iter = 20)
#> Cluster 1 vs. Cluster 2 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Disengaged -> Disengaged  -0.230       -14.2    0.0476
#> 2 Engaged -> Disengaged     -0.288       -17.3    0.0476
#> 3 Moderate -> Disengaged     0.00235       0.245  0.762 
#> 4 Disengaged -> Engaged      0.0706        7.13   0.0476
#> 5 Engaged -> Engaged        -0.00994      -0.402  0.571 
#> 6 Moderate -> Engaged        0.0706        6.74   0.0476
#> 7 Disengaged -> Moderate     0.160        14.9    0.0476
#> 8 Engaged -> Moderate        0.298        11.2    0.0476
#> 9 Moderate -> Moderate      -0.0730       -5.07   0.0476
#> 
#> Cluster 1 vs. Cluster 3 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Disengaged -> Disengaged  -0.195       -19.6    0.0476
#> 2 Engaged -> Disengaged     -0.0422      -10.6    0.0476
#> 3 Moderate -> Disengaged    -0.0609       -6.63   0.0476
#> 4 Disengaged -> Engaged      0.0471        5.76   0.0476
#> 5 Engaged -> Engaged        -0.00360      -0.271  0.762 
#> 6 Moderate -> Engaged       -0.0256       -2.79   0.0476
#> 7 Disengaged -> Moderate     0.147        22.3    0.0476
#> 8 Engaged -> Moderate        0.0458        3.30   0.0476
#> 9 Moderate -> Moderate       0.0864        7.11   0.0476
#> 
#> Cluster 2 vs. Cluster 3 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Disengaged -> Disengaged   0.0358        3.52   0.0476
#> 2 Engaged -> Disengaged      0.246        29.5    0.0476
#> 3 Moderate -> Disengaged    -0.0632       -5.89   0.0476
#> 4 Disengaged -> Engaged     -0.0234       -2.32   0.0476
#> 5 Engaged -> Engaged         0.00633       0.403  0.571 
#> 6 Moderate -> Engaged       -0.0962       -7.47   0.0476
#> 7 Disengaged -> Moderate    -0.0124       -4.62   0.0476
#> 8 Engaged -> Moderate       -0.252       -15.4    0.0476
#> 9 Moderate -> Moderate       0.159         8.70   0.0476
#> 
```

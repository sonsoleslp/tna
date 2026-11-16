# Summarize Permutation Test Results

Returns the results of a permutation test as a tidy table, for the edges
or for the centrality measures.

## Usage

``` r
# S3 method for class 'tna_permutation'
summary(object, what = "edges", digits = 2L, ...)

# S3 method for class 'group_tna_permutation'
summary(object, what = "edges", digits = 2L, ...)
```

## Arguments

- object:

  A `tna_permutation` or a `group_tna_permutation` object.

- what:

  A `character` string, the results to return: `"edges"` (the default)
  or `"centralities"`. The centralities are available when the test was
  run with `measures`.

- digits:

  An `integer`, the number of decimals of `diff_true` and `effect_size`.
  The p-values are not rounded. The default is 2.

- ...:

  Ignored.

## Value

A `data.frame` with one row per edge, or per state and centrality
measure, and per pair of groups (in the first column `pair`) for a
`group_tna_permutation` object. For the edges, the columns are `from`,
`to`, `diff_true` (the observed difference in the edge weight),
`effect_size` (the difference divided by the standard deviation of the
permuted differences, `NA` when they do not vary), and `p_value`. For
the centralities, `state` and `centrality` replace `from` and `to`. An
error of class `tna_error_permutation` is raised when
`what = "centralities"` and the test was run without `measures`.

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
[`reliability()`](https://sonsoles.me/tna/reference/reliability.md),
[`reprune()`](https://sonsoles.me/tna/reference/reprune.md),
[`summary.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md),
[`summary.tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.tna_bootstrap.md),
[`summary.tna_markov_order()`](https://sonsoles.me/tna/reference/summary.tna_markov_order.md),
[`summary.tna_reliability()`](https://sonsoles.me/tna/reference/summary.tna_reliability.md),
[`summary.tna_stability()`](https://sonsoles.me/tna/reference/summary.tna_stability.md)

## Examples

``` r
model <- group_model(engagement_mmm)
# Small number of iterations for CRAN
perm <- permutation_test(model, iter = 20, measures = "InStrength")
summary(perm)
#>                       pair       from         to diff_true effect_size
#> 1  Cluster 1 vs. Cluster 2 Disengaged Disengaged     -0.23      -13.89
#> 2  Cluster 1 vs. Cluster 2    Engaged Disengaged     -0.29      -16.34
#> 3  Cluster 1 vs. Cluster 2   Moderate Disengaged      0.00        0.17
#> 4  Cluster 1 vs. Cluster 2 Disengaged    Engaged      0.07        7.57
#> 5  Cluster 1 vs. Cluster 2    Engaged    Engaged     -0.01       -0.48
#> 6  Cluster 1 vs. Cluster 2   Moderate    Engaged      0.07        5.95
#> 7  Cluster 1 vs. Cluster 2 Disengaged   Moderate      0.16       16.08
#> 8  Cluster 1 vs. Cluster 2    Engaged   Moderate      0.30       11.88
#> 9  Cluster 1 vs. Cluster 2   Moderate   Moderate     -0.07       -4.22
#> 10 Cluster 1 vs. Cluster 3 Disengaged Disengaged     -0.19      -17.66
#> 11 Cluster 1 vs. Cluster 3    Engaged Disengaged     -0.04       -7.08
#> 12 Cluster 1 vs. Cluster 3   Moderate Disengaged     -0.06       -5.11
#> 13 Cluster 1 vs. Cluster 3 Disengaged    Engaged      0.05        6.49
#> 14 Cluster 1 vs. Cluster 3    Engaged    Engaged      0.00       -0.35
#> 15 Cluster 1 vs. Cluster 3   Moderate    Engaged     -0.03       -2.98
#> 16 Cluster 1 vs. Cluster 3 Disengaged   Moderate      0.15       27.35
#> 17 Cluster 1 vs. Cluster 3    Engaged   Moderate      0.05        4.39
#> 18 Cluster 1 vs. Cluster 3   Moderate   Moderate      0.09        5.66
#> 19 Cluster 2 vs. Cluster 3 Disengaged Disengaged      0.04        3.55
#> 20 Cluster 2 vs. Cluster 3    Engaged Disengaged      0.25       23.87
#> 21 Cluster 2 vs. Cluster 3   Moderate Disengaged     -0.06       -3.84
#> 22 Cluster 2 vs. Cluster 3 Disengaged    Engaged     -0.02       -2.67
#> 23 Cluster 2 vs. Cluster 3    Engaged    Engaged      0.01        0.44
#> 24 Cluster 2 vs. Cluster 3   Moderate    Engaged     -0.10       -7.28
#> 25 Cluster 2 vs. Cluster 3 Disengaged   Moderate     -0.01       -5.09
#> 26 Cluster 2 vs. Cluster 3    Engaged   Moderate     -0.25      -18.16
#> 27 Cluster 2 vs. Cluster 3   Moderate   Moderate      0.16        7.13
#>       p_value
#> 1  0.04761905
#> 2  0.04761905
#> 3  0.95238095
#> 4  0.04761905
#> 5  0.76190476
#> 6  0.04761905
#> 7  0.04761905
#> 8  0.04761905
#> 9  0.04761905
#> 10 0.04761905
#> 11 0.04761905
#> 12 0.04761905
#> 13 0.04761905
#> 14 0.66666667
#> 15 0.04761905
#> 16 0.04761905
#> 17 0.04761905
#> 18 0.04761905
#> 19 0.04761905
#> 20 0.04761905
#> 21 0.04761905
#> 22 0.04761905
#> 23 0.52380952
#> 24 0.04761905
#> 25 0.04761905
#> 26 0.04761905
#> 27 0.04761905
summary(perm, what = "centralities")
#>                      pair      state centrality diff_true effect_size
#> 1 Cluster 1 vs. Cluster 2 Disengaged InStrength     -0.29      -12.28
#> 2 Cluster 1 vs. Cluster 2    Engaged InStrength      0.14       10.13
#> 3 Cluster 1 vs. Cluster 2   Moderate InStrength      0.46       14.09
#> 4 Cluster 1 vs. Cluster 3 Disengaged InStrength     -0.10       -7.64
#> 5 Cluster 1 vs. Cluster 3    Engaged InStrength      0.02        1.65
#> 6 Cluster 1 vs. Cluster 3   Moderate InStrength      0.19       16.67
#> 7 Cluster 2 vs. Cluster 3 Disengaged InStrength      0.18        9.09
#> 8 Cluster 2 vs. Cluster 3    Engaged InStrength     -0.12       -6.73
#> 9 Cluster 2 vs. Cluster 3   Moderate InStrength     -0.26      -19.83
#>      p_value
#> 1 0.04761905
#> 2 0.04761905
#> 3 0.04761905
#> 4 0.04761905
#> 5 0.09523810
#> 6 0.04761905
#> 7 0.04761905
#> 8 0.04761905
#> 9 0.04761905
```

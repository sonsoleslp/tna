# Compare Networks using a Permutation Test

Test edge weight differences between all pairs or a subset of pairs of a
`group_tna` object. See
[`permutation_test.tna()`](http://sonsoles.me/tna/reference/permutation_test.md)
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
  ...
)
```

## Arguments

- x:

  A `group_tna` object

- groups:

  An `integer` vector or a `character` vector of group indices or names,
  respectively, defining which groups to compare. When not provided, all
  pairs are compared (the default).

- adjust:

  A `character` string for the method to adjust p-values with for
  multiple comparisons. The default is `"none"` for no adjustment. See
  the `method` argument of
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
  details and available adjustment methods.

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
  [`centralities()`](http://sonsoles.me/tna/reference/centralities.md)
  for a list of available centrality measures.

- consecutive:

  A `logical` value. If `FALSE` (the default), all pairwise comparisons
  are performed in lexicographic order with respect to the order of the
  groups. If `TRUE`, only comparisons between consecutive pairs of
  groups are performed.

- ...:

  Additional arguments passed to
  [`centralities()`](http://sonsoles.me/tna/reference/centralities.md).

## See also

Validation functions
[`bootstrap()`](http://sonsoles.me/tna/reference/bootstrap.md),
[`deprune()`](http://sonsoles.me/tna/reference/deprune.md),
[`estimate_cs()`](http://sonsoles.me/tna/reference/estimate_centrality_stability.md),
[`permutation_test()`](http://sonsoles.me/tna/reference/permutation_test.md),
[`plot.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/plot.group_tna_bootstrap.md),
[`plot.group_tna_permutation()`](http://sonsoles.me/tna/reference/plot.group_tna_permutation.md),
[`plot.group_tna_stability()`](http://sonsoles.me/tna/reference/plot.group_tna_stability.md),
[`plot.tna_bootstrap()`](http://sonsoles.me/tna/reference/plot.tna_bootstrap.md),
[`plot.tna_permutation()`](http://sonsoles.me/tna/reference/plot.tna_permutation.md),
[`plot.tna_reliability()`](http://sonsoles.me/tna/reference/plot.tna_reliability.md),
[`plot.tna_stability()`](http://sonsoles.me/tna/reference/plot.tna_stability.md),
[`print.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/print.group_tna_bootstrap.md),
[`print.group_tna_permutation()`](http://sonsoles.me/tna/reference/print.group_tna_permutation.md),
[`print.group_tna_stability()`](http://sonsoles.me/tna/reference/print.group_tna_stability.md),
[`print.summary.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/print.summary.group_tna_bootstrap.md),
[`print.summary.tna_bootstrap()`](http://sonsoles.me/tna/reference/print.summary.tna_bootstrap.md),
[`print.tna_bootstrap()`](http://sonsoles.me/tna/reference/print.tna_bootstrap.md),
[`print.tna_clustering()`](http://sonsoles.me/tna/reference/print.tna_clustering.md),
[`print.tna_permutation()`](http://sonsoles.me/tna/reference/print.tna_permutation.md),
[`print.tna_reliability()`](http://sonsoles.me/tna/reference/print.tna_reliability.md),
[`print.tna_stability()`](http://sonsoles.me/tna/reference/print.tna_stability.md),
[`prune()`](http://sonsoles.me/tna/reference/prune.md),
[`pruning_details()`](http://sonsoles.me/tna/reference/pruning_details.md),
[`reliability()`](http://sonsoles.me/tna/reference/reliability.md),
[`reprune()`](http://sonsoles.me/tna/reference/reprune.md),
[`summary.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md),
[`summary.tna_bootstrap()`](http://sonsoles.me/tna/reference/summary.tna_bootstrap.md)

## Examples

``` r
model <- group_model(engagement_mmm)
# Small number of iterations for CRAN
permutation_test(model, iter = 20)
#> Cluster 1 vs. Cluster 2 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Active -> Active            0.0189       0.695  0.667 
#> 2 Average -> Active           0.220        3.89   0.0476
#> 3 Disengaged -> Active       -0.108       -3.93   0.0476
#> 4 Active -> Average          -0.0699      -2.86   0.0476
#> 5 Average -> Average         -0.0875      -1.66   0.190 
#> 6 Disengaged -> Average      -0.349       -7.58   0.0476
#> 7 Active -> Disengaged        0.0509       2.57   0.0476
#> 8 Average -> Disengaged      -0.132       -2.96   0.0476
#> 9 Disengaged -> Disengaged    0.457        9.54   0.0476
#> 
#> Cluster 1 vs. Cluster 3 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Active -> Active            0.277         6.46  0.0476
#> 2 Average -> Active           0.159         1.88  0.0952
#> 3 Disengaged -> Active        0.0479        1.53  0.190 
#> 4 Active -> Average          -0.0358       -1.19  0.238 
#> 5 Average -> Average         -0.277        -3.12  0.0476
#> 6 Disengaged -> Average      -0.438        -6.63  0.0476
#> 7 Active -> Disengaged       -0.241        -8.55  0.0476
#> 8 Average -> Disengaged       0.118         1.59  0.190 
#> 9 Disengaged -> Disengaged    0.390         5.25  0.0476
#> 
#> Cluster 2 vs. Cluster 3 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Active -> Active            0.258        4.22   0.0476
#> 2 Average -> Active          -0.0602      -1.30   0.333 
#> 3 Disengaged -> Active        0.156        1.98   0.0952
#> 4 Active -> Average           0.0341       1.03   0.286 
#> 5 Average -> Average         -0.190       -2.30   0.0952
#> 6 Disengaged -> Average      -0.0889      -0.803  0.333 
#> 7 Active -> Disengaged       -0.292       -6.08   0.0476
#> 8 Average -> Disengaged       0.25         3.11   0.0476
#> 9 Disengaged -> Disengaged   -0.0667      -1.05   0.333 
#> 
```

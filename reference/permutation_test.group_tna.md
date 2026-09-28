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
  [`centralities()`](https://sonsoles.me/tna/reference/centralities.md)
  for a list of available centrality measures.

- consecutive:

  A `logical` value. If `FALSE` (the default), all pairwise comparisons
  are performed in lexicographic order with respect to the order of the
  groups. If `TRUE`, only comparisons between consecutive pairs of
  groups are performed.

- ...:

  Additional arguments passed to
  [`centralities()`](https://sonsoles.me/tna/reference/centralities.md).

## See also

Validation functions
[`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md),
[`deprune()`](https://sonsoles.me/tna/reference/deprune.md),
[`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
[`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md),
[`plot.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/plot.group_tna_bootstrap.md),
[`plot.group_tna_permutation()`](https://sonsoles.me/tna/reference/plot.group_tna_permutation.md),
[`plot.group_tna_stability()`](https://sonsoles.me/tna/reference/plot.group_tna_stability.md),
[`plot.tna_bootstrap()`](https://sonsoles.me/tna/reference/plot.tna_bootstrap.md),
[`plot.tna_permutation()`](https://sonsoles.me/tna/reference/plot.tna_permutation.md),
[`plot.tna_reliability()`](https://sonsoles.me/tna/reference/plot.tna_reliability.md),
[`plot.tna_stability()`](https://sonsoles.me/tna/reference/plot.tna_stability.md),
[`print.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/print.group_tna_bootstrap.md),
[`print.group_tna_permutation()`](https://sonsoles.me/tna/reference/print.group_tna_permutation.md),
[`print.group_tna_stability()`](https://sonsoles.me/tna/reference/print.group_tna_stability.md),
[`print.summary.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/print.summary.group_tna_bootstrap.md),
[`print.summary.tna_bootstrap()`](https://sonsoles.me/tna/reference/print.summary.tna_bootstrap.md),
[`print.tna_bootstrap()`](https://sonsoles.me/tna/reference/print.tna_bootstrap.md),
[`print.tna_clustering()`](https://sonsoles.me/tna/reference/print.tna_clustering.md),
[`print.tna_permutation()`](https://sonsoles.me/tna/reference/print.tna_permutation.md),
[`print.tna_reliability()`](https://sonsoles.me/tna/reference/print.tna_reliability.md),
[`print.tna_stability()`](https://sonsoles.me/tna/reference/print.tna_stability.md),
[`prune()`](https://sonsoles.me/tna/reference/prune.md),
[`pruning_details()`](https://sonsoles.me/tna/reference/pruning_details.md),
[`reliability()`](https://sonsoles.me/tna/reference/reliability.md),
[`reprune()`](https://sonsoles.me/tna/reference/reprune.md),
[`summary.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md),
[`summary.tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.tna_bootstrap.md)

## Examples

``` r
model <- group_model(engagement_mmm)
# Small number of iterations for CRAN
permutation_test(model, iter = 20)
#> Cluster 1 vs. Cluster 2 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Disengaged -> Disengaged  -0.230       -19.0    0.0476
#> 2 Engaged -> Disengaged     -0.288       -13.3    0.0476
#> 3 Moderate -> Disengaged     0.00235       0.204  0.857 
#> 4 Disengaged -> Engaged      0.0706        6.46   0.0476
#> 5 Engaged -> Engaged        -0.00994      -0.523  0.714 
#> 6 Moderate -> Engaged        0.0706        7.91   0.0476
#> 7 Disengaged -> Moderate     0.160        17.9    0.0476
#> 8 Engaged -> Moderate        0.298        14.5    0.0476
#> 9 Moderate -> Moderate      -0.0730       -5.76   0.0476
#> 
#> Cluster 1 vs. Cluster 3 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Disengaged -> Disengaged  -0.195       -15.5    0.0476
#> 2 Engaged -> Disengaged     -0.0422       -5.67   0.0476
#> 3 Moderate -> Disengaged    -0.0609       -7.65   0.0476
#> 4 Disengaged -> Engaged      0.0471        6.13   0.0476
#> 5 Engaged -> Engaged        -0.00360      -0.337  0.762 
#> 6 Moderate -> Engaged       -0.0256       -3.99   0.0476
#> 7 Disengaged -> Moderate     0.147        16.7    0.0476
#> 8 Engaged -> Moderate        0.0458        3.94   0.0476
#> 9 Moderate -> Moderate       0.0864        7.24   0.0476
#> 
#> Cluster 2 vs. Cluster 3 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Disengaged -> Disengaged   0.0358        5.69   0.0476
#> 2 Engaged -> Disengaged      0.246        21.6    0.0476
#> 3 Moderate -> Disengaged    -0.0632       -6.93   0.0476
#> 4 Disengaged -> Engaged     -0.0234       -3.48   0.0476
#> 5 Engaged -> Engaged         0.00633       0.421  0.667 
#> 6 Moderate -> Engaged       -0.0962       -8.04   0.0476
#> 7 Disengaged -> Moderate    -0.0124       -6.56   0.0476
#> 8 Engaged -> Moderate       -0.252       -23.2    0.0476
#> 9 Moderate -> Moderate       0.159        10.0    0.0476
#> 
```

# Print Permutation Test Results

Print Permutation Test Results

## Usage

``` r
# S3 method for class 'group_tna_permutation'
print(x, ...)
```

## Arguments

- x:

  A `group_tna_permutation` object.

- ...:

  Arguments passed to
  [`print.tna_permutation()`](http://sonsoles.me/tna/reference/print.tna_permutation.md).

## Value

`x` (invisibly).

## See also

Validation functions
[`bootstrap()`](http://sonsoles.me/tna/reference/bootstrap.md),
[`deprune()`](http://sonsoles.me/tna/reference/deprune.md),
[`estimate_cs()`](http://sonsoles.me/tna/reference/estimate_centrality_stability.md),
[`permutation_test()`](http://sonsoles.me/tna/reference/permutation_test.md),
[`permutation_test.group_tna()`](http://sonsoles.me/tna/reference/permutation_test.group_tna.md),
[`plot.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/plot.group_tna_bootstrap.md),
[`plot.group_tna_permutation()`](http://sonsoles.me/tna/reference/plot.group_tna_permutation.md),
[`plot.group_tna_stability()`](http://sonsoles.me/tna/reference/plot.group_tna_stability.md),
[`plot.tna_bootstrap()`](http://sonsoles.me/tna/reference/plot.tna_bootstrap.md),
[`plot.tna_permutation()`](http://sonsoles.me/tna/reference/plot.tna_permutation.md),
[`plot.tna_reliability()`](http://sonsoles.me/tna/reference/plot.tna_reliability.md),
[`plot.tna_stability()`](http://sonsoles.me/tna/reference/plot.tna_stability.md),
[`print.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/print.group_tna_bootstrap.md),
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
perm <- permutation_test(model, iter = 20)
print(perm)
#> Cluster 1 vs. Cluster 2 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Active -> Active            0.0189       0.666  0.619 
#> 2 Average -> Active           0.220        3.09   0.0476
#> 3 Disengaged -> Active       -0.108       -4.80   0.0476
#> 4 Active -> Average          -0.0699      -2.86   0.0476
#> 5 Average -> Average         -0.0875      -1.24   0.190 
#> 6 Disengaged -> Average      -0.349       -7.20   0.0476
#> 7 Active -> Disengaged        0.0509       2.60   0.0476
#> 8 Average -> Disengaged      -0.132       -2.58   0.0476
#> 9 Disengaged -> Disengaged    0.457        9.37   0.0476
#> 
#> Cluster 1 vs. Cluster 3 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Active -> Active            0.277        5.79   0.0476
#> 2 Average -> Active           0.159        1.79   0.0952
#> 3 Disengaged -> Active        0.0479       1.34   0.286 
#> 4 Active -> Average          -0.0358      -0.699  0.524 
#> 5 Average -> Average         -0.277       -3.34   0.0476
#> 6 Disengaged -> Average      -0.438       -6.04   0.0476
#> 7 Active -> Disengaged       -0.241       -8.81   0.0476
#> 8 Average -> Disengaged       0.118        1.86   0.0952
#> 9 Disengaged -> Disengaged    0.390        4.77   0.0476
#> 
#> Cluster 2 vs. Cluster 3 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Active -> Active            0.258        3.64   0.0476
#> 2 Average -> Active          -0.0602      -1.31   0.238 
#> 3 Disengaged -> Active        0.156        2.18   0.0476
#> 4 Active -> Average           0.0341       0.990  0.333 
#> 5 Average -> Average         -0.190       -2.08   0.143 
#> 6 Disengaged -> Average      -0.0889      -0.898  0.333 
#> 7 Active -> Disengaged       -0.292       -5.77   0.0476
#> 8 Average -> Disengaged       0.25         2.64   0.0476
#> 9 Disengaged -> Disengaged   -0.0667      -0.683  0.476 
#> 
```

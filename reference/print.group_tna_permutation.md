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
[`plot.tna_stability()`](http://sonsoles.me/tna/reference/plot.tna_stability.md),
[`print.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/print.group_tna_bootstrap.md),
[`print.group_tna_stability()`](http://sonsoles.me/tna/reference/print.group_tna_stability.md),
[`print.summary.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/print.summary.group_tna_bootstrap.md),
[`print.summary.tna_bootstrap()`](http://sonsoles.me/tna/reference/print.summary.tna_bootstrap.md),
[`print.tna_bootstrap()`](http://sonsoles.me/tna/reference/print.tna_bootstrap.md),
[`print.tna_permutation()`](http://sonsoles.me/tna/reference/print.tna_permutation.md),
[`print.tna_stability()`](http://sonsoles.me/tna/reference/print.tna_stability.md),
[`prune()`](http://sonsoles.me/tna/reference/prune.md),
[`pruning_details()`](http://sonsoles.me/tna/reference/pruning_details.md),
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
#> 1 Active -> Active            0.0189       0.774  0.476 
#> 2 Average -> Active           0.220        3.17   0.0476
#> 3 Disengaged -> Active       -0.108       -4.41   0.0476
#> 4 Active -> Average          -0.0699      -2.90   0.0476
#> 5 Average -> Average         -0.0875      -1.25   0.286 
#> 6 Disengaged -> Average      -0.349       -5.49   0.0476
#> 7 Active -> Disengaged        0.0509       3.28   0.0476
#> 8 Average -> Disengaged      -0.132       -4.36   0.0476
#> 9 Disengaged -> Disengaged    0.457        7.67   0.0476
#> 
#> Cluster 1 vs. Cluster 3 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Active -> Active            0.277        6.97   0.0476
#> 2 Average -> Active           0.159        1.67   0.143 
#> 3 Disengaged -> Active        0.0479       1.63   0.190 
#> 4 Active -> Average          -0.0358      -0.921  0.381 
#> 5 Average -> Average         -0.277       -2.96   0.0476
#> 6 Disengaged -> Average      -0.438       -9.12   0.0476
#> 7 Active -> Disengaged       -0.241      -11.7    0.0476
#> 8 Average -> Disengaged       0.118        1.88   0.143 
#> 9 Disengaged -> Disengaged    0.390        7.17   0.0476
#> 
#> Cluster 2 vs. Cluster 3 :
#> # A tibble: 9 × 4
#>   edge_name                diff_true effect_size p_value
#>   <chr>                        <dbl>       <dbl>   <dbl>
#> 1 Active -> Active            0.258        3.80   0.0476
#> 2 Average -> Active          -0.0602      -1.25   0.190 
#> 3 Disengaged -> Active        0.156        2.04   0.0476
#> 4 Active -> Average           0.0341       0.966  0.381 
#> 5 Average -> Average         -0.190       -2.09   0.0476
#> 6 Disengaged -> Average      -0.0889      -0.992  0.429 
#> 7 Active -> Disengaged       -0.292       -4.70   0.0476
#> 8 Average -> Disengaged       0.25         2.73   0.0476
#> 9 Disengaged -> Disengaged   -0.0667      -0.996  0.381 
#> 
```

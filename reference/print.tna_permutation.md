# Print Permutation Test Results

Print Permutation Test Results

## Usage

``` r
# S3 method for class 'tna_permutation'
print(x, ...)
```

## Arguments

- x:

  A `tna_permutation` object.

- ...:

  Additional arguments passed to the `tibble` print method.

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
[`print.group_tna_permutation()`](http://sonsoles.me/tna/reference/print.group_tna_permutation.md),
[`print.group_tna_stability()`](http://sonsoles.me/tna/reference/print.group_tna_stability.md),
[`print.summary.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/print.summary.group_tna_bootstrap.md),
[`print.summary.tna_bootstrap()`](http://sonsoles.me/tna/reference/print.summary.tna_bootstrap.md),
[`print.tna_bootstrap()`](http://sonsoles.me/tna/reference/print.tna_bootstrap.md),
[`print.tna_stability()`](http://sonsoles.me/tna/reference/print.tna_stability.md),
[`prune()`](http://sonsoles.me/tna/reference/prune.md),
[`pruning_details()`](http://sonsoles.me/tna/reference/pruning_details.md),
[`reprune()`](http://sonsoles.me/tna/reference/reprune.md),
[`summary.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md),
[`summary.tna_bootstrap()`](http://sonsoles.me/tna/reference/summary.tna_bootstrap.md)

## Examples

``` r
model_x <- tna(group_regulation[1:200, ])
model_y <- tna(group_regulation[1001:1200, ])
# Small number of iterations for CRAN
perm <- permutation_test(model_x, model_y, iter = 20)
print(perm)
#> # A tibble: 81 × 4
#>    edge_name           diff_true effect_size p_value
#>    <chr>                   <dbl>       <dbl>   <dbl>
#>  1 adapt -> adapt       0            NaN      1     
#>  2 cohesion -> adapt    0.00541        0.945  0.857 
#>  3 consensus -> adapt  -0.000679      -0.175  0.810 
#>  4 coregulate -> adapt  0.00769        0.519  0.762 
#>  5 discuss -> adapt    -0.130         -7.31   0.0476
#>  6 emotion -> adapt     0.0101         1.70   0.286 
#>  7 monitor -> adapt    -0.00480       -0.375  1     
#>  8 plan -> adapt        0.00339        1.55   0.0476
#>  9 synthesis -> adapt  -0.159         -1.58   0.190 
#> 10 adapt -> cohesion   -0.0907        -1.13   0.429 
#> # ℹ 71 more rows
```

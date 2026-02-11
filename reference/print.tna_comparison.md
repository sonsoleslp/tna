# Print Comparison Results

Print Comparison Results

## Usage

``` r
# S3 method for class 'tna_comparison'
print(x, ...)
```

## Arguments

- x:

  A `tna_comparison` object.

- ...:

  Additional arguments passed to the tibble `print` method.

## Value

`x` (invisibly).

## See also

Model comparison functions
[`compare()`](http://sonsoles.me/tna/reference/compare.md),
[`compare.group_tna()`](http://sonsoles.me/tna/reference/compare.group_tna.md),
[`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_comparison()`](http://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md),
[`plot_compare.group_tna()`](http://sonsoles.me/tna/reference/plot_compare.group_tna.md),
[`print.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)

## Examples

``` r
model_x <- tna(group_regulation[1:200, ])
model_y <- tna(group_regulation[1001:1200, ])
comp <- compare(model_x, model_y)
print(comp)
#> Edge difference metrics
#> # A tibble: 81 × 16
#>    source     target   weight_x weight_y raw_difference absolute_difference
#>    <fct>      <fct>       <dbl>    <dbl>          <dbl>               <dbl>
#>  1 adapt      adapt     0        0             0                   0       
#>  2 cohesion   adapt     0.00541  0             0.00541             0.00541 
#>  3 consensus  adapt     0.00435  0.00503      -0.000679            0.000679
#>  4 coregulate adapt     0.0252   0.0175        0.00769             0.00769 
#>  5 discuss    adapt     0.0103   0.140        -0.130               0.130   
#>  6 emotion    adapt     0.0101   0             0.0101              0.0101  
#>  7 monitor    adapt     0.00794  0.0127       -0.00480             0.00480 
#>  8 plan       adapt     0.00339  0             0.00339             0.00339 
#>  9 synthesis  adapt     0.175    0.333        -0.159               0.159   
#> 10 adapt      cohesion  0.2      0.291        -0.0907              0.0907  
#> # ℹ 71 more rows
#> # ℹ 10 more variables: squared_difference <dbl>, relative_difference <dbl>,
#> #   similarity_strength_index <dbl>, difference_index <dbl>,
#> #   rank_difference <dbl>, percentile_difference <dbl>,
#> #   logarithmic_ratio <dbl>, standardized_weight_x <dbl>,
#> #   standardized_weight_y <dbl>, standardized_score_inflation <dbl>
#> 
#> Summary metrics of differences
#> # A tibble: 22 × 3
#>    category          metric                value
#>    <chr>             <chr>                 <dbl>
#>  1 Weight Deviations Mean Abs. Diff.      0.0437
#>  2 Weight Deviations Median Abs. Diff.    0.0278
#>  3 Weight Deviations RMS Diff.            0.0667
#>  4 Weight Deviations Max Abs. Diff.       0.251 
#>  5 Weight Deviations Rel. Mean Abs. Diff. 0.393 
#>  6 Weight Deviations CV Ratio             1.14  
#>  7 Correlations      Pearson              0.882 
#>  8 Correlations      Spearman             0.834 
#>  9 Correlations      Kendall              0.665 
#> 10 Correlations      Distance             0.772 
#> # ℹ 12 more rows
#> 
#> Network metrics
#> # A tibble: 13 × 3
#>    metric                             x        y
#>    <chr>                          <dbl>    <dbl>
#>  1 Node Count                  9   e+ 0 9   e+ 0
#>  2 Edge Count                  7.2 e+ 1 7.1 e+ 1
#>  3 Network Density             1   e+ 0 9.86e- 1
#>  4 Mean Distance               4.86e- 2 6.27e- 2
#>  5 Mean Out-Strength           1   e+ 0 1   e+ 0
#>  6 SD Out-Strength             9.68e- 1 7.04e- 1
#>  7 Mean In-Strength            1   e+ 0 1   e+ 0
#>  8 SD In-Strength              5.55e-17 3.93e-17
#>  9 Mean Out-Degree             8   e+ 0 7.89e+ 0
#> 10 SD Out-Degree               1.80e+ 0 1.17e+ 0
#> 11 Centralization (Out-Degree) 1.09e- 1 1.25e- 1
#> 12 Centralization (In-Degree)  1.09e- 1 1.25e- 1
#> 13 Reciprocity                 8.92e- 1 8.75e- 1
```

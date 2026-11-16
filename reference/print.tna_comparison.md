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
[`compare()`](https://sonsoles.me/tna/reference/compare.md),
[`compare.group_tna()`](https://sonsoles.me/tna/reference/compare.group_tna.md),
[`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_comparison()`](https://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot.tna_sequence_comparison()`](https://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare()`](https://sonsoles.me/tna/reference/plot_compare.md),
[`plot_compare.group_tna()`](https://sonsoles.me/tna/reference/plot_compare.group_tna.md),
[`print.tna_sequence_comparison()`](https://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)

## Examples

``` r
model_x <- tna(head(group_regulation, 200))
model_y <- tna(tail(group_regulation, 200))
comp <- compare(model_x, model_y)
print(comp)
#> Edge difference metrics
#> # A tibble: 81 × 16
#>    source     target   weight_x weight_y raw_difference absolute_difference
#>    <fct>      <fct>       <dbl>    <dbl>          <dbl>               <dbl>
#>  1 adapt      adapt     0        0              0                   0      
#>  2 cohesion   adapt     0.00541  0              0.00541             0.00541
#>  3 consensus  adapt     0.00435  0.00329        0.00106             0.00106
#>  4 coregulate adapt     0.0252   0.0140         0.0111              0.0111 
#>  5 discuss    adapt     0.0103   0.126         -0.116               0.116  
#>  6 emotion    adapt     0.0101   0.00735        0.00275             0.00275
#>  7 monitor    adapt     0.00794  0.0182        -0.0102              0.0102 
#>  8 plan       adapt     0.00339  0.00159        0.00180             0.00180
#>  9 synthesis  adapt     0.175    0.261         -0.0863              0.0863 
#> 10 adapt      cohesion  0.2      0.236         -0.0361              0.0361 
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
#>  1 Weight Deviations Mean Abs. Diff.      0.0438
#>  2 Weight Deviations Median Abs. Diff.    0.0260
#>  3 Weight Deviations RMS Diff.            0.0651
#>  4 Weight Deviations Max Abs. Diff.       0.241 
#>  5 Weight Deviations Rel. Mean Abs. Diff. 0.394 
#>  6 Weight Deviations CV Ratio             1.18  
#>  7 Correlations      Pearson              0.889 
#>  8 Correlations      Spearman             0.812 
#>  9 Correlations      Kendall              0.653 
#> 10 Correlations      Distance             0.873 
#> # ℹ 12 more rows
#> 
#> Network metrics
#> # A tibble: 13 × 3
#>    metric                             x        y
#>    <chr>                          <dbl>    <dbl>
#>  1 Node Count                  9   e+ 0 9   e+ 0
#>  2 Edge Count                  7.2 e+ 1 7.4 e+ 1
#>  3 Network Density             1   e+ 0 1   e+ 0
#>  4 Mean Distance               4.86e- 2 5.89e- 2
#>  5 Mean Out-Strength           1   e+ 0 1   e+ 0
#>  6 SD Out-Strength             5.55e-17 3.93e-17
#>  7 Mean In-Strength            1   e+ 0 1   e+ 0
#>  8 SD In-Strength              9.68e- 1 7.34e- 1
#>  9 Mean Out-Degree             8   e+ 0 8.22e+ 0
#> 10 SD Out-Degree               1.80e+ 0 8.33e- 1
#> 11 Centralization (Out-Degree) 1.09e- 1 7.81e- 2
#> 12 Centralization (In-Degree)  1.09e- 1 7.81e- 2
#> 13 Reciprocity                 8.92e- 1 9.25e- 1
```

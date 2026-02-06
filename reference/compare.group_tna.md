# Compare Grouped TNA Models with Comprehensive Metrics

Compare Grouped TNA Models with Comprehensive Metrics

## Usage

``` r
# S3 method for class 'group_tna'
compare(x, i = 1L, j = 2L, scaling = "none", ...)
```

## Arguments

- x:

  A `group_tna` object.

- i:

  An `integer` index or the name of the principal cluster as a
  `character` string.

- j:

  An `integer` index or the name of the secondary cluster as a
  `character` string.

- scaling:

  See [`compare.tna()`](http://sonsoles.me/tna/reference/compare.md).

- ...:

  Additional arguments passed to
  [`compare.tna()`](http://sonsoles.me/tna/reference/compare.md).

## Value

A `tna_comparison` object. See
[`compare.tna()`](http://sonsoles.me/tna/reference/compare.md) for
details.

## See also

Model comparison functions
[`compare()`](http://sonsoles.me/tna/reference/compare.md),
[`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_comparison()`](http://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md),
[`plot_compare.group_tna()`](http://sonsoles.me/tna/reference/plot_compare.group_tna.md),
[`print.tna_comparison()`](http://sonsoles.me/tna/reference/print.tna_comparison.md),
[`print.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)

## Examples

``` r
model <- group_model(engagement_mmm)
compare(model, i = 1, j = 2)
#> Edge difference metrics
#> # A tibble: 9 × 16
#>   source     target     weight_x weight_y raw_difference absolute_difference
#>   <fct>      <fct>         <dbl>    <dbl>          <dbl>               <dbl>
#> 1 Active     Active       0.860    0.841          0.0189              0.0189
#> 2 Average    Active       0.312    0.0926         0.220               0.220 
#> 3 Disengaged Active       0.0479   0.156         -0.108               0.108 
#> 4 Active     Average      0.0892   0.159         -0.0699              0.0699
#> 5 Average    Average      0.542    0.630         -0.0875              0.0875
#> 6 Disengaged Average      0.162    0.511         -0.349               0.349 
#> 7 Active     Disengaged   0.0509   0              0.0509              0.0509
#> 8 Average    Disengaged   0.146    0.278         -0.132               0.132 
#> 9 Disengaged Disengaged   0.790    0.333          0.457               0.457 
#> # ℹ 10 more variables: squared_difference <dbl>, relative_difference <dbl>,
#> #   similarity_strength_index <dbl>, difference_index <dbl>,
#> #   rank_difference <dbl>, percentile_difference <dbl>,
#> #   logarithmic_ratio <dbl>, standardized_weight_x <dbl>,
#> #   standardized_weight_y <dbl>, standardized_score_inflation <dbl>
#> 
#> Summary metrics of differences
#> # A tibble: 22 × 3
#>    category          metric               value
#>    <chr>             <chr>                <dbl>
#>  1 Weight Deviations Mean Abs. Diff.      0.166
#>  2 Weight Deviations Median Abs. Diff.    0.108
#>  3 Weight Deviations RMS Diff.            0.217
#>  4 Weight Deviations Max Abs. Diff.       0.457
#>  5 Weight Deviations Rel. Mean Abs. Diff. 0.498
#>  6 Weight Deviations CV Ratio             1.16 
#>  7 Correlations      Pearson              0.710
#>  8 Correlations      Spearman             0.733
#>  9 Correlations      Kendall              0.611
#> 10 Correlations      Distance             0.500
#> # ℹ 12 more rows
#> 
#> Network metrics
#> # A tibble: 13 × 3
#>    metric                          x     y
#>    <chr>                       <dbl> <dbl>
#>  1 Node Count                  3     3    
#>  2 Edge Count                  9     8    
#>  3 Network Density             1     1    
#>  4 Mean Distance               0.111 0.239
#>  5 Mean Out-Strength           1     1    
#>  6 SD Out-Strength             0.214 0.353
#>  7 Mean In-Strength            1     1    
#>  8 SD In-Strength              0     0    
#>  9 Mean Out-Degree             3     2.67 
#> 10 SD Out-Degree               0     0.577
#> 11 Centralization (Out-Degree) 0     0.25 
#> 12 Centralization (In-Degree)  0     0.25 
#> 13 Reciprocity                 1     0.8  
#> 
#> Centrality differences
#> # A tibble: 27 × 5
#>    state   centrality         x     y difference
#>    <fct>   <chr>          <dbl> <dbl>      <dbl>
#>  1 Active  OutStrength    0.140 0.159    -0.0189
#>  2 Active  InStrength     0.360 0.248     0.112 
#>  3 Active  ClosenessIn    4.14  4.03      0.109 
#>  4 Active  ClosenessOut   7.14  1.68      5.46  
#>  5 Active  Closeness      7.29  4.03      3.26  
#>  6 Active  Betweenness    1     1         0     
#>  7 Active  BetweennessRSP 2     1         1     
#>  8 Active  Diffusion      0.202 0.250    -0.0474
#>  9 Active  Clustering     0.308 0.789    -0.481 
#> 10 Average OutStrength    0.458 0.370     0.0875
#> # ℹ 17 more rows
#> 
#> Centrality correlations
#> # A tibble: 9 × 3
#>   centrality     Centrality     correlation
#>   <chr>          <chr>                <dbl>
#> 1 Betweenness    Betweenness         -0.5  
#> 2 BetweennessRSP BetweennessRSP       0.327
#> 3 Closeness      Closeness            0.805
#> 4 ClosenessIn    ClosenessIn         -0.883
#> 5 ClosenessOut   ClosenessOut        -1.000
#> 6 Clustering     Clustering           0.350
#> 7 Diffusion      Diffusion            0.300
#> 8 InStrength     InStrength          -0.252
#> 9 OutStrength    OutStrength          0.113
```

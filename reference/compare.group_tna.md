# Compare Grouped TNA Models with Comprehensive Metrics

Compare Grouped TNA Models with Comprehensive Metrics

## Usage

``` r
# S3 method for class 'group_tna'
compare(
  x,
  i = 1L,
  j = 2L,
  scaling = "none",
  measures = character(0),
  network = TRUE,
  ...
)
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

  A `character` string naming a scaling method to apply to the weights
  before comparing them. The supported options are:

  - `"none"`: No scaling is performed. The weights are used as is.

  - `"minmax"`: Performs min-max normalization, i.e., the minimum value
    is subtracted and the differences are scaled by the range.

  - `"max"`: Max-normalization: the values are divided by the maximum
    value.

  - `"rank"`: Applies min-max normalization to the ranks of the weights
    (computed with `ties.method = "average"`).

  - `"zscore"`: Computes the standard score, i.e. the mean weight is
    subtracted and the differences are scaled by the standard deviation.

  - `"robust"`: Computes the robust z-score, i.e. the median weight is
    subtracted and the differences are scaled by the median absolute
    deviation (using [stats::mad](https://rdrr.io/r/stats/mad.html)).

  - `"log"`: Simply the natural logarithm of the weights.

  - `"log1p"`: As above, but adds 1 to the values before taking the
    logarithm. Useful for scenarios with zero weights.

  - `"softmax"`: Performs softmax normalization.

  - `"quantile"`: Uses the empirical quantiles of the weights via
    [stats::ecdf](https://rdrr.io/r/stats/ecdf.html).

- measures:

  A `character` vector indicating which centrality measures should be
  computed. See
  [`centralities()`](https://sonsoles.me/tna/reference/centralities.md)
  for the available measures. No measures are included by default.

- network:

  A `logical` value indicating whether network metrics should be
  included in the comparison. The default is `TRUE`.

- ...:

  Additional arguments passed to
  [`compare.tna()`](https://sonsoles.me/tna/reference/compare.md).

## Value

A `tna_comparison` object. See
[`compare.tna()`](https://sonsoles.me/tna/reference/compare.md) for
details.

## See also

Model comparison functions
[`compare()`](https://sonsoles.me/tna/reference/compare.md),
[`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_comparison()`](https://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot.tna_sequence_comparison()`](https://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare()`](https://sonsoles.me/tna/reference/plot_compare.md),
[`plot_compare.group_tna()`](https://sonsoles.me/tna/reference/plot_compare.group_tna.md),
[`print.tna_comparison()`](https://sonsoles.me/tna/reference/print.tna_comparison.md),
[`print.tna_sequence_comparison()`](https://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)

## Examples

``` r
model <- group_model(engagement_mmm)
compare(model, i = 1, j = 2)
#> Edge difference metrics
#> # A tibble: 9 × 16
#>   source     target     weight_x weight_y raw_difference absolute_difference
#>   <fct>      <fct>         <dbl>    <dbl>          <dbl>               <dbl>
#> 1 Disengaged Disengaged   0.681   0.911         -0.230               0.230  
#> 2 Engaged    Disengaged   0.0203  0.308         -0.288               0.288  
#> 3 Moderate   Disengaged   0.121   0.118          0.00235             0.00235
#> 4 Disengaged Engaged      0.158   0.0877         0.0706              0.0706 
#> 5 Engaged    Engaged      0.662   0.672         -0.00994             0.00994
#> 6 Moderate   Engaged      0.124   0.0536         0.0706              0.0706 
#> 7 Disengaged Moderate     0.161   0.00107        0.160               0.160  
#> 8 Engaged    Moderate     0.318   0.0201         0.298               0.298  
#> 9 Moderate   Moderate     0.755   0.828         -0.0730              0.0730 
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
#>  1 Weight Deviations Mean Abs. Diff.      0.134 
#>  2 Weight Deviations Median Abs. Diff.    0.0730
#>  3 Weight Deviations RMS Diff.            0.172 
#>  4 Weight Deviations Max Abs. Diff.       0.298 
#>  5 Weight Deviations Rel. Mean Abs. Diff. 0.401 
#>  6 Weight Deviations CV Ratio             0.776 
#>  7 Correlations      Pearson              0.875 
#>  8 Correlations      Spearman             0.433 
#>  9 Correlations      Kendall              0.222 
#> 10 Correlations      Distance             0.869 
#> # ℹ 12 more rows
#> 
#> Network metrics
#> # A tibble: 13 × 3
#>    metric                          x        y
#>    <chr>                       <dbl>    <dbl>
#>  1 Node Count                  3     3   e+ 0
#>  2 Edge Count                  9     9   e+ 0
#>  3 Network Density             1     1   e+ 0
#>  4 Mean Distance               0.128 6.44e- 2
#>  5 Mean Out-Strength           1     1   e+ 0
#>  6 SD Out-Strength             0.212 2.93e- 1
#>  7 Mean In-Strength            1     1   e+ 0
#>  8 SD In-Strength              0     7.85e-17
#>  9 Mean Out-Degree             3     3   e+ 0
#> 10 SD Out-Degree               0     0       
#> 11 Centralization (Out-Degree) 0     0       
#> 12 Centralization (In-Degree)  0     0       
#> 13 Reciprocity                 1     1   e+ 0
```

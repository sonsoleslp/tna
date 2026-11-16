# Compare Two Matrices or TNA Models with Comprehensive Metrics

Various distances, measures of dissimilarity and similarity,
correlations and other metrics are computed to compare the models.
Optionally, the weight matrices of the models can be scaled before
comparison. The resulting object can be used to produce heatmap plots
and scatterplots to further illustrate the differences.

## Usage

``` r
compare(x, ...)

# S3 method for class 'tna'
compare(x, y, scaling = "none", measures = character(0), network = TRUE, ...)

# S3 method for class 'matrix'
compare(x, y, scaling = "none", measures = character(0), network = TRUE, ...)
```

## Arguments

- x:

  A `tna` object or a `matrix` of weights. A `matrix` must have the
  state names as row and column names.

- ...:

  Ignored.

- y:

  A `tna` object or a `matrix` of weights, with the same states as `x`.
  The states are matched by name, so their order may differ; an error of
  class `tna_error_states` is raised when the states differ or a matrix
  has no state names.

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

## Value

A `tna_comparison` object, which is a `list` containing the following
elements:

- `matrices`: A `list` containing the scaled matrices of the input `tna`
  objects or the scaled inputs themselves in the case of matrices.

- `difference_matrix`: A `matrix` of differences `x - y`.

- `edge_metrics`: A `data.frame` with one row per pair of states
  (columns `source` and `target`), the weights `weight_x` and
  `weight_y`, and edge-level metrics of their difference.

- `summary_metrics`: A `data.frame` with one row per summary metric of
  the differences across all edges, with the columns `category`,
  `metric`, and `value`.

- `network_metrics`: A `data.frame` with one row per network metric and
  the columns `metric`, `x`, and `y`.

- `centrality_differences`: A `data.frame` with one row per state and
  centrality measure, with the columns `state`, `centrality`, `x`, `y`,
  and `difference`. Only present when `measures` is not empty.

- `centrality_correlations`: A `data.frame` with one row per centrality
  measure and the correlation of the measure between `x` and `y`. Only
  present when `measures` is not empty.

## See also

Model comparison functions
[`compare.group_tna()`](https://sonsoles.me/tna/reference/compare.group_tna.md),
[`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_comparison()`](https://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot.tna_sequence_comparison()`](https://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare()`](https://sonsoles.me/tna/reference/plot_compare.md),
[`plot_compare.group_tna()`](https://sonsoles.me/tna/reference/plot_compare.group_tna.md),
[`print.tna_comparison()`](https://sonsoles.me/tna/reference/print.tna_comparison.md),
[`print.tna_sequence_comparison()`](https://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)

## Examples

``` r
# Comparing TNA models
model_x <- tna(head(group_regulation, 200))
model_y <- tna(tail(group_regulation, 200))
comp1 <- compare(model_x, model_y)

# Comparing weight matrices (with the states as row and column names)
states <- c("A", "B")
mat_x <- matrix(c(0.6, 0.3, 0.4, 0.7), 2, 2, dimnames = list(states, states))
mat_y <- matrix(c(0.5, 0.2, 0.5, 0.8), 2, 2, dimnames = list(states, states))
comp2 <- compare(mat_x, mat_y)
print(comp2)
#> Edge difference metrics
#> # A tibble: 4 × 16
#>   source target weight_x weight_y raw_difference absolute_difference
#>   <fct>  <fct>     <dbl>    <dbl>          <dbl>               <dbl>
#> 1 A      A           0.6      0.5          0.1                 0.1  
#> 2 B      A           0.3      0.2          0.1                 0.1  
#> 3 A      B           0.4      0.5         -0.1                 0.1  
#> 4 B      B           0.7      0.8         -0.100               0.100
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
#>  1 Weight Deviations Mean Abs. Diff.      0.1  
#>  2 Weight Deviations Median Abs. Diff.    0.1  
#>  3 Weight Deviations RMS Diff.            0.1  
#>  4 Weight Deviations Max Abs. Diff.       0.100
#>  5 Weight Deviations Rel. Mean Abs. Diff. 0.2  
#>  6 Weight Deviations CV Ratio             0.745
#>  7 Correlations      Pearson              0.894
#>  8 Correlations      Spearman             0.949
#>  9 Correlations      Kendall              0.913
#> 10 Correlations      Distance             0.865
#> # ℹ 12 more rows
#> 
#> Network metrics
#> # A tibble: 13 × 3
#>    metric                          x     y
#>    <chr>                       <dbl> <dbl>
#>  1 Node Count                  2     2    
#>  2 Edge Count                  4     4    
#>  3 Network Density             1     1    
#>  4 Mean Distance               0.35  0.35 
#>  5 Mean Out-Strength           1     1    
#>  6 SD Out-Strength             0     0    
#>  7 Mean In-Strength            1     1    
#>  8 SD In-Strength              0.141 0.424
#>  9 Mean Out-Degree             2     2    
#> 10 SD Out-Degree               0     0    
#> 11 Centralization (Out-Degree) 0     0    
#> 12 Centralization (In-Degree)  0     0    
#> 13 Reciprocity                 1     1    
```

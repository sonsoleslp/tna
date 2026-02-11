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

  A `tna` object or a `matrix` of weights.

- ...:

  Ignored.

- y:

  A `tna` object or a `matrix` of weights.

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
  [`centralities()`](http://sonsoles.me/tna/reference/centralities.md)
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

- `edge_metrics`: A `data.frame` of edge-level metrics about the
  differences.

- `summary_metrics`: A `data.frame` of summary metrics of the
  differences across all edges.

- `network_metrics`: A `data.frame` of network metrics for both `x` and
  `y`.

- `centrality_differences`: A `data.frame` of differences in centrality
  measures computes from `x` and `y`.

- `centrality_correlations`: A `numeric` vector of correlations of the
  centrality measures between `x` and `y`.

## See also

Model comparison functions
[`compare.group_tna()`](http://sonsoles.me/tna/reference/compare.group_tna.md),
[`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_comparison()`](http://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md),
[`plot_compare.group_tna()`](http://sonsoles.me/tna/reference/plot_compare.group_tna.md),
[`print.tna_comparison()`](http://sonsoles.me/tna/reference/print.tna_comparison.md),
[`print.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)

## Examples

``` r
# Comparing TNA models
model_x <- tna(group_regulation[1:200, ])
model_y <- tna(group_regulation[1001:1200, ])
comp1 <- compare(model_x, model_y)

# Comparing matrices
mat_x <- model_x$weights
mat_y <- model_y$weights
comp2 <- compare(mat_x, mat_y)

# Comparing a matrix to a TNA model
comp3 <- compare(mat_x, model_y)
```

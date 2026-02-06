# Plot Centrality Stability Results

This function visualizes the centrality stability results produced by
the `estimate_centrality_stability` function. It shows how different
centrality measures' correlations change as varying proportions of cases
are dropped, along with their confidence intervals (CIs).

## Usage

``` r
# S3 method for class 'tna_stability'
plot(x, level = 0.05, ...)
```

## Arguments

- x:

  A `tna_stability` object produced by `estimate_cs`.

- level:

  A `numeric` value representing the significance level for the
  confidence intervals. Defaults to `0.05`.

- ...:

  Ignored.

## Value

A `ggplot` object displaying the stability analysis plot.

## Details

The function aggregates the results for each centrality measure across
multiple proportions of dropped cases (e.g., 0.1, 0.2, ..., 0.9) and
calculates the mean and the desired quantiles for each proportion. The
confidence intervals (CIs) are computed based on the quantiles and
displayed in the plot.

If no valid data is available for a centrality measure (e.g., missing or
NA values), the function skips that measure with a warning.

The plot includes:

- The mean correlation for each centrality measure as a function of the
  proportion of dropped cases.

- Shaded confidence intervals representing CIs for each centrality
  measure.

- A horizontal dashed line at the threshold value used for calculating
  the CS-coefficient.

- A subtitle listing the CS-coefficients for each centrality measure.

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
[`print.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/print.group_tna_bootstrap.md),
[`print.group_tna_permutation()`](http://sonsoles.me/tna/reference/print.group_tna_permutation.md),
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
model <- tna(group_regulation)
cs <- estimate_cs(model, iter = 10)
plot(cs)

```

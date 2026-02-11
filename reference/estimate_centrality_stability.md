# Estimate Centrality Stability

Estimates the stability of centrality measures in a network using subset
sampling without replacement. It allows for dropping varying proportions
of cases and calculates correlations between the original centralities
and those computed using sampled subsets.

## Usage

``` r
estimate_cs(
  x,
  loops,
  normalize,
  invert,
  measures,
  iter,
  method,
  drop_prop,
  threshold,
  certainty,
  progressbar
)

estimate_centrality_stability(
  x,
  loops,
  normalize,
  invert,
  measures,
  iter,
  method,
  drop_prop,
  threshold,
  certainty,
  progressbar
)

# S3 method for class 'tna'
estimate_cs(
  x,
  loops = FALSE,
  normalize = FALSE,
  invert = TRUE,
  measures = c("InStrength", "OutStrength", "Betweenness"),
  iter = 1000,
  method = "pearson",
  drop_prop = seq(0.1, 0.9, by = 0.1),
  threshold = 0.7,
  certainty = 0.95,
  progressbar = FALSE
)

# S3 method for class 'tna'
estimate_centrality_stability(
  x,
  loops = FALSE,
  normalize = FALSE,
  invert = TRUE,
  measures = c("InStrength", "OutStrength", "Betweenness"),
  iter = 1000,
  method = "pearson",
  drop_prop = seq(0.1, 0.9, by = 0.1),
  threshold = 0.7,
  certainty = 0.95,
  progressbar = FALSE
)

# S3 method for class 'group_tna'
estimate_cs(
  x,
  loops = FALSE,
  normalize = FALSE,
  invert = TRUE,
  measures = c("InStrength", "OutStrength", "Betweenness"),
  iter = 1000,
  method = "pearson",
  drop_prop = seq(0.1, 0.9, by = 0.1),
  threshold = 0.7,
  certainty = 0.95,
  progressbar = FALSE
)

# S3 method for class 'group_tna'
estimate_centrality_stability(
  x,
  loops = FALSE,
  normalize = FALSE,
  invert = TRUE,
  measures = c("InStrength", "OutStrength", "Betweenness"),
  iter = 1000,
  method = "pearson",
  drop_prop = seq(0.1, 0.9, by = 0.1),
  threshold = 0.7,
  certainty = 0.95,
  progressbar = FALSE
)
```

## Arguments

- x:

  A `tna` or a `group_tna` object representing the temporal network
  analysis data. The object should be created from a sequence data
  object.

- loops:

  A `logical` value indicating whether to include loops in the network
  when computing the centrality measures. The default is `FALSE`.

- normalize:

  A `logical` value indicating whether to normalize the centrality
  measures. The default is `FALSE`.

- invert:

  A `logical` value indicating whether the weights should be inverted
  for distance-based measures. The default is `TRUE`.

- measures:

  A `character` vector of centrality measures to estimate. The default
  measures are `"InStrength"`, `"OutStrength"`, and `"Betweenness"`. See
  [`centralities()`](http://sonsoles.me/tna/reference/centralities.md)
  for a list of available centrality measures.

- iter:

  An `integer` specifying the number of resamples to draw. The default
  is 1000.

- method:

  A `character` string indicating the correlation coefficient type. The
  default is `"pearson"`. See
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html) for details.

- drop_prop:

  A `numeric` vector specifying the proportions of cases to drop in each
  sampling iteration. Default is a sequence from 0.1 to 0.9 in
  increments of 0.1.

- threshold:

  A `numeric` value specifying the correlation threshold for calculating
  the CS-coefficient. The default is 0.7.

- certainty:

  A `numeric` value specifying the desired level of certainty for the
  CS-coefficient. Default is 0.95.

- progressbar:

  A `logical` value. If `TRUE`, a progress bar is displayed Defaults to
  `FALSE`

## Value

A `tna_stability` object which is a `list` with an element for each
`measure` with the following elements:

- `cs_coefficient`: The centrality stability (CS) coefficient of the
  measure.

- `correlations`: A `matrix` of correlations between the original
  centrality and the resampled centralities for each drop proportion.

If `x` is a `group_tna` object, a `group_tna_stability` object is
returned instead, which is a `list` of `tna_stability` objects.

## Details

The function works by repeatedly resampling the data, dropping varying
proportions of cases, and calculating centrality measures on the
subsets. The correlation between the original centralities and the
resampled centralities is calculated for each drop proportion. The
stability of each centrality measure is then summarized using a
centrality stability (CS) coefficient, which represents the proportion
of dropped cases at which the correlations drop below a given threshold
(default 0.7).

The results can be visualized by plotting the output object showing the
stability of the centrality measures across different drop proportions,
along with confidence intervals. The CS-coefficients are displayed in
the subtitle.

## See also

Validation functions
[`bootstrap()`](http://sonsoles.me/tna/reference/bootstrap.md),
[`deprune()`](http://sonsoles.me/tna/reference/deprune.md),
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
[`print.group_tna_permutation()`](http://sonsoles.me/tna/reference/print.group_tna_permutation.md),
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
model <- tna(group_regulation)
# Small number of iterations and drop proportions for CRAN
estimate_cs(
  model,
  drop_prop = seq(0.3, 0.9, by = 0.2),
  measures = c("InStrength", "OutStrength"),
  iter = 10
)
#> Centrality Stability Coefficients
#> 
#>  InStrength OutStrength 
#>         0.9         0.9 
```

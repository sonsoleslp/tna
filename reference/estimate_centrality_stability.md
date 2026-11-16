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
  progressbar = NULL,
  detailed = NULL,
  actor = NULL,
  edges = FALSE
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
  progressbar = NULL,
  detailed = NULL,
  actor = NULL,
  edges = FALSE
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
  progressbar = FALSE,
  detailed = NULL,
  actor = NULL,
  edges = FALSE
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
  progressbar = FALSE,
  detailed = NULL,
  actor = NULL,
  edges = FALSE
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
  progressbar = FALSE,
  detailed = NULL,
  actor = NULL,
  edges = FALSE
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
  progressbar = FALSE,
  detailed = NULL,
  actor = NULL,
  edges = FALSE
)
```

## Arguments

- x:

  A `tna` or a `group_tna` object representing the temporal network
  analysis data. The object should be created from a sequence data
  object. Grouped models with scaled edge weights, and their networks,
  are not resampled (an error of class `tna_error_scaling`).

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
  [`centralities()`](https://sonsoles.me/tna/reference/centralities.md)
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
  CS-coefficient. Default is 0.95. The certainty is the share of all
  subsamples whose correlation reaches `threshold`; a subsample in which
  the correlation is undefined (the measure is constant in it) counts as
  one that does not reach it.

- progressbar:

  A `logical` value. If `TRUE`, a progress bar is displayed. Defaults to
  `FALSE`.

- detailed:

  Deprecated. This argument is ignored and will be removed in a future
  version.

- actor:

  An optional actor identifier for sequences nested in actors: a
  `character` string naming a non-sequence column of the data the model
  was built from, or a vector with one identifier per sequence (per
  sequence of the data the grouped model was built from, for a
  `group_tna` object). When given, `drop_prop` is the proportion of
  actors dropped, and each actor's sequences are dropped together. The
  default `NULL` drops single sequences.

- edges:

  A `logical` value. If `TRUE`, the stability of the edge weights is
  also estimated and returned as the element `Edges`. The default is
  `FALSE`.

## Value

A `tna_stability` object which is a `list` with an element for each
`measure` (and an element `Edges` when `edges = TRUE`) with the
following elements:

- `cs_coefficient`: The centrality stability (CS) coefficient of the
  measure.

- `correlations`: A `matrix` of correlations between the original
  centrality and the resampled centralities, with one row per resample
  (`iter` rows) and one column per drop proportion.

The drop proportions and the correlation threshold are stored in the
attributes `drop_prop` and `threshold`. Use
[`summary.tna_stability()`](https://sonsoles.me/tna/reference/summary.tna_stability.md)
for the CS-coefficients as a table.

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

With `edges = TRUE`, the stability of the edge weights is estimated in
the same way: in each subsample the network is estimated again and its
edge weights are correlated with the edge weights of the original
network (Epskamp et al., 2018). Only the edges present in the original
network are compared, self-loops included. An edge that is absent from
the original data cannot appear in a subsample, so including the absent
edges would add pairs of zeros that always agree and inflate the
correlation. The `loops`, `normalize`, and `invert` arguments do not
affect the edge weights.

The results can be visualized by plotting the output object showing the
stability of the centrality measures across different drop proportions,
along with confidence intervals. The CS-coefficients are displayed in
the subtitle.

## References

Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating
psychological networks and their accuracy: A tutorial paper. *Behavior
Research Methods*, 50(1), 195–212.
[doi:10.3758/s13428-017-0862-1](https://doi.org/10.3758/s13428-017-0862-1)

## See also

Validation functions
[`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md),
[`deprune()`](https://sonsoles.me/tna/reference/deprune.md),
[`markov_order()`](https://sonsoles.me/tna/reference/markov_order.md),
[`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md),
[`permutation_test.group_tna()`](https://sonsoles.me/tna/reference/permutation_test.group_tna.md),
[`plot.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/plot.group_tna_bootstrap.md),
[`plot.group_tna_permutation()`](https://sonsoles.me/tna/reference/plot.group_tna_permutation.md),
[`plot.group_tna_stability()`](https://sonsoles.me/tna/reference/plot.group_tna_stability.md),
[`plot.tna_bootstrap()`](https://sonsoles.me/tna/reference/plot.tna_bootstrap.md),
[`plot.tna_markov_order()`](https://sonsoles.me/tna/reference/plot.tna_markov_order.md),
[`plot.tna_permutation()`](https://sonsoles.me/tna/reference/plot.tna_permutation.md),
[`plot.tna_reliability()`](https://sonsoles.me/tna/reference/plot.tna_reliability.md),
[`plot.tna_stability()`](https://sonsoles.me/tna/reference/plot.tna_stability.md),
[`print.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/print.group_tna_bootstrap.md),
[`print.group_tna_permutation()`](https://sonsoles.me/tna/reference/print.group_tna_permutation.md),
[`print.group_tna_stability()`](https://sonsoles.me/tna/reference/print.group_tna_stability.md),
[`print.summary.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/print.summary.group_tna_bootstrap.md),
[`print.summary.tna_bootstrap()`](https://sonsoles.me/tna/reference/print.summary.tna_bootstrap.md),
[`print.summary.tna_markov_order()`](https://sonsoles.me/tna/reference/print.summary.tna_markov_order.md),
[`print.tna_bootstrap()`](https://sonsoles.me/tna/reference/print.tna_bootstrap.md),
[`print.tna_markov_order()`](https://sonsoles.me/tna/reference/print.tna_markov_order.md),
[`print.tna_permutation()`](https://sonsoles.me/tna/reference/print.tna_permutation.md),
[`print.tna_reliability()`](https://sonsoles.me/tna/reference/print.tna_reliability.md),
[`print.tna_stability()`](https://sonsoles.me/tna/reference/print.tna_stability.md),
[`prune()`](https://sonsoles.me/tna/reference/prune.md),
[`pruning_details()`](https://sonsoles.me/tna/reference/pruning_details.md),
[`reliability()`](https://sonsoles.me/tna/reference/reliability.md),
[`reprune()`](https://sonsoles.me/tna/reference/reprune.md),
[`summary.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md),
[`summary.tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.tna_bootstrap.md),
[`summary.tna_markov_order()`](https://sonsoles.me/tna/reference/summary.tna_markov_order.md),
[`summary.tna_permutation()`](https://sonsoles.me/tna/reference/summary.tna_permutation.md),
[`summary.tna_reliability()`](https://sonsoles.me/tna/reference/summary.tna_reliability.md),
[`summary.tna_stability()`](https://sonsoles.me/tna/reference/summary.tna_stability.md)

## Examples

``` r
# A small subset and few iterations
estimate_cs(
  tna(head(group_regulation, 200)),
  drop_prop = c(0.3, 0.6),
  measures = "InStrength",
  iter = 5
)
#> Centrality Stability Coefficients
#> 
#> InStrength 
#>        0.6 
# \donttest{
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
# Stability of the edge weights alongside the centralities
estimate_cs(
  model,
  drop_prop = seq(0.3, 0.9, by = 0.2),
  measures = "InStrength",
  iter = 10,
  edges = TRUE
)
#> Centrality and Edge Stability Coefficients
#> 
#> InStrength      Edges 
#>        0.9        0.9 
# }
```

# Prune a Transition Network based on Transition Probabilities

Prunes a network represented by a `tna` object by removing edges based
on a specified threshold, lowest percent of non-zero edge weights, or
the disparity filter algorithm (Serrano et al., 2009). It ensures the
network remains weakly connected.

Prunes a network represented by a `tna` object by removing edges based
on a specified threshold, lowest percent of non-zero edge weights, or
the disparity filter algorithm (Serrano et al., 2009). It ensures the
network remains weakly connected.

## Usage

``` r
prune(x, ...)

# S3 method for class 'tna'
prune(
  x,
  method = "threshold",
  threshold = 0.1,
  lowest = 0.05,
  level = 0.5,
  boot = NULL,
  ...
)

# S3 method for class 'group_tna'
prune(x, ...)
```

## Arguments

- x:

  An object of class `tna` or `group_tna`

- ...:

  Arguments passed to
  [`bootstrap()`](http://sonsoles.me/tna/reference/bootstrap.md) when
  using `method = "bootstrap"` and when a `tna_bootstrap` is not
  supplied.

- method:

  A `character` string describing the pruning method. The available
  options are `"threshold"`, `"lowest"`, `"bootstrap"` and
  `"disparity"`, corresponding to the methods listed in Details. The
  default is `"threshold"`.

- threshold:

  A numeric value specifying the edge weight threshold. Edges with
  weights below or equal to this threshold will be considered for
  removal.

- lowest:

  A `numeric` value specifying the lowest percentage of non-zero edges.
  This percentage of edges with the lowest weights will be considered
  for removal. The default is `0.05`.

- level:

  A `numeric` value representing the significance level for the
  disparity filter. Defaults to `0.5`.

- boot:

  A `tna_bootstrap` object to be used for pruning with method `"boot"`.
  The method argument is ignored if this argument is supplied.

## Value

A pruned `tna` or `group_tna` object. Details on the pruning can be
viewed with
[`pruning_details()`](http://sonsoles.me/tna/reference/pruning_details.md).
The original model can be restored with
[`deprune()`](http://sonsoles.me/tna/reference/deprune.md).

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
[`print.tna_permutation()`](http://sonsoles.me/tna/reference/print.tna_permutation.md),
[`print.tna_stability()`](http://sonsoles.me/tna/reference/print.tna_stability.md),
[`pruning_details()`](http://sonsoles.me/tna/reference/pruning_details.md),
[`reprune()`](http://sonsoles.me/tna/reference/reprune.md),
[`summary.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md),
[`summary.tna_bootstrap()`](http://sonsoles.me/tna/reference/summary.tna_bootstrap.md)

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
[`print.tna_permutation()`](http://sonsoles.me/tna/reference/print.tna_permutation.md),
[`print.tna_stability()`](http://sonsoles.me/tna/reference/print.tna_stability.md),
[`pruning_details()`](http://sonsoles.me/tna/reference/pruning_details.md),
[`reprune()`](http://sonsoles.me/tna/reference/reprune.md),
[`summary.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md),
[`summary.tna_bootstrap()`](http://sonsoles.me/tna/reference/summary.tna_bootstrap.md)

## Examples

``` r
model <- tna(group_regulation)
pruned_threshold <- prune(model, method = "threshold", threshold = 0.1)
pruned_percentile <- prune(model, method = "lowest", lowest = 0.05)
pruned_disparity <- prune(model, method = "disparity", level = 0.5)
```

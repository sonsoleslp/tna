# Prune a Transition Network based on Transition Probabilities

Prunes a network represented by a `tna` object by removing edges based
on a specified threshold, lowest percent of non-zero edge weights, the
disparity filter algorithm (Serrano et al., 2009), or bootstrap
significance.

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

  An object of class `tna` or `group_tna`.

- ...:

  Arguments passed to
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md) when
  using `method = "bootstrap"` and when a `tna_bootstrap` is not
  supplied. Method-specific arguments supplied for another method are
  ignored with a warning instead of being silently discarded.

- method:

  A `character` string describing the pruning method. The available
  options are `"threshold"`, `"lowest"`, `"bootstrap"` and
  `"disparity"`, corresponding to the methods listed in Details. The
  default is `"threshold"`.

- threshold:

  A `numeric` value specifying the edge weight threshold. Edges with
  weights below or equal to this threshold will be considered for
  removal. The default is `0.1`.

- lowest:

  A `numeric` value specifying the lowest percentage of non-zero edges.
  This percentage of edges with the lowest weights will be considered
  for removal. The default is `0.05`.

- level:

  A `numeric` value representing the significance level for the
  disparity filter. Defaults to `0.5`.

- boot:

  A `tna_bootstrap` object to be used for pruning with method
  `"bootstrap"`. Supplying a non-`NULL` value selects bootstrap pruning
  even if `method` is omitted. A warning is issued if it conflicts with
  an explicitly supplied `method`.

## Value

A pruned `tna` or `group_tna` object: the removed edges have weight zero
in `weights`, and the pruning is recorded in the attribute `pruning`.
Details on the pruning can be viewed with
[`pruning_details()`](https://sonsoles.me/tna/reference/pruning_details.md).
The original model can be restored with
[`deprune()`](https://sonsoles.me/tna/reference/deprune.md).

## Details

The available methods are:

- `"threshold"`: Removes the edges whose weight is at most `threshold`.

- `"lowest"`: Removes the edges whose weight is at most the `lowest`
  quantile of the positive edge weights.

- `"disparity"`: Keeps the edges that are significant at level `level`
  under the disparity filter of Serrano et al. (2009), either among the
  outgoing edges of their source state or among the incoming edges of
  their target state. Self-loops are always removed by this method.

- `"bootstrap"`: Keeps the edges that are significant in
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md) (the
  `model` element of a `tna_bootstrap` object).

With `"threshold"` and `"lowest"`, an edge is only removed when the
network stays weakly connected without it, so some edges below the
cut-off can be retained. The `"disparity"` and `"bootstrap"` methods do
not check the connectivity.

## References

Serrano, M. A., Boguna, M., & Vespignani, A. (2009). Extracting the
multiscale backbone of complex weighted networks. *Proceedings of the
National Academy of Sciences*, 106(16), 6483–6488.
[doi:10.1073/pnas.0808904106](https://doi.org/10.1073/pnas.0808904106)

## See also

Validation functions
[`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md),
[`deprune()`](https://sonsoles.me/tna/reference/deprune.md),
[`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
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
model <- tna(group_regulation)
pruned_threshold <- prune(model, method = "threshold", threshold = 0.1)
pruned_percentile <- prune(model, method = "lowest", lowest = 0.05)
pruned_disparity <- prune(model, method = "disparity", level = 0.5)
```

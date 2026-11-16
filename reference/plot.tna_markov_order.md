# Plot the Results of a Markov Order Test

A two-panel visualization (the plot of
`Nestimate::markov_order_test()`):

## Usage

``` r
# S3 method for class 'tna_markov_order'
plot(x, panel = "both", combined = TRUE, title = NULL, ...)

# S3 method for class 'group_tna_markov_order'
plot(x, panel = "both", combined = TRUE, title = names(x), ...)
```

## Arguments

- x:

  A `tna_markov_order` or a `group_tna_markov_order` object.

- panel:

  A `character` string, which panels to draw: `"both"` (the default),
  `"ic"`, or `"permutation"`.

- combined:

  A `logical` value. When `panel = "both"` and `combined` is `TRUE` (the
  default), the two panels are drawn side by side. When `FALSE`, the two
  `ggplot` objects are returned without drawing them. Ignored when
  `panel` is not `"both"`.

- title:

  An optional `character` string placed before the panel titles. For a
  `group_tna_markov_order` object, one per group (the group names by
  default).

- ...:

  Must be empty: an unknown argument, such as a misspelled `panel`, is
  an error.

## Value

A `ggplot` object for `panel = "ic"` or `panel = "permutation"`. For
`panel = "both"`, a named `list` of the two `ggplot` objects (`ic`,
`permutation`), invisibly, after drawing them side by side when
`combined = TRUE`. For a `group_tna_markov_order` object, the plots of
the groups are drawn one after another, and a `list` with the result for
each group is returned invisibly.

## Details

- Panel A: the log-likelihood, AIC, and BIC across the tested orders,
  with the order selected by the permutation tests (dashed line) and the
  order that minimizes BIC (dotted line) marked.

- Panel B: the permutation null density of each order with the observed
  \\G^2\\ as a vertical line, colored by rejection at `level` and
  labelled with its p-value.

Uses the Okabe-Ito colorblind-safe palette.

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
model <- tna(group_regulation)
mo <- markov_order(model, max_order = 2, iter = 50)
plot(mo)

plot(mo, panel = "ic")

```

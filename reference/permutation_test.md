# Compare Two Networks from Sequence Data using a Permutation Test

This function compares two networks built from sequence data using
permutation tests. The function builds Markov models for two sequence
objects, computes the transition probabilities, and compares them by
performing permutation tests. It returns the differences in transition
probabilities, effect sizes, estimated p-values, and confidence
intervals.

## Usage

``` r
permutation_test(x, ...)

# S3 method for class 'tna'
permutation_test(
  x,
  y,
  adjust = "none",
  iter = 1000,
  paired = FALSE,
  level = 0.05,
  measures = character(0),
  ...
)
```

## Arguments

- x:

  A `tna` object containing sequence data for the first `tna` model.

- ...:

  Additional arguments passed to
  [`centralities()`](http://sonsoles.me/tna/reference/centralities.md).

- y:

  A `tna` object containing sequence data for the second `tna` model.

- adjust:

  A `character` string for the method to adjust p-values with for
  multiple comparisons. The default is `"none"` for no adjustment. See
  the `method` argument of
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
  details and available adjustment methods.

- iter:

  An `integer` giving the number of permutations to perform. The default
  is 1000.

- paired:

  A `logical` value. If `TRUE`, perform paired permutation tests; if
  `FALSE`, perform unpaired tests. The default is `FALSE`.

- level:

  A `numeric` value giving the significance level for the permutation
  tests. The default is 0.05.

- measures:

  A `character` vector of centrality measures to test. See
  [`centralities()`](http://sonsoles.me/tna/reference/centralities.md)
  for a list of available centrality measures.

## Value

A `tna_permutation` object which is a `list` with two elements: `edges`
and `centralities`, both containing the following elements:

- `stats`: A `data.frame` of original differences, effect sizes, and
  estimated p-values for each edge or centrality measure. The effect
  size is computed as the observed difference divided by the standard
  deviation of the differences of the permuted samples.

- `diffs_true`: A `matrix` of differences in the data.

- `diffs_sig`: A `matrix` showing the significant differences.

## See also

Validation functions
[`bootstrap()`](http://sonsoles.me/tna/reference/bootstrap.md),
[`deprune()`](http://sonsoles.me/tna/reference/deprune.md),
[`estimate_cs()`](http://sonsoles.me/tna/reference/estimate_centrality_stability.md),
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
model_x <- tna(group_regulation[1:200, ])
model_y <- tna(group_regulation[1001:1200, ])
# Small number of iterations for CRAN
permutation_test(model_x, model_y, iter = 20)
#> # A tibble: 81 × 4
#>    edge_name           diff_true effect_size p_value
#>    <chr>                   <dbl>       <dbl>   <dbl>
#>  1 adapt -> adapt       0            NaN      1     
#>  2 cohesion -> adapt    0.00541        0.946  0.619 
#>  3 consensus -> adapt  -0.000679      -0.200  0.810 
#>  4 coregulate -> adapt  0.00769        0.718  0.619 
#>  5 discuss -> adapt    -0.130         -6.38   0.0476
#>  6 emotion -> adapt     0.0101         2.00   0.143 
#>  7 monitor -> adapt    -0.00480       -0.529  1     
#>  8 plan -> adapt        0.00339        1.42   0.0476
#>  9 synthesis -> adapt  -0.159         -1.48   0.190 
#> 10 adapt -> cohesion   -0.0907        -1.33   0.190 
#> # ℹ 71 more rows
```

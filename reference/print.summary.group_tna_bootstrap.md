# Print a Bootstrap Summary for a Grouped Transition Network Model

Print a Bootstrap Summary for a Grouped Transition Network Model

## Usage

``` r
# S3 method for class 'summary.group_tna_bootstrap'
print(x, ...)
```

## Arguments

- x:

  A `summary.group_tna_bootstrap` object.

- ...:

  Arguments passed to the generic `print` method.

## Value

`x` (invisibly).

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
[`plot.tna_reliability()`](http://sonsoles.me/tna/reference/plot.tna_reliability.md),
[`plot.tna_stability()`](http://sonsoles.me/tna/reference/plot.tna_stability.md),
[`print.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/print.group_tna_bootstrap.md),
[`print.group_tna_permutation()`](http://sonsoles.me/tna/reference/print.group_tna_permutation.md),
[`print.group_tna_stability()`](http://sonsoles.me/tna/reference/print.group_tna_stability.md),
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
model <- group_model(engagement_mmm)
# Low number of iteration for CRAN
boot <- bootstrap(model, iter = 10)
print(summary(boot))
#>        group       from         to     weight    p_value   sig   cr_lower
#> 1  Cluster 1     Active     Active 0.85985688 0.09090909 FALSE 0.64489266
#> 2  Cluster 1    Average     Active 0.31210322 0.09090909 FALSE 0.23407741
#> 3  Cluster 1 Disengaged     Active 0.04791061 0.09090909 FALSE 0.03593296
#> 4  Cluster 1     Active    Average 0.08919748 0.09090909 FALSE 0.06689811
#> 5  Cluster 1    Average    Average 0.54208478 0.09090909 FALSE 0.40656359
#> 6  Cluster 1 Disengaged    Average 0.16179397 0.09090909 FALSE 0.12134548
#> 7  Cluster 1     Active Disengaged 0.05094565 0.09090909 FALSE 0.03820923
#> 8  Cluster 1    Average Disengaged 0.14581200 0.09090909 FALSE 0.10935900
#> 9  Cluster 1 Disengaged Disengaged 0.79029542 0.09090909 FALSE 0.59272157
#> 10 Cluster 2     Active     Active 0.84090909 0.09090909 FALSE 0.63068182
#> 11 Cluster 2    Average     Active 0.09259259 0.18181818 FALSE 0.06944444
#> 12 Cluster 2 Disengaged     Active 0.15555556 0.54545455 FALSE 0.11666667
#> 13 Cluster 2     Active    Average 0.15909091 0.09090909 FALSE 0.11931818
#> 14 Cluster 2    Average    Average 0.62962963 0.09090909 FALSE 0.47222222
#> 15 Cluster 2 Disengaged    Average 0.51111111 0.09090909 FALSE 0.38333333
#> 16 Cluster 2    Average Disengaged 0.27777778 0.09090909 FALSE 0.20833333
#> 17 Cluster 2 Disengaged Disengaged 0.33333333 0.09090909 FALSE 0.25000000
#> 18 Cluster 3     Active     Active 0.58333333 0.18181818 FALSE 0.43750000
#> 19 Cluster 3    Average     Active 0.15277778 0.27272727 FALSE 0.11458333
#> 20 Cluster 3     Active    Average 0.12500000 0.45454545 FALSE 0.09375000
#> 21 Cluster 3    Average    Average 0.81944444 0.09090909 FALSE 0.61458333
#> 22 Cluster 3 Disengaged    Average 0.60000000 0.18181818 FALSE 0.45000000
#> 23 Cluster 3     Active Disengaged 0.29166667 0.18181818 FALSE 0.21875000
#> 24 Cluster 3    Average Disengaged 0.02777778 0.54545455 FALSE 0.02083333
#> 25 Cluster 3 Disengaged Disengaged 0.40000000 0.27272727 FALSE 0.30000000
#>      cr_upper   ci_lower   ci_upper
#> 1  1.07482109 0.85460723 0.86425217
#> 2  0.39012902 0.30510332 0.32051289
#> 3  0.05988826 0.04525312 0.05292359
#> 4  0.11149685 0.08619738 0.09224549
#> 5  0.67760598 0.53427370 0.55953825
#> 6  0.20224246 0.16145880 0.17103014
#> 7  0.06368206 0.04842582 0.05550517
#> 8  0.18226500 0.13401651 0.15032307
#> 9  0.98786928 0.78031082 0.79317049
#> 10 1.05113636 0.80774457 0.86291370
#> 11 0.11574074 0.06800863 0.11061606
#> 12 0.19444444 0.10518569 0.27008342
#> 13 0.19886364 0.13708630 0.19225543
#> 14 0.78703704 0.60635056 0.69190985
#> 15 0.63888889 0.40214003 0.56115669
#> 16 0.34722222 0.22455500 0.29930150
#> 17 0.41666667 0.30244053 0.37500000
#> 18 0.72916667 0.48450000 0.73793103
#> 19 0.19097222 0.10956183 0.19985733
#> 20 0.15625000 0.04547414 0.19100000
#> 21 1.02430556 0.78186670 0.84726351
#> 22 0.75000000 0.48939130 0.73593750
#> 23 0.36458333 0.21659483 0.36281818
#> 24 0.03472222 0.01299632 0.05609132
#> 25 0.50000000 0.26406250 0.51060870
```

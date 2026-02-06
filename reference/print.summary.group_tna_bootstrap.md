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
[`plot.tna_stability()`](http://sonsoles.me/tna/reference/plot.tna_stability.md),
[`print.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/print.group_tna_bootstrap.md),
[`print.group_tna_permutation()`](http://sonsoles.me/tna/reference/print.group_tna_permutation.md),
[`print.group_tna_stability()`](http://sonsoles.me/tna/reference/print.group_tna_stability.md),
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
#> 11 Cluster 2    Average     Active 0.09259259 0.45454545 FALSE 0.06944444
#> 12 Cluster 2 Disengaged     Active 0.15555556 0.36363636 FALSE 0.11666667
#> 13 Cluster 2     Active    Average 0.15909091 0.09090909 FALSE 0.11931818
#> 14 Cluster 2    Average    Average 0.62962963 0.09090909 FALSE 0.47222222
#> 15 Cluster 2 Disengaged    Average 0.51111111 0.09090909 FALSE 0.38333333
#> 16 Cluster 2    Average Disengaged 0.27777778 0.09090909 FALSE 0.20833333
#> 17 Cluster 2 Disengaged Disengaged 0.33333333 0.18181818 FALSE 0.25000000
#> 18 Cluster 3     Active     Active 0.58333333 0.18181818 FALSE 0.43750000
#> 19 Cluster 3    Average     Active 0.15277778 0.27272727 FALSE 0.11458333
#> 20 Cluster 3     Active    Average 0.12500000 0.54545455 FALSE 0.09375000
#> 21 Cluster 3    Average    Average 0.81944444 0.09090909 FALSE 0.61458333
#> 22 Cluster 3 Disengaged    Average 0.60000000 0.36363636 FALSE 0.45000000
#> 23 Cluster 3     Active Disengaged 0.29166667 0.27272727 FALSE 0.21875000
#> 24 Cluster 3    Average Disengaged 0.02777778 0.63636364 FALSE 0.02083333
#> 25 Cluster 3 Disengaged Disengaged 0.40000000 0.54545455 FALSE 0.30000000
#>      cr_upper   ci_lower   ci_upper
#> 1  1.07482109 0.85364644 0.86251933
#> 2  0.39012902 0.30325947 0.31750047
#> 3  0.05988826 0.04463035 0.05445306
#> 4  0.11149685 0.08420600 0.09213959
#> 5  0.67760598 0.53731300 0.55045041
#> 6  0.20224246 0.15498207 0.16765862
#> 7  0.06368206 0.04995641 0.05531253
#> 8  0.18226500 0.14439688 0.15056135
#> 9  0.98786928 0.78507248 0.79841297
#> 10 1.05113636 0.80974004 0.86878813
#> 11 0.11574074 0.05709565 0.11901439
#> 12 0.19444444 0.07105090 0.17308048
#> 13 0.19886364 0.13121187 0.19025996
#> 14 0.78703704 0.58558559 0.69922149
#> 15 0.63888889 0.45638298 0.59307274
#> 16 0.34722222 0.23013727 0.30738046
#> 17 0.41666667 0.29932932 0.41893818
#> 18 0.72916667 0.48907323 0.75780303
#> 19 0.19097222 0.10448718 0.17093407
#> 20 0.15625000 0.03561594 0.19660326
#> 21 1.02430556 0.80000000 0.84428274
#> 22 0.75000000 0.50000000 0.85166667
#> 23 0.36458333 0.20613636 0.35752632
#> 24 0.03472222 0.00625000 0.06104845
#> 25 0.50000000 0.14833333 0.50000000
```

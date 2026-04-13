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
#> 11 Cluster 2    Average     Active 0.09259259 0.36363636 FALSE 0.06944444
#> 12 Cluster 2 Disengaged     Active 0.15555556 0.81818182 FALSE 0.11666667
#> 13 Cluster 2     Active    Average 0.15909091 0.18181818 FALSE 0.11931818
#> 14 Cluster 2    Average    Average 0.62962963 0.09090909 FALSE 0.47222222
#> 15 Cluster 2 Disengaged    Average 0.51111111 0.09090909 FALSE 0.38333333
#> 16 Cluster 2    Average Disengaged 0.27777778 0.09090909 FALSE 0.20833333
#> 17 Cluster 2 Disengaged Disengaged 0.33333333 0.09090909 FALSE 0.25000000
#> 18 Cluster 3     Active     Active 0.58333333 0.27272727 FALSE 0.43750000
#> 19 Cluster 3    Average     Active 0.15277778 0.36363636 FALSE 0.11458333
#> 20 Cluster 3     Active    Average 0.12500000 0.36363636 FALSE 0.09375000
#> 21 Cluster 3    Average    Average 0.81944444 0.09090909 FALSE 0.61458333
#> 22 Cluster 3 Disengaged    Average 0.60000000 0.18181818 FALSE 0.45000000
#> 23 Cluster 3     Active Disengaged 0.29166667 0.36363636 FALSE 0.21875000
#> 24 Cluster 3    Average Disengaged 0.02777778 0.45454545 FALSE 0.02083333
#> 25 Cluster 3 Disengaged Disengaged 0.40000000 0.36363636 FALSE 0.30000000
#>      cr_upper   ci_lower   ci_upper
#> 1  1.07482109 0.85954591 0.86573870
#> 2  0.39012902 0.30290516 0.31728471
#> 3  0.05988826 0.04383269 0.05073976
#> 4  0.11149685 0.08541774 0.09131934
#> 5  0.67760598 0.53206136 0.56066242
#> 6  0.20224246 0.15522684 0.16802312
#> 7  0.06368206 0.04712164 0.05205591
#> 8  0.18226500 0.13492599 0.15272053
#> 9  0.98786928 0.78663809 0.79548282
#> 10 1.05113636 0.81567539 0.88368755
#> 11 0.11574074 0.07790807 0.13734592
#> 12 0.19444444 0.09090909 0.22638640
#> 13 0.19886364 0.11631245 0.18432461
#> 14 0.78703704 0.56212289 0.66575594
#> 15 0.63888889 0.45158730 0.62818182
#> 16 0.34722222 0.24823492 0.32439129
#> 17 0.41666667 0.27415433 0.35820513
#> 18 0.72916667 0.46235119 0.77335165
#> 19 0.19097222 0.09373447 0.19520821
#> 20 0.15625000 0.03633242 0.16250000
#> 21 1.02430556 0.78398653 0.84800124
#> 22 0.75000000 0.50489130 0.78500000
#> 23 0.36458333 0.19031593 0.37961310
#> 24 0.03472222 0.01437703 0.05826429
#> 25 0.50000000 0.21500000 0.49510870
```

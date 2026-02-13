# Print `group_tna` Bootstrap Results

Print `group_tna` Bootstrap Results

## Usage

``` r
# S3 method for class 'group_tna_bootstrap'
print(x, ...)
```

## Arguments

- x:

  A `group_tna_bootstrap` object.

- ...:

  Arguments passed to
  [`print.tna_bootstrap()`](http://sonsoles.me/tna/reference/print.tna_bootstrap.md).

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
model <- group_model(engagement_mmm)
# Low number of iteration for CRAN
boot <- bootstrap(model, iter = 10)
print(boot)
#> Cluster 1 :
#> Non-significant Edges
#> 
#>         from         to     weight    p_value   cr_lower   cr_upper   ci_lower
#> 1     Active     Active 0.85985688 0.09090909 0.64489266 1.07482109 0.85493623
#> 2    Average     Active 0.31210322 0.09090909 0.23407741 0.39012902 0.30028668
#> 3 Disengaged     Active 0.04791061 0.09090909 0.03593296 0.05988826 0.04406895
#> 4     Active    Average 0.08919748 0.09090909 0.06689811 0.11149685 0.08859510
#> 5    Average    Average 0.54208478 0.09090909 0.40656359 0.67760598 0.52819478
#> 6 Disengaged    Average 0.16179397 0.09090909 0.12134548 0.20224246 0.15808646
#> 7     Active Disengaged 0.05094565 0.09090909 0.03820923 0.06368206 0.04791113
#> 8    Average Disengaged 0.14581200 0.09090909 0.10935900 0.18226500 0.14031898
#> 9 Disengaged Disengaged 0.79029542 0.09090909 0.59272157 0.98786928 0.78136526
#>     ci_upper
#> 1 0.86103393
#> 2 0.32162154
#> 3 0.05286634
#> 4 0.09252019
#> 5 0.55389314
#> 6 0.16879777
#> 7 0.05342775
#> 8 0.15506751
#> 9 0.79413751
#> 
#> Cluster 2 :
#> Non-significant Edges
#> 
#>         from         to     weight    p_value   cr_lower  cr_upper   ci_lower
#> 1     Active     Active 0.84090909 0.09090909 0.63068182 1.0511364 0.82919607
#> 2    Average     Active 0.09259259 0.18181818 0.06944444 0.1157407 0.07936603
#> 3 Disengaged     Active 0.15555556 0.54545455 0.11666667 0.1944444 0.01875000
#> 4     Active    Average 0.15909091 0.09090909 0.11931818 0.1988636 0.12992176
#> 5    Average    Average 0.62962963 0.09090909 0.47222222 0.7870370 0.55832778
#> 6 Disengaged    Average 0.51111111 0.18181818 0.38333333 0.6388889 0.40599817
#> 8    Average Disengaged 0.27777778 0.09090909 0.20833333 0.3472222 0.24028926
#> 9 Disengaged Disengaged 0.33333333 0.09090909 0.25000000 0.4166667 0.28760504
#>    ci_upper
#> 1 0.8700782
#> 2 0.1240768
#> 3 0.2274948
#> 4 0.1708039
#> 5 0.6751148
#> 6 0.6802221
#> 8 0.3224486
#> 9 0.4002289
#> 
#> Cluster 3 :
#> Non-significant Edges
#> 
#>         from         to     weight    p_value   cr_lower   cr_upper   ci_lower
#> 1     Active     Active 0.58333333 0.09090909 0.43750000 0.72916667 0.50937500
#> 2    Average     Active 0.15277778 0.36363636 0.11458333 0.19097222 0.10317139
#> 4     Active    Average 0.12500000 0.54545455 0.09375000 0.15625000 0.05189394
#> 5    Average    Average 0.81944444 0.09090909 0.61458333 1.02430556 0.80000000
#> 6 Disengaged    Average 0.60000000 0.18181818 0.45000000 0.75000000 0.50226316
#> 7     Active Disengaged 0.29166667 0.27272727 0.21875000 0.36458333 0.21739130
#> 8    Average Disengaged 0.02777778 0.63636364 0.02083333 0.03472222 0.01756716
#> 9 Disengaged Disengaged 0.40000000 0.36363636 0.30000000 0.50000000 0.25375000
#>    ci_upper
#> 1 0.7219697
#> 2 0.1724103
#> 4 0.1499126
#> 5 0.8552632
#> 6 0.7462500
#> 7 0.3568182
#> 8 0.0495262
#> 9 0.4977368
#> 
```

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
#> 1     Active     Active 0.85985688 0.09090909 0.64489266 1.07482109 0.85317415
#> 2    Average     Active 0.31210322 0.09090909 0.23407741 0.39012902 0.30310863
#> 3 Disengaged     Active 0.04791061 0.09090909 0.03593296 0.05988826 0.04496011
#> 4     Active    Average 0.08919748 0.09090909 0.06689811 0.11149685 0.08627233
#> 5    Average    Average 0.54208478 0.09090909 0.40656359 0.67760598 0.52513775
#> 6 Disengaged    Average 0.16179397 0.09090909 0.12134548 0.20224246 0.15385115
#> 7     Active Disengaged 0.05094565 0.09090909 0.03820923 0.06368206 0.04744249
#> 8    Average Disengaged 0.14581200 0.09090909 0.10935900 0.18226500 0.13326432
#> 9 Disengaged Disengaged 0.79029542 0.09090909 0.59272157 0.98786928 0.78598441
#>     ci_upper
#> 1 0.86560607
#> 2 0.32374757
#> 3 0.05182869
#> 4 0.09373136
#> 5 0.55216022
#> 6 0.16686580
#> 7 0.05313150
#> 8 0.15455363
#> 9 0.79511552
#> 
#> Cluster 2 :
#> Non-significant Edges
#> 
#>         from         to     weight    p_value   cr_lower  cr_upper   ci_lower
#> 1     Active     Active 0.84090909 0.09090909 0.63068182 1.0511364 0.81947823
#> 2    Average     Active 0.09259259 0.36363636 0.06944444 0.1157407 0.05690651
#> 3 Disengaged     Active 0.15555556 0.63636364 0.11666667 0.1944444 0.09445707
#> 4     Active    Average 0.15909091 0.09090909 0.11931818 0.1988636 0.13619826
#> 5    Average    Average 0.62962963 0.09090909 0.47222222 0.7870370 0.53422619
#> 6 Disengaged    Average 0.51111111 0.09090909 0.38333333 0.6388889 0.39826087
#> 8    Average Disengaged 0.27777778 0.09090909 0.20833333 0.3472222 0.22554532
#> 9 Disengaged Disengaged 0.33333333 0.18181818 0.25000000 0.4166667 0.24735406
#>    ci_upper
#> 1 0.8638017
#> 2 0.1472098
#> 3 0.2388975
#> 4 0.1805218
#> 5 0.7022962
#> 6 0.6243830
#> 8 0.3185640
#> 9 0.3771528
#> 
#> Cluster 3 :
#> Non-significant Edges
#> 
#>         from         to     weight    p_value   cr_lower   cr_upper  ci_lower
#> 1     Active     Active 0.58333333 0.09090909 0.43750000 0.72916667 0.4441250
#> 2    Average     Active 0.15277778 0.18181818 0.11458333 0.19097222 0.1244397
#> 4     Active    Average 0.12500000 0.63636364 0.09375000 0.15625000 0.0747151
#> 5    Average    Average 0.81944444 0.09090909 0.61458333 1.02430556 0.7977536
#> 6 Disengaged    Average 0.60000000 0.18181818 0.45000000 0.75000000 0.5297024
#> 7     Active Disengaged 0.29166667 0.18181818 0.21875000 0.36458333 0.2520833
#> 8    Average Disengaged 0.02777778 0.63636364 0.02083333 0.03472222 0.0000000
#> 9 Disengaged Disengaged 0.40000000 0.27272727 0.30000000 0.50000000 0.1695098
#>     ci_upper
#> 1 0.66666667
#> 2 0.19844356
#> 4 0.20645833
#> 5 0.84064766
#> 6 0.83049020
#> 7 0.36652632
#> 8 0.04500988
#> 9 0.47029762
#> 
```

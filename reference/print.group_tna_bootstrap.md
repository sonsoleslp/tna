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
#> 1     Active     Active 0.85985688 0.09090909 0.64489266 1.07482109 0.85508323
#> 2    Average     Active 0.31210322 0.09090909 0.23407741 0.39012902 0.30694685
#> 3 Disengaged     Active 0.04791061 0.09090909 0.03593296 0.05988826 0.04207776
#> 4     Active    Average 0.08919748 0.09090909 0.06689811 0.11149685 0.08590456
#> 5    Average    Average 0.54208478 0.09090909 0.40656359 0.67760598 0.53181932
#> 6 Disengaged    Average 0.16179397 0.09090909 0.12134548 0.20224246 0.15804773
#> 7     Active Disengaged 0.05094565 0.09090909 0.03820923 0.06368206 0.04677474
#> 8    Average Disengaged 0.14581200 0.09090909 0.10935900 0.18226500 0.13758137
#> 9 Disengaged Disengaged 0.79029542 0.09090909 0.59272157 0.98786928 0.78283481
#>     ci_upper
#> 1 0.86543241
#> 2 0.32392615
#> 3 0.04969929
#> 4 0.09443394
#> 5 0.55078471
#> 6 0.16892178
#> 7 0.05567124
#> 8 0.15426538
#> 9 0.79259911
#> 
#> Cluster 2 :
#> Non-significant Edges
#> 
#>         from         to     weight    p_value   cr_lower  cr_upper   ci_lower
#> 1     Active     Active 0.84090909 0.09090909 0.63068182 1.0511364 0.81044882
#> 2    Average     Active 0.09259259 0.18181818 0.06944444 0.1157407 0.06605868
#> 3 Disengaged     Active 0.15555556 0.18181818 0.11666667 0.1944444 0.08555347
#> 4     Active    Average 0.15909091 0.09090909 0.11931818 0.1988636 0.13679464
#> 5    Average    Average 0.62962963 0.09090909 0.47222222 0.7870370 0.58904562
#> 6 Disengaged    Average 0.51111111 0.09090909 0.38333333 0.6388889 0.41794872
#> 8    Average Disengaged 0.27777778 0.09090909 0.20833333 0.3472222 0.23036239
#> 9 Disengaged Disengaged 0.33333333 0.18181818 0.25000000 0.4166667 0.28916981
#>    ci_upper
#> 1 0.8632054
#> 2 0.1117779
#> 3 0.1819332
#> 4 0.1895512
#> 5 0.6751707
#> 6 0.5841585
#> 8 0.3021208
#> 9 0.4426923
#> 
#> Cluster 3 :
#> Non-significant Edges
#> 
#>         from         to     weight    p_value   cr_lower   cr_upper   ci_lower
#> 1     Active     Active 0.58333333 0.09090909 0.43750000 0.72916667 0.49479010
#> 2    Average     Active 0.15277778 0.27272727 0.11458333 0.19097222 0.11789640
#> 4     Active    Average 0.12500000 0.45454545 0.09375000 0.15625000 0.09090909
#> 5    Average    Average 0.81944444 0.09090909 0.61458333 1.02430556 0.76959040
#> 6 Disengaged    Average 0.60000000 0.09090909 0.45000000 0.75000000 0.48148148
#> 7     Active Disengaged 0.29166667 0.09090909 0.21875000 0.36458333 0.24285714
#> 8    Average Disengaged 0.02777778 0.63636364 0.02083333 0.03472222 0.01341553
#> 9 Disengaged Disengaged 0.40000000 0.36363636 0.30000000 0.50000000 0.35000000
#>     ci_upper
#> 1 0.63636364
#> 2 0.20986582
#> 4 0.17391304
#> 5 0.85908861
#> 6 0.65000000
#> 7 0.33939280
#> 8 0.03947368
#> 9 0.51851852
#> 
```

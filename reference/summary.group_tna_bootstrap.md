# Summarize Bootstrap Results for a Grouped Transition Network

Summarize Bootstrap Results for a Grouped Transition Network

## Usage

``` r
# S3 method for class 'group_tna_bootstrap'
summary(object, ...)
```

## Arguments

- object:

  A `group_tna_bootstrap` object.

- ...:

  Ignored.

## Value

A `summary.group_tna_bootstrap` object containing the weight, estimated
p-value and confidence interval of each edge for each cluster.

## See also

Validation functions
[`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md),
[`deprune()`](https://sonsoles.me/tna/reference/deprune.md),
[`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
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
[`print.tna_bootstrap()`](https://sonsoles.me/tna/reference/print.tna_bootstrap.md),
[`print.tna_clustering()`](https://sonsoles.me/tna/reference/print.tna_clustering.md),
[`print.tna_permutation()`](https://sonsoles.me/tna/reference/print.tna_permutation.md),
[`print.tna_reliability()`](https://sonsoles.me/tna/reference/print.tna_reliability.md),
[`print.tna_stability()`](https://sonsoles.me/tna/reference/print.tna_stability.md),
[`prune()`](https://sonsoles.me/tna/reference/prune.md),
[`pruning_details()`](https://sonsoles.me/tna/reference/pruning_details.md),
[`reliability()`](https://sonsoles.me/tna/reference/reliability.md),
[`reprune()`](https://sonsoles.me/tna/reference/reprune.md),
[`summary.tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.tna_bootstrap.md)

## Examples

``` r
model <- group_tna(engagement_mmm)
# Small number of iterations for CRAN
boot <- bootstrap(model, iter = 10)
summary(boot)
#>        group       from         to      weight    p_value   sig     cr_lower
#> 1  Cluster 1 Disengaged Disengaged 0.680794702 0.09090909 FALSE 0.5105960265
#> 2  Cluster 1    Engaged Disengaged 0.020270270 0.18181818 FALSE 0.0152027027
#> 3  Cluster 1   Moderate Disengaged 0.120651574 0.09090909 FALSE 0.0904886803
#> 4  Cluster 1 Disengaged    Engaged 0.158278146 0.09090909 FALSE 0.1187086093
#> 5  Cluster 1    Engaged    Engaged 0.661642412 0.09090909 FALSE 0.4962318087
#> 6  Cluster 1   Moderate    Engaged 0.124240751 0.09090909 FALSE 0.0931805632
#> 7  Cluster 1 Disengaged   Moderate 0.160927152 0.09090909 FALSE 0.1206953642
#> 8  Cluster 1    Engaged   Moderate 0.318087318 0.09090909 FALSE 0.2385654886
#> 9  Cluster 1   Moderate   Moderate 0.755107675 0.09090909 FALSE 0.5663307565
#> 10 Cluster 2 Disengaged Disengaged 0.911206588 0.09090909 FALSE 0.6834049409
#> 11 Cluster 2    Engaged Disengaged 0.308310992 0.09090909 FALSE 0.2312332440
#> 12 Cluster 2   Moderate Disengaged 0.118299445 0.09090909 FALSE 0.0887245841
#> 13 Cluster 2 Disengaged    Engaged 0.087719298 0.09090909 FALSE 0.0657894737
#> 14 Cluster 2    Engaged    Engaged 0.671581769 0.09090909 FALSE 0.5036863271
#> 15 Cluster 2   Moderate    Engaged 0.053604436 0.18181818 FALSE 0.0402033272
#> 16 Cluster 2 Disengaged   Moderate 0.001074114 0.72727273 FALSE 0.0008055854
#> 17 Cluster 2    Engaged   Moderate 0.020107239 0.36363636 FALSE 0.0150804290
#> 18 Cluster 2   Moderate   Moderate 0.828096118 0.09090909 FALSE 0.6210720887
#> 19 Cluster 3 Disengaged Disengaged 0.875386574 0.09090909 FALSE 0.6565399309
#> 20 Cluster 3    Engaged Disengaged 0.062464509 0.09090909 FALSE 0.0468483816
#> 21 Cluster 3   Moderate Disengaged 0.181534460 0.09090909 FALSE 0.1361508453
#> 22 Cluster 3 Disengaged    Engaged 0.111151537 0.09090909 FALSE 0.0833636529
#> 23 Cluster 3    Engaged    Engaged 0.665247019 0.09090909 FALSE 0.4989352641
#> 24 Cluster 3   Moderate    Engaged 0.149804941 0.09090909 FALSE 0.1123537061
#> 25 Cluster 3 Disengaged   Moderate 0.013461888 0.09090909 FALSE 0.0100964162
#> 26 Cluster 3    Engaged   Moderate 0.272288472 0.09090909 FALSE 0.2042163543
#> 27 Cluster 3   Moderate   Moderate 0.668660598 0.09090909 FALSE 0.5014954486
#>       cr_upper   ci_lower    ci_upper
#> 1  0.850993377 0.65851595 0.701765465
#> 2  0.025337838 0.01522021 0.024533354
#> 3  0.150814467 0.11185895 0.127810547
#> 4  0.197847682 0.14864852 0.177030993
#> 5  0.827053015 0.65691320 0.678459220
#> 6  0.155300939 0.11830878 0.134932491
#> 7  0.201158940 0.14536605 0.173049466
#> 8  0.397609148 0.30188911 0.322162221
#> 9  0.943884594 0.74478906 0.764436142
#> 10 1.139008235 0.89767208 0.913199139
#> 11 0.385388740 0.28202381 0.324490835
#> 12 0.147874307 0.10914144 0.145351782
#> 13 0.109649123 0.08545664 0.101663465
#> 14 0.839477212 0.65166971 0.702280844
#> 15 0.067005545 0.04048663 0.062816602
#> 16 0.001342642 0.00000000 0.002935917
#> 17 0.025134048 0.01007157 0.032295178
#> 18 1.035120148 0.80996637 0.841370442
#> 19 1.094233218 0.86838162 0.880911715
#> 20 0.078080636 0.05838751 0.070077898
#> 21 0.226918075 0.17774609 0.195163994
#> 22 0.138939422 0.10789026 0.117631870
#> 23 0.831558773 0.65566963 0.677949475
#> 24 0.187256177 0.13502110 0.158687132
#> 25 0.016827360 0.01068712 0.015973300
#> 26 0.340360591 0.26212408 0.274647267
#> 27 0.835825748 0.65438711 0.683770803
```

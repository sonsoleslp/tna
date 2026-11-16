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
model <- group_model(engagement_mmm)
# Low number of iteration for CRAN
boot <- bootstrap(model, iter = 10)
print(summary(boot))
#>        group       from         to      weight    p_value   sig     cr_lower
#> 1  Cluster 1 Disengaged Disengaged 0.680794702 0.09090909 FALSE 0.5105960265
#> 2  Cluster 1    Engaged Disengaged 0.020270270 0.27272727 FALSE 0.0152027027
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
#> 15 Cluster 2   Moderate    Engaged 0.053604436 0.27272727 FALSE 0.0402033272
#> 16 Cluster 2 Disengaged   Moderate 0.001074114 0.63636364 FALSE 0.0008055854
#> 17 Cluster 2    Engaged   Moderate 0.020107239 0.27272727 FALSE 0.0150804290
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
#>       cr_upper     ci_lower    ci_upper
#> 1  0.850993377 0.6486858123 0.694624679
#> 2  0.025337838 0.0136709907 0.025720440
#> 3  0.150814467 0.1142299707 0.127398776
#> 4  0.197847682 0.1436406869 0.181857632
#> 5  0.827053015 0.6449211944 0.681228737
#> 6  0.155300939 0.1167384729 0.128714591
#> 7  0.201158940 0.1478017493 0.171318792
#> 8  0.397609148 0.3048627171 0.333299296
#> 9  0.943884594 0.7489337933 0.764574874
#> 10 1.139008235 0.9042829497 0.917331881
#> 11 0.385388740 0.2826049997 0.367432645
#> 12 0.147874307 0.1000215282 0.138888889
#> 13 0.109649123 0.0815474409 0.094902131
#> 14 0.839477212 0.6152006331 0.697407038
#> 15 0.067005545 0.0391720032 0.069142761
#> 16 0.001342642 0.0004413775 0.001697097
#> 17 0.025134048 0.0110640223 0.029568511
#> 18 1.035120148 0.8036111111 0.840613477
#> 19 1.094233218 0.8676258392 0.877983059
#> 20 0.078080636 0.0557148704 0.072175131
#> 21 0.226918075 0.1711410607 0.194412271
#> 22 0.138939422 0.1077807903 0.116718260
#> 23 0.831558773 0.6433114551 0.668836494
#> 24 0.187256177 0.1446821268 0.167360508
#> 25 0.016827360 0.0128677450 0.016510044
#> 26 0.340360591 0.2673203700 0.287718082
#> 27 0.835825748 0.6473557725 0.675728656
```

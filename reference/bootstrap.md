# Bootstrap Transition Networks from Sequence Data

Perform bootstrapping on transition networks created from sequence data
stored in a `tna` object. Bootstrapped estimates of edge weights are
returned with confidence intervals and significance testing.

## Usage

``` r
bootstrap(x, iter, level, method, threshold, consistency_range)

# S3 method for class 'tna'
bootstrap(
  x,
  iter = 1000,
  level = 0.05,
  method = "stability",
  threshold,
  consistency_range = c(0.75, 1.25)
)

# S3 method for class 'group_tna'
bootstrap(
  x,
  iter = 1000,
  level = 0.05,
  method = "stability",
  threshold,
  consistency_range = c(0.75, 1.25)
)
```

## Arguments

- x:

  A `tna` or a `group_tna` object created from sequence data.

- iter:

  An `integer` specifying the number of bootstrap samples to draw.
  Defaults to `1000`.

- level:

  A `numeric` value representing the significance level for hypothesis
  testing and confidence intervals. Defaults to `0.05`.

- method:

  A `character` string. This argument defines the bootstrap test
  statistic. The `"stability"` option (the default) compares edge
  weights against a range of "consistent" values defined by
  `consistency_range`. Weights that fall outside this range are
  considered insignificant. In other words, an edge is considered
  significant if its value is within the range in `(1 - level)` \* 100%
  of the bootstrap samples. The `"threshold"` option instead compares
  the edge weights against a user-specified `threshold` value.

- threshold:

  A `numeric` value to compare edge weights against. The default is the
  10th percentile of the edge weights. Used only when
  `method = "threshold"`.

- consistency_range:

  A `numeric` vector of length 2. Determines how much the edge weights
  may deviate (multiplicatively) from their observed values (below and
  above) before they are considered insignificant. The default is
  `c(0.75, 1.25)` which corresponds to a symmetric 25% deviation range.
  Used only when `method = "stability"`.

## Value

A `tna_bootstrap` object which is a `list` containing the following
elements:

- `weights_orig`: The original edge weight `matrix`.

- `weights_sig`: The `matrix` of significant transitions (those with
  estimated p-values below the significance level).

- `weights_mean`: The mean weight `matrix` from the bootstrap samples.

- `weights_sd`: The standard deviation `matrix` from the bootstrap
  samples.

- `cr_lower`: The lower bound `matrix` of the consistency range for the
  edge weights.

- `cr_upper`: The upper bound `matrix` of the consistency range for the
  edge weights.

- `ci_lower`: The lower bound `matrix` of the bootstrap confidence
  intervals for the edge weights.

- `ci_upper`: The upper bound `matrix` of the bootstrap confidence
  intervals for the edge weights.

- `p_values`: The `matrix` of estimated p-values for the edge weights.

- `summary`: A `data.frame` summarizing the edges, their weights,
  p-values, statistical significance, consistency ranges, and confidence
  intervals.

If `x` is a `group_tna` object, the output is a `group_tna_bootstrap`
object, which is a `list` of `tna_bootstrap` objects.

## Details

The function first computes the original edge weights for the specified
cluster from the `tna` object. It then performs bootstrapping by
resampling the sequence data and recalculating the edge weights for each
bootstrap sample. The mean and standard deviation of the transitions are
computed, and confidence intervals are derived. The function also
estimates p-values for each edge and identifies significant edges based
on the specified significance level. A matrix of significant edges
(those with estimated p-values below the significance level) is
generated. Additional statistics on removed edges (those not considered
significant) are provided.

All results, including the original transition matrix, bootstrapped
estimates, and summary statistics for removed edges, are returned in a
structured list.

## See also

Validation functions
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
model <- tna(group_regulation)
# Small number of iterations for CRAN
bootstrap(model, iter = 10)
#> Non-significant Edges
#> 
#>          from         to       weight    p_value     cr_lower    cr_upper
#> 2    cohesion      adapt 0.0029498525 0.72727273 0.0022123894 0.003687316
#> 3   consensus      adapt 0.0047400853 0.18181818 0.0035550640 0.005925107
#> 4  coregulate      adapt 0.0162436548 0.27272727 0.0121827411 0.020304569
#> 5     discuss      adapt 0.0713743356 0.09090909 0.0535307517 0.089217920
#> 6     emotion      adapt 0.0024673951 0.54545455 0.0018505464 0.003084244
#> 7     monitor      adapt 0.0111653873 0.18181818 0.0083740405 0.013956734
#> 8        plan      adapt 0.0009745006 0.54545455 0.0007308754 0.001218126
#> 9   synthesis      adapt 0.2346625767 0.09090909 0.1759969325 0.293328221
#> 10      adapt   cohesion 0.2730844794 0.09090909 0.2048133595 0.341355599
#> 11   cohesion   cohesion 0.0271386431 0.18181818 0.0203539823 0.033923304
#> 12  consensus   cohesion 0.0148522673 0.09090909 0.0111392005 0.018565334
#> 13 coregulate   cohesion 0.0360406091 0.18181818 0.0270304569 0.045050761
#> 14    discuss   cohesion 0.0475828904 0.09090909 0.0356871678 0.059478613
#> 15    emotion   cohesion 0.3253436729 0.09090909 0.2440077547 0.406679591
#> 16    monitor   cohesion 0.0558269365 0.09090909 0.0418702024 0.069783671
#> 17       plan   cohesion 0.0251745980 0.09090909 0.0188809485 0.031468248
#> 18  synthesis   cohesion 0.0337423313 0.18181818 0.0253067485 0.042177914
#> 19      adapt  consensus 0.4774066798 0.09090909 0.3580550098 0.596758350
#> 20   cohesion  consensus 0.4979351032 0.09090909 0.3734513274 0.622418879
#> 21  consensus  consensus 0.0820034761 0.09090909 0.0615026070 0.102504345
#> 22 coregulate  consensus 0.1345177665 0.09090909 0.1008883249 0.168147208
#> 23    discuss  consensus 0.3211845103 0.09090909 0.2408883827 0.401480638
#> 24    emotion  consensus 0.3204088826 0.09090909 0.2403066620 0.400511103
#> 25    monitor  consensus 0.1591067690 0.09090909 0.1193300768 0.198883461
#> 26       plan  consensus 0.2904011694 0.09090909 0.2178008771 0.363001462
#> 27  synthesis  consensus 0.4662576687 0.09090909 0.3496932515 0.582822086
#> 28      adapt coregulate 0.0216110020 0.54545455 0.0162082515 0.027013752
#> 29   cohesion coregulate 0.1191740413 0.09090909 0.0893805310 0.148967552
#> 30  consensus coregulate 0.1877073787 0.09090909 0.1407805340 0.234634223
#> 31 coregulate coregulate 0.0233502538 0.09090909 0.0175126904 0.029187817
#> 32    discuss coregulate 0.0842824601 0.09090909 0.0632118451 0.105353075
#> 33    emotion coregulate 0.0341910469 0.09090909 0.0256432852 0.042738809
#> 34    monitor coregulate 0.0579204466 0.09090909 0.0434403350 0.072400558
#> 35       plan coregulate 0.0172161767 0.09090909 0.0129121325 0.021520221
#> 36  synthesis coregulate 0.0444785276 0.36363636 0.0333588957 0.055598160
#> 37      adapt    discuss 0.0589390963 0.27272727 0.0442043222 0.073673870
#> 38   cohesion    discuss 0.0595870206 0.18181818 0.0446902655 0.074483776
#> 39  consensus    discuss 0.1880233844 0.09090909 0.1410175383 0.235029231
#> 40 coregulate    discuss 0.2736040609 0.09090909 0.2052030457 0.342005076
#> 41    discuss    discuss 0.1948873703 0.09090909 0.1461655277 0.243609213
#> 42    emotion    discuss 0.1018681706 0.09090909 0.0764011280 0.127335213
#> 43    monitor    discuss 0.3754361479 0.09090909 0.2815771110 0.469295185
#> 44       plan    discuss 0.0678902063 0.09090909 0.0509176547 0.084862758
#> 45  synthesis    discuss 0.0628834356 0.27272727 0.0471625767 0.078604294
#> 46      adapt    emotion 0.1198428291 0.09090909 0.0898821218 0.149803536
#> 47   cohesion    emotion 0.1156342183 0.09090909 0.0867256637 0.144542773
#> 48  consensus    emotion 0.0726813083 0.09090909 0.0545109812 0.090851635
#> 49 coregulate    emotion 0.1720812183 0.09090909 0.1290609137 0.215101523
#> 50    discuss    emotion 0.1057960010 0.09090909 0.0793470008 0.132245001
#> 51    emotion    emotion 0.0768417342 0.09090909 0.0576313007 0.096052168
#> 52    monitor    emotion 0.0907187718 0.09090909 0.0680390789 0.113398465
#> 53       plan    emotion 0.1468247523 0.09090909 0.1101185642 0.183530940
#> 54  synthesis    emotion 0.0705521472 0.18181818 0.0529141104 0.088190184
#> 55      adapt    monitor 0.0333988212 0.27272727 0.0250491159 0.041748527
#> 56   cohesion    monitor 0.0330383481 0.09090909 0.0247787611 0.041297935
#> 57  consensus    monitor 0.0466108390 0.09090909 0.0349581292 0.058263549
#> 58 coregulate    monitor 0.0862944162 0.09090909 0.0647208122 0.107868020
#> 59    discuss    monitor 0.0222728423 0.09090909 0.0167046317 0.027841053
#> 60    emotion    monitor 0.0363059570 0.09090909 0.0272294677 0.045382446
#> 61    monitor    monitor 0.0181437544 0.18181818 0.0136078158 0.022679693
#> 62       plan    monitor 0.0755237941 0.09090909 0.0566428455 0.094404743
#> 63  synthesis    monitor 0.0122699387 0.63636364 0.0092024540 0.015337423
#> 64      adapt       plan 0.0157170923 0.54545455 0.0117878193 0.019646365
#> 65   cohesion       plan 0.1410029499 0.09090909 0.1057522124 0.176253687
#> 66  consensus       plan 0.3957971243 0.09090909 0.2968478433 0.494746405
#> 67 coregulate       plan 0.2390862944 0.09090909 0.1793147208 0.298857868
#> 68    discuss       plan 0.0116426221 0.18181818 0.0087319666 0.014553278
#> 69    emotion       plan 0.0997532605 0.09090909 0.0748149454 0.124691576
#> 70    monitor       plan 0.2156315422 0.09090909 0.1617236567 0.269539428
#> 71       plan       plan 0.3742082183 0.09090909 0.2806561637 0.467760273
#> 72  synthesis       plan 0.0751533742 0.18181818 0.0563650307 0.093941718
#> 74   cohesion  synthesis 0.0035398230 0.54545455 0.0026548673 0.004424779
#> 75  consensus  synthesis 0.0075841365 0.18181818 0.0056881024 0.009480171
#> 76 coregulate  synthesis 0.0187817259 0.36363636 0.0140862944 0.023477157
#> 77    discuss  synthesis 0.1409769679 0.09090909 0.1057327259 0.176221210
#> 78    emotion  synthesis 0.0028198802 0.54545455 0.0021149101 0.003524850
#> 79    monitor  synthesis 0.0160502442 0.09090909 0.0120376832 0.020062805
#> 80       plan  synthesis 0.0017865844 0.36363636 0.0013399383 0.002233230
#>        ci_lower    ci_upper
#> 2  0.0007154514 0.005853452
#> 3  0.0031129137 0.005671662
#> 4  0.0132447588 0.023734177
#> 5  0.0639102694 0.077350676
#> 6  0.0013939670 0.003962884
#> 7  0.0084887477 0.016804566
#> 8  0.0006931379 0.001612124
#> 9  0.1991121290 0.265929971
#> 10 0.2438849341 0.304517088
#> 11 0.0204414777 0.034729046
#> 12 0.0121438634 0.016986071
#> 13 0.0336015394 0.047275951
#> 14 0.0407388819 0.051731668
#> 15 0.3198138081 0.348200484
#> 16 0.0546827166 0.067504871
#> 17 0.0214199094 0.027635522
#> 18 0.0243293157 0.041193801
#> 19 0.4375856164 0.510464301
#> 20 0.4816999772 0.511798335
#> 21 0.0795134000 0.085124888
#> 22 0.1279916848 0.142816360
#> 23 0.3102872939 0.331563911
#> 24 0.3061226383 0.334021665
#> 25 0.1406273228 0.165733139
#> 26 0.2904613620 0.302426062
#> 27 0.4519454547 0.474709182
#> 28 0.0138747777 0.028889862
#> 29 0.1081509428 0.135100592
#> 30 0.1845652268 0.195471833
#> 31 0.0195228825 0.028655198
#> 32 0.0774539686 0.089039981
#> 33 0.0307364985 0.037181083
#> 34 0.0481876898 0.064509509
#> 35 0.0157446573 0.019113912
#> 36 0.0314086909 0.055746528
#> 37 0.0474250807 0.083352172
#> 38 0.0515641297 0.074910880
#> 39 0.1822115208 0.194140635
#> 40 0.2526133405 0.291462058
#> 41 0.1827017499 0.206165890
#> 42 0.0911264716 0.104349397
#> 43 0.3523562576 0.388466353
#> 44 0.0606717194 0.070131385
#> 45 0.0393735329 0.080902619
#> 46 0.0994497386 0.127451022
#> 47 0.1043508374 0.122402431
#> 48 0.0667291946 0.078452793
#> 49 0.1646408934 0.186794222
#> 50 0.1017010465 0.110091005
#> 51 0.0724399974 0.081013369
#> 52 0.0793048950 0.101370278
#> 53 0.1416414240 0.155900076
#> 54 0.0636588321 0.092343707
#> 55 0.0238752319 0.044347108
#> 56 0.0256613738 0.037155249
#> 57 0.0416586253 0.047148645
#> 58 0.0788399000 0.089532508
#> 59 0.0196720929 0.025529809
#> 60 0.0317189728 0.044417392
#> 61 0.0145789467 0.025502305
#> 62 0.0716259273 0.081396523
#> 63 0.0064456936 0.023287649
#> 64 0.0138705706 0.027601400
#> 65 0.1277751328 0.162044079
#> 66 0.3863050415 0.401190207
#> 67 0.2278896741 0.250911567
#> 68 0.0100654860 0.014412443
#> 69 0.0900343110 0.105013932
#> 70 0.2020646340 0.232618406
#> 71 0.3606772819 0.372475232
#> 72 0.0642000706 0.096650418
#> 74 0.0017293635 0.004752178
#> 75 0.0063470029 0.009637314
#> 76 0.0112492187 0.024135778
#> 77 0.1366116130 0.150579286
#> 78 0.0017226244 0.003464936
#> 79 0.0126097843 0.019893227
#> 80 0.0011742999 0.002273490
```

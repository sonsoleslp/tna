# Print Bootstrap Results

Print Bootstrap Results

## Usage

``` r
# S3 method for class 'tna_bootstrap'
print(x, digits = getOption("digits"), type = "both", ...)
```

## Arguments

- x:

  A `tna_bootstrap` object.

- digits:

  An `integer` giving the minimal number of *significant* digits to
  print.

- type:

  A `character` vector giving the type of edges to print. The default
  option `"both"` prints both statistically significant and
  non-significant edges, `"sig"` prints only significant edges, and
  `"nonsig"` prints only the non-significant edges.

- ...:

  Ignored.

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
[`print.summary.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/print.summary.group_tna_bootstrap.md),
[`print.summary.tna_bootstrap()`](http://sonsoles.me/tna/reference/print.summary.tna_bootstrap.md),
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
boot <- bootstrap(model, iter = 10)
print(boot)
#> Non-significant Edges
#> 
#>          from         to       weight    p_value     cr_lower    cr_upper
#> 2    cohesion      adapt 0.0029498525 0.90909091 0.0022123894 0.003687316
#> 3   consensus      adapt 0.0047400853 0.18181818 0.0035550640 0.005925107
#> 4  coregulate      adapt 0.0162436548 0.09090909 0.0121827411 0.020304569
#> 5     discuss      adapt 0.0713743356 0.09090909 0.0535307517 0.089217920
#> 6     emotion      adapt 0.0024673951 0.81818182 0.0018505464 0.003084244
#> 7     monitor      adapt 0.0111653873 0.36363636 0.0083740405 0.013956734
#> 8        plan      adapt 0.0009745006 0.45454545 0.0007308754 0.001218126
#> 9   synthesis      adapt 0.2346625767 0.09090909 0.1759969325 0.293328221
#> 10      adapt   cohesion 0.2730844794 0.09090909 0.2048133595 0.341355599
#> 11   cohesion   cohesion 0.0271386431 0.18181818 0.0203539823 0.033923304
#> 12  consensus   cohesion 0.0148522673 0.09090909 0.0111392005 0.018565334
#> 13 coregulate   cohesion 0.0360406091 0.18181818 0.0270304569 0.045050761
#> 14    discuss   cohesion 0.0475828904 0.09090909 0.0356871678 0.059478613
#> 15    emotion   cohesion 0.3253436729 0.09090909 0.2440077547 0.406679591
#> 16    monitor   cohesion 0.0558269365 0.18181818 0.0418702024 0.069783671
#> 17       plan   cohesion 0.0251745980 0.09090909 0.0188809485 0.031468248
#> 18  synthesis   cohesion 0.0337423313 0.36363636 0.0253067485 0.042177914
#> 19      adapt  consensus 0.4774066798 0.09090909 0.3580550098 0.596758350
#> 20   cohesion  consensus 0.4979351032 0.09090909 0.3734513274 0.622418879
#> 21  consensus  consensus 0.0820034761 0.09090909 0.0615026070 0.102504345
#> 22 coregulate  consensus 0.1345177665 0.09090909 0.1008883249 0.168147208
#> 23    discuss  consensus 0.3211845103 0.09090909 0.2408883827 0.401480638
#> 24    emotion  consensus 0.3204088826 0.09090909 0.2403066620 0.400511103
#> 25    monitor  consensus 0.1591067690 0.09090909 0.1193300768 0.198883461
#> 26       plan  consensus 0.2904011694 0.09090909 0.2178008771 0.363001462
#> 27  synthesis  consensus 0.4662576687 0.09090909 0.3496932515 0.582822086
#> 28      adapt coregulate 0.0216110020 0.27272727 0.0162082515 0.027013752
#> 29   cohesion coregulate 0.1191740413 0.09090909 0.0893805310 0.148967552
#> 30  consensus coregulate 0.1877073787 0.09090909 0.1407805340 0.234634223
#> 31 coregulate coregulate 0.0233502538 0.27272727 0.0175126904 0.029187817
#> 32    discuss coregulate 0.0842824601 0.09090909 0.0632118451 0.105353075
#> 33    emotion coregulate 0.0341910469 0.09090909 0.0256432852 0.042738809
#> 34    monitor coregulate 0.0579204466 0.09090909 0.0434403350 0.072400558
#> 35       plan coregulate 0.0172161767 0.09090909 0.0129121325 0.021520221
#> 36  synthesis coregulate 0.0444785276 0.27272727 0.0333588957 0.055598160
#> 37      adapt    discuss 0.0589390963 0.18181818 0.0442043222 0.073673870
#> 38   cohesion    discuss 0.0595870206 0.09090909 0.0446902655 0.074483776
#> 39  consensus    discuss 0.1880233844 0.09090909 0.1410175383 0.235029231
#> 40 coregulate    discuss 0.2736040609 0.09090909 0.2052030457 0.342005076
#> 41    discuss    discuss 0.1948873703 0.09090909 0.1461655277 0.243609213
#> 42    emotion    discuss 0.1018681706 0.09090909 0.0764011280 0.127335213
#> 43    monitor    discuss 0.3754361479 0.09090909 0.2815771110 0.469295185
#> 44       plan    discuss 0.0678902063 0.09090909 0.0509176547 0.084862758
#> 45  synthesis    discuss 0.0628834356 0.18181818 0.0471625767 0.078604294
#> 46      adapt    emotion 0.1198428291 0.18181818 0.0898821218 0.149803536
#> 47   cohesion    emotion 0.1156342183 0.09090909 0.0867256637 0.144542773
#> 48  consensus    emotion 0.0726813083 0.09090909 0.0545109812 0.090851635
#> 49 coregulate    emotion 0.1720812183 0.09090909 0.1290609137 0.215101523
#> 50    discuss    emotion 0.1057960010 0.09090909 0.0793470008 0.132245001
#> 51    emotion    emotion 0.0768417342 0.09090909 0.0576313007 0.096052168
#> 52    monitor    emotion 0.0907187718 0.09090909 0.0680390789 0.113398465
#> 53       plan    emotion 0.1468247523 0.09090909 0.1101185642 0.183530940
#> 54  synthesis    emotion 0.0705521472 0.09090909 0.0529141104 0.088190184
#> 55      adapt    monitor 0.0333988212 0.27272727 0.0250491159 0.041748527
#> 56   cohesion    monitor 0.0330383481 0.09090909 0.0247787611 0.041297935
#> 57  consensus    monitor 0.0466108390 0.09090909 0.0349581292 0.058263549
#> 58 coregulate    monitor 0.0862944162 0.09090909 0.0647208122 0.107868020
#> 59    discuss    monitor 0.0222728423 0.09090909 0.0167046317 0.027841053
#> 60    emotion    monitor 0.0363059570 0.09090909 0.0272294677 0.045382446
#> 61    monitor    monitor 0.0181437544 0.09090909 0.0136078158 0.022679693
#> 62       plan    monitor 0.0755237941 0.09090909 0.0566428455 0.094404743
#> 63  synthesis    monitor 0.0122699387 0.36363636 0.0092024540 0.015337423
#> 64      adapt       plan 0.0157170923 0.63636364 0.0117878193 0.019646365
#> 65   cohesion       plan 0.1410029499 0.09090909 0.1057522124 0.176253687
#> 66  consensus       plan 0.3957971243 0.09090909 0.2968478433 0.494746405
#> 67 coregulate       plan 0.2390862944 0.09090909 0.1793147208 0.298857868
#> 68    discuss       plan 0.0116426221 0.09090909 0.0087319666 0.014553278
#> 69    emotion       plan 0.0997532605 0.09090909 0.0748149454 0.124691576
#> 70    monitor       plan 0.2156315422 0.09090909 0.1617236567 0.269539428
#> 71       plan       plan 0.3742082183 0.09090909 0.2806561637 0.467760273
#> 72  synthesis       plan 0.0751533742 0.09090909 0.0563650307 0.093941718
#> 74   cohesion  synthesis 0.0035398230 0.45454545 0.0026548673 0.004424779
#> 75  consensus  synthesis 0.0075841365 0.18181818 0.0056881024 0.009480171
#> 76 coregulate  synthesis 0.0187817259 0.18181818 0.0140862944 0.023477157
#> 77    discuss  synthesis 0.1409769679 0.09090909 0.1057327259 0.176221210
#> 78    emotion  synthesis 0.0028198802 0.63636364 0.0021149101 0.003524850
#> 79    monitor  synthesis 0.0160502442 0.27272727 0.0120376832 0.020062805
#> 80       plan  synthesis 0.0017865844 0.27272727 0.0013399383 0.002233230
#>        ci_lower    ci_upper
#> 2  0.0008264112 0.005759439
#> 3  0.0035946223 0.006472356
#> 4  0.0142528730 0.019462415
#> 5  0.0669883347 0.080265650
#> 6  0.0011322210 0.004791537
#> 7  0.0073569515 0.016152817
#> 8  0.0005216196 0.001442733
#> 9  0.2121013259 0.249524648
#> 10 0.2491382587 0.299339096
#> 11 0.0192488039 0.032929090
#> 12 0.0129820269 0.016236034
#> 13 0.0284376610 0.045086849
#> 14 0.0423547834 0.054203364
#> 15 0.3116193591 0.333784377
#> 16 0.0406236749 0.064675788
#> 17 0.0215380368 0.028507993
#> 18 0.0239224510 0.042703349
#> 19 0.4422066929 0.508924414
#> 20 0.4837311283 0.513391263
#> 21 0.0743151748 0.086817824
#> 22 0.1198225924 0.144769118
#> 23 0.3049972972 0.329154193
#> 24 0.3080972277 0.335298179
#> 25 0.1482771402 0.171374485
#> 26 0.2764618392 0.298888020
#> 27 0.4249830818 0.495230888
#> 28 0.0155301209 0.025631253
#> 29 0.1071954211 0.126052580
#> 30 0.1762348033 0.198192539
#> 31 0.0193891192 0.032044964
#> 32 0.0732840552 0.089722037
#> 33 0.0279513703 0.037847628
#> 34 0.0523033719 0.067841667
#> 35 0.0139824988 0.019558142
#> 36 0.0284691847 0.053590939
#> 37 0.0487262001 0.081886108
#> 38 0.0490919445 0.067638498
#> 39 0.1782221639 0.193248360
#> 40 0.2667073375 0.289189376
#> 41 0.1845838779 0.204330807
#> 42 0.0920092576 0.107790015
#> 43 0.3562865501 0.386132931
#> 44 0.0642764563 0.071607194
#> 45 0.0527129546 0.081825789
#> 46 0.1056017167 0.150522448
#> 47 0.1130103652 0.125045785
#> 48 0.0699501862 0.075535455
#> 49 0.1599788396 0.188462582
#> 50 0.1020507036 0.114466262
#> 51 0.0711154763 0.085456234
#> 52 0.0837487297 0.103906466
#> 53 0.1368493607 0.156310695
#> 54 0.0612363656 0.085113122
#> 55 0.0189835054 0.039415354
#> 56 0.0288794582 0.037013403
#> 57 0.0445557213 0.050536841
#> 58 0.0767925684 0.093648244
#> 59 0.0211417349 0.026327172
#> 60 0.0348988251 0.041115357
#> 61 0.0162898411 0.021863623
#> 62 0.0694586100 0.077081562
#> 63 0.0096402509 0.018537626
#> 64 0.0096838804 0.021140105
#> 65 0.1299958853 0.169199781
#> 66 0.3872860070 0.402779025
#> 67 0.2161807877 0.242444396
#> 68 0.0102933691 0.013612209
#> 69 0.0963717910 0.108697496
#> 70 0.1967253521 0.223555232
#> 71 0.3635642981 0.386835283
#> 72 0.0645402791 0.085672165
#> 74 0.0018942412 0.004684974
#> 75 0.0063727248 0.009269373
#> 76 0.0155179777 0.023355137
#> 77 0.1351215735 0.153364114
#> 78 0.0016859548 0.004206966
#> 79 0.0120698641 0.020171496
#> 80 0.0008105941 0.002483484
```

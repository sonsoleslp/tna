# Print a Bootstrap Summary

Print a Bootstrap Summary

## Usage

``` r
# S3 method for class 'summary.tna_bootstrap'
print(x, ...)
```

## Arguments

- x:

  A `summary.tna_bootstrap` object.

- ...:

  Arguments passed to the generic `print` method.

## Value

A `summary.tna_bootstrap` object (invisibly) containing the weight,
estimated p-value and confidence interval of each edge.

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
[`print.summary.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/print.summary.group_tna_bootstrap.md),
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
model <- tna(group_regulation)
# Small number of iterations for CRAN
boot <- bootstrap(model, iter = 10)
print(summary(boot))
#>          from         to       weight    p_value   sig     cr_lower    cr_upper
#> 2    cohesion      adapt 0.0029498525 0.63636364 FALSE 0.0022123894 0.003687316
#> 3   consensus      adapt 0.0047400853 0.09090909 FALSE 0.0035550640 0.005925107
#> 4  coregulate      adapt 0.0162436548 0.09090909 FALSE 0.0121827411 0.020304569
#> 5     discuss      adapt 0.0713743356 0.09090909 FALSE 0.0535307517 0.089217920
#> 6     emotion      adapt 0.0024673951 0.63636364 FALSE 0.0018505464 0.003084244
#> 7     monitor      adapt 0.0111653873 0.45454545 FALSE 0.0083740405 0.013956734
#> 8        plan      adapt 0.0009745006 0.27272727 FALSE 0.0007308754 0.001218126
#> 9   synthesis      adapt 0.2346625767 0.09090909 FALSE 0.1759969325 0.293328221
#> 10      adapt   cohesion 0.2730844794 0.09090909 FALSE 0.2048133595 0.341355599
#> 11   cohesion   cohesion 0.0271386431 0.18181818 FALSE 0.0203539823 0.033923304
#> 12  consensus   cohesion 0.0148522673 0.09090909 FALSE 0.0111392005 0.018565334
#> 13 coregulate   cohesion 0.0360406091 0.09090909 FALSE 0.0270304569 0.045050761
#> 14    discuss   cohesion 0.0475828904 0.09090909 FALSE 0.0356871678 0.059478613
#> 15    emotion   cohesion 0.3253436729 0.09090909 FALSE 0.2440077547 0.406679591
#> 16    monitor   cohesion 0.0558269365 0.09090909 FALSE 0.0418702024 0.069783671
#> 17       plan   cohesion 0.0251745980 0.09090909 FALSE 0.0188809485 0.031468248
#> 18  synthesis   cohesion 0.0337423313 0.18181818 FALSE 0.0253067485 0.042177914
#> 19      adapt  consensus 0.4774066798 0.09090909 FALSE 0.3580550098 0.596758350
#> 20   cohesion  consensus 0.4979351032 0.09090909 FALSE 0.3734513274 0.622418879
#> 21  consensus  consensus 0.0820034761 0.09090909 FALSE 0.0615026070 0.102504345
#> 22 coregulate  consensus 0.1345177665 0.09090909 FALSE 0.1008883249 0.168147208
#> 23    discuss  consensus 0.3211845103 0.09090909 FALSE 0.2408883827 0.401480638
#> 24    emotion  consensus 0.3204088826 0.09090909 FALSE 0.2403066620 0.400511103
#> 25    monitor  consensus 0.1591067690 0.09090909 FALSE 0.1193300768 0.198883461
#> 26       plan  consensus 0.2904011694 0.09090909 FALSE 0.2178008771 0.363001462
#> 27  synthesis  consensus 0.4662576687 0.09090909 FALSE 0.3496932515 0.582822086
#> 28      adapt coregulate 0.0216110020 0.36363636 FALSE 0.0162082515 0.027013752
#> 29   cohesion coregulate 0.1191740413 0.09090909 FALSE 0.0893805310 0.148967552
#> 30  consensus coregulate 0.1877073787 0.09090909 FALSE 0.1407805340 0.234634223
#> 31 coregulate coregulate 0.0233502538 0.27272727 FALSE 0.0175126904 0.029187817
#> 32    discuss coregulate 0.0842824601 0.09090909 FALSE 0.0632118451 0.105353075
#> 33    emotion coregulate 0.0341910469 0.09090909 FALSE 0.0256432852 0.042738809
#> 34    monitor coregulate 0.0579204466 0.18181818 FALSE 0.0434403350 0.072400558
#> 35       plan coregulate 0.0172161767 0.09090909 FALSE 0.0129121325 0.021520221
#> 36  synthesis coregulate 0.0444785276 0.54545455 FALSE 0.0333588957 0.055598160
#> 37      adapt    discuss 0.0589390963 0.27272727 FALSE 0.0442043222 0.073673870
#> 38   cohesion    discuss 0.0595870206 0.18181818 FALSE 0.0446902655 0.074483776
#> 39  consensus    discuss 0.1880233844 0.09090909 FALSE 0.1410175383 0.235029231
#> 40 coregulate    discuss 0.2736040609 0.09090909 FALSE 0.2052030457 0.342005076
#> 41    discuss    discuss 0.1948873703 0.09090909 FALSE 0.1461655277 0.243609213
#> 42    emotion    discuss 0.1018681706 0.09090909 FALSE 0.0764011280 0.127335213
#> 43    monitor    discuss 0.3754361479 0.09090909 FALSE 0.2815771110 0.469295185
#> 44       plan    discuss 0.0678902063 0.09090909 FALSE 0.0509176547 0.084862758
#> 45  synthesis    discuss 0.0628834356 0.27272727 FALSE 0.0471625767 0.078604294
#> 46      adapt    emotion 0.1198428291 0.09090909 FALSE 0.0898821218 0.149803536
#> 47   cohesion    emotion 0.1156342183 0.09090909 FALSE 0.0867256637 0.144542773
#> 48  consensus    emotion 0.0726813083 0.09090909 FALSE 0.0545109812 0.090851635
#> 49 coregulate    emotion 0.1720812183 0.09090909 FALSE 0.1290609137 0.215101523
#> 50    discuss    emotion 0.1057960010 0.09090909 FALSE 0.0793470008 0.132245001
#> 51    emotion    emotion 0.0768417342 0.09090909 FALSE 0.0576313007 0.096052168
#> 52    monitor    emotion 0.0907187718 0.09090909 FALSE 0.0680390789 0.113398465
#> 53       plan    emotion 0.1468247523 0.09090909 FALSE 0.1101185642 0.183530940
#> 54  synthesis    emotion 0.0705521472 0.18181818 FALSE 0.0529141104 0.088190184
#> 55      adapt    monitor 0.0333988212 0.27272727 FALSE 0.0250491159 0.041748527
#> 56   cohesion    monitor 0.0330383481 0.18181818 FALSE 0.0247787611 0.041297935
#> 57  consensus    monitor 0.0466108390 0.09090909 FALSE 0.0349581292 0.058263549
#> 58 coregulate    monitor 0.0862944162 0.09090909 FALSE 0.0647208122 0.107868020
#> 59    discuss    monitor 0.0222728423 0.09090909 FALSE 0.0167046317 0.027841053
#> 60    emotion    monitor 0.0363059570 0.09090909 FALSE 0.0272294677 0.045382446
#> 61    monitor    monitor 0.0181437544 0.27272727 FALSE 0.0136078158 0.022679693
#> 62       plan    monitor 0.0755237941 0.09090909 FALSE 0.0566428455 0.094404743
#> 63  synthesis    monitor 0.0122699387 0.54545455 FALSE 0.0092024540 0.015337423
#> 64      adapt       plan 0.0157170923 0.63636364 FALSE 0.0117878193 0.019646365
#> 65   cohesion       plan 0.1410029499 0.09090909 FALSE 0.1057522124 0.176253687
#> 66  consensus       plan 0.3957971243 0.09090909 FALSE 0.2968478433 0.494746405
#> 67 coregulate       plan 0.2390862944 0.09090909 FALSE 0.1793147208 0.298857868
#> 68    discuss       plan 0.0116426221 0.09090909 FALSE 0.0087319666 0.014553278
#> 69    emotion       plan 0.0997532605 0.09090909 FALSE 0.0748149454 0.124691576
#> 70    monitor       plan 0.2156315422 0.09090909 FALSE 0.1617236567 0.269539428
#> 71       plan       plan 0.3742082183 0.09090909 FALSE 0.2806561637 0.467760273
#> 72  synthesis       plan 0.0751533742 0.18181818 FALSE 0.0563650307 0.093941718
#> 74   cohesion  synthesis 0.0035398230 0.63636364 FALSE 0.0026548673 0.004424779
#> 75  consensus  synthesis 0.0075841365 0.09090909 FALSE 0.0056881024 0.009480171
#> 76 coregulate  synthesis 0.0187817259 0.36363636 FALSE 0.0140862944 0.023477157
#> 77    discuss  synthesis 0.1409769679 0.09090909 FALSE 0.1057327259 0.176221210
#> 78    emotion  synthesis 0.0028198802 0.45454545 FALSE 0.0021149101 0.003524850
#> 79    monitor  synthesis 0.0160502442 0.27272727 FALSE 0.0120376832 0.020062805
#> 80       plan  synthesis 0.0017865844 0.45454545 FALSE 0.0013399383 0.002233230
#>        ci_lower    ci_upper
#> 2  0.0023051207 0.005248637
#> 3  0.0040756335 0.005424618
#> 4  0.0131967510 0.019865362
#> 5  0.0658331933 0.077746978
#> 6  0.0013812761 0.004267071
#> 7  0.0080880071 0.015727983
#> 8  0.0003067172 0.001249968
#> 9  0.2076247995 0.259214266
#> 10 0.2388630664 0.303051305
#> 11 0.0219514526 0.036762994
#> 12 0.0116200425 0.016317623
#> 13 0.0319046670 0.042961915
#> 14 0.0414544018 0.050688666
#> 15 0.3101983592 0.337227206
#> 16 0.0435462598 0.057360360
#> 17 0.0249347757 0.028972231
#> 18 0.0269278813 0.042972846
#> 19 0.4317215551 0.506076016
#> 20 0.4791655844 0.507850453
#> 21 0.0771463033 0.084206366
#> 22 0.1187045437 0.147836122
#> 23 0.3153142640 0.330726264
#> 24 0.3115472574 0.333057610
#> 25 0.1416303649 0.172104828
#> 26 0.2770205668 0.296033080
#> 27 0.4367247154 0.500919329
#> 28 0.0171866843 0.033471186
#> 29 0.1075456767 0.137178049
#> 30 0.1863076820 0.196226997
#> 31 0.0172861547 0.030293804
#> 32 0.0784069183 0.087170214
#> 33 0.0273565233 0.036671590
#> 34 0.0492620762 0.072388156
#> 35 0.0164061494 0.020136005
#> 36 0.0406018392 0.064961304
#> 37 0.0517459536 0.076928786
#> 38 0.0525077901 0.071542223
#> 39 0.1788425427 0.198939120
#> 40 0.2593036995 0.282994321
#> 41 0.1856259880 0.204447660
#> 42 0.0925233491 0.109562340
#> 43 0.3705888944 0.390656033
#> 44 0.0641287285 0.071455340
#> 45 0.0351834266 0.077887344
#> 46 0.1006618315 0.137761419
#> 47 0.1053260854 0.130083612
#> 48 0.0677842305 0.077187741
#> 49 0.1562473989 0.180891944
#> 50 0.1010250127 0.112101206
#> 51 0.0642761213 0.089530293
#> 52 0.0817904891 0.096623380
#> 53 0.1425174686 0.149354641
#> 54 0.0588486772 0.086560700
#> 55 0.0269431483 0.052109040
#> 56 0.0309212886 0.043556378
#> 57 0.0433758012 0.049846670
#> 58 0.0841142134 0.099759158
#> 59 0.0201521504 0.025591314
#> 60 0.0294983099 0.041270145
#> 61 0.0108461026 0.022665978
#> 62 0.0701157877 0.081565389
#> 63 0.0095301860 0.023900025
#> 64 0.0071389875 0.025302188
#> 65 0.1297563584 0.158083874
#> 66 0.3880349780 0.401160106
#> 67 0.2296303423 0.247410692
#> 68 0.0089502396 0.013607302
#> 69 0.0907865531 0.105377816
#> 70 0.2010449735 0.228497649
#> 71 0.3668175582 0.382361777
#> 72 0.0537555117 0.087162176
#> 74 0.0012684440 0.005263385
#> 75 0.0063032028 0.008626856
#> 76 0.0157759609 0.025247222
#> 77 0.1335095862 0.148294477
#> 78 0.0021621008 0.004153605
#> 79 0.0093349389 0.019107614
#> 80 0.0012270030 0.002630601
```

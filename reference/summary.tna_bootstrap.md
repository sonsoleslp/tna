# Summarize Bootstrap Results

Summarize Bootstrap Results

## Usage

``` r
# S3 method for class 'tna_bootstrap'
summary(object, ...)
```

## Arguments

- object:

  A `tna_bootstrap` object.

- ...:

  Ignored.

## Value

A `summary.tna_bootstrap` object containing the weight, estimated
p-value and confidence interval of each edge.

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
[`print.summary.tna_bootstrap()`](http://sonsoles.me/tna/reference/print.summary.tna_bootstrap.md),
[`print.tna_bootstrap()`](http://sonsoles.me/tna/reference/print.tna_bootstrap.md),
[`print.tna_permutation()`](http://sonsoles.me/tna/reference/print.tna_permutation.md),
[`print.tna_stability()`](http://sonsoles.me/tna/reference/print.tna_stability.md),
[`prune()`](http://sonsoles.me/tna/reference/prune.md),
[`pruning_details()`](http://sonsoles.me/tna/reference/pruning_details.md),
[`reprune()`](http://sonsoles.me/tna/reference/reprune.md),
[`summary.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md)

## Examples

``` r
model <- tna(group_regulation)
# Small number of iterations for CRAN
boot <- bootstrap(model, iter = 50)
summary(boot)
#>          from         to       weight    p_value   sig     cr_lower    cr_upper
#> 2    cohesion      adapt 0.0029498525 0.43137255 FALSE 0.0022123894 0.003687316
#> 3   consensus      adapt 0.0047400853 0.17647059 FALSE 0.0035550640 0.005925107
#> 4  coregulate      adapt 0.0162436548 0.31372549 FALSE 0.0121827411 0.020304569
#> 5     discuss      adapt 0.0713743356 0.01960784  TRUE 0.0535307517 0.089217920
#> 6     emotion      adapt 0.0024673951 0.62745098 FALSE 0.0018505464 0.003084244
#> 7     monitor      adapt 0.0111653873 0.25490196 FALSE 0.0083740405 0.013956734
#> 8        plan      adapt 0.0009745006 0.60784314 FALSE 0.0007308754 0.001218126
#> 9   synthesis      adapt 0.2346625767 0.01960784  TRUE 0.1759969325 0.293328221
#> 10      adapt   cohesion 0.2730844794 0.01960784  TRUE 0.2048133595 0.341355599
#> 11   cohesion   cohesion 0.0271386431 0.11764706 FALSE 0.0203539823 0.033923304
#> 12  consensus   cohesion 0.0148522673 0.01960784  TRUE 0.0111392005 0.018565334
#> 13 coregulate   cohesion 0.0360406091 0.07843137 FALSE 0.0270304569 0.045050761
#> 14    discuss   cohesion 0.0475828904 0.01960784  TRUE 0.0356871678 0.059478613
#> 15    emotion   cohesion 0.3253436729 0.01960784  TRUE 0.2440077547 0.406679591
#> 16    monitor   cohesion 0.0558269365 0.01960784  TRUE 0.0418702024 0.069783671
#> 17       plan   cohesion 0.0251745980 0.01960784  TRUE 0.0188809485 0.031468248
#> 18  synthesis   cohesion 0.0337423313 0.25490196 FALSE 0.0253067485 0.042177914
#> 19      adapt  consensus 0.4774066798 0.01960784  TRUE 0.3580550098 0.596758350
#> 20   cohesion  consensus 0.4979351032 0.01960784  TRUE 0.3734513274 0.622418879
#> 21  consensus  consensus 0.0820034761 0.01960784  TRUE 0.0615026070 0.102504345
#> 22 coregulate  consensus 0.1345177665 0.01960784  TRUE 0.1008883249 0.168147208
#> 23    discuss  consensus 0.3211845103 0.01960784  TRUE 0.2408883827 0.401480638
#> 24    emotion  consensus 0.3204088826 0.01960784  TRUE 0.2403066620 0.400511103
#> 25    monitor  consensus 0.1591067690 0.01960784  TRUE 0.1193300768 0.198883461
#> 26       plan  consensus 0.2904011694 0.01960784  TRUE 0.2178008771 0.363001462
#> 27  synthesis  consensus 0.4662576687 0.01960784  TRUE 0.3496932515 0.582822086
#> 28      adapt coregulate 0.0216110020 0.35294118 FALSE 0.0162082515 0.027013752
#> 29   cohesion coregulate 0.1191740413 0.01960784  TRUE 0.0893805310 0.148967552
#> 30  consensus coregulate 0.1877073787 0.01960784  TRUE 0.1407805340 0.234634223
#> 31 coregulate coregulate 0.0233502538 0.05882353 FALSE 0.0175126904 0.029187817
#> 32    discuss coregulate 0.0842824601 0.01960784  TRUE 0.0632118451 0.105353075
#> 33    emotion coregulate 0.0341910469 0.05882353 FALSE 0.0256432852 0.042738809
#> 34    monitor coregulate 0.0579204466 0.03921569  TRUE 0.0434403350 0.072400558
#> 35       plan coregulate 0.0172161767 0.01960784  TRUE 0.0129121325 0.021520221
#> 36  synthesis coregulate 0.0444785276 0.15686275 FALSE 0.0333588957 0.055598160
#> 37      adapt    discuss 0.0589390963 0.15686275 FALSE 0.0442043222 0.073673870
#> 38   cohesion    discuss 0.0595870206 0.01960784  TRUE 0.0446902655 0.074483776
#> 39  consensus    discuss 0.1880233844 0.01960784  TRUE 0.1410175383 0.235029231
#> 40 coregulate    discuss 0.2736040609 0.01960784  TRUE 0.2052030457 0.342005076
#> 41    discuss    discuss 0.1948873703 0.01960784  TRUE 0.1461655277 0.243609213
#> 42    emotion    discuss 0.1018681706 0.01960784  TRUE 0.0764011280 0.127335213
#> 43    monitor    discuss 0.3754361479 0.01960784  TRUE 0.2815771110 0.469295185
#> 44       plan    discuss 0.0678902063 0.01960784  TRUE 0.0509176547 0.084862758
#> 45  synthesis    discuss 0.0628834356 0.19607843 FALSE 0.0471625767 0.078604294
#> 46      adapt    emotion 0.1198428291 0.05882353 FALSE 0.0898821218 0.149803536
#> 47   cohesion    emotion 0.1156342183 0.01960784  TRUE 0.0867256637 0.144542773
#> 48  consensus    emotion 0.0726813083 0.01960784  TRUE 0.0545109812 0.090851635
#> 49 coregulate    emotion 0.1720812183 0.01960784  TRUE 0.1290609137 0.215101523
#> 50    discuss    emotion 0.1057960010 0.01960784  TRUE 0.0793470008 0.132245001
#> 51    emotion    emotion 0.0768417342 0.01960784  TRUE 0.0576313007 0.096052168
#> 52    monitor    emotion 0.0907187718 0.01960784  TRUE 0.0680390789 0.113398465
#> 53       plan    emotion 0.1468247523 0.01960784  TRUE 0.1101185642 0.183530940
#> 54  synthesis    emotion 0.0705521472 0.13725490 FALSE 0.0529141104 0.088190184
#> 55      adapt    monitor 0.0333988212 0.37254902 FALSE 0.0250491159 0.041748527
#> 56   cohesion    monitor 0.0330383481 0.05882353 FALSE 0.0247787611 0.041297935
#> 57  consensus    monitor 0.0466108390 0.01960784  TRUE 0.0349581292 0.058263549
#> 58 coregulate    monitor 0.0862944162 0.01960784  TRUE 0.0647208122 0.107868020
#> 59    discuss    monitor 0.0222728423 0.01960784  TRUE 0.0167046317 0.027841053
#> 60    emotion    monitor 0.0363059570 0.03921569  TRUE 0.0272294677 0.045382446
#> 61    monitor    monitor 0.0181437544 0.17647059 FALSE 0.0136078158 0.022679693
#> 62       plan    monitor 0.0755237941 0.01960784  TRUE 0.0566428455 0.094404743
#> 63  synthesis    monitor 0.0122699387 0.47058824 FALSE 0.0092024540 0.015337423
#> 64      adapt       plan 0.0157170923 0.47058824 FALSE 0.0117878193 0.019646365
#> 65   cohesion       plan 0.1410029499 0.01960784  TRUE 0.1057522124 0.176253687
#> 66  consensus       plan 0.3957971243 0.01960784  TRUE 0.2968478433 0.494746405
#> 67 coregulate       plan 0.2390862944 0.01960784  TRUE 0.1793147208 0.298857868
#> 68    discuss       plan 0.0116426221 0.07843137 FALSE 0.0087319666 0.014553278
#> 69    emotion       plan 0.0997532605 0.01960784  TRUE 0.0748149454 0.124691576
#> 70    monitor       plan 0.2156315422 0.01960784  TRUE 0.1617236567 0.269539428
#> 71       plan       plan 0.3742082183 0.01960784  TRUE 0.2806561637 0.467760273
#> 72  synthesis       plan 0.0751533742 0.11764706 FALSE 0.0563650307 0.093941718
#> 74   cohesion  synthesis 0.0035398230 0.49019608 FALSE 0.0026548673 0.004424779
#> 75  consensus  synthesis 0.0075841365 0.11764706 FALSE 0.0056881024 0.009480171
#> 76 coregulate  synthesis 0.0187817259 0.07843137 FALSE 0.0140862944 0.023477157
#> 77    discuss  synthesis 0.1409769679 0.01960784  TRUE 0.1057327259 0.176221210
#> 78    emotion  synthesis 0.0028198802 0.47058824 FALSE 0.0021149101 0.003524850
#> 79    monitor  synthesis 0.0160502442 0.23529412 FALSE 0.0120376832 0.020062805
#> 80       plan  synthesis 0.0017865844 0.35294118 FALSE 0.0013399383 0.002233230
#>        ci_lower    ci_upper
#> 2  0.0007138311 0.005069316
#> 3  0.0029799800 0.006253917
#> 4  0.0104321908 0.023228358
#> 5  0.0645816673 0.078453460
#> 6  0.0010214083 0.004420971
#> 7  0.0070530779 0.016119179
#> 8  0.0002004778 0.001897153
#> 9  0.2133489560 0.264879094
#> 10 0.2438296178 0.305648378
#> 11 0.0190863078 0.034280146
#> 12 0.0119350415 0.017183286
#> 13 0.0286774231 0.045557054
#> 14 0.0421671026 0.052786666
#> 15 0.3081743987 0.337688736
#> 16 0.0475505564 0.067266021
#> 17 0.0219199637 0.027969580
#> 18 0.0227422700 0.045528251
#> 19 0.4338386197 0.518689498
#> 20 0.4757633210 0.517599837
#> 21 0.0755270565 0.089337552
#> 22 0.1209789327 0.146820018
#> 23 0.3109313853 0.334928708
#> 24 0.3037735679 0.336082776
#> 25 0.1331039347 0.175522286
#> 26 0.2813523910 0.301006067
#> 27 0.4331368865 0.496782299
#> 28 0.0123837711 0.037853894
#> 29 0.1039319520 0.129583252
#> 30 0.1770099828 0.196503216
#> 31 0.0177312040 0.030268713
#> 32 0.0789075096 0.090947329
#> 33 0.0285751185 0.042449215
#> 34 0.0499062310 0.069136913
#> 35 0.0148135706 0.020390421
#> 36 0.0304830046 0.059219002
#> 37 0.0416862387 0.077614407
#> 38 0.0517583446 0.070846947
#> 39 0.1741651151 0.198960953
#> 40 0.2566121636 0.291689914
#> 41 0.1842899107 0.201949330
#> 42 0.0950875707 0.112450554
#> 43 0.3463887401 0.398542373
#> 44 0.0633258407 0.072561162
#> 45 0.0422554734 0.082076260
#> 46 0.0948404439 0.146868836
#> 47 0.1016078972 0.128079894
#> 48 0.0670435264 0.078660745
#> 49 0.1542517400 0.189680979
#> 50 0.0960733738 0.116283372
#> 51 0.0656665500 0.088040855
#> 52 0.0791286880 0.103977497
#> 53 0.1420259132 0.154352854
#> 54 0.0528469962 0.091506712
#> 55 0.0171697227 0.057347355
#> 56 0.0258253129 0.040450580
#> 57 0.0411081480 0.051866679
#> 58 0.0761145789 0.096952393
#> 59 0.0177927492 0.025742558
#> 60 0.0307919183 0.041586175
#> 61 0.0123164688 0.023567608
#> 62 0.0692303982 0.084068220
#> 63 0.0064906052 0.020393097
#> 64 0.0095292050 0.025352081
#> 65 0.1277891941 0.155168667
#> 66 0.3861433912 0.407907187
#> 67 0.2197558068 0.255630986
#> 68 0.0092492371 0.014950395
#> 69 0.0889891756 0.112219012
#> 70 0.1982967033 0.238357571
#> 71 0.3593412066 0.386354585
#> 72 0.0540932636 0.097086562
#> 74 0.0012969806 0.005788463
#> 75 0.0051780638 0.009447224
#> 76 0.0135361369 0.022889671
#> 77 0.1280534709 0.147406334
#> 78 0.0010339447 0.004355670
#> 79 0.0093521227 0.023661446
#> 80 0.0009679392 0.002445539
```

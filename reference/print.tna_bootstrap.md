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
#> 2    cohesion      adapt 0.0029498525 0.63636364 0.0022123894 0.003687316
#> 3   consensus      adapt 0.0047400853 0.27272727 0.0035550640 0.005925107
#> 4  coregulate      adapt 0.0162436548 0.36363636 0.0121827411 0.020304569
#> 5     discuss      adapt 0.0713743356 0.09090909 0.0535307517 0.089217920
#> 6     emotion      adapt 0.0024673951 0.81818182 0.0018505464 0.003084244
#> 7     monitor      adapt 0.0111653873 0.27272727 0.0083740405 0.013956734
#> 8        plan      adapt 0.0009745006 0.63636364 0.0007308754 0.001218126
#> 9   synthesis      adapt 0.2346625767 0.09090909 0.1759969325 0.293328221
#> 10      adapt   cohesion 0.2730844794 0.09090909 0.2048133595 0.341355599
#> 11   cohesion   cohesion 0.0271386431 0.18181818 0.0203539823 0.033923304
#> 12  consensus   cohesion 0.0148522673 0.09090909 0.0111392005 0.018565334
#> 13 coregulate   cohesion 0.0360406091 0.18181818 0.0270304569 0.045050761
#> 14    discuss   cohesion 0.0475828904 0.09090909 0.0356871678 0.059478613
#> 15    emotion   cohesion 0.3253436729 0.09090909 0.2440077547 0.406679591
#> 16    monitor   cohesion 0.0558269365 0.18181818 0.0418702024 0.069783671
#> 17       plan   cohesion 0.0251745980 0.09090909 0.0188809485 0.031468248
#> 18  synthesis   cohesion 0.0337423313 0.27272727 0.0253067485 0.042177914
#> 19      adapt  consensus 0.4774066798 0.09090909 0.3580550098 0.596758350
#> 20   cohesion  consensus 0.4979351032 0.09090909 0.3734513274 0.622418879
#> 21  consensus  consensus 0.0820034761 0.09090909 0.0615026070 0.102504345
#> 22 coregulate  consensus 0.1345177665 0.09090909 0.1008883249 0.168147208
#> 23    discuss  consensus 0.3211845103 0.09090909 0.2408883827 0.401480638
#> 24    emotion  consensus 0.3204088826 0.09090909 0.2403066620 0.400511103
#> 25    monitor  consensus 0.1591067690 0.09090909 0.1193300768 0.198883461
#> 26       plan  consensus 0.2904011694 0.09090909 0.2178008771 0.363001462
#> 27  synthesis  consensus 0.4662576687 0.09090909 0.3496932515 0.582822086
#> 28      adapt coregulate 0.0216110020 0.36363636 0.0162082515 0.027013752
#> 29   cohesion coregulate 0.1191740413 0.09090909 0.0893805310 0.148967552
#> 30  consensus coregulate 0.1877073787 0.09090909 0.1407805340 0.234634223
#> 31 coregulate coregulate 0.0233502538 0.18181818 0.0175126904 0.029187817
#> 32    discuss coregulate 0.0842824601 0.09090909 0.0632118451 0.105353075
#> 33    emotion coregulate 0.0341910469 0.09090909 0.0256432852 0.042738809
#> 34    monitor coregulate 0.0579204466 0.09090909 0.0434403350 0.072400558
#> 35       plan coregulate 0.0172161767 0.09090909 0.0129121325 0.021520221
#> 36  synthesis coregulate 0.0444785276 0.18181818 0.0333588957 0.055598160
#> 37      adapt    discuss 0.0589390963 0.18181818 0.0442043222 0.073673870
#> 38   cohesion    discuss 0.0595870206 0.09090909 0.0446902655 0.074483776
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
#> 54  synthesis    emotion 0.0705521472 0.09090909 0.0529141104 0.088190184
#> 55      adapt    monitor 0.0333988212 0.45454545 0.0250491159 0.041748527
#> 56   cohesion    monitor 0.0330383481 0.09090909 0.0247787611 0.041297935
#> 57  consensus    monitor 0.0466108390 0.09090909 0.0349581292 0.058263549
#> 58 coregulate    monitor 0.0862944162 0.09090909 0.0647208122 0.107868020
#> 59    discuss    monitor 0.0222728423 0.09090909 0.0167046317 0.027841053
#> 60    emotion    monitor 0.0363059570 0.09090909 0.0272294677 0.045382446
#> 61    monitor    monitor 0.0181437544 0.27272727 0.0136078158 0.022679693
#> 62       plan    monitor 0.0755237941 0.09090909 0.0566428455 0.094404743
#> 63  synthesis    monitor 0.0122699387 0.54545455 0.0092024540 0.015337423
#> 64      adapt       plan 0.0157170923 0.54545455 0.0117878193 0.019646365
#> 65   cohesion       plan 0.1410029499 0.09090909 0.1057522124 0.176253687
#> 66  consensus       plan 0.3957971243 0.09090909 0.2968478433 0.494746405
#> 67 coregulate       plan 0.2390862944 0.09090909 0.1793147208 0.298857868
#> 68    discuss       plan 0.0116426221 0.18181818 0.0087319666 0.014553278
#> 69    emotion       plan 0.0997532605 0.09090909 0.0748149454 0.124691576
#> 70    monitor       plan 0.2156315422 0.09090909 0.1617236567 0.269539428
#> 71       plan       plan 0.3742082183 0.09090909 0.2806561637 0.467760273
#> 72  synthesis       plan 0.0751533742 0.18181818 0.0563650307 0.093941718
#> 74   cohesion  synthesis 0.0035398230 0.63636364 0.0026548673 0.004424779
#> 75  consensus  synthesis 0.0075841365 0.09090909 0.0056881024 0.009480171
#> 76 coregulate  synthesis 0.0187817259 0.09090909 0.0140862944 0.023477157
#> 77    discuss  synthesis 0.1409769679 0.09090909 0.1057327259 0.176221210
#> 78    emotion  synthesis 0.0028198802 0.54545455 0.0021149101 0.003524850
#> 79    monitor  synthesis 0.0160502442 0.36363636 0.0120376832 0.020062805
#> 80       plan  synthesis 0.0017865844 0.45454545 0.0013399383 0.002233230
#>        ci_lower    ci_upper
#> 2  0.0012934903 0.006705084
#> 3  0.0040672477 0.006127404
#> 4  0.0124214928 0.021776210
#> 5  0.0673916116 0.080442149
#> 6  0.0007908298 0.003846391
#> 7  0.0080333615 0.014757785
#> 8  0.0001971730 0.001870895
#> 9  0.2009652218 0.269441508
#> 10 0.2512676852 0.300274863
#> 11 0.0206287059 0.031925577
#> 12 0.0129502426 0.015947384
#> 13 0.0273343426 0.043246394
#> 14 0.0452294821 0.049925194
#> 15 0.3163843155 0.331702781
#> 16 0.0468748263 0.068667649
#> 17 0.0228083522 0.028821336
#> 18 0.0252077488 0.043655613
#> 19 0.4568138196 0.492852261
#> 20 0.4837943100 0.517080887
#> 21 0.0790600339 0.086623543
#> 22 0.1275044630 0.152538796
#> 23 0.3077234604 0.332751169
#> 24 0.3138421099 0.331496305
#> 25 0.1392619505 0.166879304
#> 26 0.2795698409 0.299116497
#> 27 0.4240661841 0.502666133
#> 28 0.0133072821 0.034754100
#> 29 0.1059391778 0.136790435
#> 30 0.1827763231 0.193213189
#> 31 0.0172137860 0.027412091
#> 32 0.0793508664 0.090278363
#> 33 0.0312528736 0.039818787
#> 34 0.0479662894 0.062218993
#> 35 0.0148953562 0.019927605
#> 36 0.0329820195 0.053742341
#> 37 0.0427205481 0.068956389
#> 38 0.0540703418 0.067966625
#> 39 0.1783321482 0.198352545
#> 40 0.2544689290 0.290746832
#> 41 0.1862081358 0.207524656
#> 42 0.0925693706 0.105475785
#> 43 0.3545770014 0.397727746
#> 44 0.0627197131 0.072251314
#> 45 0.0534001359 0.084978152
#> 46 0.1042898857 0.132132793
#> 47 0.1000988338 0.129769104
#> 48 0.0676874219 0.076752720
#> 49 0.1468385805 0.188174924
#> 50 0.1004592671 0.112862341
#> 51 0.0718871488 0.082912755
#> 52 0.0803286443 0.099442134
#> 53 0.1398467575 0.151133270
#> 54 0.0572794283 0.080787292
#> 55 0.0239628743 0.049664107
#> 56 0.0276058697 0.037662967
#> 57 0.0437475308 0.050051259
#> 58 0.0744315437 0.095144351
#> 59 0.0189334896 0.026407742
#> 60 0.0336659966 0.042529525
#> 61 0.0117006598 0.021820630
#> 62 0.0717904233 0.076580527
#> 63 0.0082078313 0.020201029
#> 64 0.0082289459 0.030150499
#> 65 0.1297638007 0.152014102
#> 66 0.3833027411 0.405088734
#> 67 0.2297810727 0.254163278
#> 68 0.0086728798 0.013128004
#> 69 0.0917534306 0.103683041
#> 70 0.1958398988 0.237199621
#> 71 0.3633006254 0.382557459
#> 72 0.0600542169 0.095416269
#> 74 0.0019092822 0.006461414
#> 75 0.0063935929 0.009055504
#> 76 0.0150533225 0.022514797
#> 77 0.1324778661 0.149844880
#> 78 0.0015842665 0.004687575
#> 79 0.0136177528 0.022902676
#> 80 0.0007694793 0.003242709
```

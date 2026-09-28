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
[`summary.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md)

## Examples

``` r
model <- tna(group_regulation)
# Small number of iterations for CRAN
boot <- bootstrap(model, iter = 50)
summary(boot)
#>          from         to       weight    p_value   sig     cr_lower    cr_upper
#> 2    cohesion      adapt 0.0029498525 0.56862745 FALSE 0.0022123894 0.003687316
#> 3   consensus      adapt 0.0047400853 0.23529412 FALSE 0.0035550640 0.005925107
#> 4  coregulate      adapt 0.0162436548 0.19607843 FALSE 0.0121827411 0.020304569
#> 5     discuss      adapt 0.0713743356 0.01960784  TRUE 0.0535307517 0.089217920
#> 6     emotion      adapt 0.0024673951 0.47058824 FALSE 0.0018505464 0.003084244
#> 7     monitor      adapt 0.0111653873 0.23529412 FALSE 0.0083740405 0.013956734
#> 8        plan      adapt 0.0009745006 0.54901961 FALSE 0.0007308754 0.001218126
#> 9   synthesis      adapt 0.2346625767 0.01960784  TRUE 0.1759969325 0.293328221
#> 10      adapt   cohesion 0.2730844794 0.01960784  TRUE 0.2048133595 0.341355599
#> 11   cohesion   cohesion 0.0271386431 0.11764706 FALSE 0.0203539823 0.033923304
#> 12  consensus   cohesion 0.0148522673 0.01960784  TRUE 0.0111392005 0.018565334
#> 13 coregulate   cohesion 0.0360406091 0.01960784  TRUE 0.0270304569 0.045050761
#> 14    discuss   cohesion 0.0475828904 0.01960784  TRUE 0.0356871678 0.059478613
#> 15    emotion   cohesion 0.3253436729 0.01960784  TRUE 0.2440077547 0.406679591
#> 16    monitor   cohesion 0.0558269365 0.03921569  TRUE 0.0418702024 0.069783671
#> 17       plan   cohesion 0.0251745980 0.01960784  TRUE 0.0188809485 0.031468248
#> 18  synthesis   cohesion 0.0337423313 0.27450980 FALSE 0.0253067485 0.042177914
#> 19      adapt  consensus 0.4774066798 0.01960784  TRUE 0.3580550098 0.596758350
#> 20   cohesion  consensus 0.4979351032 0.01960784  TRUE 0.3734513274 0.622418879
#> 21  consensus  consensus 0.0820034761 0.01960784  TRUE 0.0615026070 0.102504345
#> 22 coregulate  consensus 0.1345177665 0.01960784  TRUE 0.1008883249 0.168147208
#> 23    discuss  consensus 0.3211845103 0.01960784  TRUE 0.2408883827 0.401480638
#> 24    emotion  consensus 0.3204088826 0.01960784  TRUE 0.2403066620 0.400511103
#> 25    monitor  consensus 0.1591067690 0.01960784  TRUE 0.1193300768 0.198883461
#> 26       plan  consensus 0.2904011694 0.01960784  TRUE 0.2178008771 0.363001462
#> 27  synthesis  consensus 0.4662576687 0.01960784  TRUE 0.3496932515 0.582822086
#> 28      adapt coregulate 0.0216110020 0.47058824 FALSE 0.0162082515 0.027013752
#> 29   cohesion coregulate 0.1191740413 0.01960784  TRUE 0.0893805310 0.148967552
#> 30  consensus coregulate 0.1877073787 0.01960784  TRUE 0.1407805340 0.234634223
#> 31 coregulate coregulate 0.0233502538 0.07843137 FALSE 0.0175126904 0.029187817
#> 32    discuss coregulate 0.0842824601 0.01960784  TRUE 0.0632118451 0.105353075
#> 33    emotion coregulate 0.0341910469 0.03921569  TRUE 0.0256432852 0.042738809
#> 34    monitor coregulate 0.0579204466 0.03921569  TRUE 0.0434403350 0.072400558
#> 35       plan coregulate 0.0172161767 0.03921569  TRUE 0.0129121325 0.021520221
#> 36  synthesis coregulate 0.0444785276 0.19607843 FALSE 0.0333588957 0.055598160
#> 37      adapt    discuss 0.0589390963 0.25490196 FALSE 0.0442043222 0.073673870
#> 38   cohesion    discuss 0.0595870206 0.01960784  TRUE 0.0446902655 0.074483776
#> 39  consensus    discuss 0.1880233844 0.01960784  TRUE 0.1410175383 0.235029231
#> 40 coregulate    discuss 0.2736040609 0.01960784  TRUE 0.2052030457 0.342005076
#> 41    discuss    discuss 0.1948873703 0.01960784  TRUE 0.1461655277 0.243609213
#> 42    emotion    discuss 0.1018681706 0.01960784  TRUE 0.0764011280 0.127335213
#> 43    monitor    discuss 0.3754361479 0.01960784  TRUE 0.2815771110 0.469295185
#> 44       plan    discuss 0.0678902063 0.01960784  TRUE 0.0509176547 0.084862758
#> 45  synthesis    discuss 0.0628834356 0.07843137 FALSE 0.0471625767 0.078604294
#> 46      adapt    emotion 0.1198428291 0.01960784  TRUE 0.0898821218 0.149803536
#> 47   cohesion    emotion 0.1156342183 0.01960784  TRUE 0.0867256637 0.144542773
#> 48  consensus    emotion 0.0726813083 0.01960784  TRUE 0.0545109812 0.090851635
#> 49 coregulate    emotion 0.1720812183 0.01960784  TRUE 0.1290609137 0.215101523
#> 50    discuss    emotion 0.1057960010 0.01960784  TRUE 0.0793470008 0.132245001
#> 51    emotion    emotion 0.0768417342 0.01960784  TRUE 0.0576313007 0.096052168
#> 52    monitor    emotion 0.0907187718 0.01960784  TRUE 0.0680390789 0.113398465
#> 53       plan    emotion 0.1468247523 0.01960784  TRUE 0.1101185642 0.183530940
#> 54  synthesis    emotion 0.0705521472 0.09803922 FALSE 0.0529141104 0.088190184
#> 55      adapt    monitor 0.0333988212 0.25490196 FALSE 0.0250491159 0.041748527
#> 56   cohesion    monitor 0.0330383481 0.09803922 FALSE 0.0247787611 0.041297935
#> 57  consensus    monitor 0.0466108390 0.01960784  TRUE 0.0349581292 0.058263549
#> 58 coregulate    monitor 0.0862944162 0.01960784  TRUE 0.0647208122 0.107868020
#> 59    discuss    monitor 0.0222728423 0.05882353 FALSE 0.0167046317 0.027841053
#> 60    emotion    monitor 0.0363059570 0.01960784  TRUE 0.0272294677 0.045382446
#> 61    monitor    monitor 0.0181437544 0.23529412 FALSE 0.0136078158 0.022679693
#> 62       plan    monitor 0.0755237941 0.01960784  TRUE 0.0566428455 0.094404743
#> 63  synthesis    monitor 0.0122699387 0.37254902 FALSE 0.0092024540 0.015337423
#> 64      adapt       plan 0.0157170923 0.58823529 FALSE 0.0117878193 0.019646365
#> 65   cohesion       plan 0.1410029499 0.01960784  TRUE 0.1057522124 0.176253687
#> 66  consensus       plan 0.3957971243 0.01960784  TRUE 0.2968478433 0.494746405
#> 67 coregulate       plan 0.2390862944 0.01960784  TRUE 0.1793147208 0.298857868
#> 68    discuss       plan 0.0116426221 0.15686275 FALSE 0.0087319666 0.014553278
#> 69    emotion       plan 0.0997532605 0.01960784  TRUE 0.0748149454 0.124691576
#> 70    monitor       plan 0.2156315422 0.01960784  TRUE 0.1617236567 0.269539428
#> 71       plan       plan 0.3742082183 0.01960784  TRUE 0.2806561637 0.467760273
#> 72  synthesis       plan 0.0751533742 0.07843137 FALSE 0.0563650307 0.093941718
#> 74   cohesion  synthesis 0.0035398230 0.50980392 FALSE 0.0026548673 0.004424779
#> 75  consensus  synthesis 0.0075841365 0.13725490 FALSE 0.0056881024 0.009480171
#> 76 coregulate  synthesis 0.0187817259 0.15686275 FALSE 0.0140862944 0.023477157
#> 77    discuss  synthesis 0.1409769679 0.01960784  TRUE 0.1057327259 0.176221210
#> 78    emotion  synthesis 0.0028198802 0.52941176 FALSE 0.0021149101 0.003524850
#> 79    monitor  synthesis 0.0160502442 0.21568627 FALSE 0.0120376832 0.020062805
#> 80       plan  synthesis 0.0017865844 0.29411765 FALSE 0.0013399383 0.002233230
#>        ci_lower    ci_upper
#> 2  0.0005905420 0.005887305
#> 3  0.0034069214 0.006559300
#> 4  0.0103273531 0.020604760
#> 5  0.0628577920 0.078930947
#> 6  0.0013626561 0.004110672
#> 7  0.0076381475 0.015503711
#> 8  0.0003613675 0.001935622
#> 9  0.2042651327 0.269076940
#> 10 0.2471554820 0.318815860
#> 11 0.0182691609 0.035368613
#> 12 0.0117664503 0.016964841
#> 13 0.0304686734 0.044613057
#> 14 0.0413031866 0.052610463
#> 15 0.3059740758 0.342280395
#> 16 0.0467540392 0.068937840
#> 17 0.0218584087 0.028219579
#> 18 0.0208558146 0.045489855
#> 19 0.4368706689 0.510007591
#> 20 0.4710104452 0.522121665
#> 21 0.0767093302 0.087964148
#> 22 0.1229588581 0.148447136
#> 23 0.3101890647 0.334727523
#> 24 0.3057429036 0.333086155
#> 25 0.1356285516 0.172221730
#> 26 0.2792975224 0.298768008
#> 27 0.4335642896 0.500712871
#> 28 0.0106162035 0.037881953
#> 29 0.1038494372 0.129058015
#> 30 0.1801000241 0.197964490
#> 31 0.0180203817 0.029069771
#> 32 0.0773416872 0.091957915
#> 33 0.0298091292 0.042276323
#> 34 0.0497855345 0.069238914
#> 35 0.0144870142 0.020272819
#> 36 0.0283673370 0.062147954
#> 37 0.0419494831 0.081310797
#> 38 0.0511792661 0.071810114
#> 39 0.1786539786 0.197023117
#> 40 0.2545966710 0.294545455
#> 41 0.1797554112 0.206879382
#> 42 0.0973866891 0.113070309
#> 43 0.3518893862 0.396090328
#> 44 0.0640178547 0.072590534
#> 45 0.0491725903 0.078065494
#> 46 0.0985937988 0.138690636
#> 47 0.1037386596 0.130681364
#> 48 0.0663120854 0.078567446
#> 49 0.1581176816 0.190992309
#> 50 0.0952063088 0.115670406
#> 51 0.0676506699 0.086725315
#> 52 0.0808510638 0.102326341
#> 53 0.1395218461 0.156611428
#> 54 0.0486146999 0.090958194
#> 55 0.0184054455 0.055085714
#> 56 0.0268917718 0.042370422
#> 57 0.0412603357 0.051398033
#> 58 0.0745309478 0.098469556
#> 59 0.0179153527 0.026943415
#> 60 0.0302700184 0.043999108
#> 61 0.0127502916 0.024179913
#> 62 0.0703413252 0.083790416
#> 63 0.0066432149 0.022406441
#> 64 0.0073004202 0.025414738
#> 65 0.1264933184 0.152622697
#> 66 0.3849564294 0.404322158
#> 67 0.2232176058 0.252626499
#> 68 0.0091250438 0.015824997
#> 69 0.0902897306 0.111749341
#> 70 0.1937754099 0.234213147
#> 71 0.3582709302 0.386999922
#> 72 0.0534450796 0.091168884
#> 74 0.0013116680 0.005587800
#> 75 0.0054411345 0.009777680
#> 76 0.0128788647 0.023918818
#> 77 0.1317666271 0.150028104
#> 78 0.0011304450 0.004394254
#> 79 0.0102083957 0.021447726
#> 80 0.0008465299 0.002499659
```

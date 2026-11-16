# Print a Comparison of Sequences

Print a Comparison of Sequences

## Usage

``` r
# S3 method for class 'tna_sequence_comparison'
print(x, ...)
```

## Arguments

- x:

  A `tna_sequence_comparison` object.

- ...:

  Arguments passed to the generic `print` method.

## Value

`x` (invisibly).

## See also

Model comparison functions
[`compare()`](https://sonsoles.me/tna/reference/compare.md),
[`compare.group_tna()`](https://sonsoles.me/tna/reference/compare.group_tna.md),
[`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_comparison()`](https://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot.tna_sequence_comparison()`](https://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare()`](https://sonsoles.me/tna/reference/plot_compare.md),
[`plot_compare.group_tna()`](https://sonsoles.me/tna/reference/plot_compare.group_tna.md),
[`print.tna_comparison()`](https://sonsoles.me/tna/reference/print.tna_comparison.md)

## Examples

``` r
# A small subset (100 high and 100 low achievers) without the test
sequences <- rbind(head(group_regulation, 100), tail(group_regulation, 100))
group <- rep(c("High", "Low"), each = 100)
comp <- compare_sequences(sequences, group, test = FALSE)
print(comp)
#>                                     pattern freq_High freq_Low   prop_High
#> 1                                     adapt        21       32 0.015283843
#> 2                                  cohesion       107       83 0.077874818
#> 3                                 consensus       360      303 0.262008734
#> 4                                coregulate        86       99 0.062590975
#> 5                                   discuss       207      216 0.150655022
#> 6                                   emotion       164      133 0.119359534
#> 7                                   monitor        69       86 0.050218341
#> 8                                      plan       324      308 0.235807860
#> 9                                 synthesis        36       36 0.026200873
#> 10                          adapt->cohesion         5        8 0.003924647
#> 11                         adapt->consensus        10       14 0.007849294
#> 18                      cohesion->consensus        55       31 0.043171115
#> 19                     cohesion->coregulate         9       11 0.007064364
#> 21                        cohesion->emotion        10        6 0.007849294
#> 23                           cohesion->plan        15       13 0.011773940
#> 26                     consensus->consensus        23       22 0.018053375
#> 27                    consensus->coregulate        49       50 0.038461538
#> 28                       consensus->discuss        86       39 0.067503925
#> 29                       consensus->emotion        30       19 0.023547881
#> 30                       consensus->monitor         7       16 0.005494505
#> 31                          consensus->plan       131      131 0.102825746
#> 35                    coregulate->consensus         7       16 0.005494505
#> 37                      coregulate->discuss         8       29 0.006279435
#> 38                      coregulate->emotion        22       18 0.017268446
#> 39                      coregulate->monitor        11       10 0.008634223
#> 40                         coregulate->plan        23       15 0.018053375
#> 43                        discuss->cohesion        11        6 0.008634223
#> 44                       discuss->consensus        74       49 0.058084772
#> 45                      discuss->coregulate        15       20 0.011773940
#> 46                         discuss->discuss        35       43 0.027472527
#> 47                         discuss->emotion        21       15 0.016483516
#> 50                       discuss->synthesis        29       32 0.022762951
#> 52                        emotion->cohesion        51       43 0.040031397
#> 53                       emotion->consensus        55       39 0.043171115
#> 55                         emotion->discuss        19       11 0.014913658
#> 56                         emotion->emotion         6       15 0.004709576
#> 58                            emotion->plan        15        7 0.011773940
#> 61                       monitor->consensus        15       10 0.011773940
#> 63                         monitor->discuss        19       29 0.014913658
#> 64                         monitor->emotion         6       10 0.004709576
#> 66                            monitor->plan        15       14 0.011773940
#> 69                           plan->cohesion         8        6 0.006279435
#> 70                          plan->consensus        86       92 0.067503925
#> 72                            plan->discuss        21       30 0.016483516
#> 73                            plan->emotion        56       28 0.043956044
#> 74                            plan->monitor        26       21 0.020408163
#> 75                               plan->plan        92      106 0.072213501
#> 77                         synthesis->adapt         7        8 0.005494505
#> 79                     synthesis->consensus        20        9 0.015698587
#> 111               cohesion->consensus->plan        20       14 0.017035775
#> 132               cohesion->plan->consensus         7        5 0.005962521
#> 135                    cohesion->plan->plan         5        5 0.004258944
#> 148              consensus->consensus->plan         7       11 0.005962521
#> 153          consensus->coregulate->discuss         5       17 0.004258944
#> 154          consensus->coregulate->emotion        12        7 0.010221465
#> 156             consensus->coregulate->plan        14        6 0.011925043
#> 159           consensus->discuss->consensus        29        9 0.024701874
#> 161             consensus->discuss->discuss        15        9 0.012776831
#> 165           consensus->discuss->synthesis        15        8 0.012776831
#> 167            consensus->emotion->cohesion         8        7 0.006814310
#> 179              consensus->plan->consensus        34       35 0.028960818
#> 181                consensus->plan->discuss         7       15 0.005962521
#> 182                consensus->plan->emotion        23       12 0.019591141
#> 183                consensus->plan->monitor        10        9 0.008517888
#> 184                   consensus->plan->plan        42       46 0.035775128
#> 211           coregulate->emotion->cohesion         9        8 0.007666099
#> 244          discuss->consensus->coregulate         8       12 0.006814310
#> 246             discuss->consensus->emotion         7        5 0.005962521
#> 248                discuss->consensus->plan        30       20 0.025553663
#> 257             discuss->discuss->consensus        13       11 0.011073254
#> 259               discuss->discuss->discuss         7        7 0.005962521
#> 264              discuss->emotion->cohesion         6        6 0.005110733
#> 265             discuss->emotion->consensus         9        6 0.007666099
#> 279               discuss->synthesis->adapt         5        7 0.004258944
#> 281           discuss->synthesis->consensus        16        9 0.013628620
#> 290            emotion->cohesion->consensus        24       16 0.020442930
#> 295                 emotion->cohesion->plan        10        7 0.008517888
#> 299             emotion->consensus->discuss        11        6 0.009369676
#> 302                emotion->consensus->plan        19       18 0.016183986
#> 341             monitor->discuss->consensus         7        6 0.005962521
#> 355                monitor->plan->consensus         5        8 0.004258944
#> 371             plan->consensus->coregulate        11       11 0.009369676
#> 372                plan->consensus->discuss        21       14 0.017887564
#> 373                plan->consensus->emotion         6        6 0.005110733
#> 375                   plan->consensus->plan        34       43 0.028960818
#> 391                 plan->emotion->cohesion        17        6 0.014480409
#> 392                plan->emotion->consensus        17        6 0.014480409
#> 394                  plan->emotion->discuss        10        5 0.008517888
#> 401                  plan->monitor->discuss         9        5 0.007666099
#> 408                   plan->plan->consensus        21       31 0.017887564
#> 410                     plan->plan->discuss         7        9 0.005962521
#> 411                     plan->plan->emotion        16       11 0.013628620
#> 412                     plan->plan->monitor         9       10 0.007666099
#> 413                        plan->plan->plan        20       35 0.017035775
#> 428              synthesis->consensus->plan         5        7 0.004258944
#> 663  consensus->plan->consensus->coregulate         5        7 0.004629630
#> 664     consensus->plan->consensus->discuss         6        7 0.005555556
#> 667        consensus->plan->consensus->plan        12       16 0.011111111
#> 694        consensus->plan->plan->consensus         6       17 0.005555556
#> 698          consensus->plan->plan->monitor         6        5 0.005555556
#> 699             consensus->plan->plan->plan         7       12 0.006481481
#> 845     discuss->consensus->plan->consensus         9        6 0.008333333
#> 850          discuss->consensus->plan->plan         8        8 0.007407407
#> 960      emotion->cohesion->consensus->plan         7        9 0.006481481
#> 1006         emotion->consensus->plan->plan         5        7 0.004629630
#> 1174       plan->consensus->plan->consensus         7       12 0.006481481
#> 1179            plan->consensus->plan->plan        14       18 0.012962963
#> 1270            plan->plan->consensus->plan        11       17 0.010185185
#> 1300                 plan->plan->plan->plan         7       15 0.006481481
#> 2633      plan->plan->consensus->plan->plan         7        5 0.007007007
#>         prop_Low
#> 1    0.024691358
#> 2    0.064043210
#> 3    0.233796296
#> 4    0.076388889
#> 5    0.166666667
#> 6    0.102623457
#> 7    0.066358025
#> 8    0.237654321
#> 9    0.027777778
#> 10   0.006688963
#> 11   0.011705686
#> 18   0.025919732
#> 19   0.009197324
#> 21   0.005016722
#> 23   0.010869565
#> 26   0.018394649
#> 27   0.041806020
#> 28   0.032608696
#> 29   0.015886288
#> 30   0.013377926
#> 31   0.109531773
#> 35   0.013377926
#> 37   0.024247492
#> 38   0.015050167
#> 39   0.008361204
#> 40   0.012541806
#> 43   0.005016722
#> 44   0.040969900
#> 45   0.016722408
#> 46   0.035953177
#> 47   0.012541806
#> 50   0.026755853
#> 52   0.035953177
#> 53   0.032608696
#> 55   0.009197324
#> 56   0.012541806
#> 58   0.005852843
#> 61   0.008361204
#> 63   0.024247492
#> 64   0.008361204
#> 66   0.011705686
#> 69   0.005016722
#> 70   0.076923077
#> 72   0.025083612
#> 73   0.023411371
#> 74   0.017558528
#> 75   0.088628763
#> 77   0.006688963
#> 79   0.007525084
#> 111  0.012773723
#> 132  0.004562044
#> 135  0.004562044
#> 148  0.010036496
#> 153  0.015510949
#> 154  0.006386861
#> 156  0.005474453
#> 159  0.008211679
#> 161  0.008211679
#> 165  0.007299270
#> 167  0.006386861
#> 179  0.031934307
#> 181  0.013686131
#> 182  0.010948905
#> 183  0.008211679
#> 184  0.041970803
#> 211  0.007299270
#> 244  0.010948905
#> 246  0.004562044
#> 248  0.018248175
#> 257  0.010036496
#> 259  0.006386861
#> 264  0.005474453
#> 265  0.005474453
#> 279  0.006386861
#> 281  0.008211679
#> 290  0.014598540
#> 295  0.006386861
#> 299  0.005474453
#> 302  0.016423358
#> 341  0.005474453
#> 355  0.007299270
#> 371  0.010036496
#> 372  0.012773723
#> 373  0.005474453
#> 375  0.039233577
#> 391  0.005474453
#> 392  0.005474453
#> 394  0.004562044
#> 401  0.004562044
#> 408  0.028284672
#> 410  0.008211679
#> 411  0.010036496
#> 412  0.009124088
#> 413  0.031934307
#> 428  0.006386861
#> 663  0.006986028
#> 664  0.006986028
#> 667  0.015968064
#> 694  0.016966068
#> 698  0.004990020
#> 699  0.011976048
#> 845  0.005988024
#> 850  0.007984032
#> 960  0.008982036
#> 1006 0.006986028
#> 1174 0.011976048
#> 1179 0.017964072
#> 1270 0.016966068
#> 1300 0.014970060
#> 2633 0.005482456
# \donttest{
# With the permutation test (small number of iterations for CRAN)
sequences <- rbind(head(group_regulation, 500), tail(group_regulation, 500))
group <- rep(c("High", "Low"), each = 500)
comp <- compare_sequences(sequences, group, sub = 2:3, iter = 20)
print(comp)
#>                               pattern freq_High freq_Low    prop_High
#> 1                     adapt->cohesion        14       46 0.0021817048
#> 2                    adapt->consensus        35       80 0.0054542621
#> 5                      adapt->emotion        10       19 0.0015583606
#> 10                cohesion->consensus       267      160 0.0416082281
#> 11               cohesion->coregulate        37       58 0.0057659342
#> 12                  cohesion->discuss        11       33 0.0017141967
#> 13                  cohesion->emotion        63       49 0.0098176718
#> 14                  cohesion->monitor         9       23 0.0014025245
#> 15                     cohesion->plan        61       46 0.0095059997
#> 17                   consensus->adapt         6        5 0.0009350164
#> 18                consensus->cohesion        40       17 0.0062334424
#> 19               consensus->consensus       161      112 0.0250896057
#> 20              consensus->coregulate       272      310 0.0423874084
#> 21                 consensus->discuss       427      197 0.0665419978
#> 22                 consensus->emotion       138       95 0.0215053763
#> 23                 consensus->monitor        67       73 0.0104410161
#> 24                    consensus->plan       619      650 0.0964625214
#> 25               consensus->synthesis        15        9 0.0023375409
#> 26                  coregulate->adapt        10        6 0.0015583606
#> 27               coregulate->cohesion        16       21 0.0024933770
#> 28              coregulate->consensus        47       84 0.0073242948
#> 29             coregulate->coregulate         6       14 0.0009350164
#> 30                coregulate->discuss       104      179 0.0162069503
#> 31                coregulate->emotion        94       66 0.0146485897
#> 32                coregulate->monitor        43       42 0.0067009506
#> 33                   coregulate->plan       112      107 0.0174536388
#> 34              coregulate->synthesis         6       11 0.0009350164
#> 35                     discuss->adapt        22      109 0.0034283933
#> 36                  discuss->cohesion        64       31 0.0099735079
#> 37                 discuss->consensus       405      227 0.0631136045
#> 38                discuss->coregulate        82       98 0.0127785570
#> 39                   discuss->discuss       155      219 0.0241545894
#> 40                   discuss->emotion       119       90 0.0185444912
#> 41                   discuss->monitor        12       28 0.0018700327
#> 42                      discuss->plan        18       11 0.0028050491
#> 43                 discuss->synthesis       119      169 0.0185444912
#> 45                  emotion->cohesion       253      207 0.0394265233
#> 46                 emotion->consensus       268      200 0.0417640642
#> 47                emotion->coregulate        11       32 0.0017141967
#> 48                   emotion->discuss        91       40 0.0141810815
#> 49                   emotion->emotion        43       69 0.0067009506
#> 50                   emotion->monitor        27       28 0.0042075736
#> 51                      emotion->plan        79       65 0.0123110488
#> 54                  monitor->cohesion        15       31 0.0023375409
#> 55                 monitor->consensus        60       64 0.0093501636
#> 56                monitor->coregulate        16       24 0.0024933770
#> 57                   monitor->discuss       111      147 0.0172978027
#> 58                   monitor->emotion        31       35 0.0048309179
#> 59                   monitor->monitor         7        8 0.0010908524
#> 60                      monitor->plan        66       83 0.0102851800
#> 61                 monitor->synthesis         7        7 0.0010908524
#> 63                     plan->cohesion        46       26 0.0071684588
#> 64                    plan->consensus       422      479 0.0657628175
#> 65                   plan->coregulate        42       14 0.0065451145
#> 66                      plan->discuss        91      122 0.0141810815
#> 67                      plan->emotion       265      189 0.0412965560
#> 68                      plan->monitor        96      131 0.0149602618
#> 69                         plan->plan       484      650 0.0754246533
#> 71                   synthesis->adapt        19       54 0.0029608851
#> 73               synthesis->consensus        98       66 0.0152719339
#> 76                 synthesis->emotion         9       14 0.0014025245
#> 79         adapt->cohesion->consensus         8       21 0.0013520365
#> 88       adapt->consensus->coregulate         9       21 0.0015210411
#> 89          adapt->consensus->discuss        10        9 0.0016900456
#> 92             adapt->consensus->plan         8       25 0.0013520365
#> 123    cohesion->consensus->consensus        27       13 0.0045631232
#> 124   cohesion->consensus->coregulate        31       33 0.0052391415
#> 125      cohesion->consensus->discuss        74       22 0.0125063377
#> 126      cohesion->consensus->emotion        24       12 0.0040561095
#> 128         cohesion->consensus->plan        84       61 0.0141963833
#> 133     cohesion->coregulate->discuss         9       15 0.0015210411
#> 134     cohesion->coregulate->emotion         9        6 0.0015210411
#> 136        cohesion->coregulate->plan        10       13 0.0016900456
#> 146       cohesion->emotion->cohesion        17       13 0.0028730776
#> 147      cohesion->emotion->consensus        21       11 0.0035490958
#> 149        cohesion->emotion->discuss         6        5 0.0010140274
#> 152           cohesion->emotion->plan         6        6 0.0010140274
#> 156        cohesion->monitor->discuss         6       10 0.0010140274
#> 160         cohesion->plan->consensus        17       16 0.0028730776
#> 163           cohesion->plan->emotion        11        5 0.0018590502
#> 165              cohesion->plan->plan        17       17 0.0028730776
#> 173    consensus->cohesion->consensus        19        8 0.0032110867
#> 181   consensus->consensus->consensus        17       11 0.0028730776
#> 182  consensus->consensus->coregulate        19       18 0.0032110867
#> 183     consensus->consensus->discuss        33       15 0.0055771506
#> 184     consensus->consensus->emotion        11        8 0.0018590502
#> 185     consensus->consensus->monitor        12        6 0.0020280548
#> 186        consensus->consensus->plan        48       47 0.0081122190
#> 188      consensus->coregulate->adapt         9        5 0.0015210411
#> 189   consensus->coregulate->cohesion         8       11 0.0013520365
#> 190  consensus->coregulate->consensus        24       47 0.0040561095
#> 191 consensus->coregulate->coregulate         6       10 0.0010140274
#> 192    consensus->coregulate->discuss        63       88 0.0106472875
#> 193    consensus->coregulate->emotion        56       39 0.0094642555
#> 194    consensus->coregulate->monitor        25       21 0.0042251141
#> 195       consensus->coregulate->plan        63       55 0.0106472875
#> 197         consensus->discuss->adapt        11       18 0.0018590502
#> 199     consensus->discuss->consensus       156       39 0.0263647118
#> 200    consensus->discuss->coregulate        31       20 0.0052391415
#> 201       consensus->discuss->discuss        68       41 0.0114923103
#> 202       consensus->discuss->emotion        52       14 0.0087882373
#> 205     consensus->discuss->synthesis        49       39 0.0082812236
#> 207      consensus->emotion->cohesion        31       27 0.0052391415
#> 208     consensus->emotion->consensus        45       30 0.0076052053
#> 211       consensus->emotion->emotion        10       10 0.0016900456
#> 212       consensus->emotion->monitor         7        6 0.0011830319
#> 213          consensus->emotion->plan        11       10 0.0018590502
#> 217     consensus->monitor->consensus         7        8 0.0011830319
#> 219       consensus->monitor->discuss        22       25 0.0037181004
#> 220       consensus->monitor->emotion         8        8 0.0013520365
#> 222          consensus->monitor->plan        16       12 0.0027040730
#> 225         consensus->plan->cohesion        20       11 0.0033800913
#> 226        consensus->plan->consensus       179      174 0.0302518168
#> 228          consensus->plan->discuss        33       47 0.0055771506
#> 229          consensus->plan->emotion        99       70 0.0167314517
#> 230          consensus->plan->monitor        31       42 0.0052391415
#> 231             consensus->plan->plan       196      253 0.0331248944
#> 245   coregulate->cohesion->consensus         7        6 0.0011830319
#> 251  coregulate->consensus->consensus         5       12 0.0008450228
#> 252 coregulate->consensus->coregulate         8       20 0.0013520365
#> 253    coregulate->consensus->discuss         9        9 0.0015210411
#> 256       coregulate->consensus->plan        15       32 0.0025350684
#> 267    coregulate->discuss->consensus        46       45 0.0077742099
#> 268   coregulate->discuss->coregulate        10       14 0.0016900456
#> 269      coregulate->discuss->discuss         8       39 0.0013520365
#> 270      coregulate->discuss->emotion        10       19 0.0016900456
#> 273    coregulate->discuss->synthesis         9       31 0.0015210411
#> 275     coregulate->emotion->cohesion        35       17 0.0059151597
#> 276    coregulate->emotion->consensus        20       16 0.0033800913
#> 278      coregulate->emotion->discuss        14        7 0.0023660639
#> 279      coregulate->emotion->emotion         5        7 0.0008450228
#> 285    coregulate->monitor->consensus         7       10 0.0011830319
#> 287      coregulate->monitor->discuss        15       13 0.0025350684
#> 289         coregulate->monitor->plan        13        8 0.0021970593
#> 292       coregulate->plan->consensus        25       23 0.0042251141
#> 295         coregulate->plan->emotion        20       15 0.0033800913
#> 296         coregulate->plan->monitor         6        7 0.0010140274
#> 297            coregulate->plan->plan        38       46 0.0064221734
#> 304         discuss->adapt->consensus        12       42 0.0020280548
#> 307           discuss->adapt->emotion         5       12 0.0008450228
#> 312      discuss->cohesion->consensus        35       19 0.0059151597
#> 320     discuss->consensus->consensus        34       14 0.0057461551
#> 321    discuss->consensus->coregulate        54       47 0.0091262464
#> 322       discuss->consensus->discuss        97       20 0.0163934426
#> 323       discuss->consensus->emotion        28       19 0.0047321278
#> 324       discuss->consensus->monitor        11       15 0.0018590502
#> 325          discuss->consensus->plan       139       95 0.0234916343
#> 329    discuss->coregulate->consensus         8       11 0.0013520365
#> 330      discuss->coregulate->discuss        19       36 0.0032110867
#> 331      discuss->coregulate->emotion        14       11 0.0023660639
#> 332      discuss->coregulate->monitor         6        8 0.0010140274
#> 333         discuss->coregulate->plan        17       19 0.0028730776
#> 336        discuss->discuss->cohesion         7       10 0.0011830319
#> 337       discuss->discuss->consensus        61       45 0.0103092784
#> 338      discuss->discuss->coregulate         9       19 0.0015210411
#> 339         discuss->discuss->discuss        21       53 0.0035490958
#> 340         discuss->discuss->emotion        16       15 0.0027040730
#> 343       discuss->discuss->synthesis        19       31 0.0032110867
#> 344        discuss->emotion->cohesion        30       29 0.0050701369
#> 345       discuss->emotion->consensus        40       31 0.0067601825
#> 350            discuss->emotion->plan        18       10 0.0030420821
#> 356         discuss->monitor->discuss         5        7 0.0008450228
#> 366         discuss->synthesis->adapt        14       42 0.0023660639
#> 368     discuss->synthesis->consensus        69       57 0.0116613149
#> 377      emotion->cohesion->consensus       126       66 0.0212945750
#> 378     emotion->cohesion->coregulate        22       28 0.0037181004
#> 379        emotion->cohesion->discuss         7       22 0.0011830319
#> 380        emotion->cohesion->emotion        34       29 0.0057461551
#> 382           emotion->cohesion->plan        34       24 0.0057461551
#> 385     emotion->consensus->consensus        26       10 0.0043941186
#> 386    emotion->consensus->coregulate        39       32 0.0065911780
#> 387       emotion->consensus->discuss        61       29 0.0103092784
#> 388       emotion->consensus->emotion        20        7 0.0033800913
#> 389       emotion->consensus->monitor         9        8 0.0015210411
#> 390          emotion->consensus->plan        82       90 0.0138583742
#> 400       emotion->discuss->consensus        34       14 0.0057461551
#> 402         emotion->discuss->discuss        14        5 0.0023660639
#> 406       emotion->discuss->synthesis        11        5 0.0018590502
#> 407        emotion->emotion->cohesion        18       19 0.0030420821
#> 408       emotion->emotion->consensus        14       19 0.0023660639
#> 417         emotion->monitor->discuss         7        8 0.0011830319
#> 423          emotion->plan->consensus        21       18 0.0035490958
#> 426            emotion->plan->emotion        12        5 0.0020280548
#> 428               emotion->plan->plan        24       23 0.0040561095
#> 435      monitor->cohesion->consensus         8       13 0.0013520365
#> 442    monitor->consensus->coregulate        10       12 0.0016900456
#> 443       monitor->consensus->discuss        18        9 0.0030420821
#> 446          monitor->consensus->plan        21       30 0.0035490958
#> 450      monitor->coregulate->discuss         5        5 0.0008450228
#> 455        monitor->discuss->cohesion        14       10 0.0023660639
#> 456       monitor->discuss->consensus        42       27 0.0070981917
#> 457      monitor->discuss->coregulate         7       11 0.0011830319
#> 458         monitor->discuss->discuss        13       30 0.0021970593
#> 459         monitor->discuss->emotion         8       14 0.0013520365
#> 462       monitor->discuss->synthesis        11       23 0.0018590502
#> 463        monitor->emotion->cohesion         5       17 0.0008450228
#> 464       monitor->emotion->consensus        15        8 0.0025350684
#> 474          monitor->plan->consensus        16       24 0.0027040730
#> 477            monitor->plan->emotion        13       11 0.0021970593
#> 479               monitor->plan->plan        30       32 0.0050701369
#> 487         plan->cohesion->consensus        30       13 0.0050701369
#> 493         plan->consensus->cohesion         6        6 0.0010140274
#> 494        plan->consensus->consensus        35       36 0.0059151597
#> 495       plan->consensus->coregulate        72       93 0.0121683285
#> 496          plan->consensus->discuss        85       60 0.0143653879
#> 497          plan->consensus->emotion        29       28 0.0049011323
#> 498          plan->consensus->monitor        22       21 0.0037181004
#> 499             plan->consensus->plan       140      189 0.0236606388
#> 509          plan->discuss->consensus        28       25 0.0047321278
#> 510         plan->discuss->coregulate        11       12 0.0018590502
#> 511            plan->discuss->discuss        15       28 0.0025350684
#> 512            plan->discuss->emotion         7        9 0.0011830319
#> 515          plan->discuss->synthesis        12       19 0.0020280548
#> 517           plan->emotion->cohesion        80       55 0.0135203650
#> 518          plan->emotion->consensus        79       53 0.0133513605
#> 520            plan->emotion->discuss        36       13 0.0060841643
#> 521            plan->emotion->emotion        13       18 0.0021970593
#> 523               plan->emotion->plan        24       15 0.0040561095
#> 526          plan->monitor->consensus        23       24 0.0038871050
#> 528            plan->monitor->discuss        32       37 0.0054081460
#> 529            plan->monitor->emotion         8        9 0.0013520365
#> 531               plan->monitor->plan        14       23 0.0023660639
#> 534              plan->plan->cohesion        12        5 0.0020280548
#> 535             plan->plan->consensus       132      179 0.0223086023
#> 536            plan->plan->coregulate        12        9 0.0020280548
#> 537               plan->plan->discuss        31       48 0.0052391415
#> 538               plan->plan->emotion        83       69 0.0140273787
#> 539               plan->plan->monitor        38       57 0.0064221734
#> 540                  plan->plan->plan       140      232 0.0236606388
#> 546       synthesis->adapt->consensus        10       25 0.0016900456
#> 559   synthesis->consensus->consensus         5        7 0.0008450228
#> 560  synthesis->consensus->coregulate        13       12 0.0021970593
#> 561     synthesis->consensus->discuss        22       10 0.0037181004
#> 564        synthesis->consensus->plan        36       30 0.0060841643
#> 581     synthesis->emotion->consensus         5        5 0.0008450228
#>         prop_Low effect_size p_value
#> 1   0.0072213501  5.36653628       1
#> 2   0.0125588697  5.30328338       1
#> 5   0.0029827316  1.67634676       1
#> 10  0.0251177394  7.78622243       1
#> 11  0.0091051805  1.13905241       1
#> 12  0.0051805338  5.79131948       1
#> 13  0.0076923077  1.16750897       1
#> 14  0.0036106750  3.80211421       1
#> 15  0.0072213501  1.70259525       1
#> 17  0.0007849294 -1.13939699       1
#> 18  0.0026687598  4.67788419       1
#> 19  0.0175824176  2.22115564       1
#> 20  0.0486656201  1.08292342       1
#> 21  0.0309262166 14.08724962       1
#> 22  0.0149136578  2.98267777       1
#> 23  0.0114599686 -0.53189320       1
#> 24  0.1020408163  0.42484079       1
#> 25  0.0014128728  0.96919598       1
#> 26  0.0009419152  0.04267533       1
#> 27  0.0032967033  0.69304740       1
#> 28  0.0131868132  3.97260936       1
#> 29  0.0021978022  1.67526272       1
#> 30  0.0281004710  4.58541464       1
#> 31  0.0103610675  1.60453014       1
#> 32  0.0065934066 -1.32590938       1
#> 33  0.0167974882 -0.61247035       1
#> 34  0.0017268446  0.32866075       1
#> 35  0.0171114600  7.99357389       1
#> 36  0.0048665620  4.18458383       1
#> 37  0.0356357928 11.50086777       1
#> 38  0.0153846154  0.97878386       1
#> 39  0.0343799058  5.16437005       1
#> 40  0.0141287284  2.14317556       1
#> 41  0.0043956044  3.95777158       1
#> 42  0.0017268446  0.38176096       1
#> 43  0.0265306122  4.22632338       1
#> 45  0.0324960754  2.80027361       1
#> 46  0.0313971743  4.02480656       1
#> 47  0.0050235479  3.71850235       1
#> 48  0.0062794349  7.83622081       1
#> 49  0.0108320251  1.72051660       1
#> 50  0.0043956044 -1.23255138       1
#> 51  0.0102040816  0.85720493       1
#> 54  0.0048665620  2.88485244       1
#> 55  0.0100470958 -0.51320661       1
#> 56  0.0037676609  0.93606404       1
#> 57  0.0230769231  3.79385076       1
#> 58  0.0054945055 -0.29101999       1
#> 59  0.0012558870 -1.15394442       1
#> 60  0.0130298273  1.09666196       1
#> 61  0.0010989011 -1.72696180       1
#> 63  0.0040816327  2.16358244       1
#> 64  0.0751962323  2.55547290       1
#> 65  0.0021978022  7.41233885       1
#> 66  0.0191522763  1.91745041       1
#> 67  0.0296703297  4.54811055       1
#> 68  0.0205651491  2.52960244       1
#> 69  0.1020408163  5.38030587       1
#> 71  0.0084772370  7.32250699       1
#> 73  0.0103610675  2.50552835       1
#> 76  0.0021978022  0.46954856       1
#> 79  0.0035775128  3.26032093       1
#> 88  0.0035775128  1.31827418       1
#> 89  0.0015332198 -1.18616055       1
#> 92  0.0042589438  3.10030111       1
#> 123 0.0022146508  1.56094509       1
#> 124 0.0056218058 -0.66855703       1
#> 125 0.0037478705  6.25712083       1
#> 126 0.0020442930  1.22495158       1
#> 128 0.0103918228  1.31596885       1
#> 133 0.0025553663  1.51748108       1
#> 134 0.0010221465 -0.03127653       1
#> 136 0.0022146508 -0.76210140       1
#> 146 0.0022146508 -0.10254090       1
#> 147 0.0018739353  1.83027494       1
#> 149 0.0008517888 -1.29920249       1
#> 152 0.0010221465 -1.13726269       1
#> 156 0.0017035775  0.29974673       1
#> 160 0.0027257240 -0.91150054       1
#> 163 0.0008517888  1.92257587       1
#> 165 0.0028960818 -1.44501153       1
#> 173 0.0013628620  2.84051102       1
#> 181 0.0018739353 -0.02780558       1
#> 182 0.0030664395 -0.87141851       1
#> 183 0.0025553663  2.63012653       1
#> 184 0.0013628620 -0.31717419       1
#> 185 0.0010221465  0.96032243       1
#> 186 0.0080068143 -1.25860953       1
#> 188 0.0008517888  0.03285643       1
#> 189 0.0018739353  0.13416724       1
#> 190 0.0080068143  3.56599144       1
#> 191 0.0017035775  0.41728754       1
#> 192 0.0149914821  1.76864244       1
#> 193 0.0066439523  0.98756041       1
#> 194 0.0035775128 -0.58692722       1
#> 195 0.0093696763  0.27075637       1
#> 197 0.0030664395  0.82881223       1
#> 199 0.0066439523 12.08151851       1
#> 200 0.0034071550  1.10296439       1
#> 201 0.0069846678  3.28842192       1
#> 202 0.0023850085  7.81957409       1
#> 205 0.0066439523  0.38996819       1
#> 207 0.0045996593 -0.11016527       1
#> 208 0.0051107325  1.93237773       1
#> 211 0.0017035775 -1.26510015       1
#> 212 0.0010221465 -0.85449606       1
#> 213 0.0017035775 -1.27444043       1
#> 217 0.0013628620 -0.79414593       1
#> 219 0.0042589438 -0.50455019       1
#> 220 0.0013628620 -1.31399103       1
#> 222 0.0020442930  0.14293366       1
#> 225 0.0018739353  1.00472202       1
#> 226 0.0296422487 -0.81154638       1
#> 228 0.0080068143  1.34427040       1
#> 229 0.0119250426  1.19217582       1
#> 230 0.0071550256  0.63516278       1
#> 231 0.0431005111  4.32120978       1
#> 245 0.0010221465 -0.96253128       1
#> 251 0.0020442930  1.61154056       1
#> 252 0.0034071550  3.29356191       1
#> 253 0.0015332198 -1.12238642       1
#> 256 0.0054514480  2.86936520       1
#> 267 0.0076660988 -1.19984311       1
#> 268 0.0023850085  0.01023180       1
#> 269 0.0066439523 10.48905527       1
#> 270 0.0032367973  0.63175790       1
#> 273 0.0052810903  4.39053069       1
#> 275 0.0028960818  2.40600487       1
#> 276 0.0027257240 -0.32512551       1
#> 278 0.0011925043  1.22628681       1
#> 279 0.0011925043 -0.43312431       1
#> 285 0.0017035775  0.22816859       1
#> 287 0.0022146508 -0.86375727       1
#> 289 0.0013628620  0.81617277       1
#> 292 0.0039182283 -0.78742705       1
#> 295 0.0025553663  0.67231933       1
#> 296 0.0011925043 -0.88788051       1
#> 297 0.0078364566  0.52620587       1
#> 304 0.0071550256  6.22863465       1
#> 307 0.0020442930  1.04124641       1
#> 312 0.0032367973  3.29354156       1
#> 320 0.0023850085  4.20705912       1
#> 321 0.0080068143 -0.22030236       1
#> 322 0.0034071550 13.44895202       1
#> 323 0.0032367973  0.81383896       1
#> 324 0.0025553663 -0.21296134       1
#> 325 0.0161839864  2.39816911       1
#> 329 0.0018739353 -0.53820034       1
#> 330 0.0061328790  2.46836215       1
#> 331 0.0018739353 -0.50628768       1
#> 332 0.0013628620 -0.32389392       1
#> 333 0.0032367973 -0.32412959       1
#> 336 0.0017035775 -0.38325822       1
#> 337 0.0076660988  1.19226564       1
#> 338 0.0032367973  2.04689797       1
#> 339 0.0090289608  5.62787548       1
#> 340 0.0025553663 -1.53557058       1
#> 343 0.0052810903  1.74338236       1
#> 344 0.0049403748 -1.00813693       1
#> 345 0.0052810903  0.60933077       1
#> 350 0.0017035775  1.44877407       1
#> 356 0.0011925043 -0.35002757       1
#> 366 0.0071550256  6.56957172       1
#> 368 0.0097103918  0.41700682       1
#> 377 0.0112436116  6.77094282       1
#> 378 0.0047700170 -0.02660763       1
#> 379 0.0037478705  3.71989839       1
#> 380 0.0049403748 -0.21890806       1
#> 382 0.0040885860  1.19144009       1
#> 385 0.0017035775  2.54016850       1
#> 386 0.0054514480  0.05355620       1
#> 387 0.0049403748  4.43872771       1
#> 388 0.0011925043  3.46089489       1
#> 389 0.0013628620 -1.07052742       1
#> 390 0.0153321976 -0.32611896       1
#> 400 0.0023850085  4.24293831       1
#> 402 0.0008517888  1.82299650       1
#> 406 0.0008517888  1.47262727       1
#> 407 0.0032367973 -1.14566899       1
#> 408 0.0032367973  0.17505184       1
#> 417 0.0013628620 -0.86332336       1
#> 423 0.0030664395 -0.76554131       1
#> 426 0.0008517888  1.35217076       1
#> 428 0.0039182283 -1.22407016       1
#> 435 0.0022146508  0.45285152       1
#> 442 0.0020442930 -0.51655751       1
#> 443 0.0015332198  1.92904915       1
#> 446 0.0051107325  0.54584540       1
#> 450 0.0008517888 -1.85577332       1
#> 455 0.0017035775 -0.35421873       1
#> 456 0.0045996593  2.11266449       1
#> 457 0.0018739353  0.11081407       1
#> 458 0.0051107325  5.13391064       1
#> 459 0.0023850085  0.84343020       1
#> 462 0.0039182283  1.74849677       1
#> 463 0.0028960818  4.20379021       1
#> 464 0.0013628620  1.01905952       1
#> 474 0.0040885860  0.79821415       1
#> 477 0.0018739353 -0.64170923       1
#> 479 0.0054514480 -1.18021570       1
#> 487 0.0022146508  2.45239878       1
#> 493 0.0010221465 -1.64804814       1
#> 494 0.0061328790 -0.97221537       1
#> 495 0.0158432709  1.89251257       1
#> 496 0.0102214651  2.71916788       1
#> 497 0.0047700170 -1.21113051       1
#> 498 0.0035775128 -1.04040630       1
#> 499 0.0321976150  2.78286125       1
#> 509 0.0042589438 -0.77631271       1
#> 510 0.0020442930 -0.75591387       1
#> 511 0.0047700170  1.20633812       1
#> 512 0.0015332198 -0.49544241       1
#> 515 0.0032367973  1.20510737       1
#> 517 0.0093696763  2.29127512       1
#> 518 0.0090289608  2.26400513       1
#> 520 0.0022146508  5.45604933       1
#> 521 0.0030664395  0.34015861       1
#> 523 0.0025553663  0.68465707       1
#> 526 0.0040885860 -1.02693707       1
#> 528 0.0063032368 -0.08812227       1
#> 529 0.0015332198 -0.94137814       1
#> 531 0.0039182283  1.41066877       1
#> 534 0.0008517888  2.36947768       1
#> 535 0.0304940375  3.38752094       1
#> 536 0.0015332198 -0.17217685       1
#> 537 0.0081771721  1.78438203       1
#> 538 0.0117546848  0.95900005       1
#> 539 0.0097103918  2.94858065       1
#> 540 0.0395229983  4.30232806       1
#> 546 0.0042589438  2.83588570       1
#> 559 0.0011925043 -0.02250613       1
#> 560 0.0020442930 -1.18806502       1
#> 561 0.0017035775  2.25744884       1
#> 564 0.0051107325  0.02822067       1
#> 581 0.0008517888 -1.35191388       1
# }
```

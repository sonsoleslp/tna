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
[`compare()`](http://sonsoles.me/tna/reference/compare.md),
[`compare.group_tna()`](http://sonsoles.me/tna/reference/compare.group_tna.md),
[`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_comparison()`](http://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md),
[`plot_compare.group_tna()`](http://sonsoles.me/tna/reference/plot_compare.group_tna.md),
[`print.tna_comparison()`](http://sonsoles.me/tna/reference/print.tna_comparison.md)

## Examples

``` r
group <- c(rep("High", 1000), rep("Low", 1000))
comp <- compare_sequences(group_regulation, group)
print(comp)
#>                                                    pattern freq_High freq_Low
#> 1                                                    adapt       155      399
#> 2                                                 cohesion      1018      821
#> 3                                                consensus      3651     3146
#> 4                                               coregulate       959     1174
#> 5                                                  emotion      1686     1389
#> 6                                                  monitor       668      848
#> 7                                                     plan      3102     3521
#> 8                                                synthesis       316      413
#> 9                                          adapt->cohesion        37      102
#> 10                                        adapt->consensus        73      170
#> 11                                          adapt->discuss         5       25
#> 12                                      cohesion->cohesion        41        5
#> 13                                     cohesion->consensus       503      341
#> 14                                    cohesion->coregulate        76      126
#> 15                                     consensus->cohesion        67       27
#> 16                                      consensus->discuss       789      401
#> 17                                      consensus->emotion       276      184
#> 18                                   coregulate->consensus        97      168
#> 19                                     coregulate->discuss       210      329
#> 20                                          discuss->adapt        48      234
#> 21                                       discuss->cohesion       124       64
#> 22                                      discuss->consensus       851      418
#> 23                                      discuss->synthesis       213      344
#> 24                                      emotion->consensus       521      388
#> 25                                        emotion->discuss       189      100
#> 26                                        plan->coregulate        69       37
#> 27                                           plan->emotion       527      377
#> 28                                              plan->plan       948     1356
#> 29                                        synthesis->adapt        40      113
#> 31                                         synthesis->plan        40        9
#> 33                                      consensus->monitor       120      175
#> 34                                  coregulate->coregulate        12       34
#> 36                                           plan->discuss       174      244
#> 38                                       cohesion->monitor        16       40
#> 39                                       emotion->cohesion       505      418
#> 40                                      synthesis->discuss         8       33
#> 41                                        monitor->discuss       234      304
#> 43                                          cohesion->plan       142       97
#> 44                                        discuss->discuss       339      431
#> 45                                          adapt->emotion        20       41
#> 46                              adapt->cohesion->consensus        22       49
#> 48                            adapt->consensus->coregulate        14       42
#> 49                                  adapt->consensus->plan        21       61
#> 50                            cohesion->consensus->discuss       129       44
#> 51                          consensus->cohesion->consensus        33       10
#> 52                            consensus->discuss->cohesion        39        7
#> 53                           consensus->discuss->consensus       291       87
#> 54                             consensus->discuss->discuss       140       79
#> 55                             consensus->discuss->emotion        92       33
#> 56                                consensus->plan->emotion       202      139
#> 57                                   consensus->plan->plan       382      495
#> 58                              coregulate->discuss->adapt         6       32
#> 59                            coregulate->discuss->discuss        27       65
#> 60                          coregulate->discuss->synthesis        20       63
#> 61                                discuss->adapt->cohesion        12       58
#> 62                               discuss->adapt->consensus        26       96
#> 64                                 discuss->adapt->emotion         6       25
#> 65                            discuss->cohesion->consensus        64       30
#> 66                             discuss->consensus->discuss       198       41
#> 67                                discuss->consensus->plan       302      181
#> 68                                 discuss->discuss->adapt         6       49
#> 69                             discuss->discuss->consensus       144       86
#> 70                               discuss->synthesis->adapt        29       89
#> 73                            emotion->cohesion->consensus       235      164
#> 74                             emotion->consensus->discuss       111       57
#> 75                             emotion->discuss->consensus        75       19
#> 77                               monitor->discuss->discuss        27       65
#> 78                               plan->cohesion->consensus        53       24
#> 79                                plan->consensus->monitor        28       59
#> 81                                plan->emotion->consensus       167      107
#> 82                                  plan->emotion->discuss        61       24
#> 83                                   plan->plan->consensus       256      363
#> 84                                        plan->plan->plan       282      510
#> 85                              synthesis->adapt->cohesion         6       29
#> 87                                       cohesion->discuss        38       63
#> 89                                        discuss->monitor        33       55
#> 90                                                 discuss      2166     2101
#> 94                                       cohesion->emotion       111       85
#> 95                                        consensus->adapt        14       16
#> 96                                    consensus->consensus       283      236
#> 97                                   consensus->coregulate       580      608
#> 98                                         consensus->plan      1236     1269
#> 99                                    consensus->synthesis        27       21
#> 100                                      coregulate->adapt        20       12
#> 101                                   coregulate->cohesion        32       39
#> 102                                    coregulate->emotion       182      157
#> 103                                    coregulate->monitor        86       84
#> 104                                       coregulate->plan       238      233
#> 105                                  coregulate->synthesis        17       20
#> 106                                    discuss->coregulate       145      188
#> 107                                       discuss->emotion       225      193
#> 108                                          discuss->plan        25       21
#> 110                                    emotion->coregulate        36       61
#> 111                                       emotion->emotion        97      121
#> 112                                       emotion->monitor        49       54
#> 113                                          emotion->plan       140      143
#> 114                                         monitor->adapt         7        9
#> 115                                      monitor->cohesion        31       49
#> 116                                     monitor->consensus       101      127
#> 117                                    monitor->coregulate        32       51
#> 118                                       monitor->emotion        61       69
#> 119                                       monitor->monitor        12       14
#> 120                                          monitor->plan       143      166
#> 121                                     monitor->synthesis        12       11
#> 123                                         plan->cohesion        91       64
#> 124                                        plan->consensus       850      938
#> 125                                          plan->monitor       219      246
#> 126                                    synthesis->cohesion         8       14
#> 127                                   synthesis->consensus       160      144
#> 128                                     synthesis->emotion        18       28
#> 138                              adapt->consensus->discuss        18       17
#> 160                              adapt->emotion->consensus        10        8
#> 187                         cohesion->consensus->consensus        44       27
#> 188                        cohesion->consensus->coregulate        67       69
#> 189                           cohesion->consensus->emotion        46       22
#> 190                           cohesion->consensus->monitor        11       13
#> 191                              cohesion->consensus->plan       169      135
#> 195                        cohesion->coregulate->consensus         7       21
#> 197                          cohesion->coregulate->discuss        15       31
#> 198                          cohesion->coregulate->emotion        15       20
#> 200                             cohesion->coregulate->plan        20       26
#> 204                           cohesion->discuss->consensus        15       12
#> 208                           cohesion->discuss->synthesis         8        5
#> 210                            cohesion->emotion->cohesion        30       27
#> 211                           cohesion->emotion->consensus        36       18
#> 213                             cohesion->emotion->discuss        13        9
#> 214                             cohesion->emotion->emotion         5        6
#> 216                                cohesion->emotion->plan        10       10
#> 221                             cohesion->monitor->discuss        10       17
#> 225                              cohesion->plan->consensus        36       31
#> 228                                cohesion->plan->emotion        28        9
#> 229                                cohesion->plan->monitor         8        5
#> 230                                   cohesion->plan->plan        43       36
#> 236                             consensus->adapt->cohesion         6        7
#> 237                            consensus->adapt->consensus         5        8
#> 239                        consensus->cohesion->coregulate         8        5
#> 246                        consensus->consensus->consensus        27       19
#> 247                       consensus->consensus->coregulate        41       40
#> 248                          consensus->consensus->discuss        56       26
#> 249                          consensus->consensus->emotion        25       17
#> 250                          consensus->consensus->monitor        15       11
#> 251                             consensus->consensus->plan        82      105
#> 253                           consensus->coregulate->adapt        15        8
#> 254                        consensus->coregulate->cohesion        19       19
#> 255                       consensus->coregulate->consensus        53       90
#> 256                      consensus->coregulate->coregulate        11       24
#> 257                         consensus->coregulate->discuss       135      166
#> 258                         consensus->coregulate->emotion       110       85
#> 259                         consensus->coregulate->monitor        52       41
#> 260                            consensus->coregulate->plan       141      116
#> 261                       consensus->coregulate->synthesis         7        8
#> 262                              consensus->discuss->adapt        22       43
#> 263                         consensus->discuss->coregulate        56       35
#> 264                            consensus->discuss->monitor        11        8
#> 265                               consensus->discuss->plan         7        5
#> 266                          consensus->discuss->synthesis        72       76
#> 268                           consensus->emotion->cohesion        72       52
#> 269                          consensus->emotion->consensus        86       52
#> 271                            consensus->emotion->discuss        33       17
#> 272                            consensus->emotion->emotion        18       16
#> 273                            consensus->emotion->monitor        13        9
#> 274                               consensus->emotion->plan        19       19
#> 277                           consensus->monitor->cohesion         6       12
#> 278                          consensus->monitor->consensus        17       23
#> 279                         consensus->monitor->coregulate         5        6
#> 280                            consensus->monitor->discuss        40       70
#> 281                            consensus->monitor->emotion         9       14
#> 283                               consensus->monitor->plan        30       27
#> 286                              consensus->plan->cohesion        37       18
#> 287                             consensus->plan->consensus       352      334
#> 288                            consensus->plan->coregulate        29       14
#> 289                               consensus->plan->discuss        67       91
#> 290                               consensus->plan->monitor        86       86
#> 294                        consensus->synthesis->consensus         9        5
#> 300                           coregulate->adapt->consensus         8        6
#> 306                        coregulate->cohesion->consensus        16       15
#> 311                             coregulate->cohesion->plan         5        5
#> 313                       coregulate->consensus->consensus        10       20
#> 314                      coregulate->consensus->coregulate        14       36
#> 315                         coregulate->consensus->discuss        16       20
#> 316                         coregulate->consensus->emotion        14        9
#> 318                            coregulate->consensus->plan        34       67
#> 328                          coregulate->discuss->cohesion         9        8
#> 329                         coregulate->discuss->consensus        86       68
#> 330                        coregulate->discuss->coregulate        16       27
#> 331                           coregulate->discuss->emotion        16       29
#> 332                           coregulate->discuss->monitor         5        6
#> 335                          coregulate->emotion->cohesion        56       44
#> 336                         coregulate->emotion->consensus        45       34
#> 338                           coregulate->emotion->discuss        25       18
#> 339                           coregulate->emotion->emotion        11       13
#> 341                              coregulate->emotion->plan        19       18
#> 345                         coregulate->monitor->consensus        14       16
#> 347                           coregulate->monitor->discuss        30       25
#> 350                              coregulate->monitor->plan        21       16
#> 353                             coregulate->plan->cohesion         6        5
#> 354                            coregulate->plan->consensus        64       56
#> 356                              coregulate->plan->discuss        16       12
#> 357                              coregulate->plan->emotion        36       29
#> 358                              coregulate->plan->monitor        19       15
#> 359                                 coregulate->plan->plan        71       99
#> 362                       coregulate->synthesis->consensus         6        8
#> 373                          discuss->cohesion->coregulate         7       10
#> 374                             discuss->cohesion->discuss         5        8
#> 375                             discuss->cohesion->emotion        12        6
#> 377                                discuss->cohesion->plan        13        5
#> 380                          discuss->consensus->consensus        59       28
#> 381                         discuss->consensus->coregulate       128       78
#> 382                            discuss->consensus->emotion        54       30
#> 383                            discuss->consensus->monitor        27       26
#> 386                          discuss->coregulate->cohesion         7        7
#> 387                         discuss->coregulate->consensus        17       20
#> 389                           discuss->coregulate->discuss        32       64
#> 390                           discuss->coregulate->emotion        25       20
#> 391                           discuss->coregulate->monitor         9       18
#> 392                              discuss->coregulate->plan        32       38
#> 394                             discuss->discuss->cohesion        14       17
#> 395                           discuss->discuss->coregulate        16       33
#> 396                              discuss->discuss->discuss        53       98
#> 397                              discuss->discuss->emotion        33       41
#> 398                              discuss->discuss->monitor         5        8
#> 399                                 discuss->discuss->plan         7        5
#> 400                            discuss->discuss->synthesis        31       60
#> 401                             discuss->emotion->cohesion        62       61
#> 402                            discuss->emotion->consensus        70       63
#> 403                           discuss->emotion->coregulate         5        9
#> 404                              discuss->emotion->discuss        26       11
#> 405                              discuss->emotion->emotion         9       10
#> 406                              discuss->emotion->monitor         5        5
#> 407                                 discuss->emotion->plan        24       16
#> 413                              discuss->monitor->discuss        16       20
#> 419                               discuss->plan->consensus         8        7
#> 423                                    discuss->plan->plan         5        6
#> 424                           discuss->synthesis->cohesion         5       12
#> 425                          discuss->synthesis->consensus       113      122
#> 427                            discuss->synthesis->emotion         8       23
#> 429                               discuss->synthesis->plan        28        9
#> 433                          emotion->cohesion->coregulate        40       65
#> 434                             emotion->cohesion->discuss        20       35
#> 435                             emotion->cohesion->emotion        64       45
#> 436                             emotion->cohesion->monitor         7       24
#> 437                                emotion->cohesion->plan        70       46
#> 441                          emotion->consensus->consensus        51       27
#> 442                         emotion->consensus->coregulate        87       71
#> 443                            emotion->consensus->emotion        29       16
#> 444                            emotion->consensus->monitor        18       24
#> 445                               emotion->consensus->plan       165      155
#> 450                           emotion->coregulate->discuss         5       20
#> 451                           emotion->coregulate->emotion         8        6
#> 452                           emotion->coregulate->monitor         7        8
#> 453                              emotion->coregulate->plan        10       11
#> 457                           emotion->discuss->coregulate         8        9
#> 458                              emotion->discuss->discuss        34       21
#> 459                              emotion->discuss->emotion        19        8
#> 462                            emotion->discuss->synthesis        21       16
#> 464                             emotion->emotion->cohesion        35       36
#> 465                            emotion->emotion->consensus        29       33
#> 467                              emotion->emotion->discuss         9        5
#> 468                              emotion->emotion->emotion         9       17
#> 473                            emotion->monitor->consensus         5       10
#> 475                              emotion->monitor->discuss        15       18
#> 476                              emotion->monitor->emotion         5        5
#> 481                               emotion->plan->consensus        38       37
#> 483                                 emotion->plan->discuss        10       10
#> 484                                 emotion->plan->emotion        21       14
#> 485                                 emotion->plan->monitor         9       14
#> 486                                    emotion->plan->plan        43       53
#> 495                           monitor->cohesion->consensus        16       22
#> 502                          monitor->consensus->consensus         5       12
#> 503                         monitor->consensus->coregulate        20       21
#> 504                            monitor->consensus->discuss        27       18
#> 506                            monitor->consensus->monitor         5       10
#> 507                               monitor->consensus->plan        31       52
#> 511                         monitor->coregulate->consensus         5        9
#> 513                           monitor->coregulate->discuss         8       11
#> 516                              monitor->coregulate->plan         7        9
#> 518                             monitor->discuss->cohesion        23       16
#> 519                            monitor->discuss->consensus        88       54
#> 520                           monitor->discuss->coregulate        15       28
#> 521                              monitor->discuss->emotion        25       32
#> 524                            monitor->discuss->synthesis        25       50
#> 525                             monitor->emotion->cohesion        12       23
#> 526                            monitor->emotion->consensus        21       22
#> 531                                 monitor->emotion->plan        10        5
#> 539                               monitor->plan->consensus        33       49
#> 541                                 monitor->plan->discuss         5       10
#> 542                                 monitor->plan->emotion        24       20
#> 543                                 monitor->plan->monitor         8        8
#> 544                                    monitor->plan->plan        57       60
#> 558                                plan->cohesion->emotion         9        8
#> 560                                   plan->cohesion->plan        12        9
#> 562                              plan->consensus->cohesion        12       10
#> 563                             plan->consensus->consensus        62       65
#> 564                            plan->consensus->coregulate       147      184
#> 565                               plan->consensus->discuss       161      117
#> 566                               plan->consensus->emotion        71       60
#> 567                                  plan->consensus->plan       289      354
#> 568                             plan->consensus->synthesis         6       10
#> 571                              plan->coregulate->discuss        10       13
#> 572                              plan->coregulate->emotion        13        5
#> 574                                 plan->coregulate->plan        23        8
#> 576                                plan->discuss->cohesion         6        7
#> 577                               plan->discuss->consensus        70       47
#> 578                              plan->discuss->coregulate        20       25
#> 579                                 plan->discuss->discuss        26       52
#> 580                                 plan->discuss->emotion        14       20
#> 583                               plan->discuss->synthesis        18       36
#> 585                                plan->emotion->cohesion       161      107
#> 586                              plan->emotion->coregulate        11       22
#> 587                                 plan->emotion->emotion        30       35
#> 588                                 plan->emotion->monitor         9       14
#> 589                                    plan->emotion->plan        43       37
#> 592                                plan->monitor->cohesion         9       10
#> 593                               plan->monitor->consensus        34       44
#> 594                              plan->monitor->coregulate         9       18
#> 595                                 plan->monitor->discuss        76       75
#> 596                                 plan->monitor->emotion        22       16
#> 598                                    plan->monitor->plan        45       55
#> 601                                   plan->plan->cohesion        23       27
#> 602                                 plan->plan->coregulate        17       16
#> 603                                    plan->plan->discuss        51       93
#> 604                                    plan->plan->emotion       164      137
#> 605                                    plan->plan->monitor        70       92
#> 611                            synthesis->adapt->consensus        20       48
#> 624                        synthesis->consensus->consensus         9       13
#> 625                       synthesis->consensus->coregulate        22       23
#> 626                          synthesis->consensus->discuss        31       26
#> 629                             synthesis->consensus->plan        58       58
#> 646                           synthesis->emotion->cohesion         5       10
#> 647                          synthesis->emotion->consensus         8        9
#> 661                                  synthesis->plan->plan        10        5
#> 669                       adapt->cohesion->consensus->plan         9       17
#> 702                  adapt->consensus->coregulate->discuss         6       15
#> 726                      adapt->consensus->plan->consensus         5       12
#> 731                           adapt->consensus->plan->plan         6       34
#> 845                   cohesion->consensus->consensus->plan        11       13
#> 850               cohesion->consensus->coregulate->discuss        18       23
#> 851               cohesion->consensus->coregulate->emotion        10        5
#> 853                  cohesion->consensus->coregulate->plan        12       10
#> 856                cohesion->consensus->discuss->consensus        42       12
#> 857               cohesion->consensus->discuss->coregulate        10        6
#> 858                  cohesion->consensus->discuss->discuss        27       13
#> 859                  cohesion->consensus->discuss->emotion         9        5
#> 864                cohesion->consensus->emotion->consensus        18        6
#> 876                   cohesion->consensus->plan->consensus        52       37
#> 878                     cohesion->consensus->plan->discuss        10        9
#> 879                     cohesion->consensus->plan->emotion        28       19
#> 880                     cohesion->consensus->plan->monitor         7       12
#> 881                        cohesion->consensus->plan->plan        49       46
#> 899               cohesion->coregulate->discuss->consensus         5        5
#> 942                     cohesion->discuss->consensus->plan         8        5
#> 962                 cohesion->emotion->cohesion->consensus        11       17
#> 974                     cohesion->emotion->consensus->plan         9        9
#> 1035                 cohesion->plan->consensus->coregulate         9        6
#> 1039                       cohesion->plan->consensus->plan        13       12
#> 1065                       cohesion->plan->plan->consensus        16        8
#> 1067                         cohesion->plan->plan->emotion         7        5
#> 1069                            cohesion->plan->plan->plan        11       16
#> 1119                 consensus->consensus->consensus->plan        12        9
#> 1123             consensus->consensus->coregulate->discuss         6       12
#> 1124             consensus->consensus->coregulate->emotion        12        7
#> 1126                consensus->consensus->coregulate->plan        13        9
#> 1130              consensus->consensus->discuss->consensus        24        6
#> 1134              consensus->consensus->discuss->synthesis         5        8
#> 1136              consensus->consensus->emotion->consensus         6        5
#> 1145                consensus->consensus->monitor->discuss         8        6
#> 1149                 consensus->consensus->plan->consensus        36       33
#> 1152                   consensus->consensus->plan->emotion        13       10
#> 1154                      consensus->consensus->plan->plan        20       35
#> 1165            consensus->coregulate->cohesion->consensus         9        5
#> 1170           consensus->coregulate->consensus->consensus         7       10
#> 1171          consensus->coregulate->consensus->coregulate         7       24
#> 1172             consensus->coregulate->consensus->discuss        10       13
#> 1175                consensus->coregulate->consensus->plan        19       33
#> 1187             consensus->coregulate->discuss->consensus        60       35
#> 1188            consensus->coregulate->discuss->coregulate         9       12
#> 1189               consensus->coregulate->discuss->discuss        12       32
#> 1190               consensus->coregulate->discuss->emotion         9       12
#> 1193             consensus->coregulate->discuss->synthesis        14       33
#> 1194              consensus->coregulate->emotion->cohesion        33       18
#> 1195             consensus->coregulate->emotion->consensus        28       23
#> 1197               consensus->coregulate->emotion->discuss        15       10
#> 1198               consensus->coregulate->emotion->emotion         6        7
#> 1200                  consensus->coregulate->emotion->plan        12       10
#> 1204             consensus->coregulate->monitor->consensus        10       10
#> 1206               consensus->coregulate->monitor->discuss        17       10
#> 1208                  consensus->coregulate->monitor->plan        13       10
#> 1212                consensus->coregulate->plan->consensus        37       26
#> 1214                  consensus->coregulate->plan->discuss         7        7
#> 1215                  consensus->coregulate->plan->emotion        22       18
#> 1216                  consensus->coregulate->plan->monitor        14        5
#> 1217                     consensus->coregulate->plan->plan        39       49
#> 1224                  consensus->discuss->adapt->consensus        12       20
#> 1240             consensus->discuss->consensus->coregulate        42       14
#> 1241                consensus->discuss->consensus->discuss        63       10
#> 1242                consensus->discuss->consensus->emotion        16        6
#> 1243                consensus->discuss->consensus->monitor         7       11
#> 1244                   consensus->discuss->consensus->plan       103       36
#> 1249               consensus->discuss->coregulate->discuss        12       15
#> 1252                  consensus->discuss->coregulate->plan        13        9
#> 1255                consensus->discuss->discuss->consensus        51       10
#> 1256               consensus->discuss->discuss->coregulate         6        8
#> 1257                  consensus->discuss->discuss->discuss        26       16
#> 1258                  consensus->discuss->discuss->emotion        16        7
#> 1261                consensus->discuss->discuss->synthesis        14       14
#> 1262                 consensus->discuss->emotion->cohesion        23       12
#> 1263                consensus->discuss->emotion->consensus        33       11
#> 1281                  consensus->discuss->synthesis->adapt         9       26
#> 1283              consensus->discuss->synthesis->consensus        40       28
#> 1291               consensus->emotion->cohesion->consensus        37       15
#> 1292              consensus->emotion->cohesion->coregulate         7        9
#> 1294                 consensus->emotion->cohesion->emotion        10        5
#> 1296                    consensus->emotion->cohesion->plan         7       10
#> 1299             consensus->emotion->consensus->coregulate        21       10
#> 1300                consensus->emotion->consensus->discuss        17       10
#> 1303                   consensus->emotion->consensus->plan        24       20
#> 1316                 consensus->emotion->emotion->cohesion         9        6
#> 1332                        consensus->emotion->plan->plan         6        6
#> 1354                consensus->monitor->discuss->consensus        16       16
#> 1360                consensus->monitor->discuss->synthesis         5       13
#> 1372                   consensus->monitor->plan->consensus         7        8
#> 1377                        consensus->monitor->plan->plan        14        9
#> 1384                  consensus->plan->cohesion->consensus        25        7
#> 1391                  consensus->plan->consensus->cohesion         8        5
#> 1392                 consensus->plan->consensus->consensus        31       20
#> 1393                consensus->plan->consensus->coregulate        64       66
#> 1394                   consensus->plan->consensus->discuss        64       54
#> 1395                   consensus->plan->consensus->emotion        34       20
#> 1396                   consensus->plan->consensus->monitor        13       15
#> 1397                      consensus->plan->consensus->plan       108      120
#> 1408                   consensus->plan->discuss->consensus        30       20
#> 1409                  consensus->plan->discuss->coregulate         9        6
#> 1410                     consensus->plan->discuss->discuss        13       22
#> 1411                     consensus->plan->discuss->emotion         6        6
#> 1415                    consensus->plan->emotion->cohesion        67       44
#> 1416                   consensus->plan->emotion->consensus        64       41
#> 1418                     consensus->plan->emotion->discuss        18        6
#> 1419                     consensus->plan->emotion->emotion        12        8
#> 1420                     consensus->plan->emotion->monitor         5        6
#> 1421                        consensus->plan->emotion->plan        14       14
#> 1425                   consensus->plan->monitor->consensus         8       16
#> 1427                     consensus->plan->monitor->discuss        34       24
#> 1430                        consensus->plan->monitor->plan        18       24
#> 1433                       consensus->plan->plan->cohesion         8       10
#> 1434                      consensus->plan->plan->consensus        92      141
#> 1435                     consensus->plan->plan->coregulate        10        6
#> 1436                        consensus->plan->plan->discuss        25       32
#> 1437                        consensus->plan->plan->emotion        62       47
#> 1438                        consensus->plan->plan->monitor        31       34
#> 1439                           consensus->plan->plan->plan       118      187
#> 1484                 coregulate->cohesion->consensus->plan         5        8
#> 1520             coregulate->consensus->discuss->consensus         9        7
#> 1535                coregulate->consensus->plan->consensus        10       14
#> 1538                  coregulate->consensus->plan->emotion         5       10
#> 1540                     coregulate->consensus->plan->plan        11       33
#> 1573             coregulate->discuss->consensus->consensus         7        7
#> 1574            coregulate->discuss->consensus->coregulate        13       14
#> 1576               coregulate->discuss->consensus->emotion         5        5
#> 1578                  coregulate->discuss->consensus->plan        31       29
#> 1580              coregulate->discuss->coregulate->discuss         5        9
#> 1587               coregulate->discuss->discuss->consensus        15       17
#> 1593                coregulate->discuss->emotion->cohesion         5        9
#> 1610             coregulate->discuss->synthesis->consensus        13       29
#> 1618              coregulate->emotion->cohesion->consensus        22       20
#> 1621                coregulate->emotion->cohesion->emotion         9        5
#> 1626            coregulate->emotion->consensus->coregulate         5        5
#> 1627               coregulate->emotion->consensus->discuss         8        5
#> 1630                  coregulate->emotion->consensus->plan        18       16
#> 1660                       coregulate->emotion->plan->plan         5        6
#> 1684               coregulate->monitor->discuss->consensus        13        6
#> 1694                  coregulate->monitor->plan->consensus         6        5
#> 1698                       coregulate->monitor->plan->plan         7        5
#> 1709               coregulate->plan->consensus->coregulate        10        9
#> 1710                  coregulate->plan->consensus->discuss        12        7
#> 1711                  coregulate->plan->consensus->emotion         6        9
#> 1713                     coregulate->plan->consensus->plan        18       20
#> 1725                   coregulate->plan->emotion->cohesion        15        5
#> 1726                  coregulate->plan->emotion->consensus         8       12
#> 1735                    coregulate->plan->monitor->discuss         5        8
#> 1739                     coregulate->plan->plan->consensus        21       26
#> 1742                       coregulate->plan->plan->emotion        18       11
#> 1743                       coregulate->plan->plan->monitor         6       10
#> 1744                          coregulate->plan->plan->plan        16       31
#> 1760                   discuss->adapt->cohesion->consensus         8       28
#> 1769                 discuss->adapt->consensus->coregulate         6       23
#> 1770                    discuss->adapt->consensus->discuss         7        7
#> 1773                       discuss->adapt->consensus->plan         5       32
#> 1805              discuss->cohesion->consensus->coregulate         9       11
#> 1806                 discuss->cohesion->consensus->discuss        20        7
#> 1809                    discuss->cohesion->consensus->plan        21        7
#> 1843             discuss->consensus->consensus->coregulate         8        6
#> 1847                   discuss->consensus->consensus->plan        21       13
#> 1850             discuss->consensus->coregulate->consensus        11       11
#> 1852               discuss->consensus->coregulate->discuss        29       13
#> 1853               discuss->consensus->coregulate->emotion        23       13
#> 1854               discuss->consensus->coregulate->monitor        11        8
#> 1855                  discuss->consensus->coregulate->plan        32       16
#> 1859                discuss->consensus->discuss->consensus        68        8
#> 1861                  discuss->consensus->discuss->discuss        36        6
#> 1865                discuss->consensus->discuss->synthesis        17        9
#> 1866                 discuss->consensus->emotion->cohesion        16        8
#> 1867                discuss->consensus->emotion->consensus        17        6
#> 1869                  discuss->consensus->emotion->discuss         7        5
#> 1877                  discuss->consensus->monitor->discuss        11        5
#> 1884                   discuss->consensus->plan->consensus        89       45
#> 1886                     discuss->consensus->plan->discuss        14       12
#> 1887                     discuss->consensus->plan->emotion        46       19
#> 1888                     discuss->consensus->plan->monitor        24        8
#> 1889                        discuss->consensus->plan->plan        95       81
#> 1903                  discuss->coregulate->consensus->plan         6        8
#> 1905               discuss->coregulate->discuss->consensus        10       14
#> 1906              discuss->coregulate->discuss->coregulate         6        6
#> 1907                 discuss->coregulate->discuss->discuss         6       12
#> 1910                discuss->coregulate->emotion->cohesion        10        9
#> 1924                  discuss->coregulate->plan->consensus         9       11
#> 1929                       discuss->coregulate->plan->plan        12       13
#> 1940                 discuss->discuss->cohesion->consensus         8        9
#> 1947                discuss->discuss->consensus->consensus         6        7
#> 1948               discuss->discuss->consensus->coregulate        20       13
#> 1949                  discuss->discuss->consensus->discuss        36        7
#> 1950                  discuss->discuss->consensus->emotion        15        9
#> 1952                     discuss->discuss->consensus->plan        51       37
#> 1963                  discuss->discuss->discuss->consensus        16       26
#> 1965                    discuss->discuss->discuss->discuss         8       19
#> 1966                    discuss->discuss->discuss->emotion         8        6
#> 1969                  discuss->discuss->discuss->synthesis         6       13
#> 1970                   discuss->discuss->emotion->cohesion        10       14
#> 1971                  discuss->discuss->emotion->consensus         9       14
#> 1987                    discuss->discuss->synthesis->adapt         5       13
#> 1989                discuss->discuss->synthesis->consensus        13       21
#> 1997                 discuss->emotion->cohesion->consensus        31       28
#> 1998                discuss->emotion->cohesion->coregulate         6        9
#> 2000                   discuss->emotion->cohesion->emotion         5        8
#> 2005                discuss->emotion->consensus->consensus         6        8
#> 2006               discuss->emotion->consensus->coregulate         8       12
#> 2010                     discuss->emotion->consensus->plan        29       25
#> 2033                     discuss->emotion->plan->consensus         7        8
#> 2036                          discuss->emotion->plan->plan         5        7
#> 2082                   discuss->synthesis->adapt->cohesion         5       24
#> 2083                  discuss->synthesis->adapt->consensus        13       35
#> 2097             discuss->synthesis->consensus->coregulate        18       20
#> 2098                discuss->synthesis->consensus->discuss        23       20
#> 2101                   discuss->synthesis->consensus->plan        40       48
#> 2130                        discuss->synthesis->plan->plan         5        5
#> 2145               emotion->cohesion->consensus->consensus        23       16
#> 2146              emotion->cohesion->consensus->coregulate        31       31
#> 2147                 emotion->cohesion->consensus->discuss        61       19
#> 2148                 emotion->cohesion->consensus->emotion        24        9
#> 2150                    emotion->cohesion->consensus->plan        79       70
#> 2153              emotion->cohesion->coregulate->consensus         6       12
#> 2155                emotion->cohesion->coregulate->discuss         5       20
#> 2156                emotion->cohesion->coregulate->emotion         9        9
#> 2158                   emotion->cohesion->coregulate->plan         8       11
#> 2162                 emotion->cohesion->discuss->consensus         5        8
#> 2168                  emotion->cohesion->emotion->cohesion        16       13
#> 2169                 emotion->cohesion->emotion->consensus        21        9
#> 2171                   emotion->cohesion->emotion->discuss         9        6
#> 2183                    emotion->cohesion->plan->consensus        17       13
#> 2186                      emotion->cohesion->plan->emotion        13        6
#> 2188                         emotion->cohesion->plan->plan        22       15
#> 2199             emotion->consensus->consensus->coregulate         8        6
#> 2203                   emotion->consensus->consensus->plan        13       10
#> 2207             emotion->consensus->coregulate->consensus         6        9
#> 2209               emotion->consensus->coregulate->discuss        22       20
#> 2210               emotion->consensus->coregulate->emotion        18       12
#> 2212                  emotion->consensus->coregulate->plan        22       11
#> 2214                    emotion->consensus->discuss->adapt         7        6
#> 2216                emotion->consensus->discuss->consensus        41        9
#> 2217               emotion->consensus->discuss->coregulate         7        7
#> 2218                  emotion->consensus->discuss->discuss        20        8
#> 2219                  emotion->consensus->discuss->emotion        14        5
#> 2222                emotion->consensus->discuss->synthesis        11       14
#> 2224                emotion->consensus->emotion->consensus        13        5
#> 2238                   emotion->consensus->plan->consensus        47       43
#> 2240                     emotion->consensus->plan->discuss         7       11
#> 2241                     emotion->consensus->plan->emotion        25       19
#> 2242                     emotion->consensus->plan->monitor        14        9
#> 2243                        emotion->consensus->plan->plan        51       56
#> 2292               emotion->discuss->consensus->coregulate        13        6
#> 2296                     emotion->discuss->consensus->plan        27       10
#> 2323                emotion->discuss->synthesis->consensus        14        5
#> 2330                 emotion->emotion->cohesion->consensus        18        7
#> 2333                   emotion->emotion->cohesion->emotion         5        7
#> 2338                  emotion->emotion->consensus->discuss         5        6
#> 2341                     emotion->emotion->consensus->plan        15       21
#> 2355                  emotion->emotion->emotion->consensus         7        5
#> 2408                  emotion->plan->consensus->coregulate         8        7
#> 2412                        emotion->plan->consensus->plan         8       14
#> 2422                      emotion->plan->emotion->cohesion        10        5
#> 2436                        emotion->plan->plan->consensus         9       10
#> 2441                             emotion->plan->plan->plan        11       20
#> 2461                 monitor->cohesion->consensus->discuss         6        5
#> 2510                   monitor->consensus->plan->consensus        12       16
#> 2515                        monitor->consensus->plan->plan         7       23
#> 2558                 monitor->discuss->cohesion->consensus        11        5
#> 2565                monitor->discuss->consensus->consensus         8        6
#> 2566               monitor->discuss->consensus->coregulate        13        8
#> 2567                  monitor->discuss->consensus->discuss        21       10
#> 2570                     monitor->discuss->consensus->plan        27       23
#> 2575                 monitor->discuss->coregulate->discuss         5        8
#> 2581                  monitor->discuss->discuss->consensus        13       12
#> 2587                   monitor->discuss->emotion->cohesion         5        9
#> 2588                  monitor->discuss->emotion->consensus         9       12
#> 2603                monitor->discuss->synthesis->consensus        14       20
#> 2609                 monitor->emotion->cohesion->consensus         7        9
#> 2621                     monitor->emotion->consensus->plan         8        6
#> 2656                  monitor->plan->consensus->coregulate         5       11
#> 2660                        monitor->plan->consensus->plan        12       20
#> 2669                      monitor->plan->emotion->cohesion         7        9
#> 2682                        monitor->plan->plan->consensus        19       15
#> 2685                          monitor->plan->plan->emotion         9        6
#> 2687                             monitor->plan->plan->plan        17       23
#> 2722                       plan->cohesion->consensus->plan        15       11
#> 2754                 plan->consensus->consensus->consensus         6        8
#> 2755                plan->consensus->consensus->coregulate        13       11
#> 2756                   plan->consensus->consensus->discuss         7        9
#> 2759                      plan->consensus->consensus->plan        18       25
#> 2763                plan->consensus->coregulate->consensus        17       34
#> 2765                  plan->consensus->coregulate->discuss        35       46
#> 2766                  plan->consensus->coregulate->emotion        30       23
#> 2767                  plan->consensus->coregulate->monitor        12       14
#> 2768                     plan->consensus->coregulate->plan        36       41
#> 2772                   plan->consensus->discuss->consensus        56       21
#> 2773                  plan->consensus->discuss->coregulate         8       11
#> 2774                     plan->consensus->discuss->discuss        33       24
#> 2775                     plan->consensus->discuss->emotion        25       11
#> 2778                   plan->consensus->discuss->synthesis        15       22
#> 2780                    plan->consensus->emotion->cohesion        18       21
#> 2781                   plan->consensus->emotion->consensus        19       14
#> 2783                     plan->consensus->emotion->discuss         8        6
#> 2784                     plan->consensus->emotion->emotion         5        8
#> 2788                   plan->consensus->monitor->consensus         5        7
#> 2790                     plan->consensus->monitor->discuss         8       23
#> 2793                        plan->consensus->monitor->plan         7       11
#> 2796                       plan->consensus->plan->cohesion         6        8
#> 2797                      plan->consensus->plan->consensus        63       94
#> 2798                     plan->consensus->plan->coregulate         7        6
#> 2799                        plan->consensus->plan->discuss        19       25
#> 2800                        plan->consensus->plan->emotion        52       36
#> 2801                        plan->consensus->plan->monitor        25       29
#> 2802                           plan->consensus->plan->plan        97      128
#> 2848                  plan->discuss->consensus->coregulate         8       11
#> 2852                        plan->discuss->consensus->plan        30       20
#> 2859                       plan->discuss->coregulate->plan         6        5
#> 2863                     plan->discuss->discuss->consensus        12        9
#> 2869                     plan->discuss->discuss->synthesis         5        7
#> 2871                     plan->discuss->emotion->consensus         6        9
#> 2888                   plan->discuss->synthesis->consensus         7        7
#> 2895                    plan->emotion->cohesion->consensus        76       46
#> 2896                   plan->emotion->cohesion->coregulate         8       15
#> 2897                      plan->emotion->cohesion->discuss        10        6
#> 2898                      plan->emotion->cohesion->emotion        22       13
#> 2900                         plan->emotion->cohesion->plan        20       14
#> 2903                   plan->emotion->consensus->consensus        16        8
#> 2904                  plan->emotion->consensus->coregulate        28       22
#> 2905                     plan->emotion->consensus->discuss        37       18
#> 2907                     plan->emotion->consensus->monitor         7        7
#> 2908                        plan->emotion->consensus->plan        47       36
#> 2927                      plan->emotion->emotion->cohesion         9       11
#> 2928                     plan->emotion->emotion->consensus         8       10
#> 2942                        plan->emotion->plan->consensus        14        8
#> 2947                             plan->emotion->plan->plan        14       15
#> 2959                  plan->monitor->consensus->coregulate         7        5
#> 2960                     plan->monitor->consensus->discuss         7        7
#> 2963                        plan->monitor->consensus->plan         9       20
#> 2973                     plan->monitor->discuss->consensus        22       12
#> 2974                    plan->monitor->discuss->coregulate         5        8
#> 2975                       plan->monitor->discuss->discuss        13       20
#> 2976                       plan->monitor->discuss->emotion        13        5
#> 2979                     plan->monitor->discuss->synthesis         7        9
#> 2980                      plan->monitor->emotion->cohesion         5        7
#> 2981                     plan->monitor->emotion->consensus         6        5
#> 2993                        plan->monitor->plan->consensus         7       14
#> 2996                          plan->monitor->plan->emotion         8        6
#> 2998                             plan->monitor->plan->plan        19       19
#> 3006                       plan->plan->cohesion->consensus        11        9
#> 3014                      plan->plan->consensus->consensus        18       30
#> 3015                     plan->plan->consensus->coregulate        38       75
#> 3016                        plan->plan->consensus->discuss        47       40
#> 3017                        plan->plan->consensus->emotion        16       24
#> 3018                        plan->plan->consensus->monitor         8       27
#> 3019                           plan->plan->consensus->plan        99      134
#> 3028                         plan->plan->discuss->cohesion         5        6
#> 3029                        plan->plan->discuss->consensus        18       21
#> 3031                          plan->plan->discuss->discuss         7       15
#> 3035                        plan->plan->discuss->synthesis         7        8
#> 3036                         plan->plan->emotion->cohesion        41       33
#> 3037                        plan->plan->emotion->consensus        56       37
#> 3038                       plan->plan->emotion->coregulate         6        7
#> 3039                          plan->plan->emotion->discuss        22       10
#> 3040                          plan->plan->emotion->emotion        11       19
#> 3042                             plan->plan->emotion->plan        14       15
#> 3045                        plan->plan->monitor->consensus        14       17
#> 3047                          plan->plan->monitor->discuss        20       24
#> 3048                          plan->plan->monitor->emotion         7        7
#> 3050                             plan->plan->monitor->plan        15       22
#> 3053                            plan->plan->plan->cohesion         7        7
#> 3054                           plan->plan->plan->consensus        78      129
#> 3056                             plan->plan->plan->discuss         9       41
#> 3057                             plan->plan->plan->emotion        50       54
#> 3058                             plan->plan->plan->monitor        20       31
#> 3059                                plan->plan->plan->plan        83      195
#> 3078                  synthesis->adapt->consensus->discuss         6        6
#> 3081                     synthesis->adapt->consensus->plan         6       17
#> 3134              synthesis->consensus->discuss->consensus        10        8
#> 3153                 synthesis->consensus->plan->consensus        11       13
#> 3154                   synthesis->consensus->plan->discuss         5        6
#> 3157                      synthesis->consensus->plan->plan        22       23
#> 3265           adapt->cohesion->consensus->plan->consensus         5        7
#> 3575   cohesion->consensus->coregulate->discuss->consensus         7        5
#> 3610         cohesion->consensus->discuss->consensus->plan        14        5
#> 3675      cohesion->consensus->plan->consensus->coregulate         5       11
#> 3676         cohesion->consensus->plan->consensus->discuss        10        5
#> 3679            cohesion->consensus->plan->consensus->plan        16       11
#> 3688          cohesion->consensus->plan->emotion->cohesion         8       12
#> 3702            cohesion->consensus->plan->plan->consensus        15       14
#> 3704              cohesion->consensus->plan->plan->emotion        10        7
#> 3706                 cohesion->consensus->plan->plan->plan        15       10
#> 4010                 cohesion->plan->consensus->plan->plan         6        6
#> 4250           consensus->consensus->plan->consensus->plan        13       15
#> 4272           consensus->consensus->plan->plan->consensus         9       12
#> 4277                consensus->consensus->plan->plan->plan         5       10
#> 4340     consensus->coregulate->consensus->plan->consensus         5        7
#> 4344          consensus->coregulate->consensus->plan->plan         6       15
#> 4377 consensus->coregulate->discuss->consensus->coregulate         8        7
#> 4381       consensus->coregulate->discuss->consensus->plan        22       14
#> 4389    consensus->coregulate->discuss->discuss->consensus         6       10
#> 4395     consensus->coregulate->discuss->emotion->cohesion         5        5
#> 4411  consensus->coregulate->discuss->synthesis->consensus         9       12
#> 4417   consensus->coregulate->emotion->cohesion->consensus        14        8
#> 4428       consensus->coregulate->emotion->consensus->plan        11       11
#> 4496          consensus->coregulate->plan->consensus->plan        14        9
#> 4520          consensus->coregulate->plan->plan->consensus        11       13
#> 4525               consensus->coregulate->plan->plan->plan        10       16
#> 4589       consensus->discuss->consensus->coregulate->plan         7        5
#> 4612        consensus->discuss->consensus->plan->consensus        30        9
#> 4617             consensus->discuss->consensus->plan->plan        30       16
#> 4695      consensus->discuss->emotion->cohesion->consensus        12        7
#> 4740       consensus->discuss->synthesis->adapt->consensus         5        5
#> 4753        consensus->discuss->synthesis->consensus->plan        13       18
#> 4777         consensus->emotion->cohesion->consensus->plan        10        9
#> 4835             consensus->emotion->consensus->plan->plan         6       10
#> 4954          consensus->monitor->discuss->consensus->plan         5        6
#> 5049     consensus->plan->consensus->consensus->coregulate         7        5
#> 5053           consensus->plan->consensus->consensus->plan         9        6
#> 5058       consensus->plan->consensus->coregulate->discuss        15       18
#> 5059       consensus->plan->consensus->coregulate->emotion        14        8
#> 5060       consensus->plan->consensus->coregulate->monitor         5        6
#> 5061          consensus->plan->consensus->coregulate->plan        18       12
#> 5065        consensus->plan->consensus->discuss->consensus        18       11
#> 5067          consensus->plan->consensus->discuss->discuss        15       11
#> 5068          consensus->plan->consensus->discuss->emotion        10        5
#> 5071        consensus->plan->consensus->discuss->synthesis         7        8
#> 5073         consensus->plan->consensus->emotion->cohesion         8        5
#> 5074        consensus->plan->consensus->emotion->consensus        11        5
#> 5086           consensus->plan->consensus->plan->consensus        26       36
#> 5088             consensus->plan->consensus->plan->discuss         6       13
#> 5089             consensus->plan->consensus->plan->emotion        21        9
#> 5090             consensus->plan->consensus->plan->monitor         8        9
#> 5091                consensus->plan->consensus->plan->plan        34       43
#> 5123             consensus->plan->discuss->consensus->plan        10        7
#> 5152         consensus->plan->emotion->cohesion->consensus        34       21
#> 5157              consensus->plan->emotion->cohesion->plan         8        6
#> 5159       consensus->plan->emotion->consensus->coregulate        10        7
#> 5160          consensus->plan->emotion->consensus->discuss        18        5
#> 5163             consensus->plan->emotion->consensus->plan        13       15
#> 5235                  consensus->plan->monitor->plan->plan         7        9
#> 5244           consensus->plan->plan->consensus->consensus         8       14
#> 5245          consensus->plan->plan->consensus->coregulate        14       31
#> 5246             consensus->plan->plan->consensus->discuss        16       17
#> 5247             consensus->plan->plan->consensus->emotion         7        8
#> 5249                consensus->plan->plan->consensus->plan        36       47
#> 5258             consensus->plan->plan->discuss->consensus         8        5
#> 5265              consensus->plan->plan->emotion->cohesion        16       11
#> 5266             consensus->plan->plan->emotion->consensus        19       16
#> 5269               consensus->plan->plan->emotion->emotion         8        7
#> 5273             consensus->plan->plan->monitor->consensus         7        7
#> 5275               consensus->plan->plan->monitor->discuss         8        7
#> 5277                  consensus->plan->plan->monitor->plan         7        8
#> 5281                consensus->plan->plan->plan->consensus        37       42
#> 5284                  consensus->plan->plan->plan->emotion        21       20
#> 5285                  consensus->plan->plan->plan->monitor        10       11
#> 5286                     consensus->plan->plan->plan->plan        25       73
#> 5567       coregulate->discuss->consensus->plan->consensus         9       11
#> 5572            coregulate->discuss->consensus->plan->plan         9       11
#> 5601         coregulate->discuss->discuss->consensus->plan         6        6
#> 5680        coregulate->emotion->cohesion->consensus->plan         8       13
#> 5919               coregulate->plan->consensus->plan->plan         6        8
#> 6018                    coregulate->plan->plan->plan->plan         6        9
#> 6253        discuss->consensus->consensus->plan->consensus         9        5
#> 6278     discuss->consensus->coregulate->emotion->cohesion         5        5
#> 6297            discuss->consensus->coregulate->plan->plan         8        9
#> 6341     discuss->consensus->discuss->synthesis->consensus        10        5
#> 6397       discuss->consensus->plan->consensus->coregulate        20       13
#> 6398          discuss->consensus->plan->consensus->discuss        17       12
#> 6401             discuss->consensus->plan->consensus->plan        26       12
#> 6414           discuss->consensus->plan->emotion->cohesion        19        6
#> 6429             discuss->consensus->plan->plan->consensus        24       24
#> 6432               discuss->consensus->plan->plan->emotion        20        7
#> 6433               discuss->consensus->plan->plan->monitor         7       11
#> 6434                  discuss->consensus->plan->plan->plan        30       30
#> 6634          discuss->discuss->consensus->plan->consensus        17       11
#> 6639               discuss->discuss->consensus->plan->plan        16       18
#> 6748          discuss->discuss->synthesis->consensus->plan         8        7
#> 6768        discuss->emotion->cohesion->consensus->discuss         8        5
#> 6771           discuss->emotion->cohesion->consensus->plan         9        9
#> 6819          discuss->emotion->consensus->plan->consensus         8        6
#> 6822               discuss->emotion->consensus->plan->plan         9       10
#> 6973            discuss->synthesis->adapt->consensus->plan         5       11
#> 7016     discuss->synthesis->consensus->discuss->consensus         7        7
#> 7033        discuss->synthesis->consensus->plan->consensus        11       10
#> 7037             discuss->synthesis->consensus->plan->plan        16       18
#> 7141         emotion->cohesion->consensus->consensus->plan         8        5
#> 7146     emotion->cohesion->consensus->coregulate->discuss        12        9
#> 7152      emotion->cohesion->consensus->discuss->consensus        20        6
#> 7154        emotion->cohesion->consensus->discuss->discuss        14        5
#> 7169         emotion->cohesion->consensus->plan->consensus        22       20
#> 7172           emotion->cohesion->consensus->plan->emotion        12       12
#> 7174              emotion->cohesion->consensus->plan->plan        21       25
#> 7236       emotion->cohesion->emotion->cohesion->consensus         8        9
#> 7313                   emotion->cohesion->plan->plan->plan         5        6
#> 7396            emotion->consensus->coregulate->plan->plan         9        5
#> 7408          emotion->consensus->discuss->consensus->plan        11        6
#> 7479       emotion->consensus->plan->consensus->coregulate         9       11
#> 7480          emotion->consensus->plan->consensus->discuss         8        5
#> 7483             emotion->consensus->plan->consensus->plan        16       14
#> 7494           emotion->consensus->plan->emotion->cohesion         6        7
#> 7495          emotion->consensus->plan->emotion->consensus        10        6
#> 7508             emotion->consensus->plan->plan->consensus        13       12
#> 7509               emotion->consensus->plan->plan->discuss         6        5
#> 7512                  emotion->consensus->plan->plan->plan        16       25
#> 7614               emotion->discuss->consensus->plan->plan         8        8
#> 7723          emotion->emotion->consensus->plan->consensus         5        6
#> 8195          monitor->discuss->consensus->plan->consensus         6        5
#> 8200               monitor->discuss->consensus->plan->plan         7        9
#> 8230            monitor->discuss->discuss->consensus->plan         5        5
#> 8479                  monitor->plan->plan->consensus->plan         8        5
#> 8492                  monitor->plan->plan->plan->consensus         5        6
#> 8496                       monitor->plan->plan->plan->plan         9        8
#> 8643           plan->consensus->consensus->plan->consensus         9        9
#> 8648                plan->consensus->consensus->plan->plan         5       11
#> 8660          plan->consensus->coregulate->consensus->plan         8       12
#> 8670       plan->consensus->coregulate->discuss->consensus        16       10
#> 8676        plan->consensus->coregulate->emotion->cohesion        13        5
#> 8677       plan->consensus->coregulate->emotion->consensus         5        7
#> 8691          plan->consensus->coregulate->plan->consensus         9        8
#> 8694            plan->consensus->coregulate->plan->emotion         7        9
#> 8696               plan->consensus->coregulate->plan->plan        10       18
#> 8711             plan->consensus->discuss->consensus->plan        28        7
#> 8738        plan->consensus->discuss->synthesis->consensus        12       10
#> 8745         plan->consensus->emotion->cohesion->consensus        10        5
#> 8788          plan->consensus->monitor->discuss->consensus         5        5
#> 8815           plan->consensus->plan->consensus->consensus         6        5
#> 8816          plan->consensus->plan->consensus->coregulate        15       12
#> 8817             plan->consensus->plan->consensus->discuss        11       19
#> 8818             plan->consensus->plan->consensus->emotion         6        7
#> 8820                plan->consensus->plan->consensus->plan        18       32
#> 8830             plan->consensus->plan->discuss->consensus         9        6
#> 8835              plan->consensus->plan->emotion->cohesion        18        6
#> 8836             plan->consensus->plan->emotion->consensus        13       15
#> 8847               plan->consensus->plan->monitor->discuss        11        6
#> 8850                  plan->consensus->plan->monitor->plan         6       12
#> 8853                plan->consensus->plan->plan->consensus        20       40
#> 8855                  plan->consensus->plan->plan->discuss         7        9
#> 8856                  plan->consensus->plan->plan->emotion        13       12
#> 8857                  plan->consensus->plan->plan->monitor        12        6
#> 8858                     plan->consensus->plan->plan->plan        30       48
#> 8977                  plan->discuss->consensus->plan->plan        15        8
#> 9085         plan->emotion->cohesion->consensus->consensus         7        8
#> 9086        plan->emotion->cohesion->consensus->coregulate         6       11
#> 9087           plan->emotion->cohesion->consensus->discuss        20        5
#> 9090              plan->emotion->cohesion->consensus->plan        24       15
#> 9118                   plan->emotion->cohesion->plan->plan         5        5
#> 9132       plan->emotion->consensus->coregulate->consensus         5        5
#> 9134         plan->emotion->consensus->coregulate->discuss         7        5
#> 9160             plan->emotion->consensus->plan->consensus         9       11
#> 9164               plan->emotion->consensus->plan->monitor         5        5
#> 9165                  plan->emotion->consensus->plan->plan        19       11
#> 9338            plan->monitor->discuss->consensus->discuss         6        6
#> 9412                  plan->monitor->plan->plan->consensus         6        8
#> 9416                       plan->monitor->plan->plan->plan         6        7
#> 9459          plan->plan->consensus->coregulate->consensus         9       12
#> 9461            plan->plan->consensus->coregulate->discuss         9       15
#> 9462            plan->plan->consensus->coregulate->emotion         6       10
#> 9464               plan->plan->consensus->coregulate->plan         8       22
#> 9468             plan->plan->consensus->discuss->consensus        20        8
#> 9470               plan->plan->consensus->discuss->discuss         8       10
#> 9474              plan->plan->consensus->emotion->cohesion         5        9
#> 9490                plan->plan->consensus->plan->consensus        20       34
#> 9492                  plan->plan->consensus->plan->discuss        11        8
#> 9493                  plan->plan->consensus->plan->emotion        18       10
#> 9494                  plan->plan->consensus->plan->monitor        11       12
#> 9495                     plan->plan->consensus->plan->plan        28       45
#> 9527                  plan->plan->discuss->consensus->plan        10       11
#> 9556              plan->plan->emotion->cohesion->consensus        15       12
#> 9559                plan->plan->emotion->cohesion->emotion         6        9
#> 9564            plan->plan->emotion->consensus->coregulate        10        6
#> 9565               plan->plan->emotion->consensus->discuss         9        7
#> 9568                  plan->plan->emotion->consensus->plan        20       14
#> 9605                  plan->plan->monitor->consensus->plan         5        6
#> 9612               plan->plan->monitor->discuss->consensus         5        7
#> 9614                 plan->plan->monitor->discuss->discuss         6        6
#> 9630                       plan->plan->monitor->plan->plan         7        7
#> 9643               plan->plan->plan->consensus->coregulate         9       28
#> 9644                  plan->plan->plan->consensus->discuss        12       13
#> 9647                     plan->plan->plan->consensus->plan        30       52
#> 9655                  plan->plan->plan->discuss->consensus         5       10
#> 9662                   plan->plan->plan->emotion->cohesion         8       14
#> 9663                  plan->plan->plan->emotion->consensus        17       14
#> 9665                    plan->plan->plan->emotion->discuss         6        5
#> 9668                       plan->plan->plan->emotion->plan        10        7
#> 9673                    plan->plan->plan->monitor->discuss         6        8
#> 9675                       plan->plan->plan->monitor->plan         5        7
#> 9678                     plan->plan->plan->plan->consensus        18       56
#> 9681                       plan->plan->plan->plan->emotion        19       20
#> 9682                       plan->plan->plan->plan->monitor         6       13
#> 9683                          plan->plan->plan->plan->plan        31       76
#> 9890                synthesis->consensus->plan->plan->plan        11       13
#>         prop_High     prop_Low  effect_size     p_value
#> 1    0.0112965527 0.0288879235 15.738021068 0.008991009
#> 2    0.0741928431 0.0594410657  7.221853130 0.008991009
#> 3    0.2660884775 0.2277729511 14.179755681 0.008991009
#> 4    0.0698928650 0.0849985520  6.390945680 0.008991009
#> 5    0.1228773413 0.1005647263  8.892595830 0.008991009
#> 6    0.0486844982 0.0613958876  6.809381705 0.008991009
#> 7    0.2260768166 0.2549232551  6.036493149 0.008991009
#> 8    0.0230303914 0.0299015349  4.506165258 0.008991009
#> 9    0.0029085764 0.0079612863  8.181878683 0.077922078
#> 10   0.0057385426 0.0132688105  8.615111170 0.077922078
#> 11   0.0003930509 0.0019512957  4.969305489 0.077922078
#> 12   0.0032230171 0.0003902591  6.878896481 0.077922078
#> 13   0.0395409166 0.0266156728  9.402610661 0.077922078
#> 14   0.0059743731 0.0098345301  4.368270413 0.077922078
#> 15   0.0052668815 0.0021073993  5.506625001 0.077922078
#> 16   0.0620234258 0.0312987824 16.904165892 0.077922078
#> 17   0.0216964075 0.0143615361  6.537271474 0.077922078
#> 18   0.0076251867 0.0131127068  5.781174186 0.077922078
#> 19   0.0165081362 0.0256790509  6.895393967 0.077922078
#> 20   0.0037732883 0.0182641274 16.909013282 0.077922078
#> 21   0.0097476613 0.0049953169  6.338934597 0.077922078
#> 22   0.0668972565 0.0326256634 19.356943412 0.077922078
#> 23   0.0167439667 0.0268498283  7.678816967 0.077922078
#> 24   0.0409558997 0.0302841086  6.467353937 0.077922078
#> 25   0.0148573225 0.0078051826  7.595240957 0.077922078
#> 26   0.0054241019 0.0028879176  4.245926781 0.077922078
#> 27   0.0414275607 0.0294255386  7.639613100 0.077922078
#> 28   0.0745224432 0.1058382766  9.367465243 0.077922078
#> 29   0.0031444069 0.0088198564  8.389947506 0.077922078
#> 31   0.0031444069 0.0007024664  5.927100612 0.077922078
#> 33   0.0094332207 0.0136590696  4.113919832 0.155844156
#> 34   0.0009433221 0.0026537621  3.665356461 0.155844156
#> 36   0.0136781700 0.0190446456  4.746741341 0.155844156
#> 38   0.0012577628 0.0031220731  3.809753812 0.233766234
#> 39   0.0396981369 0.0326256634  3.829968779 0.233766234
#> 40   0.0006288814 0.0025757103  4.678539766 0.233766234
#> 41   0.0183947803 0.0237277552  3.922126072 0.311688312
#> 43   0.0111626444 0.0075710272  3.389862308 0.389610390
#> 44   0.0266488484 0.0336403372  3.161566700 0.389610390
#> 45   0.0015722034 0.0032001249  3.210027997 0.545454545
#> 46   0.0018769730 0.0041483237  4.058141577 0.573426573
#> 48   0.0011944373 0.0035557061  4.766698994 0.573426573
#> 49   0.0017916560 0.0051642398  5.745687261 0.573426573
#> 50   0.0110058869 0.0037250254  9.910456348 0.573426573
#> 51   0.0028154594 0.0008465967  4.622440777 0.573426573
#> 52   0.0033273611 0.0005926177  6.315961269 0.573426573
#> 53   0.0248272332 0.0073653911 15.963763110 0.573426573
#> 54   0.0119443733 0.0066881138  5.677624820 0.573426573
#> 55   0.0078491596 0.0027937690  7.507102218 0.573426573
#> 56   0.0172340244 0.0117676939  4.240110346 0.573426573
#> 57   0.0325910758 0.0419065357  5.612868015 0.573426573
#> 58   0.0005119017 0.0027091094  6.025118166 0.573426573
#> 59   0.0023035577 0.0055028784  5.078878957 0.573426573
#> 60   0.0017063390 0.0053335591  6.517053948 0.573426573
#> 61   0.0010238034 0.0049102608  7.904444242 0.573426573
#> 62   0.0022182408 0.0081273281  8.670069898 0.573426573
#> 64   0.0005119017 0.0021164917  4.364089832 0.573426573
#> 65   0.0054602850 0.0025397900  5.049292903 0.573426573
#> 66   0.0168927566 0.0034710464 13.400535849 0.573426573
#> 67   0.0257657196 0.0153233999  8.538624474 0.573426573
#> 68   0.0005119017 0.0041483237  8.028313726 0.573426573
#> 69   0.0122856412 0.0072807315  4.876077265 0.573426573
#> 70   0.0024741916 0.0075347105  7.886559536 0.573426573
#> 73   0.0200494838 0.0138841856  5.005036111 0.573426573
#> 74   0.0094701817 0.0048256011  5.881836482 0.573426573
#> 75   0.0063987714 0.0016085337  7.991745701 0.573426573
#> 77   0.0023035577 0.0055028784  5.433196904 0.573426573
#> 78   0.0045217985 0.0020318320  4.395861105 0.573426573
#> 79   0.0023888747 0.0049949204  4.116161114 0.573426573
#> 81   0.0142479311 0.0090585845  5.112653229 0.573426573
#> 82   0.0052043341 0.0020318320  5.346597794 0.573426573
#> 83   0.0218411398 0.0307314595  6.023971681 0.573426573
#> 84   0.0240593806 0.0431764307  8.239660643 0.573426573
#> 85   0.0005119017 0.0024551304  5.216744808 0.573426573
#> 87   0.0029871865 0.0049172651  2.927780143 0.779220779
#> 89   0.0025941357 0.0042928505  2.714253944 0.935064935
#> 90   0.1578602143 0.1521141037  0.687470972 1.000000000
#> 94   0.0087257291 0.0066344052  2.041705107 1.000000000
#> 95   0.0011005424 0.0012488292 -0.735818940 1.000000000
#> 96   0.0222466787 0.0184202310  1.979517612 1.000000000
#> 97   0.0455938999 0.0474555105 -0.129649361 1.000000000
#> 98   0.0971621728 0.0990477677 -0.337375111 1.000000000
#> 99   0.0021224746 0.0016390884  0.189827431 1.000000000
#> 100  0.0015722034 0.0009366219  1.076219329 1.000000000
#> 101  0.0025155255 0.0030440212  0.051386928 1.000000000
#> 102  0.0143070513 0.0122541367  1.081404822 1.000000000
#> 103  0.0067604748 0.0065563534 -0.980592800 1.000000000
#> 104  0.0187092210 0.0181860756 -0.797371923 1.000000000
#> 105  0.0013363729 0.0015610365 -0.557972356 1.000000000
#> 106  0.0113984750 0.0146737434  2.324391405 1.000000000
#> 107  0.0176872887 0.0150640025  1.531311003 1.000000000
#> 108  0.0019652543 0.0016390884 -0.319430612 1.000000000
#> 110  0.0028299662 0.0047611614  2.724466578 1.000000000
#> 111  0.0076251867 0.0094442710  0.915438772 1.000000000
#> 112  0.0038518984 0.0042147986 -0.525245019 1.000000000
#> 113  0.0110054241 0.0111614112 -1.104305352 1.000000000
#> 114  0.0005502712 0.0007024664 -0.488381042 1.000000000
#> 115  0.0024369153 0.0038245395  1.889526559 1.000000000
#> 116  0.0079396274 0.0099125820  1.539619331 1.000000000
#> 117  0.0025155255 0.0039806431  2.168308267 1.000000000
#> 118  0.0047952205 0.0053855760 -0.171058783 1.000000000
#> 119  0.0009433221 0.0010927256 -0.699623782 1.000000000
#> 120  0.0112412546 0.0129566032  0.763808147 1.000000000
#> 121  0.0009433221 0.0008585701 -0.995460543 1.000000000
#> 123  0.0071535257 0.0049953169  2.296200552 1.000000000
#> 124  0.0668186463 0.0732126132  2.223832617 1.000000000
#> 125  0.0172156277 0.0192007493  0.721344821 1.000000000
#> 126  0.0006288814 0.0010927256  0.693574442 1.000000000
#> 127  0.0125776275 0.0112394630  0.357420797 1.000000000
#> 128  0.0014149831 0.0021854511  1.118602233 1.000000000
#> 138  0.0015357051 0.0014392144 -0.994438407 1.000000000
#> 160  0.0008531695 0.0006772773 -0.465152449 1.000000000
#> 187  0.0037539459 0.0022858110  2.010082428 1.000000000
#> 188  0.0057162358 0.0058415171 -1.118388226 1.000000000
#> 189  0.0039245798 0.0018625127  3.641342611 1.000000000
#> 190  0.0009384865 0.0011005757 -0.655983343 1.000000000
#> 191  0.0144185650 0.0114290552  2.147782468 1.000000000
#> 195  0.0005972187 0.0017778530  3.136578333 1.000000000
#> 197  0.0012797543 0.0026244497  2.458802330 1.000000000
#> 198  0.0012797543 0.0016931934  0.057807405 1.000000000
#> 200  0.0017063390 0.0022011514  0.180582402 1.000000000
#> 204  0.0012797543 0.0010159160 -0.325851134 1.000000000
#> 208  0.0006825356 0.0004232983  0.062635111 1.000000000
#> 210  0.0025595086 0.0022858110 -0.568213321 1.000000000
#> 211  0.0030714103 0.0015238740  2.905580247 1.000000000
#> 213  0.0011091204 0.0007619370  0.112340453 1.000000000
#> 214  0.0004265848 0.0005079580 -0.958865251 1.000000000
#> 216  0.0008531695 0.0008465967 -1.350267734 1.000000000
#> 221  0.0008531695 0.0014392144  0.768975475 1.000000000
#> 225  0.0030714103 0.0026244497 -0.199114300 1.000000000
#> 228  0.0023888747 0.0007619370  3.687984403 1.000000000
#> 229  0.0006825356 0.0004232983  0.061616693 1.000000000
#> 230  0.0036686290 0.0030477481  0.092004969 1.000000000
#> 236  0.0005119017 0.0005926177 -0.931190220 1.000000000
#> 237  0.0004265848 0.0006772773  0.014120323 1.000000000
#> 239  0.0006825356 0.0004232983  0.094512362 1.000000000
#> 246  0.0023035577 0.0016085337  0.631953290 1.000000000
#> 247  0.0034979951 0.0033863867 -1.047622251 1.000000000
#> 248  0.0047777493 0.0022011514  4.080007496 1.000000000
#> 249  0.0021329238 0.0014392144  0.714042031 1.000000000
#> 250  0.0012797543 0.0009312563  0.076486607 1.000000000
#> 251  0.0069959901 0.0088892652  1.277386833 1.000000000
#> 253  0.0012797543 0.0006772773  1.078064297 1.000000000
#> 254  0.0016210221 0.0016085337 -1.329284434 1.000000000
#> 255  0.0045217985 0.0076193701  3.686507760 1.000000000
#> 256  0.0009384865 0.0020318320  2.160799476 1.000000000
#> 257  0.0115177886 0.0140535049  1.541412469 1.000000000
#> 258  0.0093848648 0.0071960718  1.778158918 1.000000000
#> 259  0.0044364815 0.0034710464  0.567511159 1.000000000
#> 260  0.0120296903 0.0098205215  1.377249336 1.000000000
#> 261  0.0005972187 0.0006772773 -0.983757913 1.000000000
#> 262  0.0018769730 0.0036403657  3.050640792 1.000000000
#> 263  0.0047777493 0.0029630884  2.473582528 1.000000000
#> 264  0.0009384865 0.0006772773 -0.178837270 1.000000000
#> 265  0.0005972187 0.0004232983 -0.348752979 1.000000000
#> 266  0.0061428206 0.0064341348 -0.840479606 1.000000000
#> 268  0.0061428206 0.0044023027  1.800104339 1.000000000
#> 269  0.0073372579 0.0044023027  3.635435265 1.000000000
#> 271  0.0028154594 0.0014392144  2.607269425 1.000000000
#> 272  0.0015357051 0.0013545547 -0.729508296 1.000000000
#> 273  0.0011091204 0.0007619370  0.120611798 1.000000000
#> 274  0.0016210221 0.0016085337 -1.279093475 1.000000000
#> 277  0.0005119017 0.0010159160  0.910878557 1.000000000
#> 278  0.0014503882 0.0019471724  0.225472466 1.000000000
#> 279  0.0004265848 0.0005079580 -0.907312842 1.000000000
#> 280  0.0034126781 0.0059261768  3.507619905 1.000000000
#> 281  0.0007678526 0.0011852354  0.353952013 1.000000000
#> 283  0.0025595086 0.0022858110 -0.584264864 1.000000000
#> 286  0.0031567272 0.0015238740  2.968506081 1.000000000
#> 287  0.0300315673 0.0282763292 -0.018746999 1.000000000
#> 288  0.0024741916 0.0011852354  2.738471061 1.000000000
#> 289  0.0057162358 0.0077040298  1.689898066 1.000000000
#> 290  0.0073372579 0.0072807315 -1.228942359 1.000000000
#> 294  0.0007678526 0.0004232983  0.469425594 1.000000000
#> 300  0.0006825356 0.0005079580 -0.414168754 1.000000000
#> 306  0.0013650712 0.0012698950 -1.013615977 1.000000000
#> 311  0.0004265848 0.0004232983 -1.245136005 1.000000000
#> 313  0.0008531695 0.0016931934  1.690420992 1.000000000
#> 314  0.0011944373 0.0030477481  3.613560303 1.000000000
#> 315  0.0013650712 0.0016931934 -0.244626711 1.000000000
#> 316  0.0011944373 0.0007619370  0.552312878 1.000000000
#> 318  0.0029007764 0.0056721978  4.133072128 1.000000000
#> 328  0.0007678526 0.0006772773 -0.916459791 1.000000000
#> 329  0.0073372579 0.0057568574  1.275483995 1.000000000
#> 330  0.0013650712 0.0022858110  1.230730369 1.000000000
#> 331  0.0013650712 0.0024551304  1.690120046 1.000000000
#> 332  0.0004265848 0.0005079580 -0.945540473 1.000000000
#> 335  0.0047777493 0.0037250254  0.778528387 1.000000000
#> 336  0.0038392629 0.0028784287  0.755914240 1.000000000
#> 338  0.0021329238 0.0015238740  0.434755662 1.000000000
#> 339  0.0009384865 0.0011005757 -0.681662631 1.000000000
#> 341  0.0016210221 0.0015238740 -0.992849748 1.000000000
#> 345  0.0011944373 0.0013545547 -0.734693460 1.000000000
#> 347  0.0025595086 0.0021164917 -0.157248733 1.000000000
#> 350  0.0017916560 0.0013545547  0.148672226 1.000000000
#> 353  0.0005119017 0.0004232983 -0.815638226 1.000000000
#> 354  0.0054602850 0.0047409414 -0.075495159 1.000000000
#> 356  0.0013650712 0.0010159160 -0.029143812 1.000000000
#> 357  0.0030714103 0.0024551304  0.215888003 1.000000000
#> 358  0.0016210221 0.0012698950 -0.115226985 1.000000000
#> 359  0.0060575036 0.0083813071  2.188835129 1.000000000
#> 362  0.0005119017 0.0006772773 -0.437197648 1.000000000
#> 373  0.0005972187 0.0008465967 -0.174265649 1.000000000
#> 374  0.0004265848 0.0006772773 -0.106511617 1.000000000
#> 375  0.0010238034 0.0005079580  1.083049070 1.000000000
#> 377  0.0011091204 0.0004232983  1.906055098 1.000000000
#> 380  0.0050337002 0.0023704707  4.302169123 1.000000000
#> 381  0.0109205699 0.0066034541  4.456541741 1.000000000
#> 382  0.0046071154 0.0025397900  3.164314717 1.000000000
#> 383  0.0023035577 0.0022011514 -1.054572224 1.000000000
#> 386  0.0005972187 0.0005926177 -1.309106399 1.000000000
#> 387  0.0014503882 0.0016931934 -0.531105253 1.000000000
#> 389  0.0027301425 0.0054182188  3.544679438 1.000000000
#> 390  0.0021329238 0.0016931934 -0.003181324 1.000000000
#> 391  0.0007678526 0.0015238740  1.421361001 1.000000000
#> 392  0.0027301425 0.0032170674 -0.187995090 1.000000000
#> 394  0.0011944373 0.0014392144 -0.456197191 1.000000000
#> 395  0.0013650712 0.0027937690  2.631145478 1.000000000
#> 396  0.0045217985 0.0082966475  3.885676822 1.000000000
#> 397  0.0028154594 0.0034710464  0.163033194 1.000000000
#> 398  0.0004265848 0.0006772773 -0.082517599 1.000000000
#> 399  0.0005972187 0.0004232983 -0.376511423 1.000000000
#> 400  0.0026448255 0.0050795801  3.791649809 1.000000000
#> 401  0.0052896511 0.0051642398 -1.095909460 1.000000000
#> 402  0.0059721867 0.0053335591 -0.241018270 1.000000000
#> 403  0.0004265848 0.0007619370  0.452467667 1.000000000
#> 404  0.0022182408 0.0009312563  2.775705504 1.000000000
#> 405  0.0007678526 0.0008465967 -0.998066612 1.000000000
#> 406  0.0004265848 0.0004232983 -1.244116274 1.000000000
#> 407  0.0020476069 0.0013545547  0.796131602 1.000000000
#> 413  0.0013650712 0.0016931934 -0.250381773 1.000000000
#> 419  0.0006825356 0.0005926177 -0.924054233 1.000000000
#> 423  0.0004265848 0.0005079580 -0.908552249 1.000000000
#> 424  0.0004265848 0.0010159160  1.368088684 1.000000000
#> 425  0.0096408156 0.0103284795 -0.408312841 1.000000000
#> 427  0.0006825356 0.0019471724  3.116474392 1.000000000
#> 429  0.0023888747 0.0007619370  3.785416904 1.000000000
#> 433  0.0034126781 0.0055028784  2.637680679 1.000000000
#> 434  0.0017063390 0.0029630884  2.028745945 1.000000000
#> 435  0.0054602850 0.0038096851  1.908575999 1.000000000
#> 436  0.0005972187 0.0020318320  3.680689042 1.000000000
#> 437  0.0059721867 0.0038943447  2.433368300 1.000000000
#> 441  0.0043511646 0.0022858110  3.324201249 1.000000000
#> 442  0.0074225749 0.0060108364  0.929413602 1.000000000
#> 443  0.0024741916 0.0013545547  1.925166824 1.000000000
#> 444  0.0015357051 0.0020318320  0.218682716 1.000000000
#> 445  0.0140772972 0.0131222486 -0.302410513 1.000000000
#> 450  0.0004265848 0.0016931934  3.719950192 1.000000000
#> 451  0.0006825356 0.0005079580 -0.525771457 1.000000000
#> 452  0.0005972187 0.0006772773 -0.951979729 1.000000000
#> 453  0.0008531695 0.0009312563 -1.070997277 1.000000000
#> 457  0.0006825356 0.0007619370 -0.947594178 1.000000000
#> 458  0.0029007764 0.0017778530  1.607036728 1.000000000
#> 459  0.0016210221 0.0006772773  2.044672106 1.000000000
#> 462  0.0017916560 0.0013545547  0.119586977 1.000000000
#> 464  0.0029860933 0.0030477481 -1.141296073 1.000000000
#> 465  0.0024741916 0.0027937690 -0.517858690 1.000000000
#> 467  0.0007678526 0.0004232983  0.458931368 1.000000000
#> 468  0.0007678526 0.0014392144  0.824171302 1.000000000
#> 473  0.0004265848 0.0008465967  0.686741202 1.000000000
#> 475  0.0012797543 0.0015238740 -0.487927819 1.000000000
#> 476  0.0004265848 0.0004232983 -1.279848398 1.000000000
#> 481  0.0032420442 0.0031324077 -1.063183369 1.000000000
#> 483  0.0008531695 0.0008465967 -1.335826233 1.000000000
#> 484  0.0017916560 0.0011852354  0.665176261 1.000000000
#> 485  0.0007678526 0.0011852354  0.374912905 1.000000000
#> 486  0.0036686290 0.0044869624  0.299645992 1.000000000
#> 495  0.0013650712 0.0018625127  0.215740216 1.000000000
#> 502  0.0004265848 0.0010159160  1.577857965 1.000000000
#> 503  0.0017063390 0.0017778530 -1.135134972 1.000000000
#> 504  0.0023035577 0.0015238740  1.021262121 1.000000000
#> 506  0.0004265848 0.0008465967  0.772519119 1.000000000
#> 507  0.0026448255 0.0044023027  2.459252820 1.000000000
#> 511  0.0004265848 0.0007619370  0.503390543 1.000000000
#> 513  0.0006825356 0.0009312563 -0.171025801 1.000000000
#> 516  0.0005972187 0.0007619370 -0.482069807 1.000000000
#> 518  0.0019622899 0.0013545547  0.565066179 1.000000000
#> 519  0.0075078918 0.0045716221  3.795452016 1.000000000
#> 520  0.0012797543 0.0023704707  1.750286679 1.000000000
#> 521  0.0021329238 0.0027091094  0.216943463 1.000000000
#> 524  0.0021329238 0.0042329834  3.391525866 1.000000000
#> 525  0.0010238034 0.0019471724  1.721242113 1.000000000
#> 526  0.0017916560 0.0018625127 -1.104604468 1.000000000
#> 531  0.0008531695 0.0004232983  0.832312664 1.000000000
#> 539  0.0028154594 0.0041483237  1.466552706 1.000000000
#> 541  0.0004265848 0.0008465967  0.772280646 1.000000000
#> 542  0.0020476069 0.0016931934 -0.355800302 1.000000000
#> 543  0.0006825356 0.0006772773 -1.257156107 1.000000000
#> 544  0.0048630663 0.0050795801 -0.908613962 1.000000000
#> 558  0.0007678526 0.0006772773 -0.964160556 1.000000000
#> 560  0.0010238034 0.0007619370 -0.247533433 1.000000000
#> 562  0.0010238034 0.0008465967 -0.533841591 1.000000000
#> 563  0.0052896511 0.0055028784 -1.000254323 1.000000000
#> 564  0.0125415920 0.0155773789  2.024911662 1.000000000
#> 565  0.0137360293 0.0099051812  3.114194770 1.000000000
#> 566  0.0060575036 0.0050795801  0.437188305 1.000000000
#> 567  0.0246565993 0.0299695225  2.482363791 1.000000000
#> 568  0.0005119017 0.0008465967  0.308719945 1.000000000
#> 571  0.0008531695 0.0011005757 -0.288149173 1.000000000
#> 572  0.0011091204 0.0004232983  1.742993774 1.000000000
#> 574  0.0019622899 0.0006772773  3.311883833 1.000000000
#> 576  0.0005119017 0.0005926177 -1.027565486 1.000000000
#> 577  0.0059721867 0.0039790044  2.298910430 1.000000000
#> 578  0.0017063390 0.0021164917 -0.110843821 1.000000000
#> 579  0.0022182408 0.0044023027  3.834558398 1.000000000
#> 580  0.0011944373 0.0016931934  0.260895696 1.000000000
#> 583  0.0015357051 0.0030477481  2.769668993 1.000000000
#> 585  0.0137360293 0.0090585845  4.320542732 1.000000000
#> 586  0.0009384865 0.0018625127  1.708566123 1.000000000
#> 587  0.0025595086 0.0029630884 -0.340882049 1.000000000
#> 588  0.0007678526 0.0011852354  0.392212107 1.000000000
#> 589  0.0036686290 0.0031324077 -0.216390003 1.000000000
#> 592  0.0007678526 0.0008465967 -1.015601146 1.000000000
#> 593  0.0029007764 0.0037250254  0.586866039 1.000000000
#> 594  0.0007678526 0.0015238740  1.509675678 1.000000000
#> 595  0.0064840884 0.0063494751 -1.120022400 1.000000000
#> 596  0.0018769730 0.0013545547  0.344949240 1.000000000
#> 598  0.0038392629 0.0046562817  0.307673626 1.000000000
#> 601  0.0019622899 0.0022858110 -0.488758804 1.000000000
#> 602  0.0014503882 0.0013545547 -1.037994016 1.000000000
#> 603  0.0043511646 0.0078733491  4.876824298 1.000000000
#> 604  0.0139919802 0.0115983745  1.542554254 1.000000000
#> 605  0.0059721867 0.0077886895  1.521947325 1.000000000
#> 611  0.0017063390 0.0040636641  4.254367503 1.000000000
#> 624  0.0007678526 0.0011005757  0.145960697 1.000000000
#> 625  0.0018769730 0.0019471724 -1.112645251 1.000000000
#> 626  0.0026448255 0.0022011514 -0.252717217 1.000000000
#> 629  0.0049483832 0.0049102608 -1.219095481 1.000000000
#> 646  0.0004265848 0.0008465967  0.856775637 1.000000000
#> 647  0.0006825356 0.0007619370 -0.983562501 1.000000000
#> 661  0.0008531695 0.0004232983  0.853454562 1.000000000
#> 669  0.0008358098 0.0015658101  1.302160471 1.000000000
#> 702  0.0005572065 0.0013815971  1.799821934 1.000000000
#> 726  0.0004643388 0.0011052777  1.450304424 1.000000000
#> 731  0.0005572065 0.0031316202  5.694261697 1.000000000
#> 845  0.0010215453 0.0011973842 -0.669994164 1.000000000
#> 850  0.0016716196 0.0021184489 -0.102980969 1.000000000
#> 851  0.0009286776 0.0004605324  0.831134291 1.000000000
#> 853  0.0011144131 0.0009210648 -0.549622936 1.000000000
#> 856  0.0039004458 0.0011052777  5.700396437 1.000000000
#> 857  0.0009286776 0.0005526389  0.447900537 1.000000000
#> 858  0.0025074294 0.0011973842  2.332143127 1.000000000
#> 859  0.0008358098 0.0004605324  0.489554885 1.000000000
#> 864  0.0016716196 0.0005526389  2.896402377 1.000000000
#> 876  0.0048291233 0.0034079396  1.426885002 1.000000000
#> 878  0.0009286776 0.0008289583 -0.931956720 1.000000000
#> 879  0.0026002972 0.0017500230  0.940067883 1.000000000
#> 880  0.0006500743 0.0011052777  0.537191499 1.000000000
#> 881  0.0045505201 0.0042368979 -0.791814701 1.000000000
#> 899  0.0004643388 0.0004605324 -1.247366158 1.000000000
#> 942  0.0007429421 0.0004605324  0.019526760 1.000000000
#> 962  0.0010215453 0.0015658101  0.583970061 1.000000000
#> 974  0.0008358098 0.0008289583 -1.269828511 1.000000000
#> 1035 0.0008358098 0.0005526389  0.003982556 1.000000000
#> 1039 0.0012072808 0.0011052777 -0.949916372 1.000000000
#> 1065 0.0014858841 0.0007368518  1.419153711 1.000000000
#> 1067 0.0006500743 0.0004605324 -0.276296174 1.000000000
#> 1069 0.0010215453 0.0014737036  0.244117761 1.000000000
#> 1119 0.0011144131 0.0008289583 -0.205122860 1.000000000
#> 1123 0.0005572065 0.0011052777  0.971342189 1.000000000
#> 1124 0.0011144131 0.0006447453  0.583778286 1.000000000
#> 1126 0.0012072808 0.0008289583  0.126290147 1.000000000
#> 1130 0.0022288262 0.0005526389  3.714168728 1.000000000
#> 1134 0.0004643388 0.0007368518  0.020328646 1.000000000
#> 1136 0.0005572065 0.0004605324 -0.847118467 1.000000000
#> 1145 0.0007429421 0.0005526389 -0.354281589 1.000000000
#> 1149 0.0033432392 0.0030395137 -0.676005437 1.000000000
#> 1152 0.0012072808 0.0009210648 -0.297371590 1.000000000
#> 1154 0.0018573551 0.0032237266  2.006757391 1.000000000
#> 1165 0.0008358098 0.0004605324  0.404988681 1.000000000
#> 1170 0.0006500743 0.0009210648 -0.185552247 1.000000000
#> 1171 0.0006500743 0.0022105554  3.464194936 1.000000000
#> 1172 0.0009286776 0.0011973842 -0.342402550 1.000000000
#> 1175 0.0017644874 0.0030395137  1.761907908 1.000000000
#> 1187 0.0055720654 0.0032237266  3.095056910 1.000000000
#> 1188 0.0008358098 0.0011052777 -0.312137599 1.000000000
#> 1189 0.0011144131 0.0029474072  3.680589429 1.000000000
#> 1190 0.0008358098 0.0011052777 -0.263630879 1.000000000
#> 1193 0.0013001486 0.0030395137  3.092036549 1.000000000
#> 1194 0.0030646360 0.0016579166  2.193812984 1.000000000
#> 1195 0.0026002972 0.0021184489 -0.103058212 1.000000000
#> 1197 0.0013930163 0.0009210648  0.273670791 1.000000000
#> 1198 0.0005572065 0.0006447453 -0.921657789 1.000000000
#> 1200 0.0011144131 0.0009210648 -0.529308925 1.000000000
#> 1204 0.0009286776 0.0009210648 -1.273497485 1.000000000
#> 1206 0.0015787519 0.0009210648  0.885463076 1.000000000
#> 1208 0.0012072808 0.0009210648 -0.203915969 1.000000000
#> 1212 0.0034361070 0.0023947684  1.088984311 1.000000000
#> 1214 0.0006500743 0.0006447453 -1.297069426 1.000000000
#> 1215 0.0020430906 0.0016579166 -0.215762510 1.000000000
#> 1216 0.0013001486 0.0004605324  2.374934582 1.000000000
#> 1217 0.0036218425 0.0045132173  0.431521059 1.000000000
#> 1224 0.0011144131 0.0018421295  0.970445484 1.000000000
#> 1240 0.0039004458 0.0012894907  5.042842105 1.000000000
#> 1241 0.0058506686 0.0009210648  7.899928833 1.000000000
#> 1242 0.0014858841 0.0005526389  2.235846621 1.000000000
#> 1243 0.0006500743 0.0010131712  0.220219945 1.000000000
#> 1244 0.0095653789 0.0033158331  8.421957891 1.000000000
#> 1249 0.0011144131 0.0013815971 -0.402457377 1.000000000
#> 1252 0.0012072808 0.0008289583  0.174186655 1.000000000
#> 1255 0.0047362556 0.0009210648  7.197353006 1.000000000
#> 1256 0.0005572065 0.0007368518 -0.454742905 1.000000000
#> 1257 0.0024145617 0.0014737036  1.253485810 1.000000000
#> 1258 0.0014858841 0.0006447453  1.850155606 1.000000000
#> 1261 0.0013001486 0.0012894907 -1.298741898 1.000000000
#> 1262 0.0021359584 0.0011052777  1.816002354 1.000000000
#> 1263 0.0030646360 0.0010131712  4.085801939 1.000000000
#> 1281 0.0008358098 0.0023947684  3.442546020 1.000000000
#> 1283 0.0037147103 0.0025789813  0.984390288 1.000000000
#> 1291 0.0034361070 0.0013815971  4.116329436 1.000000000
#> 1292 0.0006500743 0.0008289583 -0.578659299 1.000000000
#> 1294 0.0009286776 0.0004605324  0.876196531 1.000000000
#> 1296 0.0006500743 0.0009210648 -0.104343895 1.000000000
#> 1299 0.0019502229 0.0009210648  2.013970484 1.000000000
#> 1300 0.0015787519 0.0009210648  1.018818268 1.000000000
#> 1303 0.0022288262 0.0018421295 -0.218923933 1.000000000
#> 1316 0.0008358098 0.0005526389 -0.001776514 1.000000000
#> 1332 0.0005572065 0.0005526389 -1.330270444 1.000000000
#> 1354 0.0014858841 0.0014737036 -1.327497643 1.000000000
#> 1360 0.0004643388 0.0011973842  1.815960026 1.000000000
#> 1372 0.0006500743 0.0007368518 -0.993447249 1.000000000
#> 1377 0.0013001486 0.0008289583  0.453772595 1.000000000
#> 1384 0.0023216939 0.0006447453  3.948266630 1.000000000
#> 1391 0.0007429421 0.0004605324  0.110096888 1.000000000
#> 1392 0.0028789004 0.0018421295  1.087497813 1.000000000
#> 1393 0.0059435364 0.0060790274 -1.103650258 1.000000000
#> 1394 0.0059435364 0.0049737497  0.247436536 1.000000000
#> 1395 0.0031575037 0.0018421295  1.888049234 1.000000000
#> 1396 0.0012072808 0.0013815971 -0.733601534 1.000000000
#> 1397 0.0100297177 0.0110527770 -0.184110461 1.000000000
#> 1408 0.0027860327 0.0018421295  0.936398293 1.000000000
#> 1409 0.0008358098 0.0005526389 -0.057552049 1.000000000
#> 1410 0.0012072808 0.0020263425  1.228731494 1.000000000
#> 1411 0.0005572065 0.0005526389 -1.235879446 1.000000000
#> 1415 0.0062221397 0.0040526849  2.306874056 1.000000000
#> 1416 0.0059435364 0.0037763655  2.309529749 1.000000000
#> 1418 0.0016716196 0.0005526389  2.785849349 1.000000000
#> 1419 0.0011144131 0.0007368518  0.213032346 1.000000000
#> 1420 0.0004643388 0.0005526389 -0.889005086 1.000000000
#> 1421 0.0013001486 0.0012894907 -1.325744277 1.000000000
#> 1425 0.0007429421 0.0014737036  1.298891162 1.000000000
#> 1427 0.0031575037 0.0022105554  0.923940718 1.000000000
#> 1430 0.0016716196 0.0022105554  0.170159961 1.000000000
#> 1433 0.0007429421 0.0009210648 -0.578792295 1.000000000
#> 1434 0.0085438336 0.0129870130  3.887688917 1.000000000
#> 1435 0.0009286776 0.0005526389  0.376802502 1.000000000
#> 1436 0.0023216939 0.0029474072  0.135342014 1.000000000
#> 1437 0.0057578009 0.0043290043  1.312491578 1.000000000
#> 1438 0.0028789004 0.0031316202 -0.786301903 1.000000000
#> 1439 0.0109583952 0.0172239108  5.330906451 1.000000000
#> 1484 0.0004643388 0.0007368518  0.062818227 1.000000000
#> 1520 0.0008358098 0.0006447453 -0.446606609 1.000000000
#> 1535 0.0009286776 0.0012894907  0.052323511 1.000000000
#> 1538 0.0004643388 0.0009210648  0.822622500 1.000000000
#> 1540 0.0010215453 0.0030395137  4.317693840 1.000000000
#> 1573 0.0006500743 0.0006447453 -1.318605475 1.000000000
#> 1574 0.0012072808 0.0012894907 -1.059332987 1.000000000
#> 1576 0.0004643388 0.0004605324 -1.290611571 1.000000000
#> 1578 0.0028789004 0.0026710878 -0.860001532 1.000000000
#> 1580 0.0004643388 0.0008289583  0.287435790 1.000000000
#> 1587 0.0013930163 0.0015658101 -0.722544681 1.000000000
#> 1593 0.0004643388 0.0008289583  0.427682966 1.000000000
#> 1610 0.0012072808 0.0026710878  2.682971316 1.000000000
#> 1618 0.0020430906 0.0018421295 -0.729752343 1.000000000
#> 1621 0.0008358098 0.0004605324  0.520309119 1.000000000
#> 1626 0.0004643388 0.0004605324 -1.247119028 1.000000000
#> 1627 0.0007429421 0.0004605324  0.044388735 1.000000000
#> 1630 0.0016716196 0.0014737036 -0.761611948 1.000000000
#> 1660 0.0004643388 0.0005526389 -0.906388047 1.000000000
#> 1684 0.0012072808 0.0005526389  1.506871174 1.000000000
#> 1694 0.0005572065 0.0004605324 -0.863384751 1.000000000
#> 1698 0.0006500743 0.0004605324 -0.297148474 1.000000000
#> 1709 0.0009286776 0.0008289583 -0.931319497 1.000000000
#> 1710 0.0011144131 0.0006447453  0.676428735 1.000000000
#> 1711 0.0005572065 0.0008289583 -0.035518485 1.000000000
#> 1713 0.0016716196 0.0018421295 -0.824330238 1.000000000
#> 1725 0.0013930163 0.0004605324  2.367413381 1.000000000
#> 1726 0.0007429421 0.0011052777  0.176587682 1.000000000
#> 1735 0.0004643388 0.0007368518 -0.058094186 1.000000000
#> 1739 0.0019502229 0.0023947684 -0.157073288 1.000000000
#> 1742 0.0016716196 0.0010131712  0.907685454 1.000000000
#> 1743 0.0005572065 0.0009210648  0.340554704 1.000000000
#> 1744 0.0014858841 0.0028553007  2.400041595 1.000000000
#> 1760 0.0007429421 0.0025789813  4.244573650 1.000000000
#> 1769 0.0005572065 0.0021184489  3.926665593 1.000000000
#> 1770 0.0006500743 0.0006447453 -1.235448088 1.000000000
#> 1773 0.0004643388 0.0029474072  5.389992503 1.000000000
#> 1805 0.0008358098 0.0010131712 -0.574154845 1.000000000
#> 1806 0.0018573551 0.0006447453  2.947810832 1.000000000
#> 1809 0.0019502229 0.0006447453  3.373427247 1.000000000
#> 1843 0.0007429421 0.0005526389 -0.422646099 1.000000000
#> 1847 0.0019502229 0.0011973842  0.867171890 1.000000000
#> 1850 0.0010215453 0.0010131712 -1.325060702 1.000000000
#> 1852 0.0026931649 0.0011973842  2.832505106 1.000000000
#> 1853 0.0021359584 0.0011973842  1.548634417 1.000000000
#> 1854 0.0010215453 0.0007368518 -0.218587770 1.000000000
#> 1855 0.0029717682 0.0014737036  2.546062679 1.000000000
#> 1859 0.0063150074 0.0007368518  9.730127339 1.000000000
#> 1861 0.0033432392 0.0005526389  6.150101150 1.000000000
#> 1865 0.0015787519 0.0008289583  1.393348692 1.000000000
#> 1866 0.0014858841 0.0007368518  1.369830382 1.000000000
#> 1867 0.0015787519 0.0005526389  2.558890694 1.000000000
#> 1869 0.0006500743 0.0004605324 -0.336235423 1.000000000
#> 1877 0.0010215453 0.0004605324  1.240131786 1.000000000
#> 1884 0.0082652303 0.0041447914  4.935316433 1.000000000
#> 1886 0.0013001486 0.0011052777 -0.637819770 1.000000000
#> 1887 0.0042719168 0.0017500230  4.425427061 1.000000000
#> 1888 0.0022288262 0.0007368518  3.381285345 1.000000000
#> 1889 0.0088224368 0.0074606245  0.619176970 1.000000000
#> 1903 0.0005572065 0.0007368518 -0.413634759 1.000000000
#> 1905 0.0009286776 0.0012894907  0.008519602 1.000000000
#> 1906 0.0005572065 0.0005526389 -1.355945349 1.000000000
#> 1907 0.0005572065 0.0011052777  1.008240548 1.000000000
#> 1910 0.0009286776 0.0008289583 -0.987355456 1.000000000
#> 1924 0.0008358098 0.0010131712 -0.578461694 1.000000000
#> 1929 0.0011144131 0.0011973842 -1.025243878 1.000000000
#> 1940 0.0007429421 0.0008289583 -0.937359758 1.000000000
#> 1947 0.0005572065 0.0006447453 -1.005692136 1.000000000
#> 1948 0.0018573551 0.0011973842  0.678413312 1.000000000
#> 1949 0.0033432392 0.0006447453  5.684920884 1.000000000
#> 1950 0.0013930163 0.0008289583  0.707106718 1.000000000
#> 1952 0.0047362556 0.0034079396  1.261714229 1.000000000
#> 1963 0.0014858841 0.0023947684  1.243458186 1.000000000
#> 1965 0.0007429421 0.0017500230  1.704465070 1.000000000
#> 1966 0.0007429421 0.0005526389 -0.383098079 1.000000000
#> 1969 0.0005572065 0.0011973842  1.365882018 1.000000000
#> 1970 0.0009286776 0.0012894907 -0.086124672 1.000000000
#> 1971 0.0008358098 0.0012894907  0.408973874 1.000000000
#> 1987 0.0004643388 0.0011973842  1.675637243 1.000000000
#> 1989 0.0012072808 0.0019342360  0.996899984 1.000000000
#> 1997 0.0028789004 0.0025789813 -0.630524917 1.000000000
#> 1998 0.0005572065 0.0008289583 -0.069611137 1.000000000
#> 2000 0.0004643388 0.0007368518  0.015947443 1.000000000
#> 2005 0.0005572065 0.0007368518 -0.416646909 1.000000000
#> 2006 0.0007429421 0.0011052777  0.181167934 1.000000000
#> 2010 0.0026931649 0.0023026619 -0.361172451 1.000000000
#> 2033 0.0006500743 0.0007368518 -1.002208847 1.000000000
#> 2036 0.0004643388 0.0006447453 -0.369675397 1.000000000
#> 2082 0.0004643388 0.0022105554  4.712046590 1.000000000
#> 2083 0.0012072808 0.0032237266  4.075681750 1.000000000
#> 2097 0.0016716196 0.0018421295 -0.814968727 1.000000000
#> 2098 0.0021359584 0.0018421295 -0.533796682 1.000000000
#> 2101 0.0037147103 0.0044211108  0.078323191 1.000000000
#> 2130 0.0004643388 0.0004605324 -1.314370584 1.000000000
#> 2145 0.0021359584 0.0014737036  0.548160206 1.000000000
#> 2146 0.0028789004 0.0028553007 -1.241386932 1.000000000
#> 2147 0.0056649331 0.0017500230  6.827714906 1.000000000
#> 2148 0.0022288262 0.0008289583  3.136917463 1.000000000
#> 2150 0.0073365527 0.0064474533  0.017023883 1.000000000
#> 2153 0.0005572065 0.0011052777  1.099610157 1.000000000
#> 2155 0.0004643388 0.0018421295  3.407405732 1.000000000
#> 2156 0.0008358098 0.0008289583 -1.211856568 1.000000000
#> 2158 0.0007429421 0.0010131712 -0.199498226 1.000000000
#> 2162 0.0004643388 0.0007368518  0.027496505 1.000000000
#> 2168 0.0014858841 0.0011973842 -0.342156741 1.000000000
#> 2169 0.0019502229 0.0008289583  2.337041504 1.000000000
#> 2171 0.0008358098 0.0005526389 -0.018128005 1.000000000
#> 2183 0.0015787519 0.0011973842  0.023441048 1.000000000
#> 2186 0.0012072808 0.0005526389  1.368574202 1.000000000
#> 2188 0.0020430906 0.0013815971  0.710755405 1.000000000
#> 2199 0.0007429421 0.0005526389 -0.425886071 1.000000000
#> 2203 0.0012072808 0.0009210648 -0.270679484 1.000000000
#> 2207 0.0005572065 0.0008289583 -0.080237529 1.000000000
#> 2209 0.0020430906 0.0018421295 -0.778749800 1.000000000
#> 2210 0.0016716196 0.0011052777  0.522984241 1.000000000
#> 2212 0.0020430906 0.0010131712  1.953179112 1.000000000
#> 2214 0.0006500743 0.0005526389 -0.909004327 1.000000000
#> 2216 0.0038075780 0.0008289583  6.514807181 1.000000000
#> 2217 0.0006500743 0.0006447453 -1.319308042 1.000000000
#> 2218 0.0018573551 0.0007368518  2.577467651 1.000000000
#> 2219 0.0013001486 0.0004605324  2.233665459 1.000000000
#> 2222 0.0010215453 0.0012894907 -0.320017942 1.000000000
#> 2224 0.0012072808 0.0004605324  1.852097615 1.000000000
#> 2238 0.0043647845 0.0039605784 -0.559691111 1.000000000
#> 2240 0.0006500743 0.0010131712  0.207898505 1.000000000
#> 2241 0.0023216939 0.0017500230  0.217105511 1.000000000
#> 2242 0.0013001486 0.0008289583  0.482152227 1.000000000
#> 2243 0.0047362556 0.0051579626 -0.553545629 1.000000000
#> 2292 0.0012072808 0.0005526389  1.362127245 1.000000000
#> 2296 0.0025074294 0.0009210648  3.390088916 1.000000000
#> 2323 0.0013001486 0.0004605324  2.268194822 1.000000000
#> 2330 0.0016716196 0.0006447453  2.437530067 1.000000000
#> 2333 0.0004643388 0.0006447453 -0.427447553 1.000000000
#> 2338 0.0004643388 0.0005526389 -0.887076881 1.000000000
#> 2341 0.0013930163 0.0019342360  0.349104565 1.000000000
#> 2355 0.0006500743 0.0004605324 -0.323385480 1.000000000
#> 2408 0.0007429421 0.0006447453 -0.901619836 1.000000000
#> 2412 0.0007429421 0.0012894907  0.714659526 1.000000000
#> 2422 0.0009286776 0.0004605324  0.872089253 1.000000000
#> 2436 0.0008358098 0.0009210648 -0.995183312 1.000000000
#> 2441 0.0010215453 0.0018421295  1.408830067 1.000000000
#> 2461 0.0005572065 0.0004605324 -0.871523119 1.000000000
#> 2510 0.0011144131 0.0014737036 -0.103633348 1.000000000
#> 2515 0.0006500743 0.0021184489  3.359973973 1.000000000
#> 2558 0.0010215453 0.0004605324  1.196913801 1.000000000
#> 2565 0.0007429421 0.0005526389 -0.344816994 1.000000000
#> 2566 0.0012072808 0.0007368518  0.524987903 1.000000000
#> 2567 0.0019502229 0.0009210648  2.042809580 1.000000000
#> 2570 0.0025074294 0.0021184489 -0.307701640 1.000000000
#> 2575 0.0004643388 0.0007368518 -0.043768251 1.000000000
#> 2581 0.0012072808 0.0011052777 -0.977399380 1.000000000
#> 2587 0.0004643388 0.0008289583  0.392596037 1.000000000
#> 2588 0.0008358098 0.0011052777 -0.274312323 1.000000000
#> 2603 0.0013001486 0.0018421295  0.303123712 1.000000000
#> 2609 0.0006500743 0.0008289583 -0.482975300 1.000000000
#> 2621 0.0007429421 0.0005526389 -0.369873305 1.000000000
#> 2656 0.0004643388 0.0010131712  1.140387524 1.000000000
#> 2660 0.0011144131 0.0018421295  0.869527948 1.000000000
#> 2669 0.0006500743 0.0008289583 -0.508897995 1.000000000
#> 2682 0.0017644874 0.0013815971 -0.103961335 1.000000000
#> 2685 0.0008358098 0.0005526389 -0.020401288 1.000000000
#> 2687 0.0015787519 0.0021184489  0.248741344 1.000000000
#> 2722 0.0013930163 0.0010131712  0.005686050 1.000000000
#> 2754 0.0005572065 0.0007368518 -0.462209158 1.000000000
#> 2755 0.0012072808 0.0010131712 -0.591318363 1.000000000
#> 2756 0.0006500743 0.0008289583 -0.490247566 1.000000000
#> 2759 0.0016716196 0.0023026619  0.291049057 1.000000000
#> 2763 0.0015787519 0.0031316202  2.648843355 1.000000000
#> 2765 0.0032503715 0.0042368979  0.639206213 1.000000000
#> 2766 0.0027860327 0.0021184489  0.300289788 1.000000000
#> 2767 0.0011144131 0.0012894907 -0.799520721 1.000000000
#> 2768 0.0033432392 0.0037763655 -0.449595160 1.000000000
#> 2772 0.0052005944 0.0019342360  5.352669735 1.000000000
#> 2773 0.0007429421 0.0010131712 -0.196838231 1.000000000
#> 2774 0.0030646360 0.0022105554  0.746664189 1.000000000
#> 2775 0.0023216939 0.0010131712  2.683139592 1.000000000
#> 2778 0.0013930163 0.0020263425  0.532537508 1.000000000
#> 2780 0.0016716196 0.0019342360 -0.592165734 1.000000000
#> 2781 0.0017644874 0.0012894907  0.168115042 1.000000000
#> 2783 0.0007429421 0.0005526389 -0.389210496 1.000000000
#> 2784 0.0004643388 0.0007368518  0.050540353 1.000000000
#> 2788 0.0004643388 0.0006447453 -0.356764518 1.000000000
#> 2790 0.0007429421 0.0021184489  3.047381138 1.000000000
#> 2793 0.0006500743 0.0010131712  0.266387861 1.000000000
#> 2796 0.0005572065 0.0007368518 -0.545554254 1.000000000
#> 2797 0.0058506686 0.0086580087  2.394801628 1.000000000
#> 2798 0.0006500743 0.0005526389 -0.906142895 1.000000000
#> 2799 0.0017644874 0.0023026619  0.038277586 1.000000000
#> 2800 0.0048291233 0.0033158331  1.571566166 1.000000000
#> 2801 0.0023216939 0.0026710878 -0.511647781 1.000000000
#> 2802 0.0090081724 0.0117896288  1.893822026 1.000000000
#> 2848 0.0007429421 0.0010131712 -0.218071987 1.000000000
#> 2852 0.0027860327 0.0018421295  1.125168086 1.000000000
#> 2859 0.0005572065 0.0004605324 -0.844657986 1.000000000
#> 2863 0.0011144131 0.0008289583 -0.166141838 1.000000000
#> 2869 0.0004643388 0.0006447453 -0.349404168 1.000000000
#> 2871 0.0005572065 0.0008289583 -0.098028536 1.000000000
#> 2888 0.0006500743 0.0006447453 -1.290290861 1.000000000
#> 2895 0.0070579495 0.0042368979  3.408968223 1.000000000
#> 2896 0.0007429421 0.0013815971  1.075809767 1.000000000
#> 2897 0.0009286776 0.0005526389  0.371546204 1.000000000
#> 2898 0.0020430906 0.0011973842  1.188142694 1.000000000
#> 2900 0.0018573551 0.0012894907  0.329111196 1.000000000
#> 2903 0.0014858841 0.0007368518  1.425658961 1.000000000
#> 2904 0.0026002972 0.0020263425  0.207796761 1.000000000
#> 2905 0.0034361070 0.0016579166  2.912171128 1.000000000
#> 2907 0.0006500743 0.0006447453 -1.241861434 1.000000000
#> 2908 0.0043647845 0.0033158331  0.714171155 1.000000000
#> 2927 0.0008358098 0.0010131712 -0.602263081 1.000000000
#> 2928 0.0007429421 0.0009210648 -0.592089580 1.000000000
#> 2942 0.0013001486 0.0007368518  0.888688454 1.000000000
#> 2947 0.0013001486 0.0013815971 -1.104165632 1.000000000
#> 2959 0.0006500743 0.0004605324 -0.273019447 1.000000000
#> 2960 0.0006500743 0.0006447453 -1.291230210 1.000000000
#> 2963 0.0008358098 0.0018421295  2.172689303 1.000000000
#> 2973 0.0020430906 0.0011052777  1.626077449 1.000000000
#> 2974 0.0004643388 0.0007368518  0.004134909 1.000000000
#> 2975 0.0012072808 0.0018421295  0.694414569 1.000000000
#> 2976 0.0012072808 0.0004605324  1.847586998 1.000000000
#> 2979 0.0006500743 0.0008289583 -0.563395975 1.000000000
#> 2980 0.0004643388 0.0006447453 -0.358425384 1.000000000
#> 2981 0.0005572065 0.0004605324 -0.867170567 1.000000000
#> 2993 0.0006500743 0.0012894907  1.124465786 1.000000000
#> 2996 0.0007429421 0.0005526389 -0.423684892 1.000000000
#> 2998 0.0017644874 0.0017500230 -1.258092044 1.000000000
#> 3006 0.0010215453 0.0008289583 -0.500401413 1.000000000
#> 3014 0.0016716196 0.0027631943  1.582747297 1.000000000
#> 3015 0.0035289747 0.0069079856  4.293325160 1.000000000
#> 3016 0.0043647845 0.0036842590 -0.048572840 1.000000000
#> 3017 0.0014858841 0.0022105554  0.672381851 1.000000000
#> 3018 0.0007429421 0.0024868748  4.073884044 1.000000000
#> 3019 0.0091939079 0.0123422677  2.500121683 1.000000000
#> 3028 0.0004643388 0.0005526389 -1.026966121 1.000000000
#> 3029 0.0016716196 0.0019342360 -0.522504658 1.000000000
#> 3031 0.0006500743 0.0013815971  1.572267495 1.000000000
#> 3035 0.0006500743 0.0007368518 -0.945798694 1.000000000
#> 3036 0.0038075780 0.0030395137  0.325404097 1.000000000
#> 3037 0.0052005944 0.0034079396  2.025463160 1.000000000
#> 3038 0.0005572065 0.0006447453 -0.982485135 1.000000000
#> 3039 0.0020430906 0.0009210648  2.178958210 1.000000000
#> 3040 0.0010215453 0.0017500230  1.076943083 1.000000000
#> 3042 0.0013001486 0.0013815971 -1.060011623 1.000000000
#> 3045 0.0013001486 0.0015658101 -0.441590927 1.000000000
#> 3047 0.0018573551 0.0022105554 -0.371424556 1.000000000
#> 3048 0.0006500743 0.0006447453 -1.246808368 1.000000000
#> 3050 0.0013930163 0.0020263425  0.566534382 1.000000000
#> 3053 0.0006500743 0.0006447453 -1.267449869 1.000000000
#> 3054 0.0072436850 0.0118817353  4.497600044 1.000000000
#> 3056 0.0008358098 0.0037763655  6.735239573 1.000000000
#> 3057 0.0046433878 0.0049737497 -0.751570597 1.000000000
#> 3058 0.0018573551 0.0028553007  1.197072061 1.000000000
#> 3059 0.0077080238 0.0179607626  6.215154174 1.000000000
#> 3078 0.0005572065 0.0005526389 -1.285963592 1.000000000
#> 3081 0.0005572065 0.0015658101  2.482166782 1.000000000
#> 3134 0.0009286776 0.0007368518 -0.506143543 1.000000000
#> 3153 0.0010215453 0.0011973842 -0.653483894 1.000000000
#> 3154 0.0004643388 0.0005526389 -0.914299024 1.000000000
#> 3157 0.0020430906 0.0021184489 -1.121106962 1.000000000
#> 3265 0.0005051015 0.0007022472 -0.382764130 1.000000000
#> 3575 0.0007071421 0.0005016051 -0.336986506 1.000000000
#> 3610 0.0014142843 0.0005016051  2.264200013 1.000000000
#> 3675 0.0005051015 0.0011035313  1.134586611 1.000000000
#> 3676 0.0010102031 0.0005016051  0.802846998 1.000000000
#> 3679 0.0016163249 0.0011035313  0.302115944 1.000000000
#> 3688 0.0008081624 0.0012038523  0.060570977 1.000000000
#> 3702 0.0015153046 0.0014044944 -0.992670643 1.000000000
#> 3704 0.0010102031 0.0007022472 -0.098232411 1.000000000
#> 3706 0.0015153046 0.0010032103  0.318955420 1.000000000
#> 4010 0.0006061218 0.0006019262 -1.243806113 1.000000000
#> 4250 0.0013132640 0.0015048154 -0.711861566 1.000000000
#> 4272 0.0009091827 0.0012038523 -0.320562210 1.000000000
#> 4277 0.0005051015 0.0010032103  0.791536069 1.000000000
#> 4340 0.0005051015 0.0007022472 -0.299275829 1.000000000
#> 4344 0.0006061218 0.0015048154  2.022113548 1.000000000
#> 4377 0.0008081624 0.0007022472 -0.915306224 1.000000000
#> 4381 0.0022224467 0.0014044944  0.964028036 1.000000000
#> 4389 0.0006061218 0.0010032103  0.326005751 1.000000000
#> 4395 0.0005051015 0.0005016051 -1.307155190 1.000000000
#> 4411 0.0009091827 0.0012038523 -0.252372537 1.000000000
#> 4417 0.0014142843 0.0008025682  0.925376430 1.000000000
#> 4428 0.0011112234 0.0011035313 -1.253870451 1.000000000
#> 4496 0.0014142843 0.0009028892  0.401120884 1.000000000
#> 4520 0.0011112234 0.0013041734 -0.674132630 1.000000000
#> 4525 0.0010102031 0.0016051364  0.646008959 1.000000000
#> 4589 0.0007071421 0.0005016051 -0.319843095 1.000000000
#> 4612 0.0030306092 0.0009028892  4.294767224 1.000000000
#> 4617 0.0030306092 0.0016051364  2.245519213 1.000000000
#> 4695 0.0012122437 0.0007022472  0.509773663 1.000000000
#> 4740 0.0005051015 0.0005016051 -1.264755000 1.000000000
#> 4753 0.0013132640 0.0018057785  0.124052980 1.000000000
#> 4777 0.0010102031 0.0009028892 -0.902574522 1.000000000
#> 4835 0.0006061218 0.0010032103  0.364365084 1.000000000
#> 4954 0.0005051015 0.0006019262 -0.930800340 1.000000000
#> 5049 0.0007071421 0.0005016051 -0.273229602 1.000000000
#> 5053 0.0009091827 0.0006019262 -0.021696459 1.000000000
#> 5058 0.0015153046 0.0018057785 -0.493317627 1.000000000
#> 5059 0.0014142843 0.0008025682  0.755863343 1.000000000
#> 5060 0.0005051015 0.0006019262 -0.918571139 1.000000000
#> 5061 0.0018183655 0.0012038523  0.546550699 1.000000000
#> 5065 0.0018183655 0.0011035313  0.856513485 1.000000000
#> 5067 0.0015153046 0.0011035313  0.016527783 1.000000000
#> 5068 0.0010102031 0.0005016051  0.829989428 1.000000000
#> 5071 0.0007071421 0.0008025682 -0.949707013 1.000000000
#> 5073 0.0008081624 0.0005016051  0.040299232 1.000000000
#> 5074 0.0011112234 0.0005016051  1.267864883 1.000000000
#> 5086 0.0026265279 0.0036115570  0.467579942 1.000000000
#> 5088 0.0006061218 0.0013041734  1.244574077 1.000000000
#> 5089 0.0021214264 0.0009028892  2.263946350 1.000000000
#> 5090 0.0008081624 0.0009028892 -0.949957395 1.000000000
#> 5091 0.0034346904 0.0043138042  0.302007383 1.000000000
#> 5123 0.0010102031 0.0007022472 -0.079533986 1.000000000
#> 5152 0.0034346904 0.0021067416  1.690634498 1.000000000
#> 5157 0.0008081624 0.0006019262 -0.425403160 1.000000000
#> 5159 0.0010102031 0.0007022472 -0.063654479 1.000000000
#> 5160 0.0018183655 0.0005016051  3.215669290 1.000000000
#> 5163 0.0013132640 0.0015048154 -0.724338730 1.000000000
#> 5235 0.0007071421 0.0009028892 -0.492726056 1.000000000
#> 5244 0.0008081624 0.0014044944  0.778154661 1.000000000
#> 5245 0.0014142843 0.0031099518  2.823222877 1.000000000
#> 5246 0.0016163249 0.0017054575 -1.128014277 1.000000000
#> 5247 0.0007071421 0.0008025682 -0.978429990 1.000000000
#> 5249 0.0036367310 0.0047150883  0.648232861 1.000000000
#> 5258 0.0008081624 0.0005016051  0.087464924 1.000000000
#> 5265 0.0016163249 0.0011035313  0.305451786 1.000000000
#> 5266 0.0019193858 0.0016051364 -0.474897633 1.000000000
#> 5269 0.0008081624 0.0007022472 -0.933008241 1.000000000
#> 5273 0.0007071421 0.0007022472 -1.237408668 1.000000000
#> 5275 0.0008081624 0.0007022472 -0.877621381 1.000000000
#> 5277 0.0007071421 0.0008025682 -0.960309753 1.000000000
#> 5281 0.0037377513 0.0042134831 -0.490457111 1.000000000
#> 5284 0.0021214264 0.0020064205 -1.014425794 1.000000000
#> 5285 0.0010102031 0.0011035313 -1.088734636 1.000000000
#> 5286 0.0025255076 0.0073234350  6.636678297 1.000000000
#> 5567 0.0009091827 0.0011035313 -0.598431950 1.000000000
#> 5572 0.0009091827 0.0011035313 -0.564979403 1.000000000
#> 5601 0.0006061218 0.0006019262 -1.271246392 1.000000000
#> 5680 0.0008081624 0.0013041734  0.508463198 1.000000000
#> 5919 0.0006061218 0.0008025682 -0.433493160 1.000000000
#> 6018 0.0006061218 0.0009028892 -0.027017537 1.000000000
#> 6253 0.0009091827 0.0005016051  0.457935969 1.000000000
#> 6278 0.0005051015 0.0005016051 -1.316876350 1.000000000
#> 6297 0.0008081624 0.0009028892 -0.976589829 1.000000000
#> 6341 0.0010102031 0.0005016051  0.891104947 1.000000000
#> 6397 0.0020204061 0.0013041734  0.723666265 1.000000000
#> 6398 0.0017173452 0.0012038523  0.248266303 1.000000000
#> 6401 0.0026265279 0.0012038523  2.474258679 1.000000000
#> 6414 0.0019193858 0.0006019262  3.074985098 1.000000000
#> 6429 0.0024244873 0.0024077047 -1.301434683 1.000000000
#> 6432 0.0020204061 0.0007022472  2.935050944 1.000000000
#> 6433 0.0007071421 0.0011035313  0.185185617 1.000000000
#> 6434 0.0030306092 0.0030096308 -1.299710639 1.000000000
#> 6634 0.0017173452 0.0011035313  0.637100084 1.000000000
#> 6639 0.0016163249 0.0018057785 -0.796143150 1.000000000
#> 6748 0.0008081624 0.0007022472 -0.892784960 1.000000000
#> 6768 0.0008081624 0.0005016051  0.005217466 1.000000000
#> 6771 0.0009091827 0.0009028892 -1.326270373 1.000000000
#> 6819 0.0008081624 0.0006019262 -0.379168821 1.000000000
#> 6822 0.0009091827 0.0010032103 -1.028194226 1.000000000
#> 6973 0.0005051015 0.0011035313  1.155956003 1.000000000
#> 7016 0.0007071421 0.0007022472 -1.285737276 1.000000000
#> 7033 0.0011112234 0.0010032103 -0.952827794 1.000000000
#> 7037 0.0016163249 0.0018057785 -0.746690084 1.000000000
#> 7141 0.0008081624 0.0005016051  0.087320598 1.000000000
#> 7146 0.0012122437 0.0009028892 -0.283885583 1.000000000
#> 7152 0.0020204061 0.0006019262  3.501489360 1.000000000
#> 7154 0.0014142843 0.0005016051  2.131615917 1.000000000
#> 7169 0.0022224467 0.0020064205 -0.798108580 1.000000000
#> 7172 0.0012122437 0.0012038523 -1.265747618 1.000000000
#> 7174 0.0021214264 0.0025080257 -0.386393260 1.000000000
#> 7236 0.0008081624 0.0009028892 -0.922934702 1.000000000
#> 7313 0.0005051015 0.0006019262 -0.920896147 1.000000000
#> 7396 0.0009091827 0.0005016051  0.526564546 1.000000000
#> 7408 0.0011112234 0.0006019262  0.783749008 1.000000000
#> 7479 0.0009091827 0.0011035313 -0.649226679 1.000000000
#> 7480 0.0008081624 0.0005016051  0.072126000 1.000000000
#> 7483 0.0016163249 0.0014044944 -0.676379005 1.000000000
#> 7494 0.0006061218 0.0007022472 -0.939903295 1.000000000
#> 7495 0.0010102031 0.0006019262  0.337580266 1.000000000
#> 7508 0.0013132640 0.0012038523 -1.030507913 1.000000000
#> 7509 0.0006061218 0.0005016051 -0.911807334 1.000000000
#> 7512 0.0016163249 0.0025080257  1.010895702 1.000000000
#> 7614 0.0008081624 0.0008025682 -1.278905882 1.000000000
#> 7723 0.0005051015 0.0006019262 -0.892342003 1.000000000
#> 8195 0.0006061218 0.0005016051 -0.881018283 1.000000000
#> 8200 0.0007071421 0.0009028892 -0.447478408 1.000000000
#> 8230 0.0005051015 0.0005016051 -1.251158391 1.000000000
#> 8479 0.0008081624 0.0005016051  0.045581209 1.000000000
#> 8492 0.0005051015 0.0006019262 -0.880992851 1.000000000
#> 8496 0.0009091827 0.0008025682 -0.924692970 1.000000000
#> 8643 0.0009091827 0.0009028892 -1.388697525 1.000000000
#> 8648 0.0005051015 0.0011035313  1.135003558 1.000000000
#> 8660 0.0008081624 0.0012038523  0.129795532 1.000000000
#> 8670 0.0016163249 0.0010032103  0.673390085 1.000000000
#> 8676 0.0013132640 0.0005016051  1.807191088 1.000000000
#> 8677 0.0005051015 0.0007022472 -0.393112960 1.000000000
#> 8691 0.0009091827 0.0008025682 -0.895445299 1.000000000
#> 8694 0.0007071421 0.0009028892 -0.501185870 1.000000000
#> 8696 0.0010102031 0.0018057785  1.258843778 1.000000000
#> 8711 0.0028285685 0.0007022472  4.294623473 1.000000000
#> 8738 0.0012122437 0.0010032103 -0.629240222 1.000000000
#> 8745 0.0010102031 0.0005016051  0.883676371 1.000000000
#> 8788 0.0005051015 0.0005016051 -1.254018060 1.000000000
#> 8815 0.0006061218 0.0005016051 -0.866268679 1.000000000
#> 8816 0.0015153046 0.0012038523 -0.359445277 1.000000000
#> 8817 0.0011112234 0.0019060995  1.063304519 1.000000000
#> 8818 0.0006061218 0.0007022472 -0.945198216 1.000000000
#> 8820 0.0018183655 0.0032102729  1.700643971 1.000000000
#> 8830 0.0009091827 0.0006019262 -0.127762040 1.000000000
#> 8835 0.0018183655 0.0006019262  2.792551588 1.000000000
#> 8836 0.0013132640 0.0015048154 -0.781173772 1.000000000
#> 8847 0.0011112234 0.0006019262  0.720614463 1.000000000
#> 8850 0.0006061218 0.0012038523  1.007324580 1.000000000
#> 8853 0.0020204061 0.0040128411  2.839086232 1.000000000
#> 8855 0.0007071421 0.0009028892 -0.499661988 1.000000000
#> 8856 0.0013132640 0.0012038523 -0.993356551 1.000000000
#> 8857 0.0012122437 0.0006019262  1.110304948 1.000000000
#> 8858 0.0030306092 0.0048154093  2.032299230 1.000000000
#> 8977 0.0015153046 0.0008025682  1.197474759 1.000000000
#> 9085 0.0007071421 0.0008025682 -1.005756386 1.000000000
#> 9086 0.0006061218 0.0011035313  0.724098580 1.000000000
#> 9087 0.0020204061 0.0005016051  3.729668592 1.000000000
#> 9090 0.0024244873 0.0015048154  1.106040716 1.000000000
#> 9118 0.0005051015 0.0005016051 -1.242015914 1.000000000
#> 9132 0.0005051015 0.0005016051 -1.277113143 1.000000000
#> 9134 0.0007071421 0.0005016051 -0.364165757 1.000000000
#> 9160 0.0009091827 0.0011035313 -0.584467028 1.000000000
#> 9164 0.0005051015 0.0005016051 -1.244554640 1.000000000
#> 9165 0.0019193858 0.0011035313  1.173852268 1.000000000
#> 9338 0.0006061218 0.0006019262 -1.265373252 1.000000000
#> 9412 0.0006061218 0.0008025682 -0.467298841 1.000000000
#> 9416 0.0006061218 0.0007022472 -0.949023315 1.000000000
#> 9459 0.0009091827 0.0012038523 -0.300306358 1.000000000
#> 9461 0.0009091827 0.0015048154  0.631724583 1.000000000
#> 9462 0.0006061218 0.0010032103  0.282101701 1.000000000
#> 9464 0.0008081624 0.0022070626  2.778530165 1.000000000
#> 9468 0.0020204061 0.0008025682  2.643216103 1.000000000
#> 9470 0.0008081624 0.0010032103 -0.554352616 1.000000000
#> 9474 0.0005051015 0.0009028892  0.446507818 1.000000000
#> 9490 0.0020204061 0.0034109149  1.930388380 1.000000000
#> 9492 0.0011112234 0.0008025682 -0.228083429 1.000000000
#> 9493 0.0018183655 0.0010032103  1.253637287 1.000000000
#> 9494 0.0011112234 0.0012038523 -1.006326828 1.000000000
#> 9495 0.0028285685 0.0045144462  1.926653527 1.000000000
#> 9527 0.0010102031 0.0011035313 -1.012739094 1.000000000
#> 9556 0.0015153046 0.0012038523 -0.350274672 1.000000000
#> 9559 0.0006061218 0.0009028892 -0.042608185 1.000000000
#> 9564 0.0010102031 0.0006019262  0.385592407 1.000000000
#> 9565 0.0009091827 0.0007022472 -0.457331952 1.000000000
#> 9568 0.0020204061 0.0014044944  0.466309420 1.000000000
#> 9605 0.0005051015 0.0006019262 -0.887751145 1.000000000
#> 9612 0.0005051015 0.0007022472 -0.380298333 1.000000000
#> 9614 0.0006061218 0.0006019262 -1.263446641 1.000000000
#> 9630 0.0007071421 0.0007022472 -1.216658172 1.000000000
#> 9643 0.0009091827 0.0028089888  3.686766537 1.000000000
#> 9644 0.0012122437 0.0013041734 -1.040547561 1.000000000
#> 9647 0.0030306092 0.0052166934  2.564435344 1.000000000
#> 9655 0.0005051015 0.0010032103  0.860294180 1.000000000
#> 9662 0.0008081624 0.0014044944  0.839792167 1.000000000
#> 9663 0.0017173452 0.0014044944 -0.438398830 1.000000000
#> 9665 0.0006061218 0.0005016051 -0.875053576 1.000000000
#> 9668 0.0010102031 0.0007022472 -0.085822821 1.000000000
#> 9673 0.0006061218 0.0008025682 -0.398823172 1.000000000
#> 9675 0.0005051015 0.0007022472 -0.336808386 1.000000000
#> 9678 0.0018183655 0.0056179775  6.064323727 1.000000000
#> 9681 0.0019193858 0.0020064205 -1.128520166 1.000000000
#> 9682 0.0006061218 0.0013041734  1.369015131 1.000000000
#> 9683 0.0031316295 0.0076243981  3.614988845 1.000000000
#> 9890 0.0011112234 0.0013041734 -0.685925259 1.000000000
```

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
#> 33                                       cohesion->monitor        16       40
#> 34                                      consensus->monitor       120      175
#> 35                                  coregulate->coregulate        12       34
#> 36                                       emotion->cohesion       505      418
#> 38                                           plan->discuss       174      244
#> 39                                          cohesion->plan       142       97
#> 40                                      synthesis->discuss         8       33
#> 41                                        monitor->discuss       234      304
#> 43                                        discuss->discuss       339      431
#> 45                                          adapt->emotion        20       41
#> 47                            adapt->consensus->coregulate        14       42
#> 48                                  adapt->consensus->plan        21       61
#> 49                            cohesion->consensus->discuss       129       44
#> 50                          consensus->cohesion->consensus        33       10
#> 51                            consensus->discuss->cohesion        39        7
#> 52                           consensus->discuss->consensus       291       87
#> 53                             consensus->discuss->discuss       140       79
#> 54                             consensus->discuss->emotion        92       33
#> 55                                consensus->plan->emotion       202      139
#> 56                                   consensus->plan->plan       382      495
#> 57                              coregulate->discuss->adapt         6       32
#> 58                            coregulate->discuss->discuss        27       65
#> 59                          coregulate->discuss->synthesis        20       63
#> 60                                discuss->adapt->cohesion        12       58
#> 61                               discuss->adapt->consensus        26       96
#> 63                                 discuss->adapt->emotion         6       25
#> 64                            discuss->cohesion->consensus        64       30
#> 65                             discuss->consensus->discuss       198       41
#> 66                                discuss->consensus->plan       302      181
#> 67                                 discuss->discuss->adapt         6       49
#> 68                             discuss->discuss->consensus       144       86
#> 69                               discuss->synthesis->adapt        29       89
#> 71                                discuss->synthesis->plan        28        9
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
#> 88                                        discuss->monitor        33       55
#> 89                                                 discuss      2166     2101
#> 93                                       cohesion->emotion       111       85
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
#> 130                             adapt->cohesion->consensus        22       49
#> 139                              adapt->consensus->discuss        18       17
#> 161                              adapt->emotion->consensus        10        8
#> 188                         cohesion->consensus->consensus        44       27
#> 189                        cohesion->consensus->coregulate        67       69
#> 190                           cohesion->consensus->emotion        46       22
#> 191                           cohesion->consensus->monitor        11       13
#> 192                              cohesion->consensus->plan       169      135
#> 196                        cohesion->coregulate->consensus         7       21
#> 198                          cohesion->coregulate->discuss        15       31
#> 199                          cohesion->coregulate->emotion        15       20
#> 201                             cohesion->coregulate->plan        20       26
#> 205                           cohesion->discuss->consensus        15       12
#> 209                           cohesion->discuss->synthesis         8        5
#> 211                            cohesion->emotion->cohesion        30       27
#> 212                           cohesion->emotion->consensus        36       18
#> 214                             cohesion->emotion->discuss        13        9
#> 215                             cohesion->emotion->emotion         5        6
#> 217                                cohesion->emotion->plan        10       10
#> 222                             cohesion->monitor->discuss        10       17
#> 226                              cohesion->plan->consensus        36       31
#> 229                                cohesion->plan->emotion        28        9
#> 230                                cohesion->plan->monitor         8        5
#> 231                                   cohesion->plan->plan        43       36
#> 237                             consensus->adapt->cohesion         6        7
#> 238                            consensus->adapt->consensus         5        8
#> 240                        consensus->cohesion->coregulate         8        5
#> 247                        consensus->consensus->consensus        27       19
#> 248                       consensus->consensus->coregulate        41       40
#> 249                          consensus->consensus->discuss        56       26
#> 250                          consensus->consensus->emotion        25       17
#> 251                          consensus->consensus->monitor        15       11
#> 252                             consensus->consensus->plan        82      105
#> 254                           consensus->coregulate->adapt        15        8
#> 255                        consensus->coregulate->cohesion        19       19
#> 256                       consensus->coregulate->consensus        53       90
#> 257                      consensus->coregulate->coregulate        11       24
#> 258                         consensus->coregulate->discuss       135      166
#> 259                         consensus->coregulate->emotion       110       85
#> 260                         consensus->coregulate->monitor        52       41
#> 261                            consensus->coregulate->plan       141      116
#> 262                       consensus->coregulate->synthesis         7        8
#> 263                              consensus->discuss->adapt        22       43
#> 264                         consensus->discuss->coregulate        56       35
#> 265                            consensus->discuss->monitor        11        8
#> 266                               consensus->discuss->plan         7        5
#> 267                          consensus->discuss->synthesis        72       76
#> 269                           consensus->emotion->cohesion        72       52
#> 270                          consensus->emotion->consensus        86       52
#> 272                            consensus->emotion->discuss        33       17
#> 273                            consensus->emotion->emotion        18       16
#> 274                            consensus->emotion->monitor        13        9
#> 275                               consensus->emotion->plan        19       19
#> 278                           consensus->monitor->cohesion         6       12
#> 279                          consensus->monitor->consensus        17       23
#> 280                         consensus->monitor->coregulate         5        6
#> 281                            consensus->monitor->discuss        40       70
#> 282                            consensus->monitor->emotion         9       14
#> 284                               consensus->monitor->plan        30       27
#> 287                              consensus->plan->cohesion        37       18
#> 288                             consensus->plan->consensus       352      334
#> 289                            consensus->plan->coregulate        29       14
#> 290                               consensus->plan->discuss        67       91
#> 291                               consensus->plan->monitor        86       86
#> 295                        consensus->synthesis->consensus         9        5
#> 301                           coregulate->adapt->consensus         8        6
#> 307                        coregulate->cohesion->consensus        16       15
#> 312                             coregulate->cohesion->plan         5        5
#> 314                       coregulate->consensus->consensus        10       20
#> 315                      coregulate->consensus->coregulate        14       36
#> 316                         coregulate->consensus->discuss        16       20
#> 317                         coregulate->consensus->emotion        14        9
#> 319                            coregulate->consensus->plan        34       67
#> 329                          coregulate->discuss->cohesion         9        8
#> 330                         coregulate->discuss->consensus        86       68
#> 331                        coregulate->discuss->coregulate        16       27
#> 332                           coregulate->discuss->emotion        16       29
#> 333                           coregulate->discuss->monitor         5        6
#> 336                          coregulate->emotion->cohesion        56       44
#> 337                         coregulate->emotion->consensus        45       34
#> 339                           coregulate->emotion->discuss        25       18
#> 340                           coregulate->emotion->emotion        11       13
#> 342                              coregulate->emotion->plan        19       18
#> 346                         coregulate->monitor->consensus        14       16
#> 348                           coregulate->monitor->discuss        30       25
#> 351                              coregulate->monitor->plan        21       16
#> 354                             coregulate->plan->cohesion         6        5
#> 355                            coregulate->plan->consensus        64       56
#> 357                              coregulate->plan->discuss        16       12
#> 358                              coregulate->plan->emotion        36       29
#> 359                              coregulate->plan->monitor        19       15
#> 360                                 coregulate->plan->plan        71       99
#> 363                       coregulate->synthesis->consensus         6        8
#> 374                          discuss->cohesion->coregulate         7       10
#> 375                             discuss->cohesion->discuss         5        8
#> 376                             discuss->cohesion->emotion        12        6
#> 378                                discuss->cohesion->plan        13        5
#> 381                          discuss->consensus->consensus        59       28
#> 382                         discuss->consensus->coregulate       128       78
#> 383                            discuss->consensus->emotion        54       30
#> 384                            discuss->consensus->monitor        27       26
#> 387                          discuss->coregulate->cohesion         7        7
#> 388                         discuss->coregulate->consensus        17       20
#> 390                           discuss->coregulate->discuss        32       64
#> 391                           discuss->coregulate->emotion        25       20
#> 392                           discuss->coregulate->monitor         9       18
#> 393                              discuss->coregulate->plan        32       38
#> 395                             discuss->discuss->cohesion        14       17
#> 396                           discuss->discuss->coregulate        16       33
#> 397                              discuss->discuss->discuss        53       98
#> 398                              discuss->discuss->emotion        33       41
#> 399                              discuss->discuss->monitor         5        8
#> 400                                 discuss->discuss->plan         7        5
#> 401                            discuss->discuss->synthesis        31       60
#> 402                             discuss->emotion->cohesion        62       61
#> 403                            discuss->emotion->consensus        70       63
#> 404                           discuss->emotion->coregulate         5        9
#> 405                              discuss->emotion->discuss        26       11
#> 406                              discuss->emotion->emotion         9       10
#> 407                              discuss->emotion->monitor         5        5
#> 408                                 discuss->emotion->plan        24       16
#> 414                              discuss->monitor->discuss        16       20
#> 420                               discuss->plan->consensus         8        7
#> 424                                    discuss->plan->plan         5        6
#> 425                           discuss->synthesis->cohesion         5       12
#> 426                          discuss->synthesis->consensus       113      122
#> 428                            discuss->synthesis->emotion         8       23
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
#>         prop_High     prop_Low   effect_size     p_value
#> 1    0.0112965527 0.0288879235 15.4866444466 0.008991009
#> 2    0.0741928431 0.0594410657  7.1740022385 0.008991009
#> 3    0.2660884775 0.2277729511 14.1996850823 0.008991009
#> 4    0.0698928650 0.0849985520  6.3310325884 0.008991009
#> 5    0.1228773413 0.1005647263  8.9867614687 0.008991009
#> 6    0.0486844982 0.0613958876  6.7269068166 0.008991009
#> 7    0.2260768166 0.2549232551  6.0087848061 0.008991009
#> 8    0.0230303914 0.0299015349  4.4845554626 0.008991009
#> 9    0.0029085764 0.0079612863  8.0117822341 0.077922078
#> 10   0.0057385426 0.0132688105  8.9268233904 0.077922078
#> 11   0.0003930509 0.0019512957  4.8256861201 0.077922078
#> 12   0.0032230171 0.0003902591  7.0409551876 0.077922078
#> 13   0.0395409166 0.0266156728  9.3880427150 0.077922078
#> 14   0.0059743731 0.0098345301  4.2006244881 0.077922078
#> 15   0.0052668815 0.0021073993  5.6185005971 0.077922078
#> 16   0.0620234258 0.0312987824 16.8765140607 0.077922078
#> 17   0.0216964075 0.0143615361  6.6319801096 0.077922078
#> 18   0.0076251867 0.0131127068  5.6798726573 0.077922078
#> 19   0.0165081362 0.0256790509  6.7423267204 0.077922078
#> 20   0.0037732883 0.0182641274 16.9138084908 0.077922078
#> 21   0.0097476613 0.0049953169  6.2683598810 0.077922078
#> 22   0.0668972565 0.0326256634 19.2924830520 0.077922078
#> 23   0.0167439667 0.0268498283  7.5302964091 0.077922078
#> 24   0.0409558997 0.0302841086  6.4648588661 0.077922078
#> 25   0.0148573225 0.0078051826  7.7370324979 0.077922078
#> 26   0.0054241019 0.0028879176  4.1428888504 0.077922078
#> 27   0.0414275607 0.0294255386  7.7025040066 0.077922078
#> 28   0.0745224432 0.1058382766  9.2228352293 0.077922078
#> 29   0.0031444069 0.0088198564  8.3689997778 0.077922078
#> 31   0.0031444069 0.0007024664  6.0503134120 0.077922078
#> 33   0.0012577628 0.0031220731  3.7619704423 0.155844156
#> 34   0.0094332207 0.0136590696  4.0244825697 0.155844156
#> 35   0.0009433221 0.0026537621  3.8057504644 0.155844156
#> 36   0.0396981369 0.0326256634  3.7860362603 0.155844156
#> 38   0.0136781700 0.0190446456  4.8150379509 0.155844156
#> 39   0.0111626444 0.0075710272  3.4317470980 0.233766234
#> 40   0.0006288814 0.0025757103  4.7569441529 0.233766234
#> 41   0.0183947803 0.0237277552  4.0290900732 0.311688312
#> 43   0.0266488484 0.0336403372  3.2505181238 0.389610390
#> 45   0.0015722034 0.0032001249  3.1308768117 0.545454545
#> 47   0.0011944373 0.0035557061  4.8146264877 0.573426573
#> 48   0.0017916560 0.0051642398  5.6901151409 0.573426573
#> 49   0.0110058869 0.0037250254  9.7863471018 0.573426573
#> 50   0.0028154594 0.0008465967  4.6702473256 0.573426573
#> 51   0.0033273611 0.0005926177  6.4844997108 0.573426573
#> 52   0.0248272332 0.0073653911 15.8684998444 0.573426573
#> 53   0.0119443733 0.0066881138  5.7887633493 0.573426573
#> 54   0.0078491596 0.0027937690  7.4440196761 0.573426573
#> 55   0.0172340244 0.0117676939  4.2963365125 0.573426573
#> 56   0.0325910758 0.0419065357  5.5120383472 0.573426573
#> 57   0.0005119017 0.0027091094  6.0481716847 0.573426573
#> 58   0.0023035577 0.0055028784  5.1660103883 0.573426573
#> 59   0.0017063390 0.0053335591  6.5496394424 0.573426573
#> 60   0.0010238034 0.0049102608  8.0571333871 0.573426573
#> 61   0.0022182408 0.0081273281  8.9632306183 0.573426573
#> 63   0.0005119017 0.0021164917  4.4108036678 0.573426573
#> 64   0.0054602850 0.0025397900  5.1473957139 0.573426573
#> 65   0.0168927566 0.0034710464 13.0520160192 0.573426573
#> 66   0.0257657196 0.0153233999  8.2679680828 0.573426573
#> 67   0.0005119017 0.0041483237  7.9630958995 0.573426573
#> 68   0.0122856412 0.0072807315  4.9451280278 0.573426573
#> 69   0.0024741916 0.0075347105  7.7643195981 0.573426573
#> 71   0.0023888747 0.0007619370  3.8334007949 0.573426573
#> 73   0.0200494838 0.0138841856  4.9813034938 0.573426573
#> 74   0.0094701817 0.0048256011  5.9276525952 0.573426573
#> 75   0.0063987714 0.0016085337  8.2320471794 0.573426573
#> 77   0.0023035577 0.0055028784  5.3775371086 0.573426573
#> 78   0.0045217985 0.0020318320  4.4196623749 0.573426573
#> 79   0.0023888747 0.0049949204  4.2148641927 0.573426573
#> 81   0.0142479311 0.0090585845  4.9675590941 0.573426573
#> 82   0.0052043341 0.0020318320  5.3085609887 0.573426573
#> 83   0.0218411398 0.0307314595  6.2010396184 0.573426573
#> 84   0.0240593806 0.0431764307  8.0873389790 0.573426573
#> 85   0.0005119017 0.0024551304  5.1245420689 0.573426573
#> 87   0.0029871865 0.0049172651  2.9239359806 0.701298701
#> 88   0.0025941357 0.0042928505  2.7381871309 0.935064935
#> 89   0.1578602143 0.1521141037  0.6709016018 1.000000000
#> 93   0.0087257291 0.0066344052  1.9987330688 1.000000000
#> 95   0.0011005424 0.0012488292 -0.7468360177 1.000000000
#> 96   0.0222466787 0.0184202310  1.9249552616 1.000000000
#> 97   0.0455938999 0.0474555105 -0.1161942287 1.000000000
#> 98   0.0971621728 0.0990477677 -0.3418033133 1.000000000
#> 99   0.0021224746 0.0016390884  0.1747706178 1.000000000
#> 100  0.0015722034 0.0009366219  1.0829357474 1.000000000
#> 101  0.0025155255 0.0030440212  0.0741827367 1.000000000
#> 102  0.0143070513 0.0122541367  1.0239362108 1.000000000
#> 103  0.0067604748 0.0065563534 -0.9980058623 1.000000000
#> 104  0.0187092210 0.0181860756 -0.7923343344 1.000000000
#> 105  0.0013363729 0.0015610365 -0.5689840168 1.000000000
#> 106  0.0113984750 0.0146737434  2.3822989213 1.000000000
#> 107  0.0176872887 0.0150640025  1.5193770912 1.000000000
#> 108  0.0019652543 0.0016390884 -0.3132182566 1.000000000
#> 110  0.0028299662 0.0047611614  2.6679350670 1.000000000
#> 111  0.0076251867 0.0094442710  0.8900955501 1.000000000
#> 112  0.0038518984 0.0042147986 -0.5172457811 1.000000000
#> 113  0.0110054241 0.0111614112 -1.1026687414 1.000000000
#> 114  0.0005502712 0.0007024664 -0.5161031022 1.000000000
#> 115  0.0024369153 0.0038245395  1.8834436567 1.000000000
#> 116  0.0079396274 0.0099125820  1.5231568049 1.000000000
#> 117  0.0025155255 0.0039806431  2.1674682443 1.000000000
#> 118  0.0047952205 0.0053855760 -0.2069114042 1.000000000
#> 119  0.0009433221 0.0010927256 -0.6991125009 1.000000000
#> 120  0.0112412546 0.0129566032  0.8078903270 1.000000000
#> 121  0.0009433221 0.0008585701 -1.0140554869 1.000000000
#> 123  0.0071535257 0.0049953169  2.2779171560 1.000000000
#> 124  0.0668186463 0.0732126132  2.1919575293 1.000000000
#> 125  0.0172156277 0.0192007493  0.7356230632 1.000000000
#> 126  0.0006288814 0.0010927256  0.7270079035 1.000000000
#> 127  0.0125776275 0.0112394630  0.3252557566 1.000000000
#> 128  0.0014149831 0.0021854511  1.1080072090 1.000000000
#> 130  0.0018769730 0.0041483237  4.0524997858 1.000000000
#> 139  0.0015357051 0.0014392144 -0.9846846096 1.000000000
#> 161  0.0008531695 0.0006772773 -0.4469026879 1.000000000
#> 188  0.0037539459 0.0022858110  2.0589167123 1.000000000
#> 189  0.0057162358 0.0058415171 -1.1261312782 1.000000000
#> 190  0.0039245798 0.0018625127  3.6304198476 1.000000000
#> 191  0.0009384865 0.0011005757 -0.6735069033 1.000000000
#> 192  0.0144185650 0.0114290552  2.0906582587 1.000000000
#> 196  0.0005972187 0.0017778530  3.0922486739 1.000000000
#> 198  0.0012797543 0.0026244497  2.4498093116 1.000000000
#> 199  0.0012797543 0.0016931934  0.0394969203 1.000000000
#> 201  0.0017063390 0.0022011514  0.1676089715 1.000000000
#> 205  0.0012797543 0.0010159160 -0.3598240092 1.000000000
#> 209  0.0006825356 0.0004232983  0.0631877458 1.000000000
#> 211  0.0025595086 0.0022858110 -0.5977968495 1.000000000
#> 212  0.0030714103 0.0015238740  2.8704115665 1.000000000
#> 214  0.0011091204 0.0007619370  0.1175030390 1.000000000
#> 215  0.0004265848 0.0005079580 -0.9442027787 1.000000000
#> 217  0.0008531695 0.0008465967 -1.3467950332 1.000000000
#> 222  0.0008531695 0.0014392144  0.7457694517 1.000000000
#> 226  0.0030714103 0.0026244497 -0.2137477328 1.000000000
#> 229  0.0023888747 0.0007619370  3.6883922607 1.000000000
#> 230  0.0006825356 0.0004232983  0.0801611170 1.000000000
#> 231  0.0036686290 0.0030477481  0.1077357237 1.000000000
#> 237  0.0005119017 0.0005926177 -0.9479644728 1.000000000
#> 238  0.0004265848 0.0006772773  0.0532891405 1.000000000
#> 240  0.0006825356 0.0004232983  0.1077502287 1.000000000
#> 247  0.0023035577 0.0016085337  0.6287670954 1.000000000
#> 248  0.0034979951 0.0033863867 -1.0578365845 1.000000000
#> 249  0.0047777493 0.0022011514  4.2252543379 1.000000000
#> 250  0.0021329238 0.0014392144  0.7592540847 1.000000000
#> 251  0.0012797543 0.0009312563  0.0673158567 1.000000000
#> 252  0.0069959901 0.0088892652  1.2568476302 1.000000000
#> 254  0.0012797543 0.0006772773  1.0938615341 1.000000000
#> 255  0.0016210221 0.0016085337 -1.3095558002 1.000000000
#> 256  0.0045217985 0.0076193701  3.6969434973 1.000000000
#> 257  0.0009384865 0.0020318320  2.2164805000 1.000000000
#> 258  0.0115177886 0.0140535049  1.5584701021 1.000000000
#> 259  0.0093848648 0.0071960718  1.6840661528 1.000000000
#> 260  0.0044364815 0.0034710464  0.5978174902 1.000000000
#> 261  0.0120296903 0.0098205215  1.3618847795 1.000000000
#> 262  0.0005972187 0.0006772773 -0.9743492255 1.000000000
#> 263  0.0018769730 0.0036403657  2.9738289562 1.000000000
#> 264  0.0047777493 0.0029630884  2.3725773774 1.000000000
#> 265  0.0009384865 0.0006772773 -0.1935566908 1.000000000
#> 266  0.0005972187 0.0004232983 -0.3674440929 1.000000000
#> 267  0.0061428206 0.0064341348 -0.8265597284 1.000000000
#> 269  0.0061428206 0.0044023027  1.8427077655 1.000000000
#> 270  0.0073372579 0.0044023027  3.5830598716 1.000000000
#> 272  0.0028154594 0.0014392144  2.5022102516 1.000000000
#> 273  0.0015357051 0.0013545547 -0.7291356813 1.000000000
#> 274  0.0011091204 0.0007619370  0.0783718060 1.000000000
#> 275  0.0016210221 0.0016085337 -1.3046080187 1.000000000
#> 278  0.0005119017 0.0010159160  0.9469580365 1.000000000
#> 279  0.0014503882 0.0019471724  0.2288375200 1.000000000
#> 280  0.0004265848 0.0005079580 -0.9062859043 1.000000000
#> 281  0.0034126781 0.0059261768  3.4429545553 1.000000000
#> 282  0.0007678526 0.0011852354  0.3521506893 1.000000000
#> 284  0.0025595086 0.0022858110 -0.5837337303 1.000000000
#> 287  0.0031567272 0.0015238740  3.0424991955 1.000000000
#> 288  0.0300315673 0.0282763292 -0.0001161218 1.000000000
#> 289  0.0024741916 0.0011852354  2.6399152457 1.000000000
#> 290  0.0057162358 0.0077040298  1.7326782805 1.000000000
#> 291  0.0073372579 0.0072807315 -1.2218407811 1.000000000
#> 295  0.0007678526 0.0004232983  0.4935212948 1.000000000
#> 301  0.0006825356 0.0005079580 -0.4184190165 1.000000000
#> 307  0.0013650712 0.0012698950 -1.0028458673 1.000000000
#> 312  0.0004265848 0.0004232983 -1.2545934236 1.000000000
#> 314  0.0008531695 0.0016931934  1.7183923421 1.000000000
#> 315  0.0011944373 0.0030477481  3.5470288828 1.000000000
#> 316  0.0013650712 0.0016931934 -0.2663534840 1.000000000
#> 317  0.0011944373 0.0007619370  0.5683314666 1.000000000
#> 319  0.0029007764 0.0056721978  4.0695476319 1.000000000
#> 329  0.0007678526 0.0006772773 -0.9205319829 1.000000000
#> 330  0.0073372579 0.0057568574  1.1930034629 1.000000000
#> 331  0.0013650712 0.0022858110  1.2461786198 1.000000000
#> 332  0.0013650712 0.0024551304  1.6233580899 1.000000000
#> 333  0.0004265848 0.0005079580 -0.8977667595 1.000000000
#> 336  0.0047777493 0.0037250254  0.7254623209 1.000000000
#> 337  0.0038392629 0.0028784287  0.6939360078 1.000000000
#> 339  0.0021329238 0.0015238740  0.4090879366 1.000000000
#> 340  0.0009384865 0.0011005757 -0.7080581527 1.000000000
#> 342  0.0016210221 0.0015238740 -1.0052223788 1.000000000
#> 346  0.0011944373 0.0013545547 -0.7549314249 1.000000000
#> 348  0.0025595086 0.0021164917 -0.1303959467 1.000000000
#> 351  0.0017916560 0.0013545547  0.1771628331 1.000000000
#> 354  0.0005119017 0.0004232983 -0.8294265916 1.000000000
#> 355  0.0054602850 0.0047409414 -0.0829431119 1.000000000
#> 357  0.0013650712 0.0010159160 -0.0427085915 1.000000000
#> 358  0.0030714103 0.0024551304  0.2163430622 1.000000000
#> 359  0.0016210221 0.0012698950 -0.1026213243 1.000000000
#> 360  0.0060575036 0.0083813071  2.2314521917 1.000000000
#> 363  0.0005119017 0.0006772773 -0.4320419851 1.000000000
#> 374  0.0005972187 0.0008465967 -0.1618503805 1.000000000
#> 375  0.0004265848 0.0006772773 -0.1251275062 1.000000000
#> 376  0.0010238034 0.0005079580  1.0892051791 1.000000000
#> 378  0.0011091204 0.0004232983  1.9039032043 1.000000000
#> 381  0.0050337002 0.0023704707  4.2247650068 1.000000000
#> 382  0.0109205699 0.0066034541  4.4150914730 1.000000000
#> 383  0.0046071154 0.0025397900  3.1536515219 1.000000000
#> 384  0.0023035577 0.0022011514 -1.0551070738 1.000000000
#> 387  0.0005972187 0.0005926177 -1.2769326614 1.000000000
#> 388  0.0014503882 0.0016931934 -0.5443457112 1.000000000
#> 390  0.0027301425 0.0054182188  3.5262411819 1.000000000
#> 391  0.0021329238 0.0016931934 -0.0174930834 1.000000000
#> 392  0.0007678526 0.0015238740  1.4684158856 1.000000000
#> 393  0.0027301425 0.0032170674 -0.1739663330 1.000000000
#> 395  0.0011944373 0.0014392144 -0.4560070289 1.000000000
#> 396  0.0013650712 0.0027937690  2.6645633456 1.000000000
#> 397  0.0045217985 0.0082966475  3.8456411140 1.000000000
#> 398  0.0028154594 0.0034710464  0.1805348586 1.000000000
#> 399  0.0004265848 0.0006772773 -0.1140657238 1.000000000
#> 400  0.0005972187 0.0004232983 -0.3468728481 1.000000000
#> 401  0.0026448255 0.0050795801  3.7929328088 1.000000000
#> 402  0.0052896511 0.0051642398 -1.1358804287 1.000000000
#> 403  0.0059721867 0.0053335591 -0.2215947570 1.000000000
#> 404  0.0004265848 0.0007619370  0.4496854658 1.000000000
#> 405  0.0022182408 0.0009312563  2.7166952410 1.000000000
#> 406  0.0007678526 0.0008465967 -0.9813809090 1.000000000
#> 407  0.0004265848 0.0004232983 -1.2577345128 1.000000000
#> 408  0.0020476069 0.0013545547  0.8176714603 1.000000000
#> 414  0.0013650712 0.0016931934 -0.2393320921 1.000000000
#> 420  0.0006825356 0.0005926177 -0.9401216059 1.000000000
#> 424  0.0004265848 0.0005079580 -0.9210987073 1.000000000
#> 425  0.0004265848 0.0010159160  1.3936306398 1.000000000
#> 426  0.0096408156 0.0103284795 -0.4266443708 1.000000000
#> 428  0.0006825356 0.0019471724  3.1432440660 1.000000000
#> 433  0.0034126781 0.0055028784  2.5570696356 1.000000000
#> 434  0.0017063390 0.0029630884  2.0782528181 1.000000000
#> 435  0.0054602850 0.0038096851  1.8393831361 1.000000000
#> 436  0.0005972187 0.0020318320  3.6500055655 1.000000000
#> 437  0.0059721867 0.0038943447  2.4177547215 1.000000000
#> 441  0.0043511646 0.0022858110  3.3114097013 1.000000000
#> 442  0.0074225749 0.0060108364  0.9105957381 1.000000000
#> 443  0.0024741916 0.0013545547  1.9271765377 1.000000000
#> 444  0.0015357051 0.0020318320  0.2243045291 1.000000000
#> 445  0.0140772972 0.0131222486 -0.2992797556 1.000000000
#> 450  0.0004265848 0.0016931934  3.7355552834 1.000000000
#> 451  0.0006825356 0.0005079580 -0.5037951866 1.000000000
#> 452  0.0005972187 0.0006772773 -0.9574549295 1.000000000
#> 453  0.0008531695 0.0009312563 -1.0315032733 1.000000000
#> 457  0.0006825356 0.0007619370 -0.9351377214 1.000000000
#> 458  0.0029007764 0.0017778530  1.6224188758 1.000000000
#> 459  0.0016210221 0.0006772773  2.0717512755 1.000000000
#> 462  0.0017916560 0.0013545547  0.1409836943 1.000000000
#> 464  0.0029860933 0.0030477481 -1.1320754284 1.000000000
#> 465  0.0024741916 0.0027937690 -0.5186603013 1.000000000
#> 467  0.0007678526 0.0004232983  0.4864500322 1.000000000
#> 468  0.0007678526 0.0014392144  0.7812469158 1.000000000
#> 473  0.0004265848 0.0008465967  0.7175230442 1.000000000
#> 475  0.0012797543 0.0015238740 -0.5022625321 1.000000000
#> 476  0.0004265848 0.0004232983 -1.2598597982 1.000000000
#> 481  0.0032420442 0.0031324077 -1.0590380604 1.000000000
#> 483  0.0008531695 0.0008465967 -1.3262122005 1.000000000
#> 484  0.0017916560 0.0011852354  0.6971244603 1.000000000
#> 485  0.0007678526 0.0011852354  0.4066182792 1.000000000
#> 486  0.0036686290 0.0044869624  0.3114763459 1.000000000
#> 495  0.0013650712 0.0018625127  0.2481373613 1.000000000
#> 502  0.0004265848 0.0010159160  1.5352466810 1.000000000
#> 503  0.0017063390 0.0017778530 -1.1350446149 1.000000000
#> 504  0.0023035577 0.0015238740  0.9823005178 1.000000000
#> 506  0.0004265848 0.0008465967  0.7834153920 1.000000000
#> 507  0.0026448255 0.0044023027  2.4455874045 1.000000000
#> 511  0.0004265848 0.0007619370  0.4784073005 1.000000000
#> 513  0.0006825356 0.0009312563 -0.1422735374 1.000000000
#> 516  0.0005972187 0.0007619370 -0.4781335030 1.000000000
#> 518  0.0019622899 0.0013545547  0.5903922311 1.000000000
#> 519  0.0075078918 0.0045716221  3.7845485612 1.000000000
#> 520  0.0012797543 0.0023704707  1.7603087707 1.000000000
#> 521  0.0021329238 0.0027091094  0.2474575304 1.000000000
#> 524  0.0021329238 0.0042329834  3.4082845193 1.000000000
#> 525  0.0010238034 0.0019471724  1.7675440892 1.000000000
#> 526  0.0017916560 0.0018625127 -1.1166940966 1.000000000
#> 531  0.0008531695 0.0004232983  0.8227098262 1.000000000
#> 539  0.0028154594 0.0041483237  1.4455625553 1.000000000
#> 541  0.0004265848 0.0008465967  0.8029926588 1.000000000
#> 542  0.0020476069 0.0016931934 -0.3403978365 1.000000000
#> 543  0.0006825356 0.0006772773 -1.2817815274 1.000000000
#> 544  0.0048630663 0.0050795801 -0.9260767077 1.000000000
#> 558  0.0007678526 0.0006772773 -0.9374678936 1.000000000
#> 560  0.0010238034 0.0007619370 -0.2742271602 1.000000000
#> 562  0.0010238034 0.0008465967 -0.5375206738 1.000000000
#> 563  0.0052896511 0.0055028784 -0.9931561855 1.000000000
#> 564  0.0125415920 0.0155773789  2.0234975873 1.000000000
#> 565  0.0137360293 0.0099051812  3.0507426785 1.000000000
#> 566  0.0060575036 0.0050795801  0.4280664720 1.000000000
#> 567  0.0246565993 0.0299695225  2.5586620143 1.000000000
#> 568  0.0005119017 0.0008465967  0.3140583777 1.000000000
#> 571  0.0008531695 0.0011005757 -0.2831735989 1.000000000
#> 572  0.0011091204 0.0004232983  1.7359422389 1.000000000
#> 574  0.0019622899 0.0006772773  3.2711934577 1.000000000
#> 576  0.0005119017 0.0005926177 -0.9910335852 1.000000000
#> 577  0.0059721867 0.0039790044  2.3824819924 1.000000000
#> 578  0.0017063390 0.0021164917 -0.1266943054 1.000000000
#> 579  0.0022182408 0.0044023027  3.8142509492 1.000000000
#> 580  0.0011944373 0.0016931934  0.2513892740 1.000000000
#> 583  0.0015357051 0.0030477481  2.7832796651 1.000000000
#> 585  0.0137360293 0.0090585845  4.2493862954 1.000000000
#> 586  0.0009384865 0.0018625127  1.6767401517 1.000000000
#> 587  0.0025595086 0.0029630884 -0.3450926810 1.000000000
#> 588  0.0007678526 0.0011852354  0.3886004582 1.000000000
#> 589  0.0036686290 0.0031324077 -0.1895134419 1.000000000
#> 592  0.0007678526 0.0008465967 -1.0316027927 1.000000000
#> 593  0.0029007764 0.0037250254  0.6015687660 1.000000000
#> 594  0.0007678526 0.0015238740  1.4884222173 1.000000000
#> 595  0.0064840884 0.0063494751 -1.1265068942 1.000000000
#> 596  0.0018769730 0.0013545547  0.3355993298 1.000000000
#> 598  0.0038392629 0.0046562817  0.2852429249 1.000000000
#> 601  0.0019622899 0.0022858110 -0.5121944266 1.000000000
#> 602  0.0014503882 0.0013545547 -0.9957145675 1.000000000
#> 603  0.0043511646 0.0078733491  4.8542918061 1.000000000
#> 604  0.0139919802 0.0115983745  1.5118763796 1.000000000
#> 605  0.0059721867 0.0077886895  1.5644346686 1.000000000
#> 611  0.0017063390 0.0040636641  4.2801442997 1.000000000
#> 624  0.0007678526 0.0011005757  0.1055304026 1.000000000
#> 625  0.0018769730 0.0019471724 -1.0977008706 1.000000000
#> 626  0.0026448255 0.0022011514 -0.2478074045 1.000000000
#> 629  0.0049483832 0.0049102608 -1.2170710863 1.000000000
#> 646  0.0004265848 0.0008465967  0.9030300843 1.000000000
#> 647  0.0006825356 0.0007619370 -0.9789857192 1.000000000
#> 661  0.0008531695 0.0004232983  0.8466948522 1.000000000
#> 669  0.0008358098 0.0015658101  1.2667101655 1.000000000
#> 702  0.0005572065 0.0013815971  1.8656859940 1.000000000
#> 726  0.0004643388 0.0011052777  1.4438305718 1.000000000
#> 731  0.0005572065 0.0031316202  5.7298546932 1.000000000
#> 845  0.0010215453 0.0011973842 -0.6837895926 1.000000000
#> 850  0.0016716196 0.0021184489 -0.1076785389 1.000000000
#> 851  0.0009286776 0.0004605324  0.8219834508 1.000000000
#> 853  0.0011144131 0.0009210648 -0.5525292948 1.000000000
#> 856  0.0039004458 0.0011052777  5.4224330724 1.000000000
#> 857  0.0009286776 0.0005526389  0.4369883712 1.000000000
#> 858  0.0025074294 0.0011973842  2.4060712666 1.000000000
#> 859  0.0008358098 0.0004605324  0.4865482944 1.000000000
#> 864  0.0016716196 0.0005526389  2.8181495727 1.000000000
#> 876  0.0048291233 0.0034079396  1.4213261937 1.000000000
#> 878  0.0009286776 0.0008289583 -0.9286886150 1.000000000
#> 879  0.0026002972 0.0017500230  0.8890930558 1.000000000
#> 880  0.0006500743 0.0011052777  0.5295238492 1.000000000
#> 881  0.0045505201 0.0042368979 -0.7720596135 1.000000000
#> 899  0.0004643388 0.0004605324 -1.2411331993 1.000000000
#> 942  0.0007429421 0.0004605324  0.0179655825 1.000000000
#> 962  0.0010215453 0.0015658101  0.5288912511 1.000000000
#> 974  0.0008358098 0.0008289583 -1.2880210274 1.000000000
#> 1035 0.0008358098 0.0005526389 -0.0051701596 1.000000000
#> 1039 0.0012072808 0.0011052777 -0.9686372130 1.000000000
#> 1065 0.0014858841 0.0007368518  1.4771690439 1.000000000
#> 1067 0.0006500743 0.0004605324 -0.3076627916 1.000000000
#> 1069 0.0010215453 0.0014737036  0.2570736674 1.000000000
#> 1119 0.0011144131 0.0008289583 -0.1998501312 1.000000000
#> 1123 0.0005572065 0.0011052777  0.9549733963 1.000000000
#> 1124 0.0011144131 0.0006447453  0.5853796084 1.000000000
#> 1126 0.0012072808 0.0008289583  0.1072426708 1.000000000
#> 1130 0.0022288262 0.0005526389  3.7582617114 1.000000000
#> 1134 0.0004643388 0.0007368518  0.0227940024 1.000000000
#> 1136 0.0005572065 0.0004605324 -0.8405983683 1.000000000
#> 1145 0.0007429421 0.0005526389 -0.3800830093 1.000000000
#> 1149 0.0033432392 0.0030395137 -0.6910679471 1.000000000
#> 1152 0.0012072808 0.0009210648 -0.2982736047 1.000000000
#> 1154 0.0018573551 0.0032237266  1.9480963268 1.000000000
#> 1165 0.0008358098 0.0004605324  0.4648190886 1.000000000
#> 1170 0.0006500743 0.0009210648 -0.1942790019 1.000000000
#> 1171 0.0006500743 0.0022105554  3.4839574463 1.000000000
#> 1172 0.0009286776 0.0011973842 -0.3383161087 1.000000000
#> 1175 0.0017644874 0.0030395137  1.8202118308 1.000000000
#> 1187 0.0055720654 0.0032237266  3.0081401218 1.000000000
#> 1188 0.0008358098 0.0011052777 -0.2946456084 1.000000000
#> 1189 0.0011144131 0.0029474072  3.7825961974 1.000000000
#> 1190 0.0008358098 0.0011052777 -0.2825464283 1.000000000
#> 1193 0.0013001486 0.0030395137  3.2180883245 1.000000000
#> 1194 0.0030646360 0.0016579166  2.1182453587 1.000000000
#> 1195 0.0026002972 0.0021184489 -0.1119416495 1.000000000
#> 1197 0.0013930163 0.0009210648  0.2396724707 1.000000000
#> 1198 0.0005572065 0.0006447453 -0.9249803072 1.000000000
#> 1200 0.0011144131 0.0009210648 -0.5143218455 1.000000000
#> 1204 0.0009286776 0.0009210648 -1.2966669004 1.000000000
#> 1206 0.0015787519 0.0009210648  0.8981317731 1.000000000
#> 1208 0.0012072808 0.0009210648 -0.1960387627 1.000000000
#> 1212 0.0034361070 0.0023947684  1.0591829200 1.000000000
#> 1214 0.0006500743 0.0006447453 -1.3034242799 1.000000000
#> 1215 0.0020430906 0.0016579166 -0.2119201968 1.000000000
#> 1216 0.0013001486 0.0004605324  2.3775364720 1.000000000
#> 1217 0.0036218425 0.0045132173  0.3986836711 1.000000000
#> 1224 0.0011144131 0.0018421295  0.9983676140 1.000000000
#> 1240 0.0039004458 0.0012894907  5.1186299014 1.000000000
#> 1241 0.0058506686 0.0009210648  7.8174300578 1.000000000
#> 1242 0.0014858841 0.0005526389  2.2786422092 1.000000000
#> 1243 0.0006500743 0.0010131712  0.2416905414 1.000000000
#> 1244 0.0095653789 0.0033158331  8.1657954640 1.000000000
#> 1249 0.0011144131 0.0013815971 -0.4358350834 1.000000000
#> 1252 0.0012072808 0.0008289583  0.1558396386 1.000000000
#> 1255 0.0047362556 0.0009210648  7.3559174393 1.000000000
#> 1256 0.0005572065 0.0007368518 -0.4408079597 1.000000000
#> 1257 0.0024145617 0.0014737036  1.2936584052 1.000000000
#> 1258 0.0014858841 0.0006447453  1.9029123632 1.000000000
#> 1261 0.0013001486 0.0012894907 -1.3116403661 1.000000000
#> 1262 0.0021359584 0.0011052777  1.7594539234 1.000000000
#> 1263 0.0030646360 0.0010131712  4.0913408341 1.000000000
#> 1281 0.0008358098 0.0023947684  3.4668664047 1.000000000
#> 1283 0.0037147103 0.0025789813  1.0076660698 1.000000000
#> 1291 0.0034361070 0.0013815971  3.9522262343 1.000000000
#> 1292 0.0006500743 0.0008289583 -0.5738871973 1.000000000
#> 1294 0.0009286776 0.0004605324  0.8364896252 1.000000000
#> 1296 0.0006500743 0.0009210648 -0.1107285271 1.000000000
#> 1299 0.0019502229 0.0009210648  1.9775233160 1.000000000
#> 1300 0.0015787519 0.0009210648  0.9958513835 1.000000000
#> 1303 0.0022288262 0.0018421295 -0.2211876673 1.000000000
#> 1316 0.0008358098 0.0005526389  0.0082561994 1.000000000
#> 1332 0.0005572065 0.0005526389 -1.3347011668 1.000000000
#> 1354 0.0014858841 0.0014737036 -1.3214701651 1.000000000
#> 1360 0.0004643388 0.0011973842  1.7815163493 1.000000000
#> 1372 0.0006500743 0.0007368518 -1.0303794269 1.000000000
#> 1377 0.0013001486 0.0008289583  0.4480049353 1.000000000
#> 1384 0.0023216939 0.0006447453  3.9277848071 1.000000000
#> 1391 0.0007429421 0.0004605324  0.1417386635 1.000000000
#> 1392 0.0028789004 0.0018421295  1.0611449588 1.000000000
#> 1393 0.0059435364 0.0060790274 -1.0949365295 1.000000000
#> 1394 0.0059435364 0.0049737497  0.2320829473 1.000000000
#> 1395 0.0031575037 0.0018421295  1.8800922283 1.000000000
#> 1396 0.0012072808 0.0013815971 -0.7336255880 1.000000000
#> 1397 0.0100297177 0.0110527770 -0.1431406057 1.000000000
#> 1408 0.0027860327 0.0018421295  0.9593994668 1.000000000
#> 1409 0.0008358098 0.0005526389 -0.0788280117 1.000000000
#> 1410 0.0012072808 0.0020263425  1.2248089609 1.000000000
#> 1411 0.0005572065 0.0005526389 -1.2471048342 1.000000000
#> 1415 0.0062221397 0.0040526849  2.2838925948 1.000000000
#> 1416 0.0059435364 0.0037763655  2.2857574030 1.000000000
#> 1418 0.0016716196 0.0005526389  2.7605968700 1.000000000
#> 1419 0.0011144131 0.0007368518  0.2015900352 1.000000000
#> 1420 0.0004643388 0.0005526389 -0.8990007817 1.000000000
#> 1421 0.0013001486 0.0012894907 -1.2988303154 1.000000000
#> 1425 0.0007429421 0.0014737036  1.2824902583 1.000000000
#> 1427 0.0031575037 0.0022105554  0.9291925958 1.000000000
#> 1430 0.0016716196 0.0022105554  0.1558742376 1.000000000
#> 1433 0.0007429421 0.0009210648 -0.6050196149 1.000000000
#> 1434 0.0085438336 0.0129870130  3.8650887009 1.000000000
#> 1435 0.0009286776 0.0005526389  0.3998688891 1.000000000
#> 1436 0.0023216939 0.0029474072  0.1668020139 1.000000000
#> 1437 0.0057578009 0.0043290043  1.2801946296 1.000000000
#> 1438 0.0028789004 0.0031316202 -0.7715041935 1.000000000
#> 1439 0.0109583952 0.0172239108  5.2874909104 1.000000000
#> 1484 0.0004643388 0.0007368518  0.0639823177 1.000000000
#> 1520 0.0008358098 0.0006447453 -0.4260285740 1.000000000
#> 1535 0.0009286776 0.0012894907  0.0567057479 1.000000000
#> 1538 0.0004643388 0.0009210648  0.7914896454 1.000000000
#> 1540 0.0010215453 0.0030395137  4.2602700272 1.000000000
#> 1573 0.0006500743 0.0006447453 -1.2755911006 1.000000000
#> 1574 0.0012072808 0.0012894907 -1.0678413566 1.000000000
#> 1576 0.0004643388 0.0004605324 -1.2793441665 1.000000000
#> 1578 0.0028789004 0.0026710878 -0.8673895372 1.000000000
#> 1580 0.0004643388 0.0008289583  0.3042271937 1.000000000
#> 1587 0.0013930163 0.0015658101 -0.7205562903 1.000000000
#> 1593 0.0004643388 0.0008289583  0.4105798295 1.000000000
#> 1610 0.0012072808 0.0026710878  2.6578684427 1.000000000
#> 1618 0.0020430906 0.0018421295 -0.7479395592 1.000000000
#> 1621 0.0008358098 0.0004605324  0.5273284675 1.000000000
#> 1626 0.0004643388 0.0004605324 -1.2886023474 1.000000000
#> 1627 0.0007429421 0.0004605324  0.0655858282 1.000000000
#> 1630 0.0016716196 0.0014737036 -0.7679025609 1.000000000
#> 1660 0.0004643388 0.0005526389 -0.8993257766 1.000000000
#> 1684 0.0012072808 0.0005526389  1.5479324132 1.000000000
#> 1694 0.0005572065 0.0004605324 -0.8968838963 1.000000000
#> 1698 0.0006500743 0.0004605324 -0.2786717421 1.000000000
#> 1709 0.0009286776 0.0008289583 -0.9110490280 1.000000000
#> 1710 0.0011144131 0.0006447453  0.6460574897 1.000000000
#> 1711 0.0005572065 0.0008289583 -0.0422464715 1.000000000
#> 1713 0.0016716196 0.0018421295 -0.8282405288 1.000000000
#> 1725 0.0013930163 0.0004605324  2.4433544904 1.000000000
#> 1726 0.0007429421 0.0011052777  0.1890156619 1.000000000
#> 1735 0.0004643388 0.0007368518 -0.0440256011 1.000000000
#> 1739 0.0019502229 0.0023947684 -0.1419906473 1.000000000
#> 1742 0.0016716196 0.0010131712  0.9632880046 1.000000000
#> 1743 0.0005572065 0.0009210648  0.3129083383 1.000000000
#> 1744 0.0014858841 0.0028553007  2.4071899699 1.000000000
#> 1760 0.0007429421 0.0025789813  4.3408179084 1.000000000
#> 1769 0.0005572065 0.0021184489  3.9566232641 1.000000000
#> 1770 0.0006500743 0.0006447453 -1.2401443160 1.000000000
#> 1773 0.0004643388 0.0029474072  5.3720322339 1.000000000
#> 1805 0.0008358098 0.0010131712 -0.5857327535 1.000000000
#> 1806 0.0018573551 0.0006447453  2.8702205340 1.000000000
#> 1809 0.0019502229 0.0006447453  3.3397765493 1.000000000
#> 1843 0.0007429421 0.0005526389 -0.4159028285 1.000000000
#> 1847 0.0019502229 0.0011973842  0.9139614304 1.000000000
#> 1850 0.0010215453 0.0010131712 -1.3299254901 1.000000000
#> 1852 0.0026931649 0.0011973842  2.8655261225 1.000000000
#> 1853 0.0021359584 0.0011973842  1.5847102054 1.000000000
#> 1854 0.0010215453 0.0007368518 -0.2026609814 1.000000000
#> 1855 0.0029717682 0.0014737036  2.5374231971 1.000000000
#> 1859 0.0063150074 0.0007368518  9.3417687982 1.000000000
#> 1861 0.0033432392 0.0005526389  6.2765078283 1.000000000
#> 1865 0.0015787519 0.0008289583  1.3633777987 1.000000000
#> 1866 0.0014858841 0.0007368518  1.3748384064 1.000000000
#> 1867 0.0015787519 0.0005526389  2.6194968115 1.000000000
#> 1869 0.0006500743 0.0004605324 -0.3383453713 1.000000000
#> 1877 0.0010215453 0.0004605324  1.2837305766 1.000000000
#> 1884 0.0082652303 0.0041447914  5.0336154206 1.000000000
#> 1886 0.0013001486 0.0011052777 -0.6513841284 1.000000000
#> 1887 0.0042719168 0.0017500230  4.4127537392 1.000000000
#> 1888 0.0022288262 0.0007368518  3.3678168919 1.000000000
#> 1889 0.0088224368 0.0074606245  0.5820202005 1.000000000
#> 1903 0.0005572065 0.0007368518 -0.4612637359 1.000000000
#> 1905 0.0009286776 0.0012894907  0.0218367273 1.000000000
#> 1906 0.0005572065 0.0005526389 -1.3242294440 1.000000000
#> 1907 0.0005572065 0.0011052777  0.9904067554 1.000000000
#> 1910 0.0009286776 0.0008289583 -0.9828283643 1.000000000
#> 1924 0.0008358098 0.0010131712 -0.5643986170 1.000000000
#> 1929 0.0011144131 0.0011973842 -1.0378408493 1.000000000
#> 1940 0.0007429421 0.0008289583 -0.9479256573 1.000000000
#> 1947 0.0005572065 0.0006447453 -1.0001770606 1.000000000
#> 1948 0.0018573551 0.0011973842  0.7203441908 1.000000000
#> 1949 0.0033432392 0.0006447453  5.8855063245 1.000000000
#> 1950 0.0013930163 0.0008289583  0.6989559658 1.000000000
#> 1952 0.0047362556 0.0034079396  1.2000515350 1.000000000
#> 1963 0.0014858841 0.0023947684  1.2183676411 1.000000000
#> 1965 0.0007429421 0.0017500230  1.6679997934 1.000000000
#> 1966 0.0007429421 0.0005526389 -0.3841866961 1.000000000
#> 1969 0.0005572065 0.0011973842  1.3542918985 1.000000000
#> 1970 0.0009286776 0.0012894907 -0.0898615736 1.000000000
#> 1971 0.0008358098 0.0012894907  0.4284608068 1.000000000
#> 1987 0.0004643388 0.0011973842  1.7383474854 1.000000000
#> 1989 0.0012072808 0.0019342360  1.0122586425 1.000000000
#> 1997 0.0028789004 0.0025789813 -0.6331660357 1.000000000
#> 1998 0.0005572065 0.0008289583 -0.0555694128 1.000000000
#> 2000 0.0004643388 0.0007368518  0.0526956022 1.000000000
#> 2005 0.0005572065 0.0007368518 -0.4709921445 1.000000000
#> 2006 0.0007429421 0.0011052777  0.2029388015 1.000000000
#> 2010 0.0026931649 0.0023026619 -0.3472649811 1.000000000
#> 2033 0.0006500743 0.0007368518 -0.9960719333 1.000000000
#> 2036 0.0004643388 0.0006447453 -0.3535229094 1.000000000
#> 2082 0.0004643388 0.0022105554  4.5402687402 1.000000000
#> 2083 0.0012072808 0.0032237266  4.0022099053 1.000000000
#> 2097 0.0016716196 0.0018421295 -0.8195801160 1.000000000
#> 2098 0.0021359584 0.0018421295 -0.5391456083 1.000000000
#> 2101 0.0037147103 0.0044211108  0.0870077062 1.000000000
#> 2130 0.0004643388 0.0004605324 -1.3179314743 1.000000000
#> 2145 0.0021359584 0.0014737036  0.5439214998 1.000000000
#> 2146 0.0028789004 0.0028553007 -1.2457109056 1.000000000
#> 2147 0.0056649331 0.0017500230  6.7404300264 1.000000000
#> 2148 0.0022288262 0.0008289583  3.0509704126 1.000000000
#> 2150 0.0073365527 0.0064474533  0.0194347377 1.000000000
#> 2153 0.0005572065 0.0011052777  1.0500304772 1.000000000
#> 2155 0.0004643388 0.0018421295  3.3012584388 1.000000000
#> 2156 0.0008358098 0.0008289583 -1.2388977474 1.000000000
#> 2158 0.0007429421 0.0010131712 -0.2033447224 1.000000000
#> 2162 0.0004643388 0.0007368518  0.0042556026 1.000000000
#> 2168 0.0014858841 0.0011973842 -0.3494348460 1.000000000
#> 2169 0.0019502229 0.0008289583  2.3010363750 1.000000000
#> 2171 0.0008358098 0.0005526389 -0.0103760848 1.000000000
#> 2183 0.0015787519 0.0011973842  0.0017098829 1.000000000
#> 2186 0.0012072808 0.0005526389  1.3642480097 1.000000000
#> 2188 0.0020430906 0.0013815971  0.7300492256 1.000000000
#> 2199 0.0007429421 0.0005526389 -0.4363942277 1.000000000
#> 2203 0.0012072808 0.0009210648 -0.2669187052 1.000000000
#> 2207 0.0005572065 0.0008289583 -0.0593075644 1.000000000
#> 2209 0.0020430906 0.0018421295 -0.7882882306 1.000000000
#> 2210 0.0016716196 0.0011052777  0.5288029826 1.000000000
#> 2212 0.0020430906 0.0010131712  1.9093071631 1.000000000
#> 2214 0.0006500743 0.0005526389 -0.8957886785 1.000000000
#> 2216 0.0038075780 0.0008289583  6.1731954852 1.000000000
#> 2217 0.0006500743 0.0006447453 -1.2888674027 1.000000000
#> 2218 0.0018573551 0.0007368518  2.5316892259 1.000000000
#> 2219 0.0013001486 0.0004605324  2.1872997070 1.000000000
#> 2222 0.0010215453 0.0012894907 -0.3369340768 1.000000000
#> 2224 0.0012072808 0.0004605324  1.8679922342 1.000000000
#> 2238 0.0043647845 0.0039605784 -0.5321777328 1.000000000
#> 2240 0.0006500743 0.0010131712  0.2033814710 1.000000000
#> 2241 0.0023216939 0.0017500230  0.2822190508 1.000000000
#> 2242 0.0013001486 0.0008289583  0.4646234087 1.000000000
#> 2243 0.0047362556 0.0051579626 -0.5773561040 1.000000000
#> 2292 0.0012072808 0.0005526389  1.3785634696 1.000000000
#> 2296 0.0025074294 0.0009210648  3.5040264322 1.000000000
#> 2323 0.0013001486 0.0004605324  2.2783184565 1.000000000
#> 2330 0.0016716196 0.0006447453  2.5042012945 1.000000000
#> 2333 0.0004643388 0.0006447453 -0.4600850965 1.000000000
#> 2338 0.0004643388 0.0005526389 -0.9115659053 1.000000000
#> 2341 0.0013930163 0.0019342360  0.3336494633 1.000000000
#> 2355 0.0006500743 0.0004605324 -0.3374670595 1.000000000
#> 2408 0.0007429421 0.0006447453 -0.8781429092 1.000000000
#> 2412 0.0007429421 0.0012894907  0.7238516012 1.000000000
#> 2422 0.0009286776 0.0004605324  0.8504877332 1.000000000
#> 2436 0.0008358098 0.0009210648 -1.0109289555 1.000000000
#> 2441 0.0010215453 0.0018421295  1.4488329710 1.000000000
#> 2461 0.0005572065 0.0004605324 -0.8782854997 1.000000000
#> 2510 0.0011144131 0.0014737036 -0.0631398474 1.000000000
#> 2515 0.0006500743 0.0021184489  3.3958037135 1.000000000
#> 2558 0.0010215453 0.0004605324  1.2297463025 1.000000000
#> 2565 0.0007429421 0.0005526389 -0.3525985112 1.000000000
#> 2566 0.0012072808 0.0007368518  0.5107428018 1.000000000
#> 2567 0.0019502229 0.0009210648  2.0020200701 1.000000000
#> 2570 0.0025074294 0.0021184489 -0.3021598933 1.000000000
#> 2575 0.0004643388 0.0007368518 -0.0181317933 1.000000000
#> 2581 0.0012072808 0.0011052777 -0.9996582373 1.000000000
#> 2587 0.0004643388 0.0008289583  0.3783150791 1.000000000
#> 2588 0.0008358098 0.0011052777 -0.2736102819 1.000000000
#> 2603 0.0013001486 0.0018421295  0.2767830095 1.000000000
#> 2609 0.0006500743 0.0008289583 -0.4478762191 1.000000000
#> 2621 0.0007429421 0.0005526389 -0.3751805086 1.000000000
#> 2656 0.0004643388 0.0010131712  1.0951000436 1.000000000
#> 2660 0.0011144131 0.0018421295  0.8635670552 1.000000000
#> 2669 0.0006500743 0.0008289583 -0.5296710954 1.000000000
#> 2682 0.0017644874 0.0013815971 -0.1323742917 1.000000000
#> 2685 0.0008358098 0.0005526389 -0.0164937121 1.000000000
#> 2687 0.0015787519 0.0021184489  0.2511339098 1.000000000
#> 2722 0.0013930163 0.0010131712  0.0269332970 1.000000000
#> 2754 0.0005572065 0.0007368518 -0.4547620776 1.000000000
#> 2755 0.0012072808 0.0010131712 -0.5664403934 1.000000000
#> 2756 0.0006500743 0.0008289583 -0.4914592697 1.000000000
#> 2759 0.0016716196 0.0023026619  0.3031167522 1.000000000
#> 2763 0.0015787519 0.0031316202  2.5336128707 1.000000000
#> 2765 0.0032503715 0.0042368979  0.6251751451 1.000000000
#> 2766 0.0027860327 0.0021184489  0.2734862470 1.000000000
#> 2767 0.0011144131 0.0012894907 -0.7951471662 1.000000000
#> 2768 0.0033432392 0.0037763655 -0.4677102997 1.000000000
#> 2772 0.0052005944 0.0019342360  5.4978912387 1.000000000
#> 2773 0.0007429421 0.0010131712 -0.2436136654 1.000000000
#> 2774 0.0030646360 0.0022105554  0.7416170873 1.000000000
#> 2775 0.0023216939 0.0010131712  2.6374500316 1.000000000
#> 2778 0.0013930163 0.0020263425  0.5539794893 1.000000000
#> 2780 0.0016716196 0.0019342360 -0.5680181400 1.000000000
#> 2781 0.0017644874 0.0012894907  0.1467393776 1.000000000
#> 2783 0.0007429421 0.0005526389 -0.3977045641 1.000000000
#> 2784 0.0004643388 0.0007368518  0.0604056444 1.000000000
#> 2788 0.0004643388 0.0006447453 -0.3614583840 1.000000000
#> 2790 0.0007429421 0.0021184489  2.9870433740 1.000000000
#> 2793 0.0006500743 0.0010131712  0.2433389706 1.000000000
#> 2796 0.0005572065 0.0007368518 -0.5664282940 1.000000000
#> 2797 0.0058506686 0.0086580087  2.4243412765 1.000000000
#> 2798 0.0006500743 0.0005526389 -0.8932889434 1.000000000
#> 2799 0.0017644874 0.0023026619  0.0264111601 1.000000000
#> 2800 0.0048291233 0.0033158331  1.5829770311 1.000000000
#> 2801 0.0023216939 0.0026710878 -0.4848171685 1.000000000
#> 2802 0.0090081724 0.0117896288  1.9161301013 1.000000000
#> 2848 0.0007429421 0.0010131712 -0.2312294004 1.000000000
#> 2852 0.0027860327 0.0018421295  1.1341987105 1.000000000
#> 2859 0.0005572065 0.0004605324 -0.8479129268 1.000000000
#> 2863 0.0011144131 0.0008289583 -0.1528725494 1.000000000
#> 2869 0.0004643388 0.0006447453 -0.3695308785 1.000000000
#> 2871 0.0005572065 0.0008289583 -0.0986444422 1.000000000
#> 2888 0.0006500743 0.0006447453 -1.2858278766 1.000000000
#> 2895 0.0070579495 0.0042368979  3.4368297996 1.000000000
#> 2896 0.0007429421 0.0013815971  1.0631794808 1.000000000
#> 2897 0.0009286776 0.0005526389  0.3797757665 1.000000000
#> 2898 0.0020430906 0.0011973842  1.2328206991 1.000000000
#> 2900 0.0018573551 0.0012894907  0.3401733696 1.000000000
#> 2903 0.0014858841 0.0007368518  1.4675287427 1.000000000
#> 2904 0.0026002972 0.0020263425  0.1994953849 1.000000000
#> 2905 0.0034361070 0.0016579166  2.8800816257 1.000000000
#> 2907 0.0006500743 0.0006447453 -1.2515848019 1.000000000
#> 2908 0.0043647845 0.0033158331  0.6816755807 1.000000000
#> 2927 0.0008358098 0.0010131712 -0.6019677939 1.000000000
#> 2928 0.0007429421 0.0009210648 -0.6113018248 1.000000000
#> 2942 0.0013001486 0.0007368518  0.8957514398 1.000000000
#> 2947 0.0013001486 0.0013815971 -1.0851180684 1.000000000
#> 2959 0.0006500743 0.0004605324 -0.2738631075 1.000000000
#> 2960 0.0006500743 0.0006447453 -1.3173824225 1.000000000
#> 2963 0.0008358098 0.0018421295  2.2032371328 1.000000000
#> 2973 0.0020430906 0.0011052777  1.6167402152 1.000000000
#> 2974 0.0004643388 0.0007368518  0.0395844811 1.000000000
#> 2975 0.0012072808 0.0018421295  0.6840991434 1.000000000
#> 2976 0.0012072808 0.0004605324  1.8761173123 1.000000000
#> 2979 0.0006500743 0.0008289583 -0.5327987548 1.000000000
#> 2980 0.0004643388 0.0006447453 -0.3430283255 1.000000000
#> 2981 0.0005572065 0.0004605324 -0.8756062861 1.000000000
#> 2993 0.0006500743 0.0012894907  1.1317775806 1.000000000
#> 2996 0.0007429421 0.0005526389 -0.4218307516 1.000000000
#> 2998 0.0017644874 0.0017500230 -1.2484987923 1.000000000
#> 3006 0.0010215453 0.0008289583 -0.5228613281 1.000000000
#> 3014 0.0016716196 0.0027631943  1.5930606465 1.000000000
#> 3015 0.0035289747 0.0069079856  4.3667868722 1.000000000
#> 3016 0.0043647845 0.0036842590 -0.0405125341 1.000000000
#> 3017 0.0014858841 0.0022105554  0.7584395613 1.000000000
#> 3018 0.0007429421 0.0024868748  4.2251595809 1.000000000
#> 3019 0.0091939079 0.0123422677  2.4715422307 1.000000000
#> 3028 0.0004643388 0.0005526389 -0.9990331786 1.000000000
#> 3029 0.0016716196 0.0019342360 -0.5181264142 1.000000000
#> 3031 0.0006500743 0.0013815971  1.6213768027 1.000000000
#> 3035 0.0006500743 0.0007368518 -0.9542334737 1.000000000
#> 3036 0.0038075780 0.0030395137  0.3290631698 1.000000000
#> 3037 0.0052005944 0.0034079396  2.0280267347 1.000000000
#> 3038 0.0005572065 0.0006447453 -0.9783757057 1.000000000
#> 3039 0.0020430906 0.0009210648  2.1651206225 1.000000000
#> 3040 0.0010215453 0.0017500230  1.0294846988 1.000000000
#> 3042 0.0013001486 0.0013815971 -1.0833422383 1.000000000
#> 3045 0.0013001486 0.0015658101 -0.4465581657 1.000000000
#> 3047 0.0018573551 0.0022105554 -0.3413988926 1.000000000
#> 3048 0.0006500743 0.0006447453 -1.2678357866 1.000000000
#> 3050 0.0013930163 0.0020263425  0.5728405308 1.000000000
#> 3053 0.0006500743 0.0006447453 -1.2826652713 1.000000000
#> 3054 0.0072436850 0.0118817353  4.5888225923 1.000000000
#> 3056 0.0008358098 0.0037763655  6.5485534767 1.000000000
#> 3057 0.0046433878 0.0049737497 -0.7439621199 1.000000000
#> 3058 0.0018573551 0.0028553007  1.2110583090 1.000000000
#> 3059 0.0077080238 0.0179607626  6.1055918664 1.000000000
#> 3078 0.0005572065 0.0005526389 -1.2584253006 1.000000000
#> 3081 0.0005572065 0.0015658101  2.5827429440 1.000000000
#> 3134 0.0009286776 0.0007368518 -0.4972288862 1.000000000
#> 3153 0.0010215453 0.0011973842 -0.6372091601 1.000000000
#> 3154 0.0004643388 0.0005526389 -0.8927523257 1.000000000
#> 3157 0.0020430906 0.0021184489 -1.1256615285 1.000000000
#> 3265 0.0005051015 0.0007022472 -0.3920952551 1.000000000
#> 3575 0.0007071421 0.0005016051 -0.3074876152 1.000000000
#> 3610 0.0014142843 0.0005016051  2.1601194485 1.000000000
#> 3675 0.0005051015 0.0011035313  1.1747827715 1.000000000
#> 3676 0.0010102031 0.0005016051  0.8616098325 1.000000000
#> 3679 0.0016163249 0.0011035313  0.3004945873 1.000000000
#> 3688 0.0008081624 0.0012038523  0.0335962342 1.000000000
#> 3702 0.0015153046 0.0014044944 -1.0325186798 1.000000000
#> 3704 0.0010102031 0.0007022472 -0.1135545031 1.000000000
#> 3706 0.0015153046 0.0010032103  0.3664553307 1.000000000
#> 4010 0.0006061218 0.0006019262 -1.2739572229 1.000000000
#> 4250 0.0013132640 0.0015048154 -0.7086185150 1.000000000
#> 4272 0.0009091827 0.0012038523 -0.3204965331 1.000000000
#> 4277 0.0005051015 0.0010032103  0.7892335360 1.000000000
#> 4340 0.0005051015 0.0007022472 -0.2689457645 1.000000000
#> 4344 0.0006061218 0.0015048154  2.0654839732 1.000000000
#> 4377 0.0008081624 0.0007022472 -0.9078512458 1.000000000
#> 4381 0.0022224467 0.0014044944  0.9233000836 1.000000000
#> 4389 0.0006061218 0.0010032103  0.3634935175 1.000000000
#> 4395 0.0005051015 0.0005016051 -1.2975247462 1.000000000
#> 4411 0.0009091827 0.0012038523 -0.2318277203 1.000000000
#> 4417 0.0014142843 0.0008025682  0.9153112805 1.000000000
#> 4428 0.0011112234 0.0011035313 -1.2562503147 1.000000000
#> 4496 0.0014142843 0.0009028892  0.4520601963 1.000000000
#> 4520 0.0011112234 0.0013041734 -0.6771655140 1.000000000
#> 4525 0.0010102031 0.0016051364  0.6427163221 1.000000000
#> 4589 0.0007071421 0.0005016051 -0.2845615222 1.000000000
#> 4612 0.0030306092 0.0009028892  4.4068615224 1.000000000
#> 4617 0.0030306092 0.0016051364  2.1988692889 1.000000000
#> 4695 0.0012122437 0.0007022472  0.5403349383 1.000000000
#> 4740 0.0005051015 0.0005016051 -1.2501787419 1.000000000
#> 4753 0.0013132640 0.0018057785  0.1367343158 1.000000000
#> 4777 0.0010102031 0.0009028892 -0.9104455636 1.000000000
#> 4835 0.0006061218 0.0010032103  0.3330052647 1.000000000
#> 4954 0.0005051015 0.0006019262 -0.9094870300 1.000000000
#> 5049 0.0007071421 0.0005016051 -0.2754748346 1.000000000
#> 5053 0.0009091827 0.0006019262 -0.0022998328 1.000000000
#> 5058 0.0015153046 0.0018057785 -0.4987604508 1.000000000
#> 5059 0.0014142843 0.0008025682  0.7562255984 1.000000000
#> 5060 0.0005051015 0.0006019262 -0.8995818380 1.000000000
#> 5061 0.0018183655 0.0012038523  0.5273097783 1.000000000
#> 5065 0.0018183655 0.0011035313  0.8985006655 1.000000000
#> 5067 0.0015153046 0.0011035313  0.0271660577 1.000000000
#> 5068 0.0010102031 0.0005016051  0.8230244056 1.000000000
#> 5071 0.0007071421 0.0008025682 -0.9287955613 1.000000000
#> 5073 0.0008081624 0.0005016051  0.0457581975 1.000000000
#> 5074 0.0011112234 0.0005016051  1.2485337019 1.000000000
#> 5086 0.0026265279 0.0036115570  0.4964415178 1.000000000
#> 5088 0.0006061218 0.0013041734  1.2995434075 1.000000000
#> 5089 0.0021214264 0.0009028892  2.3063785078 1.000000000
#> 5090 0.0008081624 0.0009028892 -0.9684183102 1.000000000
#> 5091 0.0034346904 0.0043138042  0.3218952402 1.000000000
#> 5123 0.0010102031 0.0007022472 -0.0709743238 1.000000000
#> 5152 0.0034346904 0.0021067416  1.6522181837 1.000000000
#> 5157 0.0008081624 0.0006019262 -0.4240347105 1.000000000
#> 5159 0.0010102031 0.0007022472 -0.0687341300 1.000000000
#> 5160 0.0018183655 0.0005016051  3.1025786593 1.000000000
#> 5163 0.0013132640 0.0015048154 -0.7408186992 1.000000000
#> 5235 0.0007071421 0.0009028892 -0.4692882496 1.000000000
#> 5244 0.0008081624 0.0014044944  0.7922662211 1.000000000
#> 5245 0.0014142843 0.0031099518  2.7933475868 1.000000000
#> 5246 0.0016163249 0.0017054575 -1.1080343807 1.000000000
#> 5247 0.0007071421 0.0008025682 -0.9416724441 1.000000000
#> 5249 0.0036367310 0.0047150883  0.6290135971 1.000000000
#> 5258 0.0008081624 0.0005016051  0.0899454680 1.000000000
#> 5265 0.0016163249 0.0011035313  0.3148818469 1.000000000
#> 5266 0.0019193858 0.0016051364 -0.4702597005 1.000000000
#> 5269 0.0008081624 0.0007022472 -0.9379432182 1.000000000
#> 5273 0.0007071421 0.0007022472 -1.2654233532 1.000000000
#> 5275 0.0008081624 0.0007022472 -0.8924573159 1.000000000
#> 5277 0.0007071421 0.0008025682 -0.9352249867 1.000000000
#> 5281 0.0037377513 0.0042134831 -0.4590018808 1.000000000
#> 5284 0.0021214264 0.0020064205 -1.0213079269 1.000000000
#> 5285 0.0010102031 0.0011035313 -1.0775945395 1.000000000
#> 5286 0.0025255076 0.0073234350  6.5949844036 1.000000000
#> 5567 0.0009091827 0.0011035313 -0.5909771754 1.000000000
#> 5572 0.0009091827 0.0011035313 -0.5658198086 1.000000000
#> 5601 0.0006061218 0.0006019262 -1.2622688794 1.000000000
#> 5680 0.0008081624 0.0013041734  0.5163815336 1.000000000
#> 5919 0.0006061218 0.0008025682 -0.4403755595 1.000000000
#> 6018 0.0006061218 0.0009028892 -0.0420967718 1.000000000
#> 6253 0.0009091827 0.0005016051  0.4751775881 1.000000000
#> 6278 0.0005051015 0.0005016051 -1.2743398218 1.000000000
#> 6297 0.0008081624 0.0009028892 -0.9965921975 1.000000000
#> 6341 0.0010102031 0.0005016051  0.8573914093 1.000000000
#> 6397 0.0020204061 0.0013041734  0.7275557768 1.000000000
#> 6398 0.0017173452 0.0012038523  0.2511602394 1.000000000
#> 6401 0.0026265279 0.0012038523  2.4785939592 1.000000000
#> 6414 0.0019193858 0.0006019262  3.0368641427 1.000000000
#> 6429 0.0024244873 0.0024077047 -1.3197252705 1.000000000
#> 6432 0.0020204061 0.0007022472  2.8958621552 1.000000000
#> 6433 0.0007071421 0.0011035313  0.2035401046 1.000000000
#> 6434 0.0030306092 0.0030096308 -1.2882624722 1.000000000
#> 6634 0.0017173452 0.0011035313  0.6736782475 1.000000000
#> 6639 0.0016163249 0.0018057785 -0.8159638560 1.000000000
#> 6748 0.0008081624 0.0007022472 -0.8744274779 1.000000000
#> 6768 0.0008081624 0.0005016051  0.0376999147 1.000000000
#> 6771 0.0009091827 0.0009028892 -1.2999425600 1.000000000
#> 6819 0.0008081624 0.0006019262 -0.3760814282 1.000000000
#> 6822 0.0009091827 0.0010032103 -1.0303135899 1.000000000
#> 6973 0.0005051015 0.0011035313  1.2264399210 1.000000000
#> 7016 0.0007071421 0.0007022472 -1.2539207601 1.000000000
#> 7033 0.0011112234 0.0010032103 -0.9296899049 1.000000000
#> 7037 0.0016163249 0.0018057785 -0.7637776585 1.000000000
#> 7141 0.0008081624 0.0005016051  0.0540054850 1.000000000
#> 7146 0.0012122437 0.0009028892 -0.2948078703 1.000000000
#> 7152 0.0020204061 0.0006019262  3.4206390165 1.000000000
#> 7154 0.0014142843 0.0005016051  2.1586540658 1.000000000
#> 7169 0.0022224467 0.0020064205 -0.7944927013 1.000000000
#> 7172 0.0012122437 0.0012038523 -1.2765023196 1.000000000
#> 7174 0.0021214264 0.0025080257 -0.3659176444 1.000000000
#> 7236 0.0008081624 0.0009028892 -0.9264966537 1.000000000
#> 7313 0.0005051015 0.0006019262 -0.9308827717 1.000000000
#> 7396 0.0009091827 0.0005016051  0.5141567547 1.000000000
#> 7408 0.0011112234 0.0006019262  0.7450856718 1.000000000
#> 7479 0.0009091827 0.0011035313 -0.6451607814 1.000000000
#> 7480 0.0008081624 0.0005016051  0.0754067286 1.000000000
#> 7483 0.0016163249 0.0014044944 -0.6605541614 1.000000000
#> 7494 0.0006061218 0.0007022472 -0.9078957973 1.000000000
#> 7495 0.0010102031 0.0006019262  0.3673364350 1.000000000
#> 7508 0.0013132640 0.0012038523 -1.0212024130 1.000000000
#> 7509 0.0006061218 0.0005016051 -0.8895944913 1.000000000
#> 7512 0.0016163249 0.0025080257  0.9291092839 1.000000000
#> 7614 0.0008081624 0.0008025682 -1.2633748562 1.000000000
#> 7723 0.0005051015 0.0006019262 -0.9034082027 1.000000000
#> 8195 0.0006061218 0.0005016051 -0.8662946156 1.000000000
#> 8200 0.0007071421 0.0009028892 -0.4719011096 1.000000000
#> 8230 0.0005051015 0.0005016051 -1.2304085144 1.000000000
#> 8479 0.0008081624 0.0005016051  0.0194550755 1.000000000
#> 8492 0.0005051015 0.0006019262 -0.8940098292 1.000000000
#> 8496 0.0009091827 0.0008025682 -0.9320532668 1.000000000
#> 8643 0.0009091827 0.0009028892 -1.3911451291 1.000000000
#> 8648 0.0005051015 0.0011035313  1.1693705673 1.000000000
#> 8660 0.0008081624 0.0012038523  0.1163166976 1.000000000
#> 8670 0.0016163249 0.0010032103  0.7108428413 1.000000000
#> 8676 0.0013132640 0.0005016051  1.7253881653 1.000000000
#> 8677 0.0005051015 0.0007022472 -0.3843932418 1.000000000
#> 8691 0.0009091827 0.0008025682 -0.9087975235 1.000000000
#> 8694 0.0007071421 0.0009028892 -0.5000022869 1.000000000
#> 8696 0.0010102031 0.0018057785  1.1831892655 1.000000000
#> 8711 0.0028285685 0.0007022472  4.2847339472 1.000000000
#> 8738 0.0012122437 0.0010032103 -0.6159632864 1.000000000
#> 8745 0.0010102031 0.0005016051  0.8926087916 1.000000000
#> 8788 0.0005051015 0.0005016051 -1.2575106472 1.000000000
#> 8815 0.0006061218 0.0005016051 -0.8551683601 1.000000000
#> 8816 0.0015153046 0.0012038523 -0.3740682564 1.000000000
#> 8817 0.0011112234 0.0019060995  1.1045173088 1.000000000
#> 8818 0.0006061218 0.0007022472 -0.9286935080 1.000000000
#> 8820 0.0018183655 0.0032102729  1.7487159046 1.000000000
#> 8830 0.0009091827 0.0006019262 -0.1395732282 1.000000000
#> 8835 0.0018183655 0.0006019262  2.8191712825 1.000000000
#> 8836 0.0013132640 0.0015048154 -0.7835231712 1.000000000
#> 8847 0.0011112234 0.0006019262  0.6858270222 1.000000000
#> 8850 0.0006061218 0.0012038523  1.0426113569 1.000000000
#> 8853 0.0020204061 0.0040128411  2.8026823749 1.000000000
#> 8855 0.0007071421 0.0009028892 -0.4904774679 1.000000000
#> 8856 0.0013132640 0.0012038523 -0.9882772002 1.000000000
#> 8857 0.0012122437 0.0006019262  1.1620030853 1.000000000
#> 8858 0.0030306092 0.0048154093  2.0341499512 1.000000000
#> 8977 0.0015153046 0.0008025682  1.2160964120 1.000000000
#> 9085 0.0007071421 0.0008025682 -0.9992736806 1.000000000
#> 9086 0.0006061218 0.0011035313  0.7540269964 1.000000000
#> 9087 0.0020204061 0.0005016051  3.7099345909 1.000000000
#> 9090 0.0024244873 0.0015048154  1.0936730657 1.000000000
#> 9118 0.0005051015 0.0005016051 -1.2291859562 1.000000000
#> 9132 0.0005051015 0.0005016051 -1.3174359363 1.000000000
#> 9134 0.0007071421 0.0005016051 -0.3505021347 1.000000000
#> 9160 0.0009091827 0.0011035313 -0.5754497994 1.000000000
#> 9164 0.0005051015 0.0005016051 -1.2456358040 1.000000000
#> 9165 0.0019193858 0.0011035313  1.1363243500 1.000000000
#> 9338 0.0006061218 0.0006019262 -1.2553555198 1.000000000
#> 9412 0.0006061218 0.0008025682 -0.4534492024 1.000000000
#> 9416 0.0006061218 0.0007022472 -0.9581127409 1.000000000
#> 9459 0.0009091827 0.0012038523 -0.2892477746 1.000000000
#> 9461 0.0009091827 0.0015048154  0.6524250785 1.000000000
#> 9462 0.0006061218 0.0010032103  0.2809200532 1.000000000
#> 9464 0.0008081624 0.0022070626  2.7065208441 1.000000000
#> 9468 0.0020204061 0.0008025682  2.6443139320 1.000000000
#> 9470 0.0008081624 0.0010032103 -0.5440829793 1.000000000
#> 9474 0.0005051015 0.0009028892  0.5095545715 1.000000000
#> 9490 0.0020204061 0.0034109149  1.8967979433 1.000000000
#> 9492 0.0011112234 0.0008025682 -0.2386546209 1.000000000
#> 9493 0.0018183655 0.0010032103  1.2652450173 1.000000000
#> 9494 0.0011112234 0.0012038523 -1.0149220176 1.000000000
#> 9495 0.0028285685 0.0045144462  1.9130357996 1.000000000
#> 9527 0.0010102031 0.0011035313 -1.0158729482 1.000000000
#> 9556 0.0015153046 0.0012038523 -0.3312648290 1.000000000
#> 9559 0.0006061218 0.0009028892 -0.0606283926 1.000000000
#> 9564 0.0010102031 0.0006019262  0.3745372214 1.000000000
#> 9565 0.0009091827 0.0007022472 -0.4196846705 1.000000000
#> 9568 0.0020204061 0.0014044944  0.4821059810 1.000000000
#> 9605 0.0005051015 0.0006019262 -0.8704529671 1.000000000
#> 9612 0.0005051015 0.0007022472 -0.3724279539 1.000000000
#> 9614 0.0006061218 0.0006019262 -1.2524854815 1.000000000
#> 9630 0.0007071421 0.0007022472 -1.2205713234 1.000000000
#> 9643 0.0009091827 0.0028089888  3.7177938480 1.000000000
#> 9644 0.0012122437 0.0013041734 -1.0635376000 1.000000000
#> 9647 0.0030306092 0.0052166934  2.5740586297 1.000000000
#> 9655 0.0005051015 0.0010032103  0.9054719494 1.000000000
#> 9662 0.0008081624 0.0014044944  0.8457244658 1.000000000
#> 9663 0.0017173452 0.0014044944 -0.4055869698 1.000000000
#> 9665 0.0006061218 0.0005016051 -0.8984586715 1.000000000
#> 9668 0.0010102031 0.0007022472 -0.0956222864 1.000000000
#> 9673 0.0006061218 0.0008025682 -0.4095009557 1.000000000
#> 9675 0.0005051015 0.0007022472 -0.3455544943 1.000000000
#> 9678 0.0018183655 0.0056179775  6.0474910936 1.000000000
#> 9681 0.0019193858 0.0020064205 -1.1634267860 1.000000000
#> 9682 0.0006061218 0.0013041734  1.3771338892 1.000000000
#> 9683 0.0031316295 0.0076243981  3.6060076552 1.000000000
#> 9890 0.0011112234 0.0013041734 -0.6507366925 1.000000000
```

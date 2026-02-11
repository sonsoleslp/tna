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
#> 5                                                  discuss      2166     2101
#> 6                                                  emotion      1686     1389
#> 7                                                  monitor       668      848
#> 8                                                     plan      3102     3521
#> 9                                                synthesis       316      413
#> 10                                         adapt->cohesion        37      102
#> 11                                        adapt->consensus        73      170
#> 13                                          adapt->discuss         5       25
#> 14                                          adapt->emotion        20       41
#> 18                                      cohesion->cohesion        41        5
#> 19                                     cohesion->consensus       503      341
#> 20                                    cohesion->coregulate        76      126
#> 21                                       cohesion->discuss        38       63
#> 22                                       cohesion->emotion       111       85
#> 23                                       cohesion->monitor        16       40
#> 24                                          cohesion->plan       142       97
#> 26                                        consensus->adapt        14       16
#> 27                                     consensus->cohesion        67       27
#> 28                                    consensus->consensus       283      236
#> 29                                   consensus->coregulate       580      608
#> 30                                      consensus->discuss       789      401
#> 31                                      consensus->emotion       276      184
#> 32                                      consensus->monitor       120      175
#> 33                                         consensus->plan      1236     1269
#> 34                                    consensus->synthesis        27       21
#> 35                                       coregulate->adapt        20       12
#> 36                                    coregulate->cohesion        32       39
#> 37                                   coregulate->consensus        97      168
#> 38                                  coregulate->coregulate        12       34
#> 39                                     coregulate->discuss       210      329
#> 40                                     coregulate->emotion       182      157
#> 41                                     coregulate->monitor        86       84
#> 42                                        coregulate->plan       238      233
#> 43                                   coregulate->synthesis        17       20
#> 44                                          discuss->adapt        48      234
#> 45                                       discuss->cohesion       124       64
#> 46                                      discuss->consensus       851      418
#> 47                                     discuss->coregulate       145      188
#> 48                                        discuss->discuss       339      431
#> 49                                        discuss->emotion       225      193
#> 50                                        discuss->monitor        33       55
#> 51                                           discuss->plan        25       21
#> 52                                      discuss->synthesis       213      344
#> 54                                       emotion->cohesion       505      418
#> 55                                      emotion->consensus       521      388
#> 56                                     emotion->coregulate        36       61
#> 57                                        emotion->discuss       189      100
#> 58                                        emotion->emotion        97      121
#> 59                                        emotion->monitor        49       54
#> 60                                           emotion->plan       140      143
#> 62                                          monitor->adapt         7        9
#> 63                                       monitor->cohesion        31       49
#> 64                                      monitor->consensus       101      127
#> 65                                     monitor->coregulate        32       51
#> 66                                        monitor->discuss       234      304
#> 67                                        monitor->emotion        61       69
#> 68                                        monitor->monitor        12       14
#> 69                                           monitor->plan       143      166
#> 70                                      monitor->synthesis        12       11
#> 72                                          plan->cohesion        91       64
#> 73                                         plan->consensus       850      938
#> 74                                        plan->coregulate        69       37
#> 75                                           plan->discuss       174      244
#> 76                                           plan->emotion       527      377
#> 77                                           plan->monitor       219      246
#> 78                                              plan->plan       948     1356
#> 80                                        synthesis->adapt        40      113
#> 81                                     synthesis->cohesion         8       14
#> 82                                    synthesis->consensus       160      144
#> 84                                      synthesis->discuss         8       33
#> 85                                      synthesis->emotion        18       28
#> 87                                         synthesis->plan        40        9
#> 89                              adapt->cohesion->consensus        22       49
#> 99                            adapt->consensus->coregulate        14       42
#> 100                              adapt->consensus->discuss        18       17
#> 103                                 adapt->consensus->plan        21       61
#> 123                              adapt->emotion->consensus        10        8
#> 150                         cohesion->consensus->consensus        44       27
#> 151                        cohesion->consensus->coregulate        67       69
#> 152                           cohesion->consensus->discuss       129       44
#> 153                           cohesion->consensus->emotion        46       22
#> 154                           cohesion->consensus->monitor        11       13
#> 155                              cohesion->consensus->plan       169      135
#> 159                        cohesion->coregulate->consensus         7       21
#> 161                          cohesion->coregulate->discuss        15       31
#> 162                          cohesion->coregulate->emotion        15       20
#> 164                             cohesion->coregulate->plan        20       26
#> 168                           cohesion->discuss->consensus        15       12
#> 172                           cohesion->discuss->synthesis         8        5
#> 174                            cohesion->emotion->cohesion        30       27
#> 175                           cohesion->emotion->consensus        36       18
#> 177                             cohesion->emotion->discuss        13        9
#> 178                             cohesion->emotion->emotion         5        6
#> 180                                cohesion->emotion->plan        10       10
#> 185                             cohesion->monitor->discuss        10       17
#> 189                              cohesion->plan->consensus        36       31
#> 192                                cohesion->plan->emotion        28        9
#> 193                                cohesion->plan->monitor         8        5
#> 194                                   cohesion->plan->plan        43       36
#> 200                             consensus->adapt->cohesion         6        7
#> 201                            consensus->adapt->consensus         5        8
#> 203                         consensus->cohesion->consensus        33       10
#> 204                        consensus->cohesion->coregulate         8        5
#> 211                        consensus->consensus->consensus        27       19
#> 212                       consensus->consensus->coregulate        41       40
#> 213                          consensus->consensus->discuss        56       26
#> 214                          consensus->consensus->emotion        25       17
#> 215                          consensus->consensus->monitor        15       11
#> 216                             consensus->consensus->plan        82      105
#> 218                           consensus->coregulate->adapt        15        8
#> 219                        consensus->coregulate->cohesion        19       19
#> 220                       consensus->coregulate->consensus        53       90
#> 221                      consensus->coregulate->coregulate        11       24
#> 222                         consensus->coregulate->discuss       135      166
#> 223                         consensus->coregulate->emotion       110       85
#> 224                         consensus->coregulate->monitor        52       41
#> 225                            consensus->coregulate->plan       141      116
#> 226                       consensus->coregulate->synthesis         7        8
#> 227                              consensus->discuss->adapt        22       43
#> 228                           consensus->discuss->cohesion        39        7
#> 229                          consensus->discuss->consensus       291       87
#> 230                         consensus->discuss->coregulate        56       35
#> 231                            consensus->discuss->discuss       140       79
#> 232                            consensus->discuss->emotion        92       33
#> 233                            consensus->discuss->monitor        11        8
#> 234                               consensus->discuss->plan         7        5
#> 235                          consensus->discuss->synthesis        72       76
#> 237                           consensus->emotion->cohesion        72       52
#> 238                          consensus->emotion->consensus        86       52
#> 240                            consensus->emotion->discuss        33       17
#> 241                            consensus->emotion->emotion        18       16
#> 242                            consensus->emotion->monitor        13        9
#> 243                               consensus->emotion->plan        19       19
#> 246                           consensus->monitor->cohesion         6       12
#> 247                          consensus->monitor->consensus        17       23
#> 248                         consensus->monitor->coregulate         5        6
#> 249                            consensus->monitor->discuss        40       70
#> 250                            consensus->monitor->emotion         9       14
#> 252                               consensus->monitor->plan        30       27
#> 255                              consensus->plan->cohesion        37       18
#> 256                             consensus->plan->consensus       352      334
#> 257                            consensus->plan->coregulate        29       14
#> 258                               consensus->plan->discuss        67       91
#> 259                               consensus->plan->emotion       202      139
#> 260                               consensus->plan->monitor        86       86
#> 261                                  consensus->plan->plan       382      495
#> 265                        consensus->synthesis->consensus         9        5
#> 271                           coregulate->adapt->consensus         8        6
#> 277                        coregulate->cohesion->consensus        16       15
#> 282                             coregulate->cohesion->plan         5        5
#> 284                       coregulate->consensus->consensus        10       20
#> 285                      coregulate->consensus->coregulate        14       36
#> 286                         coregulate->consensus->discuss        16       20
#> 287                         coregulate->consensus->emotion        14        9
#> 289                            coregulate->consensus->plan        34       67
#> 299                             coregulate->discuss->adapt         6       32
#> 300                          coregulate->discuss->cohesion         9        8
#> 301                         coregulate->discuss->consensus        86       68
#> 302                        coregulate->discuss->coregulate        16       27
#> 303                           coregulate->discuss->discuss        27       65
#> 304                           coregulate->discuss->emotion        16       29
#> 305                           coregulate->discuss->monitor         5        6
#> 307                         coregulate->discuss->synthesis        20       63
#> 309                          coregulate->emotion->cohesion        56       44
#> 310                         coregulate->emotion->consensus        45       34
#> 312                           coregulate->emotion->discuss        25       18
#> 313                           coregulate->emotion->emotion        11       13
#> 315                              coregulate->emotion->plan        19       18
#> 319                         coregulate->monitor->consensus        14       16
#> 321                           coregulate->monitor->discuss        30       25
#> 324                              coregulate->monitor->plan        21       16
#> 327                             coregulate->plan->cohesion         6        5
#> 328                            coregulate->plan->consensus        64       56
#> 330                              coregulate->plan->discuss        16       12
#> 331                              coregulate->plan->emotion        36       29
#> 332                              coregulate->plan->monitor        19       15
#> 333                                 coregulate->plan->plan        71       99
#> 336                       coregulate->synthesis->consensus         6        8
#> 342                               discuss->adapt->cohesion        12       58
#> 343                              discuss->adapt->consensus        26       96
#> 346                                discuss->adapt->emotion         6       25
#> 351                           discuss->cohesion->consensus        64       30
#> 352                          discuss->cohesion->coregulate         7       10
#> 353                             discuss->cohesion->discuss         5        8
#> 354                             discuss->cohesion->emotion        12        6
#> 356                                discuss->cohesion->plan        13        5
#> 359                          discuss->consensus->consensus        59       28
#> 360                         discuss->consensus->coregulate       128       78
#> 361                            discuss->consensus->discuss       198       41
#> 362                            discuss->consensus->emotion        54       30
#> 363                            discuss->consensus->monitor        27       26
#> 364                               discuss->consensus->plan       302      181
#> 367                          discuss->coregulate->cohesion         7        7
#> 368                         discuss->coregulate->consensus        17       20
#> 370                           discuss->coregulate->discuss        32       64
#> 371                           discuss->coregulate->emotion        25       20
#> 372                           discuss->coregulate->monitor         9       18
#> 373                              discuss->coregulate->plan        32       38
#> 375                                discuss->discuss->adapt         6       49
#> 376                             discuss->discuss->cohesion        14       17
#> 377                            discuss->discuss->consensus       144       86
#> 378                           discuss->discuss->coregulate        16       33
#> 379                              discuss->discuss->discuss        53       98
#> 380                              discuss->discuss->emotion        33       41
#> 381                              discuss->discuss->monitor         5        8
#> 382                                 discuss->discuss->plan         7        5
#> 383                            discuss->discuss->synthesis        31       60
#> 384                             discuss->emotion->cohesion        62       61
#> 385                            discuss->emotion->consensus        70       63
#> 386                           discuss->emotion->coregulate         5        9
#> 387                              discuss->emotion->discuss        26       11
#> 388                              discuss->emotion->emotion         9       10
#> 389                              discuss->emotion->monitor         5        5
#> 390                                 discuss->emotion->plan        24       16
#> 396                              discuss->monitor->discuss        16       20
#> 402                               discuss->plan->consensus         8        7
#> 406                                    discuss->plan->plan         5        6
#> 407                              discuss->synthesis->adapt        29       89
#> 408                           discuss->synthesis->cohesion         5       12
#> 409                          discuss->synthesis->consensus       113      122
#> 412                            discuss->synthesis->emotion         8       23
#> 414                               discuss->synthesis->plan        28        9
#> 419                           emotion->cohesion->consensus       235      164
#> 420                          emotion->cohesion->coregulate        40       65
#> 421                             emotion->cohesion->discuss        20       35
#> 422                             emotion->cohesion->emotion        64       45
#> 423                             emotion->cohesion->monitor         7       24
#> 424                                emotion->cohesion->plan        70       46
#> 428                          emotion->consensus->consensus        51       27
#> 429                         emotion->consensus->coregulate        87       71
#> 430                            emotion->consensus->discuss       111       57
#> 431                            emotion->consensus->emotion        29       16
#> 432                            emotion->consensus->monitor        18       24
#> 433                               emotion->consensus->plan       165      155
#> 438                           emotion->coregulate->discuss         5       20
#> 439                           emotion->coregulate->emotion         8        6
#> 440                           emotion->coregulate->monitor         7        8
#> 441                              emotion->coregulate->plan        10       11
#> 445                            emotion->discuss->consensus        75       19
#> 446                           emotion->discuss->coregulate         8        9
#> 447                              emotion->discuss->discuss        34       21
#> 448                              emotion->discuss->emotion        19        8
#> 451                            emotion->discuss->synthesis        21       16
#> 453                             emotion->emotion->cohesion        35       36
#> 454                            emotion->emotion->consensus        29       33
#> 456                              emotion->emotion->discuss         9        5
#> 457                              emotion->emotion->emotion         9       17
#> 462                            emotion->monitor->consensus         5       10
#> 464                              emotion->monitor->discuss        15       18
#> 465                              emotion->monitor->emotion         5        5
#> 470                               emotion->plan->consensus        38       37
#> 472                                 emotion->plan->discuss        10       10
#> 473                                 emotion->plan->emotion        21       14
#> 474                                 emotion->plan->monitor         9       14
#> 475                                    emotion->plan->plan        43       53
#> 484                           monitor->cohesion->consensus        16       22
#> 491                          monitor->consensus->consensus         5       12
#> 492                         monitor->consensus->coregulate        20       21
#> 493                            monitor->consensus->discuss        27       18
#> 495                            monitor->consensus->monitor         5       10
#> 496                               monitor->consensus->plan        31       52
#> 500                         monitor->coregulate->consensus         5        9
#> 502                           monitor->coregulate->discuss         8       11
#> 505                              monitor->coregulate->plan         7        9
#> 508                             monitor->discuss->cohesion        23       16
#> 509                            monitor->discuss->consensus        88       54
#> 510                           monitor->discuss->coregulate        15       28
#> 511                              monitor->discuss->discuss        27       65
#> 512                              monitor->discuss->emotion        25       32
#> 515                            monitor->discuss->synthesis        25       50
#> 516                             monitor->emotion->cohesion        12       23
#> 517                            monitor->emotion->consensus        21       22
#> 522                                 monitor->emotion->plan        10        5
#> 530                               monitor->plan->consensus        33       49
#> 532                                 monitor->plan->discuss         5       10
#> 533                                 monitor->plan->emotion        24       20
#> 534                                 monitor->plan->monitor         8        8
#> 535                                    monitor->plan->plan        57       60
#> 547                              plan->cohesion->consensus        53       24
#> 550                                plan->cohesion->emotion         9        8
#> 552                                   plan->cohesion->plan        12        9
#> 554                              plan->consensus->cohesion        12       10
#> 555                             plan->consensus->consensus        62       65
#> 556                            plan->consensus->coregulate       147      184
#> 557                               plan->consensus->discuss       161      117
#> 558                               plan->consensus->emotion        71       60
#> 559                               plan->consensus->monitor        28       59
#> 560                                  plan->consensus->plan       289      354
#> 561                             plan->consensus->synthesis         6       10
#> 564                              plan->coregulate->discuss        10       13
#> 565                              plan->coregulate->emotion        13        5
#> 567                                 plan->coregulate->plan        23        8
#> 570                                plan->discuss->cohesion         6        7
#> 571                               plan->discuss->consensus        70       47
#> 572                              plan->discuss->coregulate        20       25
#> 573                                 plan->discuss->discuss        26       52
#> 574                                 plan->discuss->emotion        14       20
#> 577                               plan->discuss->synthesis        18       36
#> 579                                plan->emotion->cohesion       161      107
#> 580                               plan->emotion->consensus       167      107
#> 581                              plan->emotion->coregulate        11       22
#> 582                                 plan->emotion->discuss        61       24
#> 583                                 plan->emotion->emotion        30       35
#> 584                                 plan->emotion->monitor         9       14
#> 585                                    plan->emotion->plan        43       37
#> 588                                plan->monitor->cohesion         9       10
#> 589                               plan->monitor->consensus        34       44
#> 590                              plan->monitor->coregulate         9       18
#> 591                                 plan->monitor->discuss        76       75
#> 592                                 plan->monitor->emotion        22       16
#> 594                                    plan->monitor->plan        45       55
#> 597                                   plan->plan->cohesion        23       27
#> 598                                  plan->plan->consensus       256      363
#> 599                                 plan->plan->coregulate        17       16
#> 600                                    plan->plan->discuss        51       93
#> 601                                    plan->plan->emotion       164      137
#> 602                                    plan->plan->monitor        70       92
#> 603                                       plan->plan->plan       282      510
#> 609                             synthesis->adapt->cohesion         6       29
#> 610                            synthesis->adapt->consensus        20       48
#> 623                        synthesis->consensus->consensus         9       13
#> 624                       synthesis->consensus->coregulate        22       23
#> 625                          synthesis->consensus->discuss        31       26
#> 628                             synthesis->consensus->plan        58       58
#> 645                           synthesis->emotion->cohesion         5       10
#> 646                          synthesis->emotion->consensus         8        9
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
#>         prop_High     prop_Low
#> 1    0.0112965527 0.0288879235
#> 2    0.0741928431 0.0594410657
#> 3    0.2660884775 0.2277729511
#> 4    0.0698928650 0.0849985520
#> 5    0.1578602143 0.1521141037
#> 6    0.1228773413 0.1005647263
#> 7    0.0486844982 0.0613958876
#> 8    0.2260768166 0.2549232551
#> 9    0.0230303914 0.0299015349
#> 10   0.0029085764 0.0079612863
#> 11   0.0057385426 0.0132688105
#> 13   0.0003930509 0.0019512957
#> 14   0.0015722034 0.0032001249
#> 18   0.0032230171 0.0003902591
#> 19   0.0395409166 0.0266156728
#> 20   0.0059743731 0.0098345301
#> 21   0.0029871865 0.0049172651
#> 22   0.0087257291 0.0066344052
#> 23   0.0012577628 0.0031220731
#> 24   0.0111626444 0.0075710272
#> 26   0.0011005424 0.0012488292
#> 27   0.0052668815 0.0021073993
#> 28   0.0222466787 0.0184202310
#> 29   0.0455938999 0.0474555105
#> 30   0.0620234258 0.0312987824
#> 31   0.0216964075 0.0143615361
#> 32   0.0094332207 0.0136590696
#> 33   0.0971621728 0.0990477677
#> 34   0.0021224746 0.0016390884
#> 35   0.0015722034 0.0009366219
#> 36   0.0025155255 0.0030440212
#> 37   0.0076251867 0.0131127068
#> 38   0.0009433221 0.0026537621
#> 39   0.0165081362 0.0256790509
#> 40   0.0143070513 0.0122541367
#> 41   0.0067604748 0.0065563534
#> 42   0.0187092210 0.0181860756
#> 43   0.0013363729 0.0015610365
#> 44   0.0037732883 0.0182641274
#> 45   0.0097476613 0.0049953169
#> 46   0.0668972565 0.0326256634
#> 47   0.0113984750 0.0146737434
#> 48   0.0266488484 0.0336403372
#> 49   0.0176872887 0.0150640025
#> 50   0.0025941357 0.0042928505
#> 51   0.0019652543 0.0016390884
#> 52   0.0167439667 0.0268498283
#> 54   0.0396981369 0.0326256634
#> 55   0.0409558997 0.0302841086
#> 56   0.0028299662 0.0047611614
#> 57   0.0148573225 0.0078051826
#> 58   0.0076251867 0.0094442710
#> 59   0.0038518984 0.0042147986
#> 60   0.0110054241 0.0111614112
#> 62   0.0005502712 0.0007024664
#> 63   0.0024369153 0.0038245395
#> 64   0.0079396274 0.0099125820
#> 65   0.0025155255 0.0039806431
#> 66   0.0183947803 0.0237277552
#> 67   0.0047952205 0.0053855760
#> 68   0.0009433221 0.0010927256
#> 69   0.0112412546 0.0129566032
#> 70   0.0009433221 0.0008585701
#> 72   0.0071535257 0.0049953169
#> 73   0.0668186463 0.0732126132
#> 74   0.0054241019 0.0028879176
#> 75   0.0136781700 0.0190446456
#> 76   0.0414275607 0.0294255386
#> 77   0.0172156277 0.0192007493
#> 78   0.0745224432 0.1058382766
#> 80   0.0031444069 0.0088198564
#> 81   0.0006288814 0.0010927256
#> 82   0.0125776275 0.0112394630
#> 84   0.0006288814 0.0025757103
#> 85   0.0014149831 0.0021854511
#> 87   0.0031444069 0.0007024664
#> 89   0.0018769730 0.0041483237
#> 99   0.0011944373 0.0035557061
#> 100  0.0015357051 0.0014392144
#> 103  0.0017916560 0.0051642398
#> 123  0.0008531695 0.0006772773
#> 150  0.0037539459 0.0022858110
#> 151  0.0057162358 0.0058415171
#> 152  0.0110058869 0.0037250254
#> 153  0.0039245798 0.0018625127
#> 154  0.0009384865 0.0011005757
#> 155  0.0144185650 0.0114290552
#> 159  0.0005972187 0.0017778530
#> 161  0.0012797543 0.0026244497
#> 162  0.0012797543 0.0016931934
#> 164  0.0017063390 0.0022011514
#> 168  0.0012797543 0.0010159160
#> 172  0.0006825356 0.0004232983
#> 174  0.0025595086 0.0022858110
#> 175  0.0030714103 0.0015238740
#> 177  0.0011091204 0.0007619370
#> 178  0.0004265848 0.0005079580
#> 180  0.0008531695 0.0008465967
#> 185  0.0008531695 0.0014392144
#> 189  0.0030714103 0.0026244497
#> 192  0.0023888747 0.0007619370
#> 193  0.0006825356 0.0004232983
#> 194  0.0036686290 0.0030477481
#> 200  0.0005119017 0.0005926177
#> 201  0.0004265848 0.0006772773
#> 203  0.0028154594 0.0008465967
#> 204  0.0006825356 0.0004232983
#> 211  0.0023035577 0.0016085337
#> 212  0.0034979951 0.0033863867
#> 213  0.0047777493 0.0022011514
#> 214  0.0021329238 0.0014392144
#> 215  0.0012797543 0.0009312563
#> 216  0.0069959901 0.0088892652
#> 218  0.0012797543 0.0006772773
#> 219  0.0016210221 0.0016085337
#> 220  0.0045217985 0.0076193701
#> 221  0.0009384865 0.0020318320
#> 222  0.0115177886 0.0140535049
#> 223  0.0093848648 0.0071960718
#> 224  0.0044364815 0.0034710464
#> 225  0.0120296903 0.0098205215
#> 226  0.0005972187 0.0006772773
#> 227  0.0018769730 0.0036403657
#> 228  0.0033273611 0.0005926177
#> 229  0.0248272332 0.0073653911
#> 230  0.0047777493 0.0029630884
#> 231  0.0119443733 0.0066881138
#> 232  0.0078491596 0.0027937690
#> 233  0.0009384865 0.0006772773
#> 234  0.0005972187 0.0004232983
#> 235  0.0061428206 0.0064341348
#> 237  0.0061428206 0.0044023027
#> 238  0.0073372579 0.0044023027
#> 240  0.0028154594 0.0014392144
#> 241  0.0015357051 0.0013545547
#> 242  0.0011091204 0.0007619370
#> 243  0.0016210221 0.0016085337
#> 246  0.0005119017 0.0010159160
#> 247  0.0014503882 0.0019471724
#> 248  0.0004265848 0.0005079580
#> 249  0.0034126781 0.0059261768
#> 250  0.0007678526 0.0011852354
#> 252  0.0025595086 0.0022858110
#> 255  0.0031567272 0.0015238740
#> 256  0.0300315673 0.0282763292
#> 257  0.0024741916 0.0011852354
#> 258  0.0057162358 0.0077040298
#> 259  0.0172340244 0.0117676939
#> 260  0.0073372579 0.0072807315
#> 261  0.0325910758 0.0419065357
#> 265  0.0007678526 0.0004232983
#> 271  0.0006825356 0.0005079580
#> 277  0.0013650712 0.0012698950
#> 282  0.0004265848 0.0004232983
#> 284  0.0008531695 0.0016931934
#> 285  0.0011944373 0.0030477481
#> 286  0.0013650712 0.0016931934
#> 287  0.0011944373 0.0007619370
#> 289  0.0029007764 0.0056721978
#> 299  0.0005119017 0.0027091094
#> 300  0.0007678526 0.0006772773
#> 301  0.0073372579 0.0057568574
#> 302  0.0013650712 0.0022858110
#> 303  0.0023035577 0.0055028784
#> 304  0.0013650712 0.0024551304
#> 305  0.0004265848 0.0005079580
#> 307  0.0017063390 0.0053335591
#> 309  0.0047777493 0.0037250254
#> 310  0.0038392629 0.0028784287
#> 312  0.0021329238 0.0015238740
#> 313  0.0009384865 0.0011005757
#> 315  0.0016210221 0.0015238740
#> 319  0.0011944373 0.0013545547
#> 321  0.0025595086 0.0021164917
#> 324  0.0017916560 0.0013545547
#> 327  0.0005119017 0.0004232983
#> 328  0.0054602850 0.0047409414
#> 330  0.0013650712 0.0010159160
#> 331  0.0030714103 0.0024551304
#> 332  0.0016210221 0.0012698950
#> 333  0.0060575036 0.0083813071
#> 336  0.0005119017 0.0006772773
#> 342  0.0010238034 0.0049102608
#> 343  0.0022182408 0.0081273281
#> 346  0.0005119017 0.0021164917
#> 351  0.0054602850 0.0025397900
#> 352  0.0005972187 0.0008465967
#> 353  0.0004265848 0.0006772773
#> 354  0.0010238034 0.0005079580
#> 356  0.0011091204 0.0004232983
#> 359  0.0050337002 0.0023704707
#> 360  0.0109205699 0.0066034541
#> 361  0.0168927566 0.0034710464
#> 362  0.0046071154 0.0025397900
#> 363  0.0023035577 0.0022011514
#> 364  0.0257657196 0.0153233999
#> 367  0.0005972187 0.0005926177
#> 368  0.0014503882 0.0016931934
#> 370  0.0027301425 0.0054182188
#> 371  0.0021329238 0.0016931934
#> 372  0.0007678526 0.0015238740
#> 373  0.0027301425 0.0032170674
#> 375  0.0005119017 0.0041483237
#> 376  0.0011944373 0.0014392144
#> 377  0.0122856412 0.0072807315
#> 378  0.0013650712 0.0027937690
#> 379  0.0045217985 0.0082966475
#> 380  0.0028154594 0.0034710464
#> 381  0.0004265848 0.0006772773
#> 382  0.0005972187 0.0004232983
#> 383  0.0026448255 0.0050795801
#> 384  0.0052896511 0.0051642398
#> 385  0.0059721867 0.0053335591
#> 386  0.0004265848 0.0007619370
#> 387  0.0022182408 0.0009312563
#> 388  0.0007678526 0.0008465967
#> 389  0.0004265848 0.0004232983
#> 390  0.0020476069 0.0013545547
#> 396  0.0013650712 0.0016931934
#> 402  0.0006825356 0.0005926177
#> 406  0.0004265848 0.0005079580
#> 407  0.0024741916 0.0075347105
#> 408  0.0004265848 0.0010159160
#> 409  0.0096408156 0.0103284795
#> 412  0.0006825356 0.0019471724
#> 414  0.0023888747 0.0007619370
#> 419  0.0200494838 0.0138841856
#> 420  0.0034126781 0.0055028784
#> 421  0.0017063390 0.0029630884
#> 422  0.0054602850 0.0038096851
#> 423  0.0005972187 0.0020318320
#> 424  0.0059721867 0.0038943447
#> 428  0.0043511646 0.0022858110
#> 429  0.0074225749 0.0060108364
#> 430  0.0094701817 0.0048256011
#> 431  0.0024741916 0.0013545547
#> 432  0.0015357051 0.0020318320
#> 433  0.0140772972 0.0131222486
#> 438  0.0004265848 0.0016931934
#> 439  0.0006825356 0.0005079580
#> 440  0.0005972187 0.0006772773
#> 441  0.0008531695 0.0009312563
#> 445  0.0063987714 0.0016085337
#> 446  0.0006825356 0.0007619370
#> 447  0.0029007764 0.0017778530
#> 448  0.0016210221 0.0006772773
#> 451  0.0017916560 0.0013545547
#> 453  0.0029860933 0.0030477481
#> 454  0.0024741916 0.0027937690
#> 456  0.0007678526 0.0004232983
#> 457  0.0007678526 0.0014392144
#> 462  0.0004265848 0.0008465967
#> 464  0.0012797543 0.0015238740
#> 465  0.0004265848 0.0004232983
#> 470  0.0032420442 0.0031324077
#> 472  0.0008531695 0.0008465967
#> 473  0.0017916560 0.0011852354
#> 474  0.0007678526 0.0011852354
#> 475  0.0036686290 0.0044869624
#> 484  0.0013650712 0.0018625127
#> 491  0.0004265848 0.0010159160
#> 492  0.0017063390 0.0017778530
#> 493  0.0023035577 0.0015238740
#> 495  0.0004265848 0.0008465967
#> 496  0.0026448255 0.0044023027
#> 500  0.0004265848 0.0007619370
#> 502  0.0006825356 0.0009312563
#> 505  0.0005972187 0.0007619370
#> 508  0.0019622899 0.0013545547
#> 509  0.0075078918 0.0045716221
#> 510  0.0012797543 0.0023704707
#> 511  0.0023035577 0.0055028784
#> 512  0.0021329238 0.0027091094
#> 515  0.0021329238 0.0042329834
#> 516  0.0010238034 0.0019471724
#> 517  0.0017916560 0.0018625127
#> 522  0.0008531695 0.0004232983
#> 530  0.0028154594 0.0041483237
#> 532  0.0004265848 0.0008465967
#> 533  0.0020476069 0.0016931934
#> 534  0.0006825356 0.0006772773
#> 535  0.0048630663 0.0050795801
#> 547  0.0045217985 0.0020318320
#> 550  0.0007678526 0.0006772773
#> 552  0.0010238034 0.0007619370
#> 554  0.0010238034 0.0008465967
#> 555  0.0052896511 0.0055028784
#> 556  0.0125415920 0.0155773789
#> 557  0.0137360293 0.0099051812
#> 558  0.0060575036 0.0050795801
#> 559  0.0023888747 0.0049949204
#> 560  0.0246565993 0.0299695225
#> 561  0.0005119017 0.0008465967
#> 564  0.0008531695 0.0011005757
#> 565  0.0011091204 0.0004232983
#> 567  0.0019622899 0.0006772773
#> 570  0.0005119017 0.0005926177
#> 571  0.0059721867 0.0039790044
#> 572  0.0017063390 0.0021164917
#> 573  0.0022182408 0.0044023027
#> 574  0.0011944373 0.0016931934
#> 577  0.0015357051 0.0030477481
#> 579  0.0137360293 0.0090585845
#> 580  0.0142479311 0.0090585845
#> 581  0.0009384865 0.0018625127
#> 582  0.0052043341 0.0020318320
#> 583  0.0025595086 0.0029630884
#> 584  0.0007678526 0.0011852354
#> 585  0.0036686290 0.0031324077
#> 588  0.0007678526 0.0008465967
#> 589  0.0029007764 0.0037250254
#> 590  0.0007678526 0.0015238740
#> 591  0.0064840884 0.0063494751
#> 592  0.0018769730 0.0013545547
#> 594  0.0038392629 0.0046562817
#> 597  0.0019622899 0.0022858110
#> 598  0.0218411398 0.0307314595
#> 599  0.0014503882 0.0013545547
#> 600  0.0043511646 0.0078733491
#> 601  0.0139919802 0.0115983745
#> 602  0.0059721867 0.0077886895
#> 603  0.0240593806 0.0431764307
#> 609  0.0005119017 0.0024551304
#> 610  0.0017063390 0.0040636641
#> 623  0.0007678526 0.0011005757
#> 624  0.0018769730 0.0019471724
#> 625  0.0026448255 0.0022011514
#> 628  0.0049483832 0.0049102608
#> 645  0.0004265848 0.0008465967
#> 646  0.0006825356 0.0007619370
#> 661  0.0008531695 0.0004232983
#> 669  0.0008358098 0.0015658101
#> 702  0.0005572065 0.0013815971
#> 726  0.0004643388 0.0011052777
#> 731  0.0005572065 0.0031316202
#> 845  0.0010215453 0.0011973842
#> 850  0.0016716196 0.0021184489
#> 851  0.0009286776 0.0004605324
#> 853  0.0011144131 0.0009210648
#> 856  0.0039004458 0.0011052777
#> 857  0.0009286776 0.0005526389
#> 858  0.0025074294 0.0011973842
#> 859  0.0008358098 0.0004605324
#> 864  0.0016716196 0.0005526389
#> 876  0.0048291233 0.0034079396
#> 878  0.0009286776 0.0008289583
#> 879  0.0026002972 0.0017500230
#> 880  0.0006500743 0.0011052777
#> 881  0.0045505201 0.0042368979
#> 899  0.0004643388 0.0004605324
#> 942  0.0007429421 0.0004605324
#> 962  0.0010215453 0.0015658101
#> 974  0.0008358098 0.0008289583
#> 1035 0.0008358098 0.0005526389
#> 1039 0.0012072808 0.0011052777
#> 1065 0.0014858841 0.0007368518
#> 1067 0.0006500743 0.0004605324
#> 1069 0.0010215453 0.0014737036
#> 1119 0.0011144131 0.0008289583
#> 1123 0.0005572065 0.0011052777
#> 1124 0.0011144131 0.0006447453
#> 1126 0.0012072808 0.0008289583
#> 1130 0.0022288262 0.0005526389
#> 1134 0.0004643388 0.0007368518
#> 1136 0.0005572065 0.0004605324
#> 1145 0.0007429421 0.0005526389
#> 1149 0.0033432392 0.0030395137
#> 1152 0.0012072808 0.0009210648
#> 1154 0.0018573551 0.0032237266
#> 1165 0.0008358098 0.0004605324
#> 1170 0.0006500743 0.0009210648
#> 1171 0.0006500743 0.0022105554
#> 1172 0.0009286776 0.0011973842
#> 1175 0.0017644874 0.0030395137
#> 1187 0.0055720654 0.0032237266
#> 1188 0.0008358098 0.0011052777
#> 1189 0.0011144131 0.0029474072
#> 1190 0.0008358098 0.0011052777
#> 1193 0.0013001486 0.0030395137
#> 1194 0.0030646360 0.0016579166
#> 1195 0.0026002972 0.0021184489
#> 1197 0.0013930163 0.0009210648
#> 1198 0.0005572065 0.0006447453
#> 1200 0.0011144131 0.0009210648
#> 1204 0.0009286776 0.0009210648
#> 1206 0.0015787519 0.0009210648
#> 1208 0.0012072808 0.0009210648
#> 1212 0.0034361070 0.0023947684
#> 1214 0.0006500743 0.0006447453
#> 1215 0.0020430906 0.0016579166
#> 1216 0.0013001486 0.0004605324
#> 1217 0.0036218425 0.0045132173
#> 1224 0.0011144131 0.0018421295
#> 1240 0.0039004458 0.0012894907
#> 1241 0.0058506686 0.0009210648
#> 1242 0.0014858841 0.0005526389
#> 1243 0.0006500743 0.0010131712
#> 1244 0.0095653789 0.0033158331
#> 1249 0.0011144131 0.0013815971
#> 1252 0.0012072808 0.0008289583
#> 1255 0.0047362556 0.0009210648
#> 1256 0.0005572065 0.0007368518
#> 1257 0.0024145617 0.0014737036
#> 1258 0.0014858841 0.0006447453
#> 1261 0.0013001486 0.0012894907
#> 1262 0.0021359584 0.0011052777
#> 1263 0.0030646360 0.0010131712
#> 1281 0.0008358098 0.0023947684
#> 1283 0.0037147103 0.0025789813
#> 1291 0.0034361070 0.0013815971
#> 1292 0.0006500743 0.0008289583
#> 1294 0.0009286776 0.0004605324
#> 1296 0.0006500743 0.0009210648
#> 1299 0.0019502229 0.0009210648
#> 1300 0.0015787519 0.0009210648
#> 1303 0.0022288262 0.0018421295
#> 1316 0.0008358098 0.0005526389
#> 1332 0.0005572065 0.0005526389
#> 1354 0.0014858841 0.0014737036
#> 1360 0.0004643388 0.0011973842
#> 1372 0.0006500743 0.0007368518
#> 1377 0.0013001486 0.0008289583
#> 1384 0.0023216939 0.0006447453
#> 1391 0.0007429421 0.0004605324
#> 1392 0.0028789004 0.0018421295
#> 1393 0.0059435364 0.0060790274
#> 1394 0.0059435364 0.0049737497
#> 1395 0.0031575037 0.0018421295
#> 1396 0.0012072808 0.0013815971
#> 1397 0.0100297177 0.0110527770
#> 1408 0.0027860327 0.0018421295
#> 1409 0.0008358098 0.0005526389
#> 1410 0.0012072808 0.0020263425
#> 1411 0.0005572065 0.0005526389
#> 1415 0.0062221397 0.0040526849
#> 1416 0.0059435364 0.0037763655
#> 1418 0.0016716196 0.0005526389
#> 1419 0.0011144131 0.0007368518
#> 1420 0.0004643388 0.0005526389
#> 1421 0.0013001486 0.0012894907
#> 1425 0.0007429421 0.0014737036
#> 1427 0.0031575037 0.0022105554
#> 1430 0.0016716196 0.0022105554
#> 1433 0.0007429421 0.0009210648
#> 1434 0.0085438336 0.0129870130
#> 1435 0.0009286776 0.0005526389
#> 1436 0.0023216939 0.0029474072
#> 1437 0.0057578009 0.0043290043
#> 1438 0.0028789004 0.0031316202
#> 1439 0.0109583952 0.0172239108
#> 1484 0.0004643388 0.0007368518
#> 1520 0.0008358098 0.0006447453
#> 1535 0.0009286776 0.0012894907
#> 1538 0.0004643388 0.0009210648
#> 1540 0.0010215453 0.0030395137
#> 1573 0.0006500743 0.0006447453
#> 1574 0.0012072808 0.0012894907
#> 1576 0.0004643388 0.0004605324
#> 1578 0.0028789004 0.0026710878
#> 1580 0.0004643388 0.0008289583
#> 1587 0.0013930163 0.0015658101
#> 1593 0.0004643388 0.0008289583
#> 1610 0.0012072808 0.0026710878
#> 1618 0.0020430906 0.0018421295
#> 1621 0.0008358098 0.0004605324
#> 1626 0.0004643388 0.0004605324
#> 1627 0.0007429421 0.0004605324
#> 1630 0.0016716196 0.0014737036
#> 1660 0.0004643388 0.0005526389
#> 1684 0.0012072808 0.0005526389
#> 1694 0.0005572065 0.0004605324
#> 1698 0.0006500743 0.0004605324
#> 1709 0.0009286776 0.0008289583
#> 1710 0.0011144131 0.0006447453
#> 1711 0.0005572065 0.0008289583
#> 1713 0.0016716196 0.0018421295
#> 1725 0.0013930163 0.0004605324
#> 1726 0.0007429421 0.0011052777
#> 1735 0.0004643388 0.0007368518
#> 1739 0.0019502229 0.0023947684
#> 1742 0.0016716196 0.0010131712
#> 1743 0.0005572065 0.0009210648
#> 1744 0.0014858841 0.0028553007
#> 1760 0.0007429421 0.0025789813
#> 1769 0.0005572065 0.0021184489
#> 1770 0.0006500743 0.0006447453
#> 1773 0.0004643388 0.0029474072
#> 1805 0.0008358098 0.0010131712
#> 1806 0.0018573551 0.0006447453
#> 1809 0.0019502229 0.0006447453
#> 1843 0.0007429421 0.0005526389
#> 1847 0.0019502229 0.0011973842
#> 1850 0.0010215453 0.0010131712
#> 1852 0.0026931649 0.0011973842
#> 1853 0.0021359584 0.0011973842
#> 1854 0.0010215453 0.0007368518
#> 1855 0.0029717682 0.0014737036
#> 1859 0.0063150074 0.0007368518
#> 1861 0.0033432392 0.0005526389
#> 1865 0.0015787519 0.0008289583
#> 1866 0.0014858841 0.0007368518
#> 1867 0.0015787519 0.0005526389
#> 1869 0.0006500743 0.0004605324
#> 1877 0.0010215453 0.0004605324
#> 1884 0.0082652303 0.0041447914
#> 1886 0.0013001486 0.0011052777
#> 1887 0.0042719168 0.0017500230
#> 1888 0.0022288262 0.0007368518
#> 1889 0.0088224368 0.0074606245
#> 1903 0.0005572065 0.0007368518
#> 1905 0.0009286776 0.0012894907
#> 1906 0.0005572065 0.0005526389
#> 1907 0.0005572065 0.0011052777
#> 1910 0.0009286776 0.0008289583
#> 1924 0.0008358098 0.0010131712
#> 1929 0.0011144131 0.0011973842
#> 1940 0.0007429421 0.0008289583
#> 1947 0.0005572065 0.0006447453
#> 1948 0.0018573551 0.0011973842
#> 1949 0.0033432392 0.0006447453
#> 1950 0.0013930163 0.0008289583
#> 1952 0.0047362556 0.0034079396
#> 1963 0.0014858841 0.0023947684
#> 1965 0.0007429421 0.0017500230
#> 1966 0.0007429421 0.0005526389
#> 1969 0.0005572065 0.0011973842
#> 1970 0.0009286776 0.0012894907
#> 1971 0.0008358098 0.0012894907
#> 1987 0.0004643388 0.0011973842
#> 1989 0.0012072808 0.0019342360
#> 1997 0.0028789004 0.0025789813
#> 1998 0.0005572065 0.0008289583
#> 2000 0.0004643388 0.0007368518
#> 2005 0.0005572065 0.0007368518
#> 2006 0.0007429421 0.0011052777
#> 2010 0.0026931649 0.0023026619
#> 2033 0.0006500743 0.0007368518
#> 2036 0.0004643388 0.0006447453
#> 2082 0.0004643388 0.0022105554
#> 2083 0.0012072808 0.0032237266
#> 2097 0.0016716196 0.0018421295
#> 2098 0.0021359584 0.0018421295
#> 2101 0.0037147103 0.0044211108
#> 2130 0.0004643388 0.0004605324
#> 2145 0.0021359584 0.0014737036
#> 2146 0.0028789004 0.0028553007
#> 2147 0.0056649331 0.0017500230
#> 2148 0.0022288262 0.0008289583
#> 2150 0.0073365527 0.0064474533
#> 2153 0.0005572065 0.0011052777
#> 2155 0.0004643388 0.0018421295
#> 2156 0.0008358098 0.0008289583
#> 2158 0.0007429421 0.0010131712
#> 2162 0.0004643388 0.0007368518
#> 2168 0.0014858841 0.0011973842
#> 2169 0.0019502229 0.0008289583
#> 2171 0.0008358098 0.0005526389
#> 2183 0.0015787519 0.0011973842
#> 2186 0.0012072808 0.0005526389
#> 2188 0.0020430906 0.0013815971
#> 2199 0.0007429421 0.0005526389
#> 2203 0.0012072808 0.0009210648
#> 2207 0.0005572065 0.0008289583
#> 2209 0.0020430906 0.0018421295
#> 2210 0.0016716196 0.0011052777
#> 2212 0.0020430906 0.0010131712
#> 2214 0.0006500743 0.0005526389
#> 2216 0.0038075780 0.0008289583
#> 2217 0.0006500743 0.0006447453
#> 2218 0.0018573551 0.0007368518
#> 2219 0.0013001486 0.0004605324
#> 2222 0.0010215453 0.0012894907
#> 2224 0.0012072808 0.0004605324
#> 2238 0.0043647845 0.0039605784
#> 2240 0.0006500743 0.0010131712
#> 2241 0.0023216939 0.0017500230
#> 2242 0.0013001486 0.0008289583
#> 2243 0.0047362556 0.0051579626
#> 2292 0.0012072808 0.0005526389
#> 2296 0.0025074294 0.0009210648
#> 2323 0.0013001486 0.0004605324
#> 2330 0.0016716196 0.0006447453
#> 2333 0.0004643388 0.0006447453
#> 2338 0.0004643388 0.0005526389
#> 2341 0.0013930163 0.0019342360
#> 2355 0.0006500743 0.0004605324
#> 2408 0.0007429421 0.0006447453
#> 2412 0.0007429421 0.0012894907
#> 2422 0.0009286776 0.0004605324
#> 2436 0.0008358098 0.0009210648
#> 2441 0.0010215453 0.0018421295
#> 2461 0.0005572065 0.0004605324
#> 2510 0.0011144131 0.0014737036
#> 2515 0.0006500743 0.0021184489
#> 2558 0.0010215453 0.0004605324
#> 2565 0.0007429421 0.0005526389
#> 2566 0.0012072808 0.0007368518
#> 2567 0.0019502229 0.0009210648
#> 2570 0.0025074294 0.0021184489
#> 2575 0.0004643388 0.0007368518
#> 2581 0.0012072808 0.0011052777
#> 2587 0.0004643388 0.0008289583
#> 2588 0.0008358098 0.0011052777
#> 2603 0.0013001486 0.0018421295
#> 2609 0.0006500743 0.0008289583
#> 2621 0.0007429421 0.0005526389
#> 2656 0.0004643388 0.0010131712
#> 2660 0.0011144131 0.0018421295
#> 2669 0.0006500743 0.0008289583
#> 2682 0.0017644874 0.0013815971
#> 2685 0.0008358098 0.0005526389
#> 2687 0.0015787519 0.0021184489
#> 2722 0.0013930163 0.0010131712
#> 2754 0.0005572065 0.0007368518
#> 2755 0.0012072808 0.0010131712
#> 2756 0.0006500743 0.0008289583
#> 2759 0.0016716196 0.0023026619
#> 2763 0.0015787519 0.0031316202
#> 2765 0.0032503715 0.0042368979
#> 2766 0.0027860327 0.0021184489
#> 2767 0.0011144131 0.0012894907
#> 2768 0.0033432392 0.0037763655
#> 2772 0.0052005944 0.0019342360
#> 2773 0.0007429421 0.0010131712
#> 2774 0.0030646360 0.0022105554
#> 2775 0.0023216939 0.0010131712
#> 2778 0.0013930163 0.0020263425
#> 2780 0.0016716196 0.0019342360
#> 2781 0.0017644874 0.0012894907
#> 2783 0.0007429421 0.0005526389
#> 2784 0.0004643388 0.0007368518
#> 2788 0.0004643388 0.0006447453
#> 2790 0.0007429421 0.0021184489
#> 2793 0.0006500743 0.0010131712
#> 2796 0.0005572065 0.0007368518
#> 2797 0.0058506686 0.0086580087
#> 2798 0.0006500743 0.0005526389
#> 2799 0.0017644874 0.0023026619
#> 2800 0.0048291233 0.0033158331
#> 2801 0.0023216939 0.0026710878
#> 2802 0.0090081724 0.0117896288
#> 2848 0.0007429421 0.0010131712
#> 2852 0.0027860327 0.0018421295
#> 2859 0.0005572065 0.0004605324
#> 2863 0.0011144131 0.0008289583
#> 2869 0.0004643388 0.0006447453
#> 2871 0.0005572065 0.0008289583
#> 2888 0.0006500743 0.0006447453
#> 2895 0.0070579495 0.0042368979
#> 2896 0.0007429421 0.0013815971
#> 2897 0.0009286776 0.0005526389
#> 2898 0.0020430906 0.0011973842
#> 2900 0.0018573551 0.0012894907
#> 2903 0.0014858841 0.0007368518
#> 2904 0.0026002972 0.0020263425
#> 2905 0.0034361070 0.0016579166
#> 2907 0.0006500743 0.0006447453
#> 2908 0.0043647845 0.0033158331
#> 2927 0.0008358098 0.0010131712
#> 2928 0.0007429421 0.0009210648
#> 2942 0.0013001486 0.0007368518
#> 2947 0.0013001486 0.0013815971
#> 2959 0.0006500743 0.0004605324
#> 2960 0.0006500743 0.0006447453
#> 2963 0.0008358098 0.0018421295
#> 2973 0.0020430906 0.0011052777
#> 2974 0.0004643388 0.0007368518
#> 2975 0.0012072808 0.0018421295
#> 2976 0.0012072808 0.0004605324
#> 2979 0.0006500743 0.0008289583
#> 2980 0.0004643388 0.0006447453
#> 2981 0.0005572065 0.0004605324
#> 2993 0.0006500743 0.0012894907
#> 2996 0.0007429421 0.0005526389
#> 2998 0.0017644874 0.0017500230
#> 3006 0.0010215453 0.0008289583
#> 3014 0.0016716196 0.0027631943
#> 3015 0.0035289747 0.0069079856
#> 3016 0.0043647845 0.0036842590
#> 3017 0.0014858841 0.0022105554
#> 3018 0.0007429421 0.0024868748
#> 3019 0.0091939079 0.0123422677
#> 3028 0.0004643388 0.0005526389
#> 3029 0.0016716196 0.0019342360
#> 3031 0.0006500743 0.0013815971
#> 3035 0.0006500743 0.0007368518
#> 3036 0.0038075780 0.0030395137
#> 3037 0.0052005944 0.0034079396
#> 3038 0.0005572065 0.0006447453
#> 3039 0.0020430906 0.0009210648
#> 3040 0.0010215453 0.0017500230
#> 3042 0.0013001486 0.0013815971
#> 3045 0.0013001486 0.0015658101
#> 3047 0.0018573551 0.0022105554
#> 3048 0.0006500743 0.0006447453
#> 3050 0.0013930163 0.0020263425
#> 3053 0.0006500743 0.0006447453
#> 3054 0.0072436850 0.0118817353
#> 3056 0.0008358098 0.0037763655
#> 3057 0.0046433878 0.0049737497
#> 3058 0.0018573551 0.0028553007
#> 3059 0.0077080238 0.0179607626
#> 3078 0.0005572065 0.0005526389
#> 3081 0.0005572065 0.0015658101
#> 3134 0.0009286776 0.0007368518
#> 3153 0.0010215453 0.0011973842
#> 3154 0.0004643388 0.0005526389
#> 3157 0.0020430906 0.0021184489
#> 3265 0.0005051015 0.0007022472
#> 3575 0.0007071421 0.0005016051
#> 3610 0.0014142843 0.0005016051
#> 3675 0.0005051015 0.0011035313
#> 3676 0.0010102031 0.0005016051
#> 3679 0.0016163249 0.0011035313
#> 3688 0.0008081624 0.0012038523
#> 3702 0.0015153046 0.0014044944
#> 3704 0.0010102031 0.0007022472
#> 3706 0.0015153046 0.0010032103
#> 4010 0.0006061218 0.0006019262
#> 4250 0.0013132640 0.0015048154
#> 4272 0.0009091827 0.0012038523
#> 4277 0.0005051015 0.0010032103
#> 4340 0.0005051015 0.0007022472
#> 4344 0.0006061218 0.0015048154
#> 4377 0.0008081624 0.0007022472
#> 4381 0.0022224467 0.0014044944
#> 4389 0.0006061218 0.0010032103
#> 4395 0.0005051015 0.0005016051
#> 4411 0.0009091827 0.0012038523
#> 4417 0.0014142843 0.0008025682
#> 4428 0.0011112234 0.0011035313
#> 4496 0.0014142843 0.0009028892
#> 4520 0.0011112234 0.0013041734
#> 4525 0.0010102031 0.0016051364
#> 4589 0.0007071421 0.0005016051
#> 4612 0.0030306092 0.0009028892
#> 4617 0.0030306092 0.0016051364
#> 4695 0.0012122437 0.0007022472
#> 4740 0.0005051015 0.0005016051
#> 4753 0.0013132640 0.0018057785
#> 4777 0.0010102031 0.0009028892
#> 4835 0.0006061218 0.0010032103
#> 4954 0.0005051015 0.0006019262
#> 5049 0.0007071421 0.0005016051
#> 5053 0.0009091827 0.0006019262
#> 5058 0.0015153046 0.0018057785
#> 5059 0.0014142843 0.0008025682
#> 5060 0.0005051015 0.0006019262
#> 5061 0.0018183655 0.0012038523
#> 5065 0.0018183655 0.0011035313
#> 5067 0.0015153046 0.0011035313
#> 5068 0.0010102031 0.0005016051
#> 5071 0.0007071421 0.0008025682
#> 5073 0.0008081624 0.0005016051
#> 5074 0.0011112234 0.0005016051
#> 5086 0.0026265279 0.0036115570
#> 5088 0.0006061218 0.0013041734
#> 5089 0.0021214264 0.0009028892
#> 5090 0.0008081624 0.0009028892
#> 5091 0.0034346904 0.0043138042
#> 5123 0.0010102031 0.0007022472
#> 5152 0.0034346904 0.0021067416
#> 5157 0.0008081624 0.0006019262
#> 5159 0.0010102031 0.0007022472
#> 5160 0.0018183655 0.0005016051
#> 5163 0.0013132640 0.0015048154
#> 5235 0.0007071421 0.0009028892
#> 5244 0.0008081624 0.0014044944
#> 5245 0.0014142843 0.0031099518
#> 5246 0.0016163249 0.0017054575
#> 5247 0.0007071421 0.0008025682
#> 5249 0.0036367310 0.0047150883
#> 5258 0.0008081624 0.0005016051
#> 5265 0.0016163249 0.0011035313
#> 5266 0.0019193858 0.0016051364
#> 5269 0.0008081624 0.0007022472
#> 5273 0.0007071421 0.0007022472
#> 5275 0.0008081624 0.0007022472
#> 5277 0.0007071421 0.0008025682
#> 5281 0.0037377513 0.0042134831
#> 5284 0.0021214264 0.0020064205
#> 5285 0.0010102031 0.0011035313
#> 5286 0.0025255076 0.0073234350
#> 5567 0.0009091827 0.0011035313
#> 5572 0.0009091827 0.0011035313
#> 5601 0.0006061218 0.0006019262
#> 5680 0.0008081624 0.0013041734
#> 5919 0.0006061218 0.0008025682
#> 6018 0.0006061218 0.0009028892
#> 6253 0.0009091827 0.0005016051
#> 6278 0.0005051015 0.0005016051
#> 6297 0.0008081624 0.0009028892
#> 6341 0.0010102031 0.0005016051
#> 6397 0.0020204061 0.0013041734
#> 6398 0.0017173452 0.0012038523
#> 6401 0.0026265279 0.0012038523
#> 6414 0.0019193858 0.0006019262
#> 6429 0.0024244873 0.0024077047
#> 6432 0.0020204061 0.0007022472
#> 6433 0.0007071421 0.0011035313
#> 6434 0.0030306092 0.0030096308
#> 6634 0.0017173452 0.0011035313
#> 6639 0.0016163249 0.0018057785
#> 6748 0.0008081624 0.0007022472
#> 6768 0.0008081624 0.0005016051
#> 6771 0.0009091827 0.0009028892
#> 6819 0.0008081624 0.0006019262
#> 6822 0.0009091827 0.0010032103
#> 6973 0.0005051015 0.0011035313
#> 7016 0.0007071421 0.0007022472
#> 7033 0.0011112234 0.0010032103
#> 7037 0.0016163249 0.0018057785
#> 7141 0.0008081624 0.0005016051
#> 7146 0.0012122437 0.0009028892
#> 7152 0.0020204061 0.0006019262
#> 7154 0.0014142843 0.0005016051
#> 7169 0.0022224467 0.0020064205
#> 7172 0.0012122437 0.0012038523
#> 7174 0.0021214264 0.0025080257
#> 7236 0.0008081624 0.0009028892
#> 7313 0.0005051015 0.0006019262
#> 7396 0.0009091827 0.0005016051
#> 7408 0.0011112234 0.0006019262
#> 7479 0.0009091827 0.0011035313
#> 7480 0.0008081624 0.0005016051
#> 7483 0.0016163249 0.0014044944
#> 7494 0.0006061218 0.0007022472
#> 7495 0.0010102031 0.0006019262
#> 7508 0.0013132640 0.0012038523
#> 7509 0.0006061218 0.0005016051
#> 7512 0.0016163249 0.0025080257
#> 7614 0.0008081624 0.0008025682
#> 7723 0.0005051015 0.0006019262
#> 8195 0.0006061218 0.0005016051
#> 8200 0.0007071421 0.0009028892
#> 8230 0.0005051015 0.0005016051
#> 8479 0.0008081624 0.0005016051
#> 8492 0.0005051015 0.0006019262
#> 8496 0.0009091827 0.0008025682
#> 8643 0.0009091827 0.0009028892
#> 8648 0.0005051015 0.0011035313
#> 8660 0.0008081624 0.0012038523
#> 8670 0.0016163249 0.0010032103
#> 8676 0.0013132640 0.0005016051
#> 8677 0.0005051015 0.0007022472
#> 8691 0.0009091827 0.0008025682
#> 8694 0.0007071421 0.0009028892
#> 8696 0.0010102031 0.0018057785
#> 8711 0.0028285685 0.0007022472
#> 8738 0.0012122437 0.0010032103
#> 8745 0.0010102031 0.0005016051
#> 8788 0.0005051015 0.0005016051
#> 8815 0.0006061218 0.0005016051
#> 8816 0.0015153046 0.0012038523
#> 8817 0.0011112234 0.0019060995
#> 8818 0.0006061218 0.0007022472
#> 8820 0.0018183655 0.0032102729
#> 8830 0.0009091827 0.0006019262
#> 8835 0.0018183655 0.0006019262
#> 8836 0.0013132640 0.0015048154
#> 8847 0.0011112234 0.0006019262
#> 8850 0.0006061218 0.0012038523
#> 8853 0.0020204061 0.0040128411
#> 8855 0.0007071421 0.0009028892
#> 8856 0.0013132640 0.0012038523
#> 8857 0.0012122437 0.0006019262
#> 8858 0.0030306092 0.0048154093
#> 8977 0.0015153046 0.0008025682
#> 9085 0.0007071421 0.0008025682
#> 9086 0.0006061218 0.0011035313
#> 9087 0.0020204061 0.0005016051
#> 9090 0.0024244873 0.0015048154
#> 9118 0.0005051015 0.0005016051
#> 9132 0.0005051015 0.0005016051
#> 9134 0.0007071421 0.0005016051
#> 9160 0.0009091827 0.0011035313
#> 9164 0.0005051015 0.0005016051
#> 9165 0.0019193858 0.0011035313
#> 9338 0.0006061218 0.0006019262
#> 9412 0.0006061218 0.0008025682
#> 9416 0.0006061218 0.0007022472
#> 9459 0.0009091827 0.0012038523
#> 9461 0.0009091827 0.0015048154
#> 9462 0.0006061218 0.0010032103
#> 9464 0.0008081624 0.0022070626
#> 9468 0.0020204061 0.0008025682
#> 9470 0.0008081624 0.0010032103
#> 9474 0.0005051015 0.0009028892
#> 9490 0.0020204061 0.0034109149
#> 9492 0.0011112234 0.0008025682
#> 9493 0.0018183655 0.0010032103
#> 9494 0.0011112234 0.0012038523
#> 9495 0.0028285685 0.0045144462
#> 9527 0.0010102031 0.0011035313
#> 9556 0.0015153046 0.0012038523
#> 9559 0.0006061218 0.0009028892
#> 9564 0.0010102031 0.0006019262
#> 9565 0.0009091827 0.0007022472
#> 9568 0.0020204061 0.0014044944
#> 9605 0.0005051015 0.0006019262
#> 9612 0.0005051015 0.0007022472
#> 9614 0.0006061218 0.0006019262
#> 9630 0.0007071421 0.0007022472
#> 9643 0.0009091827 0.0028089888
#> 9644 0.0012122437 0.0013041734
#> 9647 0.0030306092 0.0052166934
#> 9655 0.0005051015 0.0010032103
#> 9662 0.0008081624 0.0014044944
#> 9663 0.0017173452 0.0014044944
#> 9665 0.0006061218 0.0005016051
#> 9668 0.0010102031 0.0007022472
#> 9673 0.0006061218 0.0008025682
#> 9675 0.0005051015 0.0007022472
#> 9678 0.0018183655 0.0056179775
#> 9681 0.0019193858 0.0020064205
#> 9682 0.0006061218 0.0013041734
#> 9683 0.0031316295 0.0076243981
#> 9890 0.0011112234 0.0013041734
```

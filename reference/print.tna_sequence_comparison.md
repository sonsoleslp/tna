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
# \donttest{
idx <- c(1:500, 1001:1500)
group <- c(rep("High", 500), rep("Low", 500))
comp <- compare_sequences(group_regulation[idx, ], group)
print(comp)
#>                                                   pattern freq_High freq_Low
#> 1                                                   adapt        73      210
#> 2                                               consensus      1868     1572
#> 3                                              coregulate       471      596
#> 4                                                 emotion       852      696
#> 5                                                    plan      1554     1785
#> 6                                                cohesion       509      415
#> 7                                                 monitor       335      417
#> 8                                         adapt->cohesion        14       56
#> 9                                        adapt->consensus        35       90
#> 10                                    cohesion->consensus       267      181
#> 11                                    consensus->cohesion        40       10
#> 12                                     consensus->discuss       427      204
#> 13                                     consensus->emotion       138       89
#> 14                                         discuss->adapt        22      125
#> 15                                     discuss->consensus       405      191
#> 16                                     emotion->consensus       268      188
#> 17                                          plan->emotion       265      188
#> 18                                             plan->plan       484      706
#> 19                                       synthesis->adapt        19       59
#> 20                                      cohesion->discuss        11       30
#> 21                                  coregulate->consensus        47       84
#> 22                                     discuss->synthesis       119      175
#> 23                                        synthesis->plan        19        5
#> 25                                    emotion->coregulate        11       29
#> 26                                      discuss->cohesion        64       33
#> 27                                       monitor->discuss       111      157
#> 28                                   cohesion->coregulate        37       68
#> 29                                      cohesion->emotion        63       36
#> 31                                              synthesis       174      209
#> 32                                     consensus->monitor        67      102
#> 33                                    coregulate->discuss       104      150
#> 34                             adapt->cohesion->consensus         8       28
#> 35                                 adapt->consensus->plan         8       36
#> 36                           cohesion->consensus->discuss        74       22
#> 38                          consensus->discuss->consensus       156       48
#> 39                            consensus->discuss->emotion        52       19
#> 41                         coregulate->discuss->synthesis         9       32
#> 43                              discuss->adapt->consensus        12       54
#> 44                           discuss->cohesion->consensus        35       11
#> 45                            discuss->consensus->discuss        97       21
#> 46                               discuss->consensus->plan       139       86
#> 48                              discuss->synthesis->adapt        14       47
#> 49                            emotion->consensus->discuss        61       28
#> 50                            emotion->discuss->consensus        34        5
#> 51                              monitor->discuss->discuss        13       35
#> 53                                       plan->plan->plan       140      278
#> 55                                 coregulate->coregulate         6       20
#> 56                                       discuss->discuss       155      212
#> 59                                       emotion->discuss        91       60
#> 60                                                discuss      1081     1042
#> 61                                         adapt->emotion        10       22
#> 65                                      cohesion->monitor         9       17
#> 66                                         cohesion->plan        61       51
#> 68                                       consensus->adapt         6       11
#> 69                                   consensus->consensus       161      124
#> 70                                  consensus->coregulate       272      298
#> 71                                        consensus->plan       619      619
#> 72                                   consensus->synthesis        15       12
#> 73                                      coregulate->adapt        10        6
#> 74                                   coregulate->cohesion        16       18
#> 75                                    coregulate->emotion        94       91
#> 76                                    coregulate->monitor        43       42
#> 77                                       coregulate->plan       112      126
#> 78                                  coregulate->synthesis         6        9
#> 79                                    discuss->coregulate        82       90
#> 80                                       discuss->emotion       119      103
#> 81                                       discuss->monitor        12       27
#> 82                                          discuss->plan        18       10
#> 84                                      emotion->cohesion       253      211
#> 85                                       emotion->emotion        43       52
#> 86                                       emotion->monitor        27       26
#> 87                                          emotion->plan        79       78
#> 90                                      monitor->cohesion        15       18
#> 91                                     monitor->consensus        60       63
#> 92                                    monitor->coregulate        16       27
#> 93                                       monitor->emotion        31       34
#> 94                                       monitor->monitor         7        6
#> 95                                          monitor->plan        66       83
#> 98                                         plan->cohesion        46       38
#> 99                                        plan->consensus       422      459
#> 100                                      plan->coregulate        42       23
#> 101                                         plan->discuss        91      122
#> 102                                         plan->monitor        96      115
#> 105                                  synthesis->consensus        98       78
#> 107                                    synthesis->emotion         9       14
#> 118                          adapt->consensus->coregulate         9       21
#> 119                             adapt->consensus->discuss        10        8
#> 157                        cohesion->consensus->consensus        27       14
#> 158                       cohesion->consensus->coregulate        31       36
#> 159                          cohesion->consensus->emotion        24       10
#> 161                             cohesion->consensus->plan        84       74
#> 166                         cohesion->coregulate->discuss         9       16
#> 167                         cohesion->coregulate->emotion         9       14
#> 169                            cohesion->coregulate->plan        10       13
#> 179                           cohesion->emotion->cohesion        17       14
#> 180                          cohesion->emotion->consensus        21        7
#> 190                            cohesion->monitor->discuss         6        7
#> 194                             cohesion->plan->consensus        17       15
#> 199                                  cohesion->plan->plan        17       19
#> 213                       consensus->consensus->consensus        17        8
#> 214                      consensus->consensus->coregulate        19       22
#> 215                         consensus->consensus->discuss        33       11
#> 216                         consensus->consensus->emotion        11        9
#> 217                         consensus->consensus->monitor        12        5
#> 218                            consensus->consensus->plan        48       58
#> 221                       consensus->coregulate->cohesion         8        8
#> 222                      consensus->coregulate->consensus        24       43
#> 223                     consensus->coregulate->coregulate         6       14
#> 224                        consensus->coregulate->discuss        63       78
#> 225                        consensus->coregulate->emotion        56       46
#> 226                        consensus->coregulate->monitor        25       20
#> 227                           consensus->coregulate->plan        63       61
#> 229                             consensus->discuss->adapt        11       25
#> 230                          consensus->discuss->cohesion        18        5
#> 231                        consensus->discuss->coregulate        31       15
#> 232                           consensus->discuss->discuss        68       38
#> 235                         consensus->discuss->synthesis        49       37
#> 237                          consensus->emotion->cohesion        31       25
#> 238                         consensus->emotion->consensus        45       22
#> 240                           consensus->emotion->discuss        15       13
#> 241                           consensus->emotion->emotion        10        6
#> 243                              consensus->emotion->plan        11        9
#> 247                         consensus->monitor->consensus         7       15
#> 249                           consensus->monitor->discuss        22       45
#> 250                           consensus->monitor->emotion         8        6
#> 252                              consensus->monitor->plan        16       15
#> 255                             consensus->plan->cohesion        20        7
#> 256                            consensus->plan->consensus       179      160
#> 257                           consensus->plan->coregulate        17       11
#> 258                              consensus->plan->discuss        33       44
#> 259                              consensus->plan->emotion        99       69
#> 260                              consensus->plan->monitor        31       44
#> 261                                 consensus->plan->plan       196      242
#> 274                       coregulate->cohesion->consensus         7        9
#> 279                      coregulate->consensus->consensus         5        8
#> 280                     coregulate->consensus->coregulate         8       16
#> 281                        coregulate->consensus->discuss         9       11
#> 284                           coregulate->consensus->plan        15       35
#> 293                        coregulate->discuss->consensus        46       23
#> 294                       coregulate->discuss->coregulate        10       13
#> 295                          coregulate->discuss->discuss         8       26
#> 296                          coregulate->discuss->emotion        10       10
#> 299                         coregulate->emotion->cohesion        35       27
#> 300                        coregulate->emotion->consensus        20       18
#> 302                          coregulate->emotion->discuss        14       11
#> 303                          coregulate->emotion->emotion         5        6
#> 305                             coregulate->emotion->plan         9       14
#> 309                        coregulate->monitor->consensus         7        6
#> 311                          coregulate->monitor->discuss        15       12
#> 314                             coregulate->monitor->plan        13        8
#> 318                           coregulate->plan->consensus        25       33
#> 320                             coregulate->plan->discuss         7        8
#> 321                             coregulate->plan->emotion        20       14
#> 322                             coregulate->plan->monitor         6        8
#> 323                                coregulate->plan->plan        38       53
#> 330                               discuss->adapt->emotion         5       13
#> 342                         discuss->consensus->consensus        34       14
#> 343                        discuss->consensus->coregulate        54       31
#> 344                           discuss->consensus->emotion        28       11
#> 345                           discuss->consensus->monitor        11       11
#> 348                        discuss->coregulate->consensus         8        9
#> 349                          discuss->coregulate->discuss        19       28
#> 350                          discuss->coregulate->emotion        14        9
#> 351                          discuss->coregulate->monitor         6       10
#> 352                             discuss->coregulate->plan        17       19
#> 354                            discuss->discuss->cohesion         7        7
#> 355                           discuss->discuss->consensus        61       41
#> 356                          discuss->discuss->coregulate         9       14
#> 357                             discuss->discuss->discuss        21       45
#> 358                             discuss->discuss->emotion        16       26
#> 361                           discuss->discuss->synthesis        19       29
#> 362                            discuss->emotion->cohesion        30       32
#> 363                           discuss->emotion->consensus        40       32
#> 365                             discuss->emotion->discuss        12       10
#> 368                                discuss->emotion->plan        18        6
#> 374                             discuss->monitor->discuss         5       13
#> 384                         discuss->synthesis->consensus        69       65
#> 389                              discuss->synthesis->plan        13        5
#> 392                          emotion->cohesion->consensus       126       98
#> 393                         emotion->cohesion->coregulate        22       37
#> 394                            emotion->cohesion->discuss         7       13
#> 395                            emotion->cohesion->emotion        34       16
#> 397                               emotion->cohesion->plan        34       22
#> 401                         emotion->consensus->consensus        26       17
#> 402                        emotion->consensus->coregulate        39       39
#> 403                           emotion->consensus->emotion        20        9
#> 404                           emotion->consensus->monitor         9       16
#> 405                              emotion->consensus->plan        82       65
#> 417                          emotion->discuss->coregulate         5        6
#> 418                             emotion->discuss->discuss        14       16
#> 419                             emotion->discuss->emotion        12        6
#> 422                           emotion->discuss->synthesis        11       11
#> 423                            emotion->emotion->cohesion        18       17
#> 424                           emotion->emotion->consensus        14       14
#> 433                             emotion->monitor->discuss         7       10
#> 438                              emotion->plan->consensus        21       19
#> 441                                emotion->plan->emotion        12        9
#> 443                                   emotion->plan->plan        24       30
#> 451                          monitor->cohesion->consensus         8        9
#> 458                        monitor->consensus->coregulate        10        9
#> 459                           monitor->consensus->discuss        18        9
#> 462                              monitor->consensus->plan        21       22
#> 468                          monitor->coregulate->discuss         5        6
#> 473                            monitor->discuss->cohesion        14        6
#> 474                           monitor->discuss->consensus        42       27
#> 475                          monitor->discuss->coregulate         7       17
#> 476                             monitor->discuss->emotion         8       18
#> 479                           monitor->discuss->synthesis        11       27
#> 480                            monitor->emotion->cohesion         5        6
#> 481                           monitor->emotion->consensus        15       14
#> 492                              monitor->plan->consensus        16       25
#> 495                                monitor->plan->emotion        13        9
#> 497                                   monitor->plan->plan        30       28
#> 505                             plan->cohesion->consensus        30       11
#> 513                            plan->consensus->consensus        35       29
#> 514                           plan->consensus->coregulate        72       91
#> 515                              plan->consensus->discuss        85       57
#> 516                              plan->consensus->emotion        29       32
#> 517                              plan->consensus->monitor        22       38
#> 518                                 plan->consensus->plan       140      165
#> 525                                plan->coregulate->plan        13        7
#> 528                              plan->discuss->consensus        28       22
#> 529                             plan->discuss->coregulate        11       13
#> 530                                plan->discuss->discuss        15       24
#> 531                                plan->discuss->emotion         7       11
#> 534                              plan->discuss->synthesis        12       17
#> 536                               plan->emotion->cohesion        80       52
#> 537                              plan->emotion->consensus        79       54
#> 539                                plan->emotion->discuss        36       11
#> 540                                plan->emotion->emotion        13       17
#> 542                                   plan->emotion->plan        24       22
#> 545                              plan->monitor->consensus        23       20
#> 547                                plan->monitor->discuss        32       38
#> 548                                plan->monitor->emotion         8        7
#> 550                                   plan->monitor->plan        14       32
#> 553                                  plan->plan->cohesion        12       22
#> 554                                 plan->plan->consensus       132      184
#> 555                                plan->plan->coregulate        12        7
#> 556                                   plan->plan->discuss        31       45
#> 557                                   plan->plan->emotion        83       68
#> 558                                   plan->plan->monitor        38       35
#> 562                           synthesis->adapt->consensus        10       23
#> 573                       synthesis->consensus->consensus         5        6
#> 574                      synthesis->consensus->coregulate        13       11
#> 575                         synthesis->consensus->discuss        22       16
#> 578                            synthesis->consensus->plan        36       28
#> 720                  cohesion->consensus->consensus->plan         9        7
#> 725              cohesion->consensus->coregulate->discuss        10       12
#> 728                 cohesion->consensus->coregulate->plan         6        7
#> 731               cohesion->consensus->discuss->consensus        28        9
#> 733                 cohesion->consensus->discuss->discuss        11        5
#> 748                  cohesion->consensus->plan->consensus        28       18
#> 749                    cohesion->consensus->plan->discuss         5        6
#> 750                    cohesion->consensus->plan->emotion        12       12
#> 752                       cohesion->consensus->plan->plan        30       27
#> 805                cohesion->emotion->cohesion->consensus         6        8
#> 858                       cohesion->plan->consensus->plan         8        6
#> 922             consensus->consensus->coregulate->discuss         5        6
#> 925                consensus->consensus->coregulate->plan         6        5
#> 943                 consensus->consensus->plan->consensus        21       19
#> 948                      consensus->consensus->plan->plan        12       18
#> 962             consensus->coregulate->consensus->discuss         5        8
#> 965                consensus->coregulate->consensus->plan         8       12
#> 974             consensus->coregulate->discuss->consensus        30       15
#> 977               consensus->coregulate->discuss->emotion         6        6
#> 980             consensus->coregulate->discuss->synthesis         7       14
#> 981              consensus->coregulate->emotion->cohesion        21       10
#> 982             consensus->coregulate->emotion->consensus        11       13
#> 984               consensus->coregulate->emotion->discuss         8        5
#> 987                  consensus->coregulate->emotion->plan         6        7
#> 990             consensus->coregulate->monitor->consensus         5        5
#> 998                consensus->coregulate->plan->consensus        13       14
#> 1001                 consensus->coregulate->plan->emotion        12        9
#> 1003                    consensus->coregulate->plan->plan        18       26
#> 1007                 consensus->discuss->adapt->consensus         6       15
#> 1019            consensus->discuss->consensus->coregulate        20        6
#> 1020               consensus->discuss->consensus->discuss        33        5
#> 1023                  consensus->discuss->consensus->plan        53       23
#> 1026              consensus->discuss->coregulate->discuss         7        6
#> 1032               consensus->discuss->discuss->consensus        17        6
#> 1034                 consensus->discuss->discuss->discuss        13        9
#> 1035                 consensus->discuss->discuss->emotion         9        5
#> 1038               consensus->discuss->discuss->synthesis        10        5
#> 1039                consensus->discuss->emotion->cohesion        11        6
#> 1040               consensus->discuss->emotion->consensus        20        5
#> 1057             consensus->discuss->synthesis->consensus        31       14
#> 1064              consensus->emotion->cohesion->consensus        19       10
#> 1071            consensus->emotion->consensus->coregulate        12        6
#> 1118               consensus->monitor->discuss->consensus         7        8
#> 1151                consensus->plan->consensus->consensus        21       12
#> 1152               consensus->plan->consensus->coregulate        34       29
#> 1153                  consensus->plan->consensus->discuss        33       25
#> 1154                  consensus->plan->consensus->emotion        13       11
#> 1155                  consensus->plan->consensus->monitor        12       10
#> 1156                     consensus->plan->consensus->plan        51       54
#> 1167                  consensus->plan->discuss->consensus        12        9
#> 1169                    consensus->plan->discuss->discuss         7       11
#> 1174                   consensus->plan->emotion->cohesion        36       22
#> 1175                  consensus->plan->emotion->consensus        31       20
#> 1180                       consensus->plan->emotion->plan         6        7
#> 1185                    consensus->plan->monitor->discuss        12       10
#> 1188                       consensus->plan->monitor->plan         5       18
#> 1189                      consensus->plan->plan->cohesion         5        9
#> 1190                     consensus->plan->plan->consensus        43       66
#> 1192                       consensus->plan->plan->discuss        15       14
#> 1193                       consensus->plan->plan->emotion        32       22
#> 1194                       consensus->plan->plan->monitor        18       11
#> 1195                          consensus->plan->plan->plan        57      101
#> 1266                    coregulate->consensus->plan->plan         5       17
#> 1290           coregulate->discuss->consensus->coregulate         7        6
#> 1294                 coregulate->discuss->consensus->plan        17        6
#> 1302              coregulate->discuss->discuss->consensus         5        8
#> 1322            coregulate->discuss->synthesis->consensus         6       18
#> 1327             coregulate->emotion->cohesion->consensus        12       16
#> 1338                 coregulate->emotion->consensus->plan         9        7
#> 1377              coregulate->monitor->discuss->consensus         6        5
#> 1397              coregulate->plan->consensus->coregulate         5        5
#> 1401                    coregulate->plan->consensus->plan         6       11
#> 1423                    coregulate->plan->plan->consensus        12       15
#> 1426                      coregulate->plan->plan->emotion         9        8
#> 1428                         coregulate->plan->plan->plan         9       16
#> 1505                  discuss->consensus->consensus->plan        15        7
#> 1511              discuss->consensus->coregulate->emotion        12        9
#> 1513                 discuss->consensus->coregulate->plan        12        6
#> 1518                 discuss->consensus->discuss->discuss        17        6
#> 1522               discuss->consensus->discuss->synthesis        10        6
#> 1526                 discuss->consensus->emotion->discuss         5        5
#> 1539                  discuss->consensus->plan->consensus        44       19
#> 1541                    discuss->consensus->plan->discuss         7        6
#> 1542                    discuss->consensus->plan->emotion        24        8
#> 1543                    discuss->consensus->plan->monitor         9        5
#> 1544                       discuss->consensus->plan->plan        37       37
#> 1561               discuss->coregulate->emotion->cohesion         5        6
#> 1578                      discuss->coregulate->plan->plan         8        8
#> 1596              discuss->discuss->consensus->coregulate         6        5
#> 1597                 discuss->discuss->consensus->discuss        17        5
#> 1600                    discuss->discuss->consensus->plan        24       19
#> 1611                 discuss->discuss->discuss->consensus         8       11
#> 1618                 discuss->discuss->emotion->consensus         8        9
#> 1631               discuss->discuss->synthesis->consensus         8        8
#> 1637                discuss->emotion->cohesion->consensus        15       15
#> 1646              discuss->emotion->consensus->coregulate         5        7
#> 1650                    discuss->emotion->consensus->plan        16        9
#> 1703                 discuss->synthesis->adapt->consensus         7       20
#> 1715            discuss->synthesis->consensus->coregulate        10       10
#> 1716               discuss->synthesis->consensus->discuss        15       13
#> 1719                  discuss->synthesis->consensus->plan        27       21
#> 1750              emotion->cohesion->consensus->consensus        16       10
#> 1751             emotion->cohesion->consensus->coregulate        15       19
#> 1752                emotion->cohesion->consensus->discuss        34       12
#> 1755                   emotion->cohesion->consensus->plan        38       41
#> 1759               emotion->cohesion->coregulate->emotion         6        7
#> 1761                  emotion->cohesion->coregulate->plan         6        6
#> 1789                        emotion->cohesion->plan->plan         7       10
#> 1802                  emotion->consensus->consensus->plan         5        8
#> 1808              emotion->consensus->coregulate->discuss        11       11
#> 1809              emotion->consensus->coregulate->emotion        10        6
#> 1811                 emotion->consensus->coregulate->plan         6        9
#> 1815              emotion->consensus->discuss->coregulate         6        5
#> 1819               emotion->consensus->discuss->synthesis         7        6
#> 1832                  emotion->consensus->plan->consensus        19       20
#> 1835                    emotion->consensus->plan->emotion        11       13
#> 1837                       emotion->consensus->plan->plan        28       21
#> 1894               emotion->discuss->synthesis->consensus        10        5
#> 1899                emotion->emotion->cohesion->consensus         8        5
#> 1909                    emotion->emotion->consensus->plan         6        8
#> 1976                       emotion->plan->plan->consensus         7        5
#> 1980                            emotion->plan->plan->plan         5       11
#> 2027                  monitor->consensus->plan->consensus        10        8
#> 2073                 monitor->discuss->consensus->discuss         9        5
#> 2076                    monitor->discuss->consensus->plan        12       13
#> 2086                 monitor->discuss->discuss->consensus         8        6
#> 2163                       monitor->plan->plan->consensus        10        7
#> 2167                            monitor->plan->plan->plan         9       12
#> 2189                      plan->cohesion->consensus->plan         7        5
#> 2219                     plan->consensus->consensus->plan         8       13
#> 2223               plan->consensus->coregulate->consensus         8       19
#> 2225                 plan->consensus->coregulate->discuss        14       20
#> 2226                 plan->consensus->coregulate->emotion        17       10
#> 2227                 plan->consensus->coregulate->monitor         6       11
#> 2228                    plan->consensus->coregulate->plan        18       21
#> 2232                  plan->consensus->discuss->consensus        29       11
#> 2234                    plan->consensus->discuss->discuss        17       12
#> 2235                    plan->consensus->discuss->emotion        13        7
#> 2238                  plan->consensus->discuss->synthesis         9        9
#> 2240                   plan->consensus->emotion->cohesion         6       10
#> 2241                  plan->consensus->emotion->consensus         7        7
#> 2250                    plan->consensus->monitor->discuss         7       18
#> 2253                       plan->consensus->monitor->plan         6        6
#> 2256                     plan->consensus->plan->consensus        29       42
#> 2257                    plan->consensus->plan->coregulate         6        5
#> 2258                       plan->consensus->plan->discuss         5       11
#> 2259                       plan->consensus->plan->emotion        24       15
#> 2260                       plan->consensus->plan->monitor        12       14
#> 2261                          plan->consensus->plan->plan        48       64
#> 2300                       plan->discuss->consensus->plan        10       10
#> 2334                   plan->emotion->cohesion->consensus        43       25
#> 2339                        plan->emotion->cohesion->plan        12        7
#> 2342                  plan->emotion->consensus->consensus         6        6
#> 2343                 plan->emotion->consensus->coregulate        12       11
#> 2344                    plan->emotion->consensus->discuss        18       10
#> 2347                       plan->emotion->consensus->plan        24       17
#> 2361                     plan->emotion->emotion->cohesion         5        6
#> 2372                       plan->emotion->plan->consensus         7        5
#> 2377                            plan->emotion->plan->plan         9        7
#> 2388                       plan->monitor->consensus->plan         6        9
#> 2396                    plan->monitor->discuss->consensus         9        5
#> 2398                      plan->monitor->discuss->discuss         5       11
#> 2415                            plan->monitor->plan->plan         5       10
#> 2418                      plan->plan->cohesion->consensus         8        7
#> 2425                     plan->plan->consensus->consensus         6       13
#> 2426                    plan->plan->consensus->coregulate        16       39
#> 2427                       plan->plan->consensus->discuss        28       21
#> 2428                       plan->plan->consensus->emotion         8       11
#> 2429                       plan->plan->consensus->monitor         6       18
#> 2430                          plan->plan->consensus->plan        52       67
#> 2440                       plan->plan->discuss->consensus         9        9
#> 2442                         plan->plan->discuss->discuss         5        7
#> 2447                        plan->plan->emotion->cohesion        19       11
#> 2448                       plan->plan->emotion->consensus        28       20
#> 2450                         plan->plan->emotion->discuss        13        6
#> 2451                         plan->plan->emotion->emotion         5       10
#> 2453                            plan->plan->emotion->plan         9       10
#> 2455                       plan->plan->monitor->consensus         9        9
#> 2457                         plan->plan->monitor->discuss        11       10
#> 2459                            plan->plan->monitor->plan         6        8
#> 2463                          plan->plan->plan->consensus        40       69
#> 2465                            plan->plan->plan->discuss         5       22
#> 2466                            plan->plan->plan->emotion        22       25
#> 2467                            plan->plan->plan->monitor         9       13
#> 2468                               plan->plan->plan->plan        46      110
#> 2520             synthesis->consensus->discuss->consensus         9        6
#> 2536                synthesis->consensus->plan->consensus         6        7
#> 2540                     synthesis->consensus->plan->plan        18        9
#> 2873           cohesion->consensus->plan->consensus->plan        11        5
#> 2887           cohesion->consensus->plan->plan->consensus         7        6
#> 2889             cohesion->consensus->plan->plan->emotion         7        5
#> 2891                cohesion->consensus->plan->plan->plan        10        7
#> 3227          consensus->consensus->plan->consensus->plan         5        8
#> 3246          consensus->consensus->plan->plan->consensus         8        8
#> 3343 consensus->coregulate->discuss->synthesis->consensus         5        6
#> 3347  consensus->coregulate->emotion->cohesion->consensus         8        6
#> 3405         consensus->coregulate->plan->consensus->plan         5        6
#> 3426         consensus->coregulate->plan->plan->consensus         6        7
#> 3493       consensus->discuss->consensus->plan->consensus        13        6
#> 3498            consensus->discuss->consensus->plan->plan        17        9
#> 3606       consensus->discuss->synthesis->consensus->plan        13       10
#> 3623        consensus->emotion->cohesion->consensus->plan         5        6
#> 3822         consensus->plan->consensus->discuss->discuss         8        6
#> 3826       consensus->plan->consensus->discuss->synthesis         5        5
#> 3839          consensus->plan->consensus->plan->consensus        12       15
#> 3844               consensus->plan->consensus->plan->plan        17       21
#> 3894        consensus->plan->emotion->cohesion->consensus        22        9
#> 3905            consensus->plan->emotion->consensus->plan         7        8
#> 3964            consensus->plan->plan->consensus->discuss         7        6
#> 3967               consensus->plan->plan->consensus->plan        18       21
#> 3983             consensus->plan->plan->emotion->cohesion         8        6
#> 3984            consensus->plan->plan->emotion->consensus         8        7
#> 3998               consensus->plan->plan->plan->consensus        20       23
#> 4001                 consensus->plan->plan->plan->emotion         9       11
#> 4003                    consensus->plan->plan->plan->plan        12       36
#> 4239       coregulate->emotion->cohesion->consensus->plan         6        9
#> 4719         discuss->consensus->plan->consensus->discuss        11        6
#> 4722            discuss->consensus->plan->consensus->plan        10        5
#> 4748            discuss->consensus->plan->plan->consensus         5       11
#> 4753                 discuss->consensus->plan->plan->plan        14       13
#> 4885         discuss->discuss->consensus->plan->consensus         7        7
#> 4890              discuss->discuss->consensus->plan->plan         6        7
#> 5143    discuss->synthesis->consensus->discuss->consensus         6        5
#> 5156       discuss->synthesis->consensus->plan->consensus         6        5
#> 5160            discuss->synthesis->consensus->plan->plan        14        6
#> 5218        emotion->cohesion->consensus->consensus->plan         6        5
#> 5222    emotion->cohesion->consensus->coregulate->discuss         8        6
#> 5243        emotion->cohesion->consensus->plan->consensus        12       11
#> 5245          emotion->cohesion->consensus->plan->emotion         6        7
#> 5247             emotion->cohesion->consensus->plan->plan        11       14
#> 5459      emotion->consensus->plan->consensus->coregulate         5        5
#> 5487                 emotion->consensus->plan->plan->plan         7       10
#> 6204      plan->consensus->coregulate->discuss->consensus         5        5
#> 6225              plan->consensus->coregulate->plan->plan         7       11
#> 6238            plan->consensus->discuss->consensus->plan        14        5
#> 6261       plan->consensus->discuss->synthesis->consensus         8        5
#> 6325               plan->consensus->plan->consensus->plan         7       16
#> 6340            plan->consensus->plan->emotion->consensus         5        7
#> 6352               plan->consensus->plan->plan->consensus        10       18
#> 6357                    plan->consensus->plan->plan->plan        14       33
#> 6504             plan->emotion->cohesion->consensus->plan        11        8
#> 6561                 plan->emotion->consensus->plan->plan        10        5
#> 6747            plan->plan->consensus->discuss->consensus        11        5
#> 6748              plan->plan->consensus->discuss->discuss         5        5
#> 6765               plan->plan->consensus->plan->consensus         9       17
#> 6769                 plan->plan->consensus->plan->monitor         5        6
#> 6770                    plan->plan->consensus->plan->plan        16       26
#> 6791                 plan->plan->discuss->consensus->plan         6        6
#> 6810             plan->plan->emotion->cohesion->consensus         6        7
#> 6821                 plan->plan->emotion->consensus->plan        10        7
#> 6873              plan->plan->plan->consensus->coregulate         6       13
#> 6874                 plan->plan->plan->consensus->discuss         8        8
#> 6877                    plan->plan->plan->consensus->plan        14       30
#> 6893                 plan->plan->plan->emotion->consensus         9        7
#> 6897                      plan->plan->plan->emotion->plan         6        6
#> 6905                    plan->plan->plan->plan->consensus         9       32
#> 6908                      plan->plan->plan->plan->emotion         9        9
#> 6910                         plan->plan->plan->plan->plan        21       48
#> 7051               synthesis->consensus->plan->plan->plan         8        5
#>         prop_High     prop_Low effect_size     p_value
#> 1    0.0105537083 0.0302506482 12.12142899 0.008991009
#> 2    0.2700592743 0.2264477096 10.81875691 0.008991009
#> 3    0.0680931039 0.0858542207  5.66858403 0.008991009
#> 4    0.1231747868 0.1002592913  6.30359458 0.008991009
#> 5    0.2246638716 0.2571305099  4.79830165 0.008991009
#> 6    0.0735868151 0.0597810429  4.48435977 0.017982018
#> 7    0.0484314009 0.0600691443  4.02607126 0.026973027
#> 8    0.0021817048 0.0086929525  7.59182746 0.077922078
#> 9    0.0054542621 0.0139708165  7.16249421 0.077922078
#> 10   0.0416082281 0.0280968643  6.15526240 0.077922078
#> 11   0.0062334424 0.0015523129  6.03762363 0.077922078
#> 12   0.0665419978 0.0316671841 13.31041803 0.077922078
#> 13   0.0215053763 0.0138155852  4.11538359 0.077922078
#> 14   0.0034283933 0.0194039118 12.50064498 0.077922078
#> 15   0.0631136045 0.0296491773 12.81553217 0.077922078
#> 16   0.0417640642 0.0291834834  5.26875550 0.077922078
#> 17   0.0412965560 0.0291834834  4.87096689 0.077922078
#> 18   0.0754246533 0.1095932940  6.96475308 0.077922078
#> 19   0.0029608851 0.0091586464  6.10011920 0.077922078
#> 20   0.0017141967 0.0046569388  3.62541095 0.155844156
#> 21   0.0073242948 0.0130394287  4.13164871 0.155844156
#> 22   0.0185444912 0.0271654766  4.41216452 0.155844156
#> 23   0.0029608851 0.0007761565  3.43159511 0.155844156
#> 25   0.0017141967 0.0045017075  3.46167030 0.233766234
#> 26   0.0099735079 0.0051226327  3.96156768 0.311688312
#> 27   0.0172978027 0.0243713133  3.69995492 0.311688312
#> 28   0.0057659342 0.0105557280  3.47675188 0.389610390
#> 29   0.0098176718 0.0055883266  3.40613909 0.389610390
#> 31   0.0251554142 0.0301065975  1.90178313 0.458541459
#> 32   0.0104410161 0.0158335921  3.41203486 0.467532468
#> 33   0.0162069503 0.0232846942  3.17179084 0.467532468
#> 34   0.0013520365 0.0047122181  4.37110128 0.515484515
#> 35   0.0013520365 0.0060585661  5.53039821 0.515484515
#> 36   0.0125063377 0.0037024571  7.85852026 0.515484515
#> 38   0.0263647118 0.0080780882 10.75660637 0.515484515
#> 39   0.0087882373 0.0031975766  5.06057528 0.515484515
#> 41   0.0015210411 0.0053853921  4.84428123 0.515484515
#> 43   0.0020280548 0.0090878492  7.40855728 0.515484515
#> 44   0.0059151597 0.0018512285  4.64637838 0.515484515
#> 45   0.0163934426 0.0035341636  8.89956856 0.515484515
#> 46   0.0234916343 0.0144732413  5.23105507 0.515484515
#> 48   0.0023660639 0.0079097947  5.58806098 0.515484515
#> 49   0.0103092784 0.0047122181  4.44945318 0.515484515
#> 50   0.0057461551 0.0008414675  6.27313859 0.515484515
#> 51   0.0021970593 0.0058902726  3.95248750 0.515484515
#> 53   0.0236606388 0.0467855941  6.51689685 0.515484515
#> 55   0.0009350164 0.0031046259  3.26035679 0.545454545
#> 56   0.0241545894 0.0329090345  3.18232876 0.623376623
#> 59   0.0141810815 0.0093138777  3.12320510 0.935064935
#> 60   0.1562816250 0.1501008355  0.35837554 1.000000000
#> 61   0.0015583606 0.0034150885  2.11066258 1.000000000
#> 65   0.0014025245 0.0026389320  1.23171966 1.000000000
#> 66   0.0095059997 0.0079167960  0.26811081 1.000000000
#> 68   0.0009350164 0.0017075442  0.70985717 1.000000000
#> 69   0.0250896057 0.0192486805  2.26748330 1.000000000
#> 70   0.0423874084 0.0462589258  0.49797502 1.000000000
#> 71   0.0964625214 0.0960881714 -1.22334864 1.000000000
#> 72   0.0023375409 0.0018627755 -0.35396346 1.000000000
#> 73   0.0015583606 0.0009313878  0.40435728 1.000000000
#> 74   0.0024933770 0.0027941633 -0.74102130 1.000000000
#> 75   0.0146485897 0.0141260478 -0.92990956 1.000000000
#> 76   0.0067009506 0.0065197144 -1.12008294 1.000000000
#> 77   0.0174536388 0.0195591431  0.10756758 1.000000000
#> 78   0.0009350164 0.0013970817 -0.03953708 1.000000000
#> 79   0.0127785570 0.0139708165 -0.34519488 1.000000000
#> 80   0.0185444912 0.0159888233  0.70117344 1.000000000
#> 81   0.0018700327 0.0041912450  2.73644347 1.000000000
#> 82   0.0028050491 0.0015523129  1.17581560 1.000000000
#> 84   0.0394265233 0.0327538032  2.20286319 1.000000000
#> 85   0.0067009506 0.0080720273  0.06884690 1.000000000
#> 86   0.0042075736 0.0040360137 -1.07824300 1.000000000
#> 87   0.0123110488 0.0121080410 -1.13649140 1.000000000
#> 90   0.0023375409 0.0027941633 -0.43849760 1.000000000
#> 91   0.0093501636 0.0097795716 -0.89067364 1.000000000
#> 92   0.0024933770 0.0041912450  1.39671690 1.000000000
#> 93   0.0048309179 0.0052778640 -0.67082727 1.000000000
#> 94   0.0010908524 0.0009313878 -0.87800218 1.000000000
#> 95   0.0102851800 0.0128841975  0.94088834 1.000000000
#> 98   0.0071684588 0.0058987892  0.24869856 1.000000000
#> 99   0.0657628175 0.0712511642  0.77789913 1.000000000
#> 100  0.0065451145 0.0035703198  2.49957181 1.000000000
#> 101  0.0141810815 0.0189382179  2.39689406 1.000000000
#> 102  0.0149602618 0.0178515989  0.96467372 1.000000000
#> 105  0.0152719339 0.0121080410  1.37194486 1.000000000
#> 107  0.0014025245 0.0021732381  0.35124080 1.000000000
#> 118  0.0015210411 0.0035341636  2.26579856 1.000000000
#> 119  0.0016900456 0.0013463480 -0.50400741 1.000000000
#> 157  0.0045631232 0.0023561091  2.25126800 1.000000000
#> 158  0.0052391415 0.0060585661 -0.33940764 1.000000000
#> 159  0.0040561095 0.0016829350  2.76060416 1.000000000
#> 161  0.0141963833 0.0124537193  0.07803570 1.000000000
#> 166  0.0015210411 0.0026926961  0.91246323 1.000000000
#> 167  0.0015210411 0.0023561091  0.46478617 1.000000000
#> 169  0.0016900456 0.0021878156 -0.32808441 1.000000000
#> 179  0.0028730776 0.0023561091 -0.42939565 1.000000000
#> 180  0.0035490958 0.0011780545  3.18326444 1.000000000
#> 190  0.0010140274 0.0011780545 -0.96628763 1.000000000
#> 194  0.0028730776 0.0025244026 -0.73615114 1.000000000
#> 199  0.0028730776 0.0031975766 -0.76252794 1.000000000
#> 213  0.0028730776 0.0013463480  1.68044213 1.000000000
#> 214  0.0032110867 0.0037024571 -0.55662348 1.000000000
#> 215  0.0055771506 0.0018512285  4.47491470 1.000000000
#> 216  0.0018590502 0.0015146415 -0.58134033 1.000000000
#> 217  0.0020280548 0.0008414675  1.59617582 1.000000000
#> 218  0.0081122190 0.0097610232  0.27688165 1.000000000
#> 221  0.0013520365 0.0013463480 -1.24175756 1.000000000
#> 222  0.0040561095 0.0072366207  2.48558077 1.000000000
#> 223  0.0010140274 0.0023561091  1.58080922 1.000000000
#> 224  0.0106472875 0.0131268933  0.75479303 1.000000000
#> 225  0.0094642555 0.0077415012  0.33975144 1.000000000
#> 226  0.0042251141 0.0033658701 -0.03194139 1.000000000
#> 227  0.0106472875 0.0102659037 -1.02084075 1.000000000
#> 229  0.0018590502 0.0042073376  2.60615493 1.000000000
#> 230  0.0030420821 0.0008414675  3.11229362 1.000000000
#> 231  0.0052391415 0.0025244026  2.45034181 1.000000000
#> 232  0.0114923103 0.0063951531  3.63290171 1.000000000
#> 235  0.0082812236 0.0062268596  0.92039773 1.000000000
#> 237  0.0052391415 0.0042073376 -0.03423477 1.000000000
#> 238  0.0076052053 0.0037024571  3.24010961 1.000000000
#> 240  0.0025350684 0.0021878156 -0.65647121 1.000000000
#> 241  0.0016900456 0.0010097610  0.37714323 1.000000000
#> 243  0.0018590502 0.0015146415 -0.56291642 1.000000000
#> 247  0.0011830319 0.0025244026  1.40576507 1.000000000
#> 249  0.0037181004 0.0075732077  3.34839802 1.000000000
#> 250  0.0013520365 0.0010097610 -0.35139027 1.000000000
#> 252  0.0027040730 0.0025244026 -0.97828135 1.000000000
#> 255  0.0033800913 0.0011780545  3.00649872 1.000000000
#> 256  0.0302518168 0.0269269606  0.50656213 1.000000000
#> 257  0.0028730776 0.0018512285  0.59280413 1.000000000
#> 258  0.0055771506 0.0074049142  0.69871653 1.000000000
#> 259  0.0167314517 0.0116122518  2.42545668 1.000000000
#> 260  0.0052391415 0.0074049142  1.28027708 1.000000000
#> 261  0.0331248944 0.0407270279  2.62758175 1.000000000
#> 274  0.0011830319 0.0015146415 -0.51733312 1.000000000
#> 279  0.0008450228 0.0013463480  0.08683897 1.000000000
#> 280  0.0013520365 0.0026926961  1.32299057 1.000000000
#> 281  0.0015210411 0.0018512285 -0.58285902 1.000000000
#> 284  0.0025350684 0.0058902726  3.23571369 1.000000000
#> 293  0.0077742099 0.0038707506  3.09108572 1.000000000
#> 294  0.0016900456 0.0021878156 -0.39978479 1.000000000
#> 295  0.0013520365 0.0043756311  3.85970227 1.000000000
#> 296  0.0016900456 0.0016829350 -1.23911606 1.000000000
#> 299  0.0059151597 0.0045439246  0.41018288 1.000000000
#> 300  0.0033800913 0.0030292831 -0.79387601 1.000000000
#> 302  0.0023660639 0.0018512285 -0.32631271 1.000000000
#> 303  0.0008450228 0.0010097610 -0.93856072 1.000000000
#> 305  0.0015210411 0.0023561091  0.41076474 1.000000000
#> 309  0.0011830319 0.0010097610 -0.86654215 1.000000000
#> 311  0.0025350684 0.0020195220 -0.33967676 1.000000000
#> 314  0.0021970593 0.0013463480  0.52648826 1.000000000
#> 318  0.0042251141 0.0055536856  0.31592115 1.000000000
#> 320  0.0011830319 0.0013463480 -0.94223724 1.000000000
#> 321  0.0033800913 0.0023561091  0.44637178 1.000000000
#> 322  0.0010140274 0.0013463480 -0.41345825 1.000000000
#> 323  0.0064221734 0.0089195557  1.24104733 1.000000000
#> 330  0.0008450228 0.0021878156  1.78390338 1.000000000
#> 342  0.0057461551 0.0023561091  3.32571998 1.000000000
#> 343  0.0091262464 0.0052170986  2.97579348 1.000000000
#> 344  0.0047321278 0.0018512285  3.26279624 1.000000000
#> 345  0.0018590502 0.0018512285 -1.32016039 1.000000000
#> 348  0.0013520365 0.0015146415 -0.98370704 1.000000000
#> 349  0.0032110867 0.0047122181  0.55483906 1.000000000
#> 350  0.0023660639 0.0015146415  0.43807710 1.000000000
#> 351  0.0010140274 0.0016829350  0.41086571 1.000000000
#> 352  0.0028730776 0.0031975766 -0.80353614 1.000000000
#> 354  0.0011830319 0.0011780545 -1.28758509 1.000000000
#> 355  0.0103092784 0.0069000337  2.04485189 1.000000000
#> 356  0.0015210411 0.0023561091  0.46395777 1.000000000
#> 357  0.0035490958 0.0075732077  2.92567605 1.000000000
#> 358  0.0027040730 0.0043756311  1.16094626 1.000000000
#> 361  0.0032110867 0.0048805116  1.13616258 1.000000000
#> 362  0.0050701369 0.0053853921 -0.91825330 1.000000000
#> 363  0.0067601825 0.0053853921  0.34624748 1.000000000
#> 365  0.0020280548 0.0016829350 -0.56890316 1.000000000
#> 368  0.0030420821 0.0010097610  2.81721055 1.000000000
#> 374  0.0008450228 0.0021878156  1.89609641 1.000000000
#> 384  0.0116613149 0.0109390778 -0.71284984 1.000000000
#> 389  0.0021970593 0.0008414675  1.66456590 1.000000000
#> 392  0.0212945750 0.0164927634  2.14437266 1.000000000
#> 393  0.0037181004 0.0062268596  1.80819680 1.000000000
#> 394  0.0011830319 0.0021878156  0.94828726 1.000000000
#> 395  0.0057461551 0.0026926961  3.19575774 1.000000000
#> 397  0.0057461551 0.0037024571  1.37379407 1.000000000
#> 401  0.0043941186 0.0028609896  0.96616946 1.000000000
#> 402  0.0065911780 0.0065634467 -1.33158718 1.000000000
#> 403  0.0033800913 0.0015146415  1.93141308 1.000000000
#> 404  0.0015210411 0.0026926961  1.08538171 1.000000000
#> 405  0.0138583742 0.0109390778  1.10167300 1.000000000
#> 417  0.0008450228 0.0010097610 -0.87598231 1.000000000
#> 418  0.0023660639 0.0026926961 -0.69872777 1.000000000
#> 419  0.0020280548 0.0010097610  0.83214811 1.000000000
#> 422  0.0018590502 0.0018512285 -1.35142476 1.000000000
#> 423  0.0030420821 0.0028609896 -0.99595902 1.000000000
#> 424  0.0023660639 0.0023561091 -1.26976071 1.000000000
#> 433  0.0011830319 0.0016829350 -0.13125517 1.000000000
#> 438  0.0035490958 0.0031975766 -0.83624965 1.000000000
#> 441  0.0020280548 0.0015146415 -0.24764012 1.000000000
#> 443  0.0040561095 0.0050488051  0.03005131 1.000000000
#> 451  0.0013520365 0.0015146415 -0.95316272 1.000000000
#> 458  0.0016900456 0.0015146415 -0.92865326 1.000000000
#> 459  0.0030420821 0.0015146415  1.62149442 1.000000000
#> 462  0.0035490958 0.0037024571 -1.07885509 1.000000000
#> 468  0.0008450228 0.0010097610 -0.88386493 1.000000000
#> 473  0.0023660639 0.0010097610  1.73848671 1.000000000
#> 474  0.0070981917 0.0045439246  1.74099490 1.000000000
#> 475  0.0011830319 0.0028609896  1.98007127 1.000000000
#> 476  0.0013520365 0.0030292831  1.94760734 1.000000000
#> 479  0.0018590502 0.0045439246  3.22022294 1.000000000
#> 480  0.0008450228 0.0010097610 -0.83626425 1.000000000
#> 481  0.0025350684 0.0023561091 -0.99296128 1.000000000
#> 492  0.0027040730 0.0042073376  0.83371071 1.000000000
#> 495  0.0021970593 0.0015146415  0.06569629 1.000000000
#> 497  0.0050701369 0.0047122181 -0.86376435 1.000000000
#> 505  0.0050701369 0.0018512285  3.86915205 1.000000000
#> 513  0.0059151597 0.0048805116 -0.01516115 1.000000000
#> 514  0.0121683285 0.0153147089  1.11057512 1.000000000
#> 515  0.0143653879 0.0095927297  2.63902777 1.000000000
#> 516  0.0049011323 0.0053853921 -0.64307694 1.000000000
#> 517  0.0037181004 0.0063951531  2.27289598 1.000000000
#> 518  0.0236606388 0.0277684281  0.89010990 1.000000000
#> 525  0.0021970593 0.0011780545  0.79404415 1.000000000
#> 528  0.0047321278 0.0037024571  0.09305559 1.000000000
#> 529  0.0018590502 0.0021878156 -0.62008022 1.000000000
#> 530  0.0025350684 0.0040390441  1.12622977 1.000000000
#> 531  0.0011830319 0.0018512285  0.19296650 1.000000000
#> 534  0.0020280548 0.0028609896  0.24066181 1.000000000
#> 536  0.0135203650 0.0087512622  2.86984436 1.000000000
#> 537  0.0133513605 0.0090878492  2.40963314 1.000000000
#> 539  0.0060841643 0.0018512285  4.69643157 1.000000000
#> 540  0.0021970593 0.0028609896 -0.10427104 1.000000000
#> 542  0.0040561095 0.0037024571 -0.85720745 1.000000000
#> 545  0.0038871050 0.0033658701 -0.51268718 1.000000000
#> 547  0.0054081460 0.0063951531 -0.06420258 1.000000000
#> 548  0.0013520365 0.0011780545 -0.90051396 1.000000000
#> 550  0.0023660639 0.0053853921  3.17917348 1.000000000
#> 553  0.0020280548 0.0037024571  1.43905798 1.000000000
#> 554  0.0223086023 0.0309660047  3.57323026 1.000000000
#> 555  0.0020280548 0.0011780545  0.61911620 1.000000000
#> 556  0.0052391415 0.0075732077  1.39530604 1.000000000
#> 557  0.0140273787 0.0114439583  0.83932695 1.000000000
#> 558  0.0064221734 0.0058902726 -0.69784532 1.000000000
#> 562  0.0016900456 0.0038707506  2.45914855 1.000000000
#> 573  0.0008450228 0.0010097610 -0.91376916 1.000000000
#> 574  0.0021970593 0.0018512285 -0.61951440 1.000000000
#> 575  0.0037181004 0.0026926961  0.34280722 1.000000000
#> 578  0.0060841643 0.0047122181  0.42874220 1.000000000
#> 720  0.0016550202 0.0012813472 -0.40857093 1.000000000
#> 725  0.0018389114 0.0021965953 -0.65415184 1.000000000
#> 728  0.0011033468 0.0012813472 -0.86226327 1.000000000
#> 731  0.0051489518 0.0016474465  3.99801764 1.000000000
#> 733  0.0020228025 0.0009152480  1.14071122 1.000000000
#> 748  0.0051489518 0.0032948929  1.24599554 1.000000000
#> 749  0.0009194557 0.0010982976 -0.90048422 1.000000000
#> 750  0.0022066936 0.0021965953 -1.36867323 1.000000000
#> 752  0.0055167341 0.0049423394 -0.64362215 1.000000000
#> 805  0.0011033468 0.0014643969 -0.41844368 1.000000000
#> 858  0.0014711291 0.0010982976 -0.38357948 1.000000000
#> 922  0.0009194557 0.0010982976 -0.92318218 1.000000000
#> 925  0.0011033468 0.0009152480 -0.83599140 1.000000000
#> 943  0.0038617139 0.0034779425 -0.80570681 1.000000000
#> 948  0.0022066936 0.0032948929  0.40222625 1.000000000
#> 962  0.0009194557 0.0014643969  0.06314635 1.000000000
#> 965  0.0014711291 0.0021965953  0.09299347 1.000000000
#> 974  0.0055167341 0.0027457441  2.42722339 1.000000000
#> 977  0.0011033468 0.0010982976 -1.28605246 1.000000000
#> 980  0.0012872380 0.0025626945  1.33832253 1.000000000
#> 981  0.0038617139 0.0018304961  2.19118835 1.000000000
#> 982  0.0020228025 0.0023796449 -0.67885007 1.000000000
#> 984  0.0014711291 0.0009152480  0.04741618 1.000000000
#> 987  0.0011033468 0.0012813472 -0.91157280 1.000000000
#> 990  0.0009194557 0.0009152480 -1.24344809 1.000000000
#> 998  0.0023905848 0.0025626945 -1.06801668 1.000000000
#> 1001 0.0022066936 0.0016474465 -0.25195496 1.000000000
#> 1003 0.0033100405 0.0047592898  0.60944265 1.000000000
#> 1007 0.0011033468 0.0027457441  1.94981329 1.000000000
#> 1019 0.0036778227 0.0010982976  3.45090159 1.000000000
#> 1020 0.0060684075 0.0009152480  5.54906127 1.000000000
#> 1023 0.0097462302 0.0042101409  4.36227012 1.000000000
#> 1026 0.0012872380 0.0010982976 -0.92760461 1.000000000
#> 1032 0.0031261493 0.0010982976  2.43926811 1.000000000
#> 1034 0.0023905848 0.0016474465  0.16930559 1.000000000
#> 1035 0.0016550202 0.0009152480  0.50936282 1.000000000
#> 1038 0.0018389114 0.0009152480  0.85498817 1.000000000
#> 1039 0.0020228025 0.0010982976  0.58978734 1.000000000
#> 1040 0.0036778227 0.0009152480  3.64844333 1.000000000
#> 1057 0.0057006252 0.0025626945  2.78070764 1.000000000
#> 1064 0.0034939316 0.0018304961  1.44951099 1.000000000
#> 1071 0.0022066936 0.0010982976  1.11285821 1.000000000
#> 1118 0.0012872380 0.0014643969 -0.95101542 1.000000000
#> 1151 0.0038617139 0.0021965953  1.24561607 1.000000000
#> 1152 0.0062522986 0.0053084386 -0.22577543 1.000000000
#> 1153 0.0060684075 0.0045762402  0.35352689 1.000000000
#> 1154 0.0023905848 0.0020135457 -0.60401979 1.000000000
#> 1155 0.0022066936 0.0018304961 -0.58476961 1.000000000
#> 1156 0.0093784480 0.0098846787 -0.88584975 1.000000000
#> 1167 0.0022066936 0.0016474465 -0.32920874 1.000000000
#> 1169 0.0012872380 0.0020135457  0.22729371 1.000000000
#> 1174 0.0066200809 0.0040270913  1.77624666 1.000000000
#> 1175 0.0057006252 0.0036609921  1.32101534 1.000000000
#> 1180 0.0011033468 0.0012813472 -0.97127965 1.000000000
#> 1185 0.0022066936 0.0018304961 -0.55067909 1.000000000
#> 1188 0.0009194557 0.0032948929  3.20638757 1.000000000
#> 1189 0.0009194557 0.0016474465  0.35634878 1.000000000
#> 1190 0.0079073189 0.0120812740  2.32518091 1.000000000
#> 1192 0.0027583670 0.0025626945 -1.02052330 1.000000000
#> 1193 0.0058845164 0.0040270913  1.11689837 1.000000000
#> 1194 0.0033100405 0.0020135457  0.90105358 1.000000000
#> 1195 0.0104817948 0.0184880103  4.61638716 1.000000000
#> 1266 0.0009194557 0.0031118433  2.97361103 1.000000000
#> 1290 0.0012872380 0.0010982976 -0.94666450 1.000000000
#> 1294 0.0031261493 0.0010982976  2.68517726 1.000000000
#> 1302 0.0009194557 0.0014643969  0.07881242 1.000000000
#> 1322 0.0011033468 0.0032948929  2.64831705 1.000000000
#> 1327 0.0022066936 0.0029287937 -0.05550283 1.000000000
#> 1338 0.0016550202 0.0012813472 -0.48687496 1.000000000
#> 1377 0.0011033468 0.0009152480 -0.87320955 1.000000000
#> 1397 0.0009194557 0.0009152480 -1.26795697 1.000000000
#> 1401 0.0011033468 0.0020135457  0.66652042 1.000000000
#> 1423 0.0022066936 0.0027457441 -0.38521682 1.000000000
#> 1426 0.0016550202 0.0014643969 -0.93448971 1.000000000
#> 1428 0.0016550202 0.0029287937  0.98973805 1.000000000
#> 1505 0.0027583670 0.0012813472  1.40257140 1.000000000
#> 1511 0.0022066936 0.0016474465 -0.21363864 1.000000000
#> 1513 0.0022066936 0.0010982976  0.98326850 1.000000000
#> 1518 0.0031261493 0.0010982976  2.43559571 1.000000000
#> 1522 0.0018389114 0.0010982976  0.35391511 1.000000000
#> 1526 0.0009194557 0.0009152480 -1.25965417 1.000000000
#> 1539 0.0080912100 0.0034779425  3.99001893 1.000000000
#> 1541 0.0012872380 0.0010982976 -0.92421543 1.000000000
#> 1542 0.0044133873 0.0014643969  3.64919969 1.000000000
#> 1543 0.0016550202 0.0009152480  0.42658006 1.000000000
#> 1544 0.0068039720 0.0067728354 -1.25783576 1.000000000
#> 1561 0.0009194557 0.0010982976 -0.91499975 1.000000000
#> 1578 0.0014711291 0.0014643969 -1.33747847 1.000000000
#> 1596 0.0011033468 0.0009152480 -0.85871176 1.000000000
#> 1597 0.0031261493 0.0009152480  2.75813265 1.000000000
#> 1600 0.0044133873 0.0034779425  0.01131102 1.000000000
#> 1611 0.0014711291 0.0020135457 -0.20822939 1.000000000
#> 1618 0.0014711291 0.0016474465 -0.98110530 1.000000000
#> 1631 0.0014711291 0.0014643969 -1.30315563 1.000000000
#> 1637 0.0027583670 0.0027457441 -1.29462390 1.000000000
#> 1646 0.0009194557 0.0012813472 -0.36963729 1.000000000
#> 1650 0.0029422582 0.0016474465  1.17293992 1.000000000
#> 1703 0.0012872380 0.0036609921  2.81469682 1.000000000
#> 1715 0.0018389114 0.0018304961 -1.32287766 1.000000000
#> 1716 0.0027583670 0.0023796449 -0.66791798 1.000000000
#> 1719 0.0049650607 0.0038440417  0.16735880 1.000000000
#> 1750 0.0029422582 0.0018304961  0.61740916 1.000000000
#> 1751 0.0027583670 0.0034779425 -0.23576929 1.000000000
#> 1752 0.0062522986 0.0021965953  4.30681285 1.000000000
#> 1755 0.0069878632 0.0075050339 -0.79001891 1.000000000
#> 1759 0.0011033468 0.0012813472 -0.93015218 1.000000000
#> 1761 0.0011033468 0.0010982976 -1.35921151 1.000000000
#> 1789 0.0012872380 0.0018304961 -0.10731591 1.000000000
#> 1802 0.0009194557 0.0014643969  0.02486300 1.000000000
#> 1808 0.0020228025 0.0020135457 -1.35627281 1.000000000
#> 1809 0.0018389114 0.0010982976  0.27794427 1.000000000
#> 1811 0.0011033468 0.0016474465 -0.01837933 1.000000000
#> 1815 0.0011033468 0.0009152480 -0.88292827 1.000000000
#> 1819 0.0012872380 0.0010982976 -0.91356937 1.000000000
#> 1832 0.0034939316 0.0036609921 -1.04093872 1.000000000
#> 1835 0.0020228025 0.0023796449 -0.66737523 1.000000000
#> 1837 0.0051489518 0.0038440417  0.34640563 1.000000000
#> 1894 0.0018389114 0.0009152480  0.85285701 1.000000000
#> 1899 0.0014711291 0.0009152480  0.01721954 1.000000000
#> 1909 0.0011033468 0.0014643969 -0.42170516 1.000000000
#> 1976 0.0012872380 0.0009152480 -0.29759941 1.000000000
#> 1980 0.0009194557 0.0020135457  1.21980001 1.000000000
#> 2027 0.0018389114 0.0014643969 -0.53963099 1.000000000
#> 2073 0.0016550202 0.0009152480  0.48095080 1.000000000
#> 2076 0.0022066936 0.0023796449 -1.05604099 1.000000000
#> 2086 0.0014711291 0.0010982976 -0.40576330 1.000000000
#> 2163 0.0018389114 0.0012813472 -0.09871376 1.000000000
#> 2167 0.0016550202 0.0021965953 -0.25111392 1.000000000
#> 2189 0.0012872380 0.0009152480 -0.25251099 1.000000000
#> 2219 0.0014711291 0.0023796449  0.44512975 1.000000000
#> 2223 0.0014711291 0.0034779425  2.18298597 1.000000000
#> 2225 0.0025744759 0.0036609921  0.39464233 1.000000000
#> 2226 0.0031261493 0.0018304961  0.79711321 1.000000000
#> 2227 0.0011033468 0.0020135457  0.62131872 1.000000000
#> 2228 0.0033100405 0.0038440417 -0.60424234 1.000000000
#> 2232 0.0053328430 0.0020135457  3.21812577 1.000000000
#> 2234 0.0031261493 0.0021965953  0.28298348 1.000000000
#> 2235 0.0023905848 0.0012813472  1.03359225 1.000000000
#> 2238 0.0016550202 0.0016474465 -1.33858070 1.000000000
#> 2240 0.0011033468 0.0018304961  0.34150489 1.000000000
#> 2241 0.0012872380 0.0012813472 -1.31329128 1.000000000
#> 2250 0.0012872380 0.0032948929  2.32581959 1.000000000
#> 2253 0.0011033468 0.0010982976 -1.31576642 1.000000000
#> 2256 0.0053328430 0.0076880835  1.04396554 1.000000000
#> 2257 0.0011033468 0.0009152480 -0.87532303 1.000000000
#> 2258 0.0009194557 0.0020135457  1.21689549 1.000000000
#> 2259 0.0044133873 0.0027457441  0.98348859 1.000000000
#> 2260 0.0022066936 0.0025626945 -0.65963258 1.000000000
#> 2261 0.0088267745 0.0117151748  1.11692724 1.000000000
#> 2300 0.0018389114 0.0018304961 -1.29588876 1.000000000
#> 2334 0.0079073189 0.0045762402  2.50597215 1.000000000
#> 2339 0.0022066936 0.0012813472  0.56643295 1.000000000
#> 2342 0.0011033468 0.0010982976 -1.28071142 1.000000000
#> 2343 0.0022066936 0.0020135457 -0.96322408 1.000000000
#> 2344 0.0033100405 0.0018304961  1.21543120 1.000000000
#> 2347 0.0044133873 0.0031118433  0.48897975 1.000000000
#> 2361 0.0009194557 0.0010982976 -0.88834812 1.000000000
#> 2372 0.0012872380 0.0009152480 -0.33315413 1.000000000
#> 2377 0.0016550202 0.0012813472 -0.48918240 1.000000000
#> 2388 0.0011033468 0.0016474465 -0.01735598 1.000000000
#> 2396 0.0016550202 0.0009152480  0.53524965 1.000000000
#> 2398 0.0009194557 0.0020135457  1.12973661 1.000000000
#> 2415 0.0009194557 0.0018304961  0.92300651 1.000000000
#> 2418 0.0014711291 0.0012813472 -0.89268316 1.000000000
#> 2425 0.0011033468 0.0023796449  1.55519162 1.000000000
#> 2426 0.0029422582 0.0071389347  3.87704278 1.000000000
#> 2427 0.0051489518 0.0038440417  0.37617092 1.000000000
#> 2428 0.0014711291 0.0020135457 -0.16846155 1.000000000
#> 2429 0.0011033468 0.0032948929  2.89047128 1.000000000
#> 2430 0.0095623391 0.0122643236  0.90217357 1.000000000
#> 2440 0.0016550202 0.0016474465 -1.31712755 1.000000000
#> 2442 0.0009194557 0.0012813472 -0.30178383 1.000000000
#> 2447 0.0034939316 0.0020135457  1.16179333 1.000000000
#> 2448 0.0051489518 0.0036609921  0.63776869 1.000000000
#> 2450 0.0023905848 0.0010982976  1.38826579 1.000000000
#> 2451 0.0009194557 0.0018304961  0.93728496 1.000000000
#> 2453 0.0016550202 0.0018304961 -0.96421672 1.000000000
#> 2455 0.0016550202 0.0016474465 -1.24945832 1.000000000
#> 2457 0.0020228025 0.0018304961 -0.99382578 1.000000000
#> 2459 0.0011033468 0.0014643969 -0.44690205 1.000000000
#> 2463 0.0073556455 0.0126304228  3.14927585 1.000000000
#> 2465 0.0009194557 0.0040270913  4.10933396 1.000000000
#> 2466 0.0040456050 0.0045762402 -0.62462032 1.000000000
#> 2467 0.0016550202 0.0023796449 -0.02197183 1.000000000
#> 2468 0.0084589923 0.0201354567  4.14405033 1.000000000
#> 2520 0.0016550202 0.0010982976 -0.00487915 1.000000000
#> 2536 0.0011033468 0.0012813472 -0.96358838 1.000000000
#> 2540 0.0033100405 0.0016474465  1.60216647 1.000000000
#> 2873 0.0021969243 0.0009956193  1.19825594 1.000000000
#> 2887 0.0013980427 0.0011947431 -0.93873657 1.000000000
#> 2889 0.0013980427 0.0009956193 -0.32452460 1.000000000
#> 2891 0.0019972039 0.0013938670 -0.16561829 1.000000000
#> 3227 0.0009986020 0.0015929908  0.05311381 1.000000000
#> 3246 0.0015977631 0.0015929908 -1.31546822 1.000000000
#> 3343 0.0009986020 0.0011947431 -0.84648970 1.000000000
#> 3347 0.0015977631 0.0011947431 -0.37965424 1.000000000
#> 3405 0.0009986020 0.0011947431 -0.91174803 1.000000000
#> 3426 0.0011983223 0.0013938670 -0.90392340 1.000000000
#> 3493 0.0025963651 0.0011947431  1.29134902 1.000000000
#> 3498 0.0033952467 0.0017921147  1.22028083 1.000000000
#> 3606 0.0025963651 0.0019912386 -0.23637230 1.000000000
#> 3623 0.0009986020 0.0011947431 -0.87181859 1.000000000
#> 3822 0.0015977631 0.0011947431 -0.46525671 1.000000000
#> 3826 0.0009986020 0.0009956193 -1.28599831 1.000000000
#> 3839 0.0023966447 0.0029868578 -0.45603102 1.000000000
#> 3844 0.0033952467 0.0041816010 -0.30382329 1.000000000
#> 3894 0.0043938486 0.0017921147  2.83564924 1.000000000
#> 3905 0.0013980427 0.0015929908 -0.95715378 1.000000000
#> 3964 0.0013980427 0.0011947431 -0.91546872 1.000000000
#> 3967 0.0035949670 0.0041816010 -0.51885481 1.000000000
#> 3983 0.0015977631 0.0011947431 -0.45038901 1.000000000
#> 3984 0.0015977631 0.0013938670 -0.93824765 1.000000000
#> 3998 0.0039944078 0.0045798487 -0.59229234 1.000000000
#> 4001 0.0017974835 0.0021903624 -0.56088099 1.000000000
#> 4003 0.0023966447 0.0071684588  4.49723033 1.000000000
#> 4239 0.0011983223 0.0017921147 -0.11640247 1.000000000
#> 4719 0.0021969243 0.0011947431  0.56283571 1.000000000
#> 4722 0.0019972039 0.0009956193  0.85231150 1.000000000
#> 4748 0.0009986020 0.0021903624  1.21470747 1.000000000
#> 4753 0.0027960855 0.0025886101 -0.99680618 1.000000000
#> 4885 0.0013980427 0.0013938670 -1.32729820 1.000000000
#> 4890 0.0011983223 0.0013938670 -0.91850339 1.000000000
#> 5143 0.0011983223 0.0009956193 -0.87314366 1.000000000
#> 5156 0.0011983223 0.0009956193 -0.85076213 1.000000000
#> 5160 0.0027960855 0.0011947431  1.71181706 1.000000000
#> 5218 0.0011983223 0.0009956193 -0.85153665 1.000000000
#> 5222 0.0015977631 0.0011947431 -0.52384647 1.000000000
#> 5243 0.0023966447 0.0021903624 -0.95210602 1.000000000
#> 5245 0.0011983223 0.0013938670 -0.92648714 1.000000000
#> 5247 0.0021969243 0.0027877340 -0.31914050 1.000000000
#> 5459 0.0009986020 0.0009956193 -1.28550000 1.000000000
#> 5487 0.0013980427 0.0019912386 -0.14357548 1.000000000
#> 6204 0.0009986020 0.0009956193 -1.30471432 1.000000000
#> 6225 0.0013980427 0.0021903624  0.23417477 1.000000000
#> 6238 0.0027960855 0.0009956193  2.03109805 1.000000000
#> 6261 0.0015977631 0.0009956193  0.07816059 1.000000000
#> 6325 0.0013980427 0.0031859817  1.60621022 1.000000000
#> 6340 0.0009986020 0.0013938670 -0.36395117 1.000000000
#> 6352 0.0019972039 0.0035842294  1.11108920 1.000000000
#> 6357 0.0027960855 0.0065710872  3.26326129 1.000000000
#> 6504 0.0021969243 0.0015929908 -0.15969365 1.000000000
#> 6561 0.0019972039 0.0009956193  0.82342794 1.000000000
#> 6747 0.0021969243 0.0009956193  1.24135693 1.000000000
#> 6748 0.0009986020 0.0009956193 -1.24663807 1.000000000
#> 6765 0.0017974835 0.0033851055  1.23017335 1.000000000
#> 6769 0.0009986020 0.0011947431 -0.87262858 1.000000000
#> 6770 0.0031955263 0.0051772202  1.20027331 1.000000000
#> 6791 0.0011983223 0.0011947431 -1.30590893 1.000000000
#> 6810 0.0011983223 0.0013938670 -0.87570991 1.000000000
#> 6821 0.0019972039 0.0013938670 -0.10946911 1.000000000
#> 6873 0.0011983223 0.0025886101  1.39696118 1.000000000
#> 6874 0.0015977631 0.0015929908 -1.32428502 1.000000000
#> 6877 0.0027960855 0.0059737157  2.61726318 1.000000000
#> 6893 0.0017974835 0.0013938670 -0.49613759 1.000000000
#> 6897 0.0011983223 0.0011947431 -1.24292707 1.000000000
#> 6905 0.0017974835 0.0063719634  4.83829198 1.000000000
#> 6908 0.0017974835 0.0017921147 -1.26603463 1.000000000
#> 6910 0.0041941282 0.0095579450  2.10432848 1.000000000
#> 7051 0.0015977631 0.0009956193  0.11550728 1.000000000
# }
```

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
#>                                               pattern freq_High    prop_High
#> 1                                   discuss-consensus       851 0.0668972565
#> 2                                   consensus-discuss       789 0.0620234258
#> 3                                       discuss-adapt        48 0.0037732883
#> 4                         consensus-discuss-consensus       291 0.0248272332
#> 5                           discuss-consensus-discuss       198 0.0168927566
#> 6                                           plan-plan       948 0.0745224432
#> 7                                      plan-plan-plan       282 0.0240593806
#> 8                 discuss-consensus-discuss-consensus        68 0.0063150074
#> 9                                 plan-plan-plan-plan        83 0.0077080238
#> 10                         cohesion-consensus-discuss       129 0.0110058869
#> 11                            discuss-adapt-consensus        26 0.0022182408
#> 12                consensus-discuss-consensus-discuss        63 0.0058506686
#> 13                                    adapt-consensus        73 0.0057385426
#> 14                                    synthesis-adapt        40 0.0031444069
#> 15                          emotion-discuss-consensus        75 0.0063987714
#> 16                   consensus-discuss-consensus-plan       103 0.0095653789
#> 17                              discuss-discuss-adapt         6 0.0005119017
#> 18                                 cohesion-consensus       503 0.0395409166
#> 19                             discuss-consensus-plan       302 0.0257657196
#> 20                            discuss-synthesis-adapt        29 0.0024741916
#> 21                                     adapt-cohesion        37 0.0029085764
#> 22                                  discuss-synthesis       213 0.0167439667
#> 23                             discuss-adapt-cohesion        12 0.0010238034
#> 24                          consensus-discuss-emotion        92 0.0078491596
#> 25                                    emotion-discuss       189 0.0148573225
#> 26                                  cohesion-cohesion        41 0.0032230171
#> 27                consensus-discuss-discuss-consensus        51 0.0047362556
#> 28                                       plan-emotion       527 0.0414275607
#> 29                                 coregulate-discuss       210 0.0165081362
#> 30                      consensus-plan-plan-plan-plan        25 0.0025255076
#> 31                 emotion-cohesion-consensus-discuss        61 0.0056649331
#> 32                         consensus-discuss-cohesion        39 0.0033273611
#> 33                       coregulate-discuss-synthesis        20 0.0017063390
#> 34                  discuss-consensus-discuss-discuss        36 0.0033432392
#> 35                emotion-consensus-discuss-consensus        41 0.0038075780
#> 36                                  emotion-consensus       521 0.0409558997
#> 37                             plan-plan-plan-discuss         9 0.0008358098
#> 38                                   discuss-cohesion       124 0.0097476613
#> 39                                     synthesis-plan        40 0.0031444069
#> 40                  discuss-discuss-consensus-discuss        36 0.0033432392
#> 41                                  consensus-emotion       276 0.0216964075
#> 42                               adapt-consensus-plan        21 0.0017916560
#> 43                      plan-plan-plan-plan-consensus        18 0.0018183655
#> 44                       discuss-adapt-consensus-plan         5 0.0004643388
#> 45                          adapt-consensus-plan-plan         6 0.0005572065
#> 46                               coregulate-consensus        97 0.0076251867
#> 47                           plan-plan-plan-plan-plan        31 0.0031316295
#> 48                          emotion-consensus-discuss       111 0.0094701817
#> 49                                plan-plan-consensus       256 0.0218411398
#> 50                          consensus-discuss-discuss       140 0.0119443733
#> 51                                 consensus-cohesion        67 0.0052668815
#> 52                           coregulate-discuss-adapt         6 0.0005119017
#> 53               cohesion-consensus-discuss-consensus        42 0.0039004458
#> 54                               plan-emotion-discuss        61 0.0052043341
#> 55                   plan-consensus-discuss-consensus        56 0.0052005944
#> 56                            monitor-discuss-discuss        27 0.0023035577
#> 57                         coregulate-discuss-discuss        27 0.0023035577
#> 58                          discuss-discuss-consensus       144 0.0122856412
#> 59                           consensus-plan-plan-plan       118 0.0109583952
#> 60                   discuss-consensus-plan-consensus        89 0.0082652303
#> 61                                  synthesis-discuss         8 0.0006288814
#> 62                           synthesis-adapt-cohesion         6 0.0005119017
#> 63             consensus-discuss-consensus-coregulate        42 0.0039004458
#> 64                             plan-emotion-consensus       167 0.0142479311
#> 65                                consensus-plan-plan       382 0.0325910758
#> 66                         adapt-consensus-coregulate        14 0.0011944373
#> 67                         emotion-cohesion-consensus       235 0.0200494838
#> 68                            discuss-discuss-discuss        53 0.0045217985
#> 69                       discuss-consensus-coregulate       128 0.0109205699
#> 70                                      adapt-discuss         5 0.0003930509
#> 71                         discuss-cohesion-consensus        64 0.0054602850
#> 72                             consensus-plan-emotion       202 0.0172340244
#> 73                           plan-plan-plan-consensus        78 0.0072436850
#> 74              plan-consensus-discuss-consensus-plan        28 0.0028285685
#> 75                                cohesion-coregulate        76 0.0059743731
#> 76                       consensus-cohesion-consensus        33 0.0028154594
#> 77                                  plan-plan-discuss        51 0.0043511646
#> 78                     plan-plan-consensus-coregulate        38 0.0035289747
#> 79                   discuss-synthesis-adapt-cohesion         5 0.0004643388
#> 80                              plan-emotion-cohesion       161 0.0137360293
#> 81                                       plan-discuss       174 0.0136781700
#> 82                     discuss-consensus-plan-emotion        46 0.0042719168
#> 83                        discuss-consensus-consensus        59 0.0050337002
#> 84                          synthesis-adapt-consensus        20 0.0017063390
#> 85                        consensus-consensus-discuss        56 0.0047777493
#> 86         consensus-discuss-consensus-plan-consensus        30 0.0030306092
#> 87                            plan-cohesion-consensus        53 0.0045217985
#> 88                              discuss-adapt-emotion         6 0.0005119017
#> 89                consensus-discuss-emotion-consensus        33 0.0030646360
#> 90                             plan-consensus-monitor        28 0.0023888747
#> 91                   discuss-adapt-cohesion-consensus         8 0.0007429421
#> 92                          coregulate-consensus-plan        34 0.0029007764
#> 93                     coregulate-consensus-plan-plan        11 0.0010215453
#> 94                                    discuss-discuss       339 0.0266488484
#> 95              consensus-consensus-discuss-consensus        24 0.0022288262
#> 96                         discuss-coregulate-discuss        32 0.0027301425
#> 97                              coregulate-coregulate        12 0.0009433221
#> 98                      consensus-plan-plan-consensus        92 0.0085438336
#> 99                                  consensus-monitor       120 0.0094332207
#> 100                          adapt-cohesion-consensus        22 0.0018769730
#> 101                                  cohesion-monitor        16 0.0012577628
#> 102                                   plan-coregulate        69 0.0054241019
#> 103                 consensus-plan-cohesion-consensus        25 0.0023216939
#> 104                       plan-plan-consensus-monitor         8 0.0007429421
#> 105                 discuss-synthesis-adapt-consensus        13 0.0012072808
#> 106                            discuss-synthesis-plan        28 0.0023888747
#> 107                             cohesion-plan-emotion        28 0.0023888747
#> 108                    consensus-coregulate-consensus        53 0.0045217985
#> 109                discuss-adapt-consensus-coregulate         6 0.0005572065
#> 110                   coregulate-consensus-coregulate        14 0.0011944373
#> 111              consensus-emotion-cohesion-consensus        37 0.0034361070
#> 112               plan-plan-plan-consensus-coregulate         9 0.0009091827
#> 113                         discuss-discuss-synthesis        31 0.0026448255
#> 114                                     cohesion-plan       142 0.0111626444
#> 115                                  emotion-cohesion       505 0.0396981369
#> 116                                   monitor-discuss       234 0.0183947803
#> 117                          emotion-cohesion-monitor         7 0.0005972187
#> 118         consensus-coregulate-consensus-coregulate         7 0.0006500743
#> 119                       consensus-emotion-consensus        86 0.0073372579
#> 120              consensus-coregulate-discuss-discuss        12 0.0011144131
#> 121                        cohesion-consensus-emotion        46 0.0039245798
#> 122           plan-emotion-cohesion-consensus-discuss        20 0.0020204061
#> 123                         monitor-discuss-consensus        88 0.0075078918
#> 124                              plan-discuss-discuss        26 0.0022182408
#> 125                        emotion-coregulate-discuss         5 0.0004265848
#> 126               emotion-cohesion-coregulate-discuss         5 0.0004643388
#> 127                         monitor-discuss-synthesis        25 0.0021329238
#> 128                         consensus-monitor-discuss        40 0.0034126781
#> 129                       monitor-consensus-plan-plan         7 0.0006500743
#> 130                 consensus-discuss-synthesis-adapt         9 0.0008358098
#> 131                    discuss-consensus-plan-monitor        24 0.0022288262
#> 132                   plan-emotion-cohesion-consensus        76 0.0070579495
#> 133                    emotion-discuss-consensus-plan        27 0.0025074294
#> 134                       emotion-consensus-consensus        51 0.0043511646
#> 135                            plan-consensus-discuss       161 0.0137360293
#> 136            consensus-coregulate-discuss-synthesis        14 0.0013001486
#> 137      emotion-cohesion-consensus-discuss-consensus        20 0.0020204061
#> 138                         discuss-consensus-emotion        54 0.0046071154
#> 139                              plan-coregulate-plan        23 0.0019622899
#> 140                                     adapt-emotion        20 0.0015722034
#> 141          consensus-plan-emotion-consensus-discuss        18 0.0018183655
#> 142            consensus-coregulate-discuss-consensus        60 0.0055720654
#> 143                         discuss-synthesis-emotion         8 0.0006825356
#> 144                    plan-consensus-monitor-discuss         8 0.0007429421
#> 145                   discuss-cohesion-consensus-plan        21 0.0019502229
#> 146                emotion-cohesion-consensus-emotion        24 0.0022288262
#> 147                    plan-emotion-consensus-discuss        37 0.0034361070
#> 148                           consensus-plan-cohesion        37 0.0031567272
#> 149                           consensus-discuss-adapt        22 0.0018769730
#> 150                     cohesion-coregulate-consensus         7 0.0005972187
#> 151                plan-consensus-plan-plan-consensus        20 0.0020204061
#> 152           discuss-consensus-plan-emotion-cohesion        19 0.0019193858
#> 153                                emotion-coregulate        36 0.0028299662
#> 154                               plan-consensus-plan       289 0.0246565993
#> 155          consensus-plan-plan-consensus-coregulate        14 0.0014142843
#> 156               plan-plan-consensus-coregulate-plan         8 0.0008081624
#> 157                                  cohesion-discuss        38 0.0029871865
#> 158                        cohesion-emotion-consensus        36 0.0030714103
#> 159              discuss-consensus-coregulate-discuss        29 0.0026931649
#> 160                     plan-consensus-plan-consensus        63 0.0058506686
#> 161                discuss-cohesion-consensus-discuss        20 0.0018573551
#> 162               discuss-consensus-plan-plan-emotion        20 0.0020204061
#> 163                           discuss-emotion-discuss        26 0.0022182408
#> 164                       emotion-cohesion-coregulate        40 0.0034126781
#> 165            coregulate-discuss-synthesis-consensus        13 0.0012072808
#> 166                     plan-plan-plan-consensus-plan        30 0.0030306092
#> 167                            plan-discuss-synthesis        18 0.0015357051
#> 168              cohesion-consensus-emotion-consensus        18 0.0016716196
#> 169                    consensus-plan-emotion-discuss        18 0.0016716196
#> 170              plan-consensus-plan-emotion-cohesion        18 0.0018183655
#> 171                        discuss-discuss-coregulate        16 0.0013650712
#> 172                                discuss-coregulate       145 0.0113984750
#> 173               plan-consensus-coregulate-consensus        17 0.0015787519
#> 174                                   discuss-monitor        33 0.0025941357
#> 175                 discuss-consensus-coregulate-plan        32 0.0029717682
#> 176                    plan-consensus-discuss-emotion        25 0.0023216939
#> 177                  consensus-plan-emotion-consensus        64 0.0059435364
#> 178                       cohesion-coregulate-discuss        15 0.0012797543
#> 179                             emotion-cohesion-plan        70 0.0059721867
#> 180                         consensus-plan-coregulate        29 0.0024741916
#> 181                            monitor-consensus-plan        31 0.0026448255
#> 182                          plan-plan-consensus-plan        99 0.0091939079
#> 183                         consensus-emotion-discuss        33 0.0028154594
#> 184                      consensus-discuss-coregulate        56 0.0047777493
#> 185             discuss-consensus-plan-consensus-plan        26 0.0026265279
#> 186                                     plan-cohesion        91 0.0071535257
#> 187                   consensus-plan-emotion-cohesion        67 0.0062221397
#> 188               discuss-consensus-emotion-consensus        17 0.0015787519
#> 189                 emotion-consensus-discuss-discuss        20 0.0018573551
#> 190             plan-plan-consensus-discuss-consensus        20 0.0020204061
#> 191                cohesion-consensus-discuss-discuss        27 0.0025074294
#> 192                               consensus-consensus       283 0.0222466787
#> 193                            plan-discuss-consensus        70 0.0059721867
#> 194                    synthesis-adapt-consensus-plan         6 0.0005572065
#> 195                  coregulate-plan-emotion-cohesion        15 0.0013930163
#> 196                emotion-cohesion-emotion-consensus        21 0.0019502229
#> 197             consensus-plan-consensus-plan-emotion        21 0.0021214264
#> 198                emotion-emotion-cohesion-consensus        18 0.0016716196
#> 199                              coregulate-plan-plan        71 0.0060575036
#> 200                         coregulate-plan-plan-plan        16 0.0014858841
#> 201                   consensus-coregulate-coregulate        11 0.0009384865
#> 202             consensus-coregulate-emotion-cohesion        33 0.0030646360
#> 203                         plan-plan-emotion-discuss        22 0.0020430906
#> 204                           cohesion-consensus-plan       169 0.0144185650
#> 205                           emotion-discuss-emotion        19 0.0016210221
#> 206                                monitor-coregulate        32 0.0025155255
#> 207             consensus-discuss-consensus-plan-plan        30 0.0030306092
#> 208               consensus-discuss-consensus-emotion        16 0.0014858841
#> 209                      cohesion-consensus-consensus        44 0.0037539459
#> 210                          plan-consensus-plan-plan        97 0.0090081724
#> 211                   discuss-discuss-discuss-discuss         8 0.0007429421
#> 212                       plan-plan-emotion-consensus        56 0.0052005944
#> 213                         plan-consensus-coregulate       147 0.0125415920
#> 214                     plan-consensus-plan-plan-plan        30 0.0030306092
#> 215                                  monitor-cohesion        31 0.0024369153
#> 216                          emotion-cohesion-discuss        20 0.0017063390
#> 217                     consensus-consensus-plan-plan        20 0.0018573551
#> 218                 emotion-consensus-discuss-emotion        14 0.0013001486
#> 219               emotion-discuss-synthesis-consensus        14 0.0013001486
#> 220                 consensus-coregulate-plan-monitor        14 0.0013001486
#> 221        emotion-cohesion-consensus-discuss-discuss        14 0.0014142843
#> 222         cohesion-consensus-discuss-consensus-plan        14 0.0014142843
#> 223                                    plan-consensus       850 0.0668186463
#> 224                     plan-plan-consensus-plan-plan        28 0.0028285685
#> 225                       plan-monitor-consensus-plan         9 0.0008358098
#> 226                                  cohesion-emotion       111 0.0087257291
#> 227                 monitor-discuss-consensus-discuss        21 0.0019502229
#> 228            consensus-emotion-consensus-coregulate        21 0.0019502229
#> 229                         emotion-consensus-emotion        29 0.0024741916
#> 230                plan-consensus-plan-consensus-plan        18 0.0018183655
#> 231                        monitor-discuss-coregulate        15 0.0012797543
#> 232                  consensus-plan-consensus-emotion        34 0.0031575037
#> 233                            consensus-plan-discuss        67 0.0057162358
#> 234               consensus-coregulate-consensus-plan        19 0.0017644874
#> 235                      consensus-coregulate-emotion       110 0.0093848648
#> 236                 emotion-consensus-coregulate-plan        22 0.0020430906
#> 237                        coregulate-discuss-emotion        16 0.0013650712
#> 238                          emotion-cohesion-emotion        64 0.0054602850
#> 239                        consensus-emotion-cohesion        72 0.0061428206
#> 240                plan-plan-consensus-plan-consensus        20 0.0020204061
#> 241          consensus-coregulate-consensus-plan-plan         6 0.0006061218
#> 242                adapt-consensus-coregulate-discuss         6 0.0005572065
#> 243                           plan-emotion-coregulate        11 0.0009384865
#> 244                consensus-discuss-emotion-cohesion        23 0.0021359584
#> 245                 consensus-discuss-discuss-emotion        16 0.0014858841
#> 246               emotion-consensus-emotion-consensus        13 0.0012072808
#> 247                      plan-monitor-discuss-emotion        13 0.0012072808
#> 248                          monitor-emotion-cohesion        12 0.0010238034
#> 249                           plan-coregulate-emotion        13 0.0011091204
#> 250                             discuss-cohesion-plan        13 0.0011091204
#> 251        plan-consensus-coregulate-emotion-cohesion        13 0.0013132640
#> 252                      consensus-coregulate-discuss       135 0.0115177886
#> 253                           emotion-discuss-discuss        34 0.0029007764
#> 254         consensus-plan-emotion-cohesion-consensus        34 0.0034346904
#> 255                       plan-consensus-plan-emotion        52 0.0048291233
#> 256               consensus-monitor-discuss-synthesis         5 0.0004643388
#> 257                   discuss-discuss-synthesis-adapt         5 0.0004643388
#> 258                    coregulate-consensus-consensus        10 0.0008531695
#> 259                            monitor-plan-consensus        33 0.0028154594
#> 260                                 plan-plan-monitor        70 0.0059721867
#> 261                                 monitor-consensus       101 0.0079396274
#> 262                                   discuss-emotion       225 0.0176872887
#> 263                    plan-monitor-discuss-consensus        22 0.0020430906
#> 264                     plan-plan-consensus-consensus        18 0.0016716196
#> 265                                 plan-plan-emotion       164 0.0139919802
#> 266                         consensus-coregulate-plan       141 0.0120296903
#> 267                          consensus-consensus-plan        82 0.0069959901
#> 268              discuss-consensus-coregulate-emotion        23 0.0021359584
#> 269                 cohesion-consensus-plan-consensus        52 0.0048291233
#> 270                        discuss-coregulate-monitor         9 0.0007678526
#> 271                           plan-monitor-coregulate         9 0.0007678526
#> 272                                   emotion-emotion        97 0.0076251867
#> 273                     coregulate-discuss-coregulate        16 0.0013650712
#> 274                         plan-plan-discuss-discuss         7 0.0006500743
#> 275                      cohesion-plan-plan-consensus        16 0.0014858841
#> 276                  plan-emotion-consensus-consensus        16 0.0014858841
#> 277                discuss-consensus-emotion-cohesion        16 0.0014858841
#> 278                        discuss-synthesis-cohesion         5 0.0004265848
#> 279                       monitor-consensus-consensus         5 0.0004265848
#> 280                    adapt-consensus-plan-consensus         5 0.0004643388
#> 281                consensus-plan-consensus-consensus        31 0.0028789004
#> 282                    discuss-discuss-consensus-plan        51 0.0047362556
#> 283                 consensus-discuss-discuss-discuss        26 0.0024145617
#> 284                      coregulate-discuss-consensus        86 0.0073372579
#> 285                            emotion-plan-plan-plan        11 0.0010215453
#> 286                  consensus-plan-monitor-consensus         8 0.0007429421
#> 287                     emotion-cohesion-plan-emotion        13 0.0012072808
#> 288              emotion-discuss-consensus-coregulate        13 0.0012072808
#> 289              coregulate-monitor-discuss-consensus        13 0.0012072808
#> 290               discuss-consensus-discuss-synthesis        17 0.0015787519
#> 291                       consensus-plan-plan-emotion        62 0.0057578009
#> 292                     plan-emotion-cohesion-emotion        22 0.0020430906
#> 293                            plan-plan-plan-monitor        20 0.0018573551
#> 294             consensus-discuss-synthesis-consensus        40 0.0037147103
#> 295                       plan-plan-plan-plan-monitor         6 0.0006061218
#> 296             consensus-plan-consensus-plan-discuss         6 0.0006061218
#> 297                 discuss-discuss-discuss-consensus        16 0.0014858841
#> 298                                coregulate-emotion       182 0.0143070513
#> 299                 discuss-discuss-discuss-synthesis         6 0.0005572065
#> 300                           emotion-emotion-emotion         9 0.0007678526
#> 301                     adapt-cohesion-consensus-plan         9 0.0008358098
#> 302                  plan-plan-consensus-plan-emotion        18 0.0018183655
#> 303                    consensus-plan-discuss-discuss        13 0.0012072808
#> 304               plan-consensus-coregulate-plan-plan        10 0.0010102031
#> 305                                 synthesis-emotion        18 0.0014149831
#> 306              plan-emotion-cohesion-consensus-plan        24 0.0024244873
#> 307                  consensus-plan-discuss-consensus        30 0.0027860327
#> 308                       plan-discuss-consensus-plan        30 0.0027860327
#> 309                  plan-emotion-consensus-plan-plan        19 0.0019193858
#> 310               consensus-coregulate-plan-consensus        37 0.0034361070
#> 311                       plan-monitor-plan-consensus         7 0.0006500743
#> 312                        consensus-coregulate-adapt        15 0.0012797543
#> 313                  plan-discuss-consensus-plan-plan        15 0.0015153046
#> 314                 discuss-consensus-monitor-discuss        11 0.0010215453
#> 315                monitor-discuss-cohesion-consensus        11 0.0010215453
#> 316        consensus-plan-consensus-emotion-consensus        11 0.0011112234
#> 317             plan-consensus-plan-consensus-discuss        11 0.0011112234
#> 318                                  coregulate-adapt        20 0.0015722034
#> 319                         plan-plan-emotion-emotion        11 0.0010215453
#> 320                      emotion-consensus-coregulate        87 0.0074225749
#> 321      cohesion-consensus-plan-consensus-coregulate         5 0.0005051015
#> 322            discuss-synthesis-adapt-consensus-plan         5 0.0005051015
#> 323                plan-consensus-consensus-plan-plan         5 0.0005051015
#> 324                 monitor-plan-consensus-coregulate         5 0.0004643388
#> 325                  plan-emotion-cohesion-coregulate         8 0.0007429421
#> 326                  emotion-consensus-plan-plan-plan        16 0.0016163249
#> 327                  discuss-consensus-consensus-plan        21 0.0019502229
#> 328                         monitor-consensus-discuss        27 0.0023035577
#> 329                       monitor-plan-consensus-plan        12 0.0011144131
#> 330                 consensus-discuss-adapt-consensus        12 0.0011144131
#> 331                    consensus-plan-monitor-discuss        34 0.0031575037
#> 332                          discuss-cohesion-emotion        12 0.0010238034
#> 333                   cohesion-consensus-plan-emotion        28 0.0026002972
#> 334                  plan-consensus-plan-plan-monitor        12 0.0012122437
#> 335       consensus-coregulate-discuss-consensus-plan        22 0.0022224467
#> 336                                      monitor-plan       143 0.0112412546
#> 337               discuss-discuss-synthesis-consensus        13 0.0012072808
#> 338               consensus-emotion-consensus-discuss        17 0.0015787519
#> 339              consensus-coregulate-monitor-discuss        17 0.0015787519
#> 340                  plan-consensus-plan-monitor-plan         6 0.0006061218
#> 341                        consensus-monitor-cohesion         6 0.0005119017
#> 342             emotion-cohesion-coregulate-consensus         6 0.0005572065
#> 343            consensus-consensus-coregulate-discuss         6 0.0005572065
#> 344                discuss-coregulate-discuss-discuss         6 0.0005572065
#> 345                      coregulate-emotion-consensus        45 0.0038392629
#> 346                       coregulate-emotion-cohesion        56 0.0047777493
#> 347                      coregulate-plan-plan-emotion        18 0.0016716196
#> 348                          cohesion-monitor-discuss        10 0.0008531695
#> 349                       plan-emotion-consensus-plan        47 0.0043647845
#> 350        consensus-plan-consensus-discuss-consensus        18 0.0018183655
#> 351                              discuss-emotion-plan        24 0.0020476069
#> 352                                      plan-monitor       219 0.0172156277
#> 353           consensus-plan-consensus-plan-consensus        26 0.0026265279
#> 354                       consensus-consensus-emotion        25 0.0021329238
#> 355                    plan-consensus-discuss-discuss        33 0.0030646360
#> 356                       plan-emotion-plan-consensus        14 0.0013001486
#> 357   consensus-coregulate-emotion-cohesion-consensus        14 0.0014142843
#> 358       consensus-plan-consensus-coregulate-emotion        14 0.0014142843
#> 359                       plan-plan-consensus-emotion        16 0.0014858841
#> 360                      consensus-coregulate-monitor        52 0.0044364815
#> 361                 plan-consensus-coregulate-discuss        35 0.0032503715
#> 362              discuss-discuss-consensus-coregulate        20 0.0018573551
#> 363                consensus-plan-plan-consensus-plan        36 0.0036367310
#> 364       discuss-consensus-plan-consensus-coregulate        20 0.0020204061
#> 365                     consensus-consensus-consensus        27 0.0023035577
#> 366           consensus-plan-plan-consensus-consensus         8 0.0008081624
#> 367                   plan-plan-plan-emotion-cohesion         8 0.0008081624
#> 368                                synthesis-cohesion         8 0.0006288814
#> 369                consensus-emotion-cohesion-emotion        10 0.0009286776
#> 370             cohesion-consensus-coregulate-emotion        10 0.0009286776
#> 371                     emotion-plan-emotion-cohesion        10 0.0009286776
#> 372                              monitor-emotion-plan        10 0.0008531695
#> 373                               synthesis-plan-plan        10 0.0008531695
#> 374                       emotion-plan-consensus-plan         8 0.0007429421
#> 375     discuss-consensus-discuss-synthesis-consensus        10 0.0010102031
#> 376          consensus-plan-consensus-discuss-emotion        10 0.0010102031
#> 377         cohesion-consensus-plan-consensus-discuss        10 0.0010102031
#> 378         plan-consensus-emotion-cohesion-consensus        10 0.0010102031
#> 379                 discuss-discuss-consensus-emotion        15 0.0013930163
#> 380                              emotion-plan-emotion        21 0.0017916560
#> 381                       discuss-consensus-plan-plan        95 0.0088224368
#> 382                      plan-monitor-discuss-discuss        13 0.0012072808
#> 383                  plan-plan-plan-discuss-consensus         5 0.0005051015
#> 384                consensus-consensus-plan-plan-plan         5 0.0005051015
#> 385                         emotion-monitor-consensus         5 0.0004265848
#> 386                              monitor-plan-discuss         5 0.0004265848
#> 387                        synthesis-emotion-cohesion         5 0.0004265848
#> 388                         monitor-consensus-monitor         5 0.0004265848
#> 389                 coregulate-consensus-plan-emotion         5 0.0004643388
#> 390                        emotion-cohesion-plan-plan        22 0.0020430906
#> 391            plan-plan-consensus-coregulate-discuss         9 0.0009091827
#> 392       plan-consensus-coregulate-discuss-consensus        16 0.0016163249
#> 393              emotion-cohesion-consensus-consensus        23 0.0021359584
#> 394                          monitor-discuss-cohesion        23 0.0019622899
#> 395               plan-consensus-plan-monitor-discuss        11 0.0011112234
#> 396          emotion-consensus-discuss-consensus-plan        11 0.0011112234
#> 397                            plan-monitor-consensus        34 0.0029007764
#> 398          discuss-discuss-consensus-plan-consensus        17 0.0017173452
#> 399               consensus-coregulate-plan-plan-plan        10 0.0010102031
#> 400                  plan-consensus-discuss-synthesis        15 0.0013930163
#> 401                            plan-plan-monitor-plan        15 0.0013930163
#> 402        plan-emotion-cohesion-consensus-coregulate         6 0.0006061218
#> 403                        coregulate-emotion-discuss        25 0.0021329238
#> 404            consensus-consensus-coregulate-emotion        12 0.0011144131
#> 405                 coregulate-plan-consensus-discuss        12 0.0011144131
#> 406              emotion-consensus-coregulate-emotion        18 0.0016716196
#> 407      consensus-discuss-emotion-cohesion-consensus        12 0.0012122437
#> 408          consensus-plan-consensus-coregulate-plan        18 0.0018183655
#> 409               cohesion-emotion-cohesion-consensus        11 0.0010215453
#> 410                    consensus-coregulate-plan-plan        39 0.0036218425
#> 411                               synthesis-consensus       160 0.0125776275
#> 412                            plan-consensus-emotion        71 0.0060575036
#> 413                   cohesion-consensus-plan-monitor         7 0.0006500743
#> 414              monitor-discuss-consensus-coregulate        13 0.0012072808
#> 415                     plan-consensus-consensus-plan        18 0.0016716196
#> 416                        plan-emotion-cohesion-plan        20 0.0018573551
#> 417                consensus-plan-consensus-plan-plan        34 0.0034346904
#> 418                                 emotion-plan-plan        43 0.0036686290
#> 419                  plan-plan-emotion-consensus-plan        20 0.0020204061
#> 420                  consensus-plan-consensus-discuss        64 0.0059435364
#> 421                                 plan-monitor-plan        45 0.0038392629
#> 422        coregulate-emotion-cohesion-consensus-plan         8 0.0008081624
#> 423                    emotion-consensus-plan-monitor        14 0.0013001486
#> 424                       consensus-monitor-plan-plan        14 0.0013001486
#> 425                 plan-consensus-coregulate-emotion        30 0.0027860327
#> 426                      coregulate-consensus-emotion        14 0.0011944373
#> 427          consensus-coregulate-plan-consensus-plan        14 0.0014142843
#> 428                        plan-plan-emotion-cohesion        41 0.0038075780
#> 429                              plan-discuss-emotion        14 0.0011944373
#> 430                              plan-monitor-emotion        22 0.0018769730
#> 431               monitor-discuss-synthesis-consensus        14 0.0013001486
#> 432              consensus-coregulate-emotion-discuss        15 0.0013930163
#> 433           consensus-coregulate-cohesion-consensus         9 0.0008358098
#> 434               coregulate-emotion-cohesion-emotion         9 0.0008358098
#> 435                cohesion-consensus-discuss-emotion         9 0.0008358098
#> 436                 cohesion-consensus-plan-plan-plan        15 0.0015153046
#> 437                     consensus-synthesis-consensus         9 0.0007678526
#> 438                           emotion-emotion-discuss         9 0.0007678526
#> 439                              emotion-plan-monitor         9 0.0007678526
#> 440                              plan-emotion-monitor         9 0.0007678526
#> 441                         consensus-monitor-emotion         9 0.0007678526
#> 442            emotion-consensus-coregulate-plan-plan         9 0.0009091827
#> 443        discuss-consensus-consensus-plan-consensus         9 0.0009091827
#> 444                 discuss-discuss-emotion-consensus         9 0.0008358098
#> 445                    emotion-emotion-consensus-plan        15 0.0013930163
#> 446              plan-plan-consensus-emotion-cohesion         5 0.0005051015
#> 447            cohesion-consensus-plan-consensus-plan        16 0.0016163249
#> 448              consensus-plan-plan-emotion-cohesion        16 0.0016163249
#> 449                      monitor-coregulate-consensus         5 0.0004265848
#> 450                        discuss-emotion-coregulate         5 0.0004265848
#> 451                        monitor-cohesion-consensus        16 0.0013650712
#> 452                  monitor-discuss-emotion-cohesion         5 0.0004643388
#> 453             coregulate-discuss-coregulate-discuss         5 0.0004643388
#> 454               coregulate-discuss-emotion-cohesion         5 0.0004643388
#> 455                    emotion-consensus-plan-emotion        25 0.0023216939
#> 456                           discuss-discuss-emotion        33 0.0028154594
#> 457                           coregulate-plan-emotion        36 0.0030714103
#> 458                    consensus-plan-plan-coregulate        10 0.0009286776
#> 459             cohesion-consensus-discuss-coregulate        10 0.0009286776
#> 460                     plan-emotion-cohesion-discuss        10 0.0009286776
#> 461                       consensus-monitor-consensus        17 0.0014503882
#> 462                           monitor-discuss-emotion        25 0.0021329238
#> 463                            monitor-plan-plan-plan        17 0.0015787519
#> 464          emotion-consensus-plan-emotion-consensus        10 0.0010102031
#> 465            plan-plan-emotion-consensus-coregulate        10 0.0010102031
#> 466                       consensus-plan-plan-discuss        25 0.0023216939
#> 467          discuss-consensus-plan-consensus-discuss        17 0.0017173452
#> 468                           cohesion-plan-plan-plan        11 0.0010215453
#> 469                         emotion-consensus-monitor        18 0.0015357051
#> 470                               consensus-synthesis        27 0.0021224746
#> 471                       consensus-plan-monitor-plan        18 0.0016716196
#> 472                          consensus-plan-consensus       352 0.0300315673
#> 473             consensus-emotion-consensus-plan-plan         6 0.0006061218
#> 474    consensus-coregulate-discuss-discuss-consensus         6 0.0006061218
#> 475            plan-plan-consensus-coregulate-emotion         6 0.0006061218
#> 476                 plan-emotion-consensus-coregulate        28 0.0026002972
#> 477                          plan-consensus-synthesis         6 0.0005119017
#> 478                      coregulate-plan-plan-monitor         6 0.0005572065
#> 479                       plan-consensus-plan-discuss        19 0.0017644874
#> 480                  plan-consensus-emotion-consensus        19 0.0017644874
#> 481                          cohesion-coregulate-plan        20 0.0017063390
#> 482                                cohesion-plan-plan        43 0.0036686290
#> 483                  discuss-synthesis-consensus-plan        40 0.0037147103
#> 484                   emotion-cohesion-consensus-plan        79 0.0073365527
#> 485        consensus-discuss-synthesis-consensus-plan        13 0.0013132640
#> 486               discuss-consensus-plan-plan-monitor         7 0.0007071421
#> 487                    emotion-consensus-plan-discuss         7 0.0006500743
#> 488               consensus-discuss-consensus-monitor         7 0.0006500743
#> 489                       plan-consensus-monitor-plan         7 0.0006500743
#> 490                    consensus-plan-emotion-emotion        12 0.0011144131
#> 491                               coregulate-cohesion        32 0.0025155255
#> 492                           coregulate-monitor-plan        21 0.0017916560
#> 493                         emotion-discuss-synthesis        21 0.0017916560
#> 494                       plan-plan-consensus-discuss        47 0.0043647845
#> 495                         coregulate-plan-consensus        64 0.0054602850
#> 496                     consensus-plan-consensus-plan       108 0.0100297177
#> 497               consensus-consensus-coregulate-plan        13 0.0012072808
#> 498                 consensus-discuss-coregulate-plan        13 0.0012072808
#> 499                          cohesion-emotion-discuss        13 0.0011091204
#> 500                         consensus-emotion-monitor        13 0.0011091204
#> 501          cohesion-consensus-plan-emotion-cohesion         8 0.0008081624
#> 502          plan-consensus-coregulate-consensus-plan         8 0.0008081624
#> 503                       cohesion-coregulate-emotion        15 0.0012797543
#> 504                 coregulate-plan-emotion-consensus         8 0.0007429421
#> 505              discuss-emotion-consensus-coregulate         8 0.0007429421
#> 506                              consensus-coregulate       580 0.0455938999
#> 507                        discuss-coregulate-emotion        25 0.0021329238
#> 508                     synthesis-consensus-consensus         9 0.0007678526
#> 509                      plan-cohesion-consensus-plan        15 0.0013930163
#> 510                       consensus-consensus-monitor        15 0.0012797543
#> 511          consensus-plan-consensus-discuss-discuss        15 0.0015153046
#> 512             cohesion-consensus-coregulate-discuss        18 0.0016716196
#> 513                                 plan-emotion-plan        43 0.0036686290
#> 514              discuss-coregulate-discuss-consensus        10 0.0009286776
#> 515               coregulate-consensus-plan-consensus        10 0.0009286776
#> 516                  discuss-discuss-emotion-cohesion        10 0.0009286776
#> 517            consensus-coregulate-emotion-consensus        28 0.0026002972
#> 518                           coregulate-plan-discuss        16 0.0013650712
#> 519                                   monitor-emotion        61 0.0047952205
#> 520                   emotion-cohesion-plan-consensus        17 0.0015787519
#> 521                           plan-discuss-coregulate        20 0.0017063390
#> 522                   cohesion-discuss-consensus-plan         8 0.0007429421
#> 523              coregulate-emotion-consensus-discuss         8 0.0007429421
#> 524                 consensus-plan-consensus-cohesion         8 0.0007429421
#> 525                        cohesion-discuss-synthesis         8 0.0006825356
#> 526                             cohesion-plan-monitor         8 0.0006825356
#> 527                     consensus-cohesion-coregulate         8 0.0006825356
#> 528                            emotion-consensus-plan       165 0.0140772972
#> 529                        coregulate-monitor-discuss        30 0.0025595086
#> 530             consensus-plan-plan-discuss-consensus         8 0.0008081624
#> 531        discuss-emotion-cohesion-consensus-discuss         8 0.0008081624
#> 532         consensus-plan-consensus-emotion-cohesion         8 0.0008081624
#> 533         emotion-cohesion-consensus-consensus-plan         8 0.0008081624
#> 534                  monitor-plan-plan-consensus-plan         8 0.0008081624
#> 535          emotion-consensus-plan-consensus-discuss         8 0.0008081624
#> 536                           discuss-coregulate-plan        32 0.0027301425
#> 537                         discuss-emotion-consensus        70 0.0059721867
#> 538                       synthesis-consensus-discuss        31 0.0026448255
#> 539                    coregulate-plan-plan-consensus        21 0.0019502229
#> 540                  monitor-consensus-plan-consensus        12 0.0011144131
#> 541                          discuss-cohesion-discuss         5 0.0004265848
#> 542                         consensus-adapt-consensus         5 0.0004265848
#> 543                           discuss-discuss-monitor         5 0.0004265848
#> 544                  discuss-emotion-cohesion-emotion         5 0.0004643388
#> 545                    plan-consensus-emotion-emotion         5 0.0004643388
#> 546                   plan-monitor-discuss-coregulate         5 0.0004643388
#> 547                monitor-discuss-coregulate-discuss         5 0.0004643388
#> 548                coregulate-cohesion-consensus-plan         5 0.0004643388
#> 549                emotion-cohesion-discuss-consensus         5 0.0004643388
#> 550                   coregulate-plan-monitor-discuss         5 0.0004643388
#> 551             consensus-consensus-discuss-synthesis         5 0.0004643388
#> 552                       monitor-plan-plan-consensus        19 0.0017644874
#> 553                           coregulate-plan-monitor        19 0.0016210221
#> 554                         monitor-plan-plan-emotion         9 0.0008358098
#> 555                  emotion-cohesion-emotion-discuss         9 0.0008358098
#> 556                cohesion-plan-consensus-coregulate         9 0.0008358098
#> 557                consensus-emotion-emotion-cohesion         9 0.0008358098
#> 558                 consensus-plan-discuss-coregulate         9 0.0008358098
#> 559           consensus-plan-consensus-consensus-plan         9 0.0009091827
#> 560             plan-consensus-plan-discuss-consensus         9 0.0009091827
#> 561                           cohesion-plan-consensus        36 0.0030714103
#> 562                plan-plan-emotion-cohesion-emotion         6 0.0006061218
#> 563                    coregulate-plan-plan-plan-plan         6 0.0006061218
#> 564                    plan-discuss-emotion-consensus         6 0.0005572065
#> 565            emotion-consensus-coregulate-consensus         6 0.0005572065
#> 566               discuss-emotion-cohesion-coregulate         6 0.0005572065
#> 567                 coregulate-plan-consensus-emotion         6 0.0005572065
#> 568                 consensus-coregulate-plan-emotion        22 0.0020430906
#> 569              cohesion-consensus-plan-plan-emotion        10 0.0010102031
#> 570             consensus-plan-discuss-consensus-plan        10 0.0010102031
#> 571       consensus-plan-emotion-consensus-coregulate        10 0.0010102031
#> 572                       plan-plan-plan-emotion-plan        10 0.0010102031
#> 573                  consensus-emotion-consensus-plan        24 0.0022288262
#> 574                              monitor-plan-emotion        24 0.0020476069
#> 575              discuss-consensus-coregulate-monitor        11 0.0010215453
#> 576                           discuss-monitor-discuss        16 0.0013650712
#> 577                      coregulate-consensus-discuss        16 0.0013650712
#> 578                         consensus-discuss-monitor        11 0.0009384865
#> 579                  plan-plan-consensus-plan-discuss        11 0.0011112234
#> 580                       discuss-cohesion-coregulate         7 0.0005972187
#> 581                   consensus-emotion-cohesion-plan         7 0.0006500743
#> 582          consensus-coregulate-consensus-consensus         7 0.0006500743
#> 583                                      discuss-plan        25 0.0019652543
#> 584                              plan-emotion-emotion        30 0.0025595086
#> 585                       discuss-synthesis-consensus       113 0.0096408156
#> 586                    plan-discuss-discuss-consensus        12 0.0011144131
#> 587                consensus-consensus-consensus-plan        12 0.0011144131
#> 588                                plan-cohesion-plan        12 0.0010238034
#> 589                    monitor-discuss-consensus-plan        27 0.0025074294
#> 590     emotion-cohesion-consensus-coregulate-discuss        12 0.0012122437
#> 591                        monitor-coregulate-discuss         8 0.0006825356
#> 592                 plan-consensus-discuss-coregulate         8 0.0007429421
#> 593                 plan-discuss-consensus-coregulate         8 0.0007429421
#> 594                  emotion-cohesion-coregulate-plan         8 0.0007429421
#> 595                                    consensus-plan      1236 0.0971621728
#> 596                    discuss-emotion-consensus-plan        29 0.0026931649
#> 597                 consensus-coregulate-monitor-plan        13 0.0012072808
#> 598                  emotion-consensus-consensus-plan        13 0.0012072808
#> 599                  consensus-consensus-plan-emotion        13 0.0012072808
#> 600                         plan-plan-monitor-discuss        20 0.0018573551
#> 601          plan-plan-consensus-coregulate-consensus         9 0.0009091827
#> 602  consensus-coregulate-discuss-synthesis-consensus         9 0.0009091827
#> 603           consensus-consensus-plan-plan-consensus         9 0.0009091827
#> 604                    plan-consensus-coregulate-plan        36 0.0033432392
#> 605              emotion-cohesion-consensus-plan-plan        21 0.0021214264
#> 606                consensus-plan-plan-plan-consensus        37 0.0037377513
#> 607           consensus-coregulate-discuss-coregulate         9 0.0008358098
#> 608                 monitor-discuss-emotion-consensus         9 0.0008358098
#> 609              consensus-coregulate-discuss-emotion         9 0.0008358098
#> 610                        cohesion-discuss-consensus        15 0.0012797543
#> 611          plan-consensus-plan-consensus-coregulate        15 0.0015153046
#> 612              plan-plan-emotion-cohesion-consensus        15 0.0015153046
#> 613                           plan-coregulate-discuss        10 0.0008531695
#> 614            consensus-coregulate-consensus-discuss        10 0.0009286776
#> 615                                plan-plan-cohesion        23 0.0019622899
#> 616                 emotion-cohesion-emotion-cohesion        16 0.0014858841
#> 617               emotion-consensus-discuss-synthesis        11 0.0010215453
#> 618                  plan-plan-plan-emotion-consensus        17 0.0017173452
#> 619                       plan-consensus-plan-monitor        25 0.0023216939
#> 620              consensus-discuss-coregulate-discuss        12 0.0011144131
#> 621             consensus-plan-plan-emotion-consensus        19 0.0019193858
#> 622                                   emotion-monitor        49 0.0038518984
#> 623                  emotion-consensus-plan-consensus        47 0.0043647845
#> 624                         emotion-emotion-consensus        29 0.0024741916
#> 625                       emotion-consensus-plan-plan        51 0.0047362556
#> 626                          discuss-discuss-cohesion        14 0.0011944373
#> 627                       plan-plan-monitor-consensus        14 0.0013001486
#> 628               discuss-synthesis-consensus-discuss        23 0.0021359584
#> 629       consensus-plan-consensus-coregulate-discuss        15 0.0015153046
#> 630                           emotion-monitor-discuss        15 0.0012797543
#> 631                              coregulate-synthesis        17 0.0013363729
#> 632                      discuss-coregulate-consensus        17 0.0014503882
#> 633                 discuss-consensus-emotion-discuss         7 0.0006500743
#> 634                 emotion-emotion-emotion-consensus         7 0.0006500743
#> 635                        cohesion-plan-plan-emotion         7 0.0006500743
#> 636                      coregulate-monitor-plan-plan         7 0.0006500743
#> 637                 plan-monitor-consensus-coregulate         7 0.0006500743
#> 638                            consensus-discuss-plan         7 0.0005972187
#> 639                              discuss-discuss-plan         7 0.0005972187
#> 640     consensus-plan-consensus-consensus-coregulate         7 0.0007071421
#> 641         plan-emotion-consensus-coregulate-discuss         7 0.0007071421
#> 642   cohesion-consensus-coregulate-discuss-consensus         7 0.0007071421
#> 643       consensus-discuss-consensus-coregulate-plan         7 0.0007071421
#> 644                       plan-plan-discuss-consensus        18 0.0016716196
#> 645                   plan-consensus-emotion-cohesion        18 0.0016716196
#> 646                            consensus-monitor-plan        30 0.0025595086
#> 647                         cohesion-emotion-cohesion        30 0.0025595086
#> 648                discuss-emotion-cohesion-consensus        31 0.0028789004
#> 649                    monitor-emotion-consensus-plan         8 0.0007429421
#> 650            emotion-consensus-consensus-coregulate         8 0.0007429421
#> 651            discuss-consensus-consensus-coregulate         8 0.0007429421
#> 652                         plan-monitor-plan-emotion         8 0.0007429421
#> 653               monitor-discuss-consensus-consensus         8 0.0007429421
#> 654                    plan-consensus-emotion-discuss         8 0.0007429421
#> 655               consensus-consensus-monitor-discuss         8 0.0007429421
#> 656                   discuss-discuss-discuss-emotion         8 0.0007429421
#> 657                        emotion-coregulate-emotion         8 0.0006825356
#> 658                        coregulate-adapt-consensus         8 0.0006825356
#> 659          discuss-emotion-consensus-plan-consensus         8 0.0008081624
#> 660              consensus-plan-emotion-cohesion-plan         8 0.0008081624
#> 661                       plan-plan-plan-monitor-plan         5 0.0005051015
#> 662       plan-consensus-coregulate-emotion-consensus         5 0.0005051015
#> 663     consensus-coregulate-consensus-plan-consensus         5 0.0005051015
#> 664               plan-plan-monitor-discuss-consensus         5 0.0005051015
#> 665           adapt-cohesion-consensus-plan-consensus         5 0.0005051015
#> 666                  emotion-emotion-cohesion-emotion         5 0.0004643388
#> 667                    plan-discuss-discuss-synthesis         5 0.0004643388
#> 668                  plan-consensus-monitor-consensus         5 0.0004643388
#> 669                         discuss-emotion-plan-plan         5 0.0004643388
#> 670                     plan-monitor-emotion-cohesion         5 0.0004643388
#> 671                consensus-consensus-plan-consensus        36 0.0033432392
#> 672            coregulate-consensus-discuss-consensus         9 0.0008358098
#> 673               plan-plan-emotion-consensus-discuss         9 0.0009091827
#> 674                                   coregulate-plan       238 0.0187092210
#> 675                  plan-monitor-plan-plan-consensus         6 0.0006061218
#> 676                    plan-plan-plan-monitor-discuss         6 0.0006061218
#> 677               coregulate-plan-consensus-plan-plan         6 0.0006061218
#> 678             synthesis-consensus-discuss-consensus        10 0.0009286776
#> 679                    coregulate-synthesis-consensus         6 0.0005119017
#> 680                           adapt-emotion-consensus        10 0.0008531695
#> 681               discuss-emotion-consensus-consensus         6 0.0005572065
#> 682                plan-consensus-consensus-consensus         6 0.0005572065
#> 683                      plan-consensus-plan-cohesion         6 0.0005572065
#> 684              consensus-discuss-discuss-coregulate         6 0.0005572065
#> 685                 discuss-coregulate-consensus-plan         6 0.0005572065
#> 686                            plan-plan-plan-emotion        50 0.0046433878
#> 687                      cohesion-consensus-plan-plan        49 0.0045505201
#> 688                      plan-plan-cohesion-consensus        11 0.0010215453
#> 689               monitor-discuss-consensus-plan-plan         7 0.0007071421
#> 690                  plan-consensus-plan-plan-discuss         7 0.0007071421
#> 691                  consensus-plan-monitor-plan-plan         7 0.0007071421
#> 692            plan-consensus-coregulate-plan-emotion         7 0.0007071421
#> 693                                     monitor-adapt         7 0.0005502712
#> 694                           monitor-coregulate-plan         7 0.0005972187
#> 695                monitor-emotion-cohesion-consensus         7 0.0006500743
#> 696                     monitor-plan-emotion-cohesion         7 0.0006500743
#> 697                    plan-monitor-discuss-synthesis         7 0.0006500743
#> 698                  plan-consensus-consensus-discuss         7 0.0006500743
#> 699             consensus-emotion-cohesion-coregulate         7 0.0006500743
#> 700                 consensus-coregulate-emotion-plan        12 0.0011144131
#> 701                cohesion-consensus-coregulate-plan        12 0.0011144131
#> 702                           plan-consensus-cohesion        12 0.0010238034
#> 703        plan-consensus-discuss-synthesis-consensus        12 0.0012122437
#> 704               plan-consensus-consensus-coregulate        13 0.0012072808
#> 705               plan-plan-consensus-discuss-discuss         8 0.0008081624
#> 706                      consensus-plan-plan-cohesion         8 0.0007429421
#> 707                    plan-emotion-emotion-consensus         8 0.0007429421
#> 708                    discuss-consensus-plan-discuss        14 0.0013001486
#> 709                       consensus-plan-plan-monitor        31 0.0028789004
#> 710       emotion-consensus-plan-consensus-coregulate         9 0.0009091827
#> 711             plan-emotion-consensus-plan-consensus         9 0.0009091827
#> 712       coregulate-discuss-consensus-plan-consensus         9 0.0009091827
#> 713            coregulate-discuss-consensus-plan-plan         9 0.0009091827
#> 714             discuss-cohesion-consensus-coregulate         9 0.0008358098
#> 715                 discuss-coregulate-plan-consensus         9 0.0008358098
#> 716                     plan-emotion-emotion-cohesion         9 0.0008358098
#> 717             emotion-consensus-plan-consensus-plan        16 0.0016163249
#> 718                       consensus-discuss-synthesis        72 0.0061428206
#> 719                 coregulate-emotion-consensus-plan        18 0.0016716196
#> 720                         consensus-emotion-emotion        18 0.0015357051
#> 721                synthesis-consensus-plan-plan-plan        11 0.0011112234
#> 722          consensus-coregulate-plan-plan-consensus        11 0.0011112234
#> 723                        cohesion-consensus-monitor        11 0.0009384865
#> 724                        coregulate-emotion-emotion        11 0.0009384865
#> 725                 cohesion-consensus-consensus-plan        11 0.0010215453
#> 726                synthesis-consensus-plan-consensus        11 0.0010215453
#> 727              emotion-consensus-coregulate-discuss        22 0.0020430906
#> 728             coregulate-emotion-cohesion-consensus        22 0.0020430906
#> 729                                   monitor-monitor        12 0.0009433221
#> 730         emotion-cohesion-consensus-plan-consensus        22 0.0022224467
#> 731                 plan-consensus-coregulate-monitor        12 0.0011144131
#> 732             consensus-plan-emotion-consensus-plan        13 0.0013132640
#> 733             plan-consensus-plan-emotion-consensus        13 0.0013132640
#> 734           consensus-consensus-plan-consensus-plan        13 0.0013132640
#> 735                  consensus-plan-consensus-monitor        13 0.0012072808
#> 736                                   consensus-adapt        14 0.0011005424
#> 737                      coregulate-monitor-consensus        14 0.0011944373
#> 738                 coregulate-discuss-consensus-plan        31 0.0028789004
#> 739              coregulate-discuss-discuss-consensus        15 0.0013930163
#> 740               discuss-discuss-consensus-plan-plan        16 0.0016163249
#> 741             discuss-synthesis-consensus-plan-plan        16 0.0016163249
#> 742                                 monitor-plan-plan        57 0.0048630663
#> 743            discuss-synthesis-consensus-coregulate        18 0.0016716196
#> 744                    coregulate-plan-consensus-plan        18 0.0016716196
#> 745                          plan-consensus-consensus        62 0.0052896511
#> 746                                coregulate-monitor        86 0.0067604748
#> 747                                      emotion-plan       140 0.0110054241
#> 748                              plan-monitor-discuss        76 0.0064840884
#> 749                          discuss-emotion-cohesion        62 0.0052896511
#> 750               consensus-plan-consensus-coregulate        64 0.0059435364
#> 751                     cohesion-consensus-coregulate        67 0.0057162358
#> 752                    consensus-consensus-coregulate        41 0.0034979951
#> 753                            emotion-plan-consensus        38 0.0032420442
#> 754                         discuss-consensus-monitor        27 0.0023035577
#> 755                           coregulate-emotion-plan        19 0.0016210221
#> 756                           adapt-consensus-discuss        18 0.0015357051
#> 757                              plan-plan-coregulate        17 0.0014503882
#> 758                  consensus-plan-plan-plan-emotion        21 0.0021214264
#> 759                     coregulate-cohesion-consensus        16 0.0013650712
#> 760                      cohesion-plan-consensus-plan        13 0.0012072808
#> 761                 monitor-discuss-discuss-consensus        13 0.0012072808
#> 762            cohesion-consensus-plan-plan-consensus        15 0.0015153046
#> 763                   cohesion-consensus-plan-discuss        10 0.0009286776
#> 764               discuss-coregulate-emotion-cohesion        10 0.0009286776
#> 765              coregulate-plan-consensus-coregulate        10 0.0009286776
#> 766             emotion-consensus-plan-plan-consensus        13 0.0013132640
#> 767                  plan-consensus-plan-plan-emotion        13 0.0013132640
#> 768                                 monitor-synthesis        12 0.0009433221
#> 769                       coregulate-discuss-cohesion         9 0.0007678526
#> 770                             plan-cohesion-emotion         9 0.0007678526
#> 771                 emotion-plan-consensus-coregulate         8 0.0007429421
#> 772        discuss-synthesis-consensus-plan-consensus        11 0.0011112234
#> 773         consensus-emotion-cohesion-consensus-plan        10 0.0010102031
#> 774                            discuss-plan-consensus         8 0.0006825356
#> 775                    plan-consensus-plan-coregulate         7 0.0006500743
#> 776                   emotion-consensus-discuss-adapt         7 0.0006500743
#> 777          plan-consensus-coregulate-plan-consensus         9 0.0009091827
#> 778                       monitor-plan-plan-plan-plan         9 0.0009091827
#> 779                monitor-cohesion-consensus-discuss         6 0.0005572065
#> 780             consensus-consensus-emotion-consensus         6 0.0005572065
#> 781                    plan-monitor-emotion-consensus         6 0.0005572065
#> 782                 coregulate-monitor-plan-consensus         6 0.0005572065
#> 783                      plan-discuss-coregulate-plan         6 0.0005572065
#> 784               consensus-plan-plan-emotion-emotion         8 0.0008081624
#> 785 consensus-coregulate-discuss-consensus-coregulate         8 0.0008081624
#> 786          discuss-discuss-synthesis-consensus-plan         8 0.0008081624
#> 787               consensus-plan-plan-monitor-discuss         8 0.0008081624
#> 788                          coregulate-plan-cohesion         6 0.0005119017
#> 789           plan-consensus-plan-consensus-consensus         6 0.0006061218
#> 790          monitor-discuss-consensus-plan-consensus         6 0.0006061218
#> 791               emotion-consensus-plan-plan-discuss         6 0.0006061218
#> 792                    plan-plan-plan-emotion-discuss         6 0.0006061218
#> 793          consensus-monitor-discuss-consensus-plan         5 0.0005051015
#> 794          emotion-emotion-consensus-plan-consensus         5 0.0005051015
#> 795                   emotion-cohesion-plan-plan-plan         5 0.0005051015
#> 796                  monitor-plan-plan-plan-consensus         5 0.0005051015
#> 797       consensus-plan-consensus-coregulate-monitor         5 0.0005051015
#> 798                  plan-plan-monitor-consensus-plan         5 0.0005051015
#> 799                       synthesis-emotion-consensus         8 0.0006825356
#> 800                        emotion-discuss-coregulate         8 0.0006825356
#> 801                  plan-plan-discuss-consensus-plan        10 0.0010102031
#> 802                  consensus-plan-plan-plan-monitor        10 0.0010102031
#> 803                       discuss-coregulate-cohesion         7 0.0005972187
#> 804                    consensus-plan-discuss-emotion         6 0.0005572065
#> 805                       consensus-emotion-plan-plan         6 0.0005572065
#> 806             discuss-coregulate-discuss-coregulate         6 0.0005572065
#> 807                 synthesis-adapt-consensus-discuss         6 0.0005572065
#> 808           emotion-cohesion-consensus-plan-emotion        12 0.0012122437
#> 809                      discuss-coregulate-plan-plan        12 0.0011144131
#> 810                           emotion-coregulate-plan        10 0.0008531695
#> 811                              emotion-plan-discuss        10 0.0008531695
#> 812                             cohesion-emotion-plan        10 0.0008531695
#> 813            discuss-consensus-coregulate-plan-plan         8 0.0008081624
#> 814       emotion-cohesion-emotion-cohesion-consensus         8 0.0008081624
#> 815             consensus-plan-consensus-plan-monitor         8 0.0008081624
#> 816            consensus-coregulate-monitor-consensus        10 0.0009286776
#> 817                discuss-discuss-cohesion-consensus         8 0.0007429421
#> 818       consensus-coregulate-emotion-consensus-plan        11 0.0011112234
#> 819                 emotion-emotion-consensus-discuss         5 0.0004643388
#> 820                  synthesis-consensus-plan-discuss         5 0.0004643388
#> 821                        plan-plan-discuss-cohesion         5 0.0004643388
#> 822                      coregulate-emotion-plan-plan         5 0.0004643388
#> 823                    consensus-plan-emotion-monitor         5 0.0004643388
#> 824                            plan-emotion-plan-plan        14 0.0013001486
#> 825                            plan-plan-emotion-plan        14 0.0013001486
#> 826                    synthesis-consensus-coregulate        22 0.0018769730
#> 827             consensus-plan-plan-consensus-discuss        16 0.0016163249
#> 828                           plan-plan-plan-cohesion         7 0.0006500743
#> 829              emotion-consensus-discuss-coregulate         7 0.0006500743
#> 830                 consensus-coregulate-plan-discuss         7 0.0006500743
#> 831                    plan-emotion-consensus-monitor         7 0.0006500743
#> 832                         plan-plan-monitor-emotion         7 0.0006500743
#> 833                   discuss-adapt-consensus-discuss         7 0.0006500743
#> 834                  plan-discuss-synthesis-consensus         7 0.0006500743
#> 835            coregulate-discuss-consensus-consensus         7 0.0006500743
#> 836                    plan-monitor-consensus-discuss         7 0.0006500743
#> 837                          synthesis-consensus-plan        58 0.0049483832
#> 838                            plan-monitor-plan-plan        19 0.0017644874
#> 839               consensus-monitor-discuss-consensus        16 0.0014858841
#> 840            discuss-consensus-coregulate-consensus        11 0.0010215453
#> 841             discuss-consensus-plan-plan-consensus        24 0.0024244873
#> 842         plan-emotion-cohesion-consensus-consensus         7 0.0007071421
#> 843             consensus-plan-plan-consensus-emotion         7 0.0007071421
#> 844                  consensus-plan-plan-monitor-plan         7 0.0007071421
#> 845        consensus-plan-consensus-discuss-synthesis         7 0.0007071421
#> 846                          emotion-emotion-cohesion        35 0.0029860933
#> 847               consensus-discuss-discuss-synthesis        14 0.0013001486
#> 848                       consensus-plan-emotion-plan        14 0.0013001486
#> 849               emotion-discuss-consensus-plan-plan         8 0.0008081624
#> 850              consensus-coregulate-emotion-emotion         6 0.0005572065
#> 851                      plan-plan-emotion-coregulate         6 0.0005572065
#> 852               discuss-discuss-consensus-consensus         6 0.0005572065
#> 853                  plan-plan-plan-consensus-discuss        12 0.0012122437
#> 854                             plan-discuss-cohesion         6 0.0005119017
#> 855                          consensus-adapt-cohesion         6 0.0005119017
#> 856                       emotion-plan-plan-consensus         9 0.0008358098
#> 857                              monitor-plan-monitor         8 0.0006825356
#> 858                      monitor-consensus-coregulate        20 0.0017063390
#> 859                  consensus-monitor-plan-consensus         7 0.0006500743
#> 860                    discuss-emotion-plan-consensus         7 0.0006500743
#> 861                       plan-plan-discuss-synthesis         7 0.0006500743
#> 862                  discuss-consensus-plan-plan-plan        30 0.0030306092
#> 863             plan-consensus-plan-consensus-emotion         6 0.0006061218
#> 864                       plan-monitor-plan-plan-plan         6 0.0006061218
#> 865           emotion-consensus-plan-emotion-cohesion         6 0.0006061218
#> 866                 cohesion-plan-consensus-plan-plan         6 0.0006061218
#> 867            plan-monitor-discuss-consensus-discuss         6 0.0006061218
#> 868                 plan-plan-monitor-discuss-discuss         6 0.0006061218
#> 869         coregulate-discuss-discuss-consensus-plan         6 0.0006061218
#> 870       plan-emotion-consensus-coregulate-consensus         5 0.0005051015
#> 871          plan-consensus-monitor-discuss-consensus         5 0.0005051015
#> 872       consensus-discuss-synthesis-adapt-consensus         5 0.0005051015
#> 873     discuss-consensus-coregulate-emotion-cohesion         5 0.0005051015
#> 874               plan-emotion-consensus-plan-monitor         5 0.0005051015
#> 875            monitor-discuss-discuss-consensus-plan         5 0.0005051015
#> 876                   plan-emotion-cohesion-plan-plan         5 0.0005051015
#> 877     consensus-coregulate-discuss-emotion-cohesion         5 0.0005051015
#> 878                            consensus-plan-monitor        86 0.0073372579
#> 879                        emotion-coregulate-monitor         7 0.0005972187
#> 880                    consensus-coregulate-synthesis         7 0.0005972187
#> 881                       discuss-synthesis-plan-plan         5 0.0004643388
#> 882           coregulate-emotion-consensus-coregulate         5 0.0004643388
#> 883             cohesion-coregulate-discuss-consensus         5 0.0004643388
#> 884              coregulate-discuss-consensus-emotion         5 0.0004643388
#> 885             emotion-cohesion-consensus-coregulate        31 0.0028789004
#> 886                  plan-plan-consensus-plan-monitor        11 0.0011112234
#> 887                       plan-plan-plan-plan-emotion        19 0.0019193858
#> 888             consensus-plan-plan-monitor-consensus         7 0.0007071421
#> 889                       plan-plan-monitor-plan-plan         7 0.0007071421
#> 890     discuss-synthesis-consensus-discuss-consensus         7 0.0007071421
#> 891                          cohesion-emotion-emotion         5 0.0004265848
#> 892                        coregulate-discuss-monitor         5 0.0004265848
#> 893                      consensus-monitor-coregulate         5 0.0004265848
#> 894                                 discuss-plan-plan         5 0.0004265848
#> 895                            consensus-emotion-plan        19 0.0016210221
#> 896                     consensus-coregulate-cohesion        19 0.0016210221
#> 897                     synthesis-consensus-plan-plan        22 0.0020430906
#> 898               emotion-cohesion-coregulate-emotion         9 0.0008358098
#> 899                   cohesion-emotion-consensus-plan         9 0.0008358098
#> 900               discuss-emotion-consensus-plan-plan         9 0.0009091827
#> 901                         monitor-emotion-consensus        21 0.0017916560
#> 902           coregulate-discuss-consensus-coregulate        13 0.0012072808
#> 903           plan-consensus-consensus-plan-consensus         9 0.0009091827
#> 904           discuss-emotion-cohesion-consensus-plan         9 0.0009091827
#> 905                           emotion-monitor-emotion         5 0.0004265848
#> 906                          coregulate-cohesion-plan         5 0.0004265848
#> 907                           discuss-emotion-monitor         5 0.0004265848
#> 908                           discuss-emotion-emotion         9 0.0007678526
#> 909                             plan-monitor-cohesion         9 0.0007678526
#>     freq_Low     prop_Low    statistic      p_value
#> 1        418 0.0326256634 1.430404e+02 5.759804e-33
#> 2        401 0.0312987824 1.228879e+02 1.475578e-28
#> 3        234 0.0182641274 1.187381e+02 1.195089e-27
#> 4         87 0.0073653911 1.088311e+02 1.767186e-25
#> 5         41 0.0034710464 1.019860e+02 5.591524e-24
#> 6       1356 0.1058382766 6.330314e+01 1.772219e-15
#> 7        510 0.0431764307 6.124868e+01 5.030238e-15
#> 8          8 0.0007368518 4.612206e+01 1.111101e-11
#> 9        195 0.0179607626 4.285378e+01 5.898807e-11
#> 10        44 0.0037250254 4.112881e+01 1.425201e-10
#> 11        96 0.0081273281 3.829024e+01 6.096696e-10
#> 12        10 0.0009210648 3.733957e+01 9.925007e-10
#> 13       170 0.0132688105 3.688818e+01 1.251019e-09
#> 14       113 0.0088198564 3.316809e+01 8.452631e-09
#> 15        19 0.0016085337 3.247327e+01 1.208431e-08
#> 16        36 0.0033158331 3.167403e+01 1.823446e-08
#> 17        49 0.0041483237 3.167218e+01 1.825181e-08
#> 18       341 0.0266156728 3.083843e+01 2.804288e-08
#> 19       181 0.0153233999 3.012043e+01 4.060303e-08
#> 20        89 0.0075347105 2.889678e+01 7.633962e-08
#> 21       102 0.0079612863 2.885223e+01 7.811611e-08
#> 22       344 0.0268498283 2.878569e+01 8.084625e-08
#> 23        58 0.0049102608 2.849400e+01 9.398925e-08
#> 24        33 0.0027937690 2.721296e+01 1.822327e-07
#> 25       100 0.0078051826 2.711293e+01 1.919101e-07
#> 26         5 0.0003902591 2.682953e+01 2.222152e-07
#> 27        10 0.0009210648 2.648150e+01 2.660743e-07
#> 28       377 0.0294255386 2.474432e+01 6.546155e-07
#> 29       329 0.0256790509 2.447463e+01 7.529459e-07
#> 30        73 0.0073234350 2.210257e+01 2.584641e-06
#> 31        19 0.0017500230 2.126893e+01 3.991489e-06
#> 32         7 0.0005926177 2.108800e+01 4.386660e-06
#> 33        63 0.0053335591 2.085302e+01 4.959106e-06
#> 34         6 0.0005526389 2.022140e+01 6.897682e-06
#> 35         9 0.0008289583 1.942834e+01 1.044455e-05
#> 36       388 0.0302841086 1.941889e+01 1.049637e-05
#> 37        41 0.0037763655 1.891968e+01 1.363391e-05
#> 38        64 0.0049953169 1.879586e+01 1.454827e-05
#> 39         9 0.0007024664 1.854396e+01 1.660304e-05
#> 40         7 0.0006447453 1.842466e+01 1.767557e-05
#> 41       184 0.0143615361 1.831937e+01 1.867980e-05
#> 42        61 0.0051642398 1.818231e+01 2.007351e-05
#> 43        56 0.0056179775 1.817302e+01 2.017163e-05
#> 44        32 0.0029474072 1.802417e+01 2.181176e-05
#> 45        34 0.0031316202 1.796824e+01 2.246214e-05
#> 46       168 0.0131127068 1.780547e+01 2.446785e-05
#> 47        76 0.0076243981 1.769003e+01 2.599859e-05
#> 48        57 0.0048256011 1.700672e+01 3.724784e-05
#> 49       363 0.0307314595 1.688944e+01 3.962143e-05
#> 50        79 0.0066881138 1.674469e+01 4.276180e-05
#> 51        27 0.0021073993 1.639666e+01 5.137573e-05
#> 52        32 0.0027091094 1.622668e+01 5.619715e-05
#> 53        12 0.0011052777 1.577172e+01 7.146257e-05
#> 54        24 0.0020318320 1.546787e+01 8.391963e-05
#> 55        21 0.0019342360 1.523682e+01 9.483627e-05
#> 56        65 0.0055028784 1.453615e+01 1.374955e-04
#> 57        65 0.0055028784 1.453615e+01 1.374955e-04
#> 58        86 0.0072807315 1.442448e+01 1.458931e-04
#> 59       187 0.0172239108 1.439762e+01 1.479892e-04
#> 60        45 0.0041447914 1.406361e+01 1.767305e-04
#> 61        33 0.0025757103 1.385440e+01 1.975338e-04
#> 62        29 0.0024551304 1.363713e+01 2.217561e-04
#> 63        14 0.0012894907 1.320444e+01 2.792866e-04
#> 64       107 0.0090585845 1.300823e+01 3.101245e-04
#> 65       495 0.0419065357 1.295900e+01 3.183869e-04
#> 66        42 0.0035557061 1.277753e+01 3.508087e-04
#> 67       164 0.0138841856 1.260819e+01 3.840594e-04
#> 68        98 0.0082966475 1.240032e+01 4.292610e-04
#> 69        78 0.0066034541 1.192876e+01 5.527392e-04
#> 70        25 0.0019512957 1.188306e+01 5.664667e-04
#> 71        30 0.0025397900 1.179186e+01 5.949026e-04
#> 72       139 0.0117676939 1.158410e+01 6.651806e-04
#> 73       129 0.0118817353 1.155457e+01 6.758288e-04
#> 74         7 0.0007022472 1.154562e+01 6.790908e-04
#> 75       126 0.0098345301 1.144545e+01 7.166939e-04
#> 76        10 0.0008465967 1.140397e+01 7.328714e-04
#> 77        93 0.0078733491 1.128654e+01 7.807121e-04
#> 78        75 0.0069079856 1.111363e+01 8.569549e-04
#> 79        24 0.0022105554 1.100860e+01 9.069008e-04
#> 80       107 0.0090585845 1.076805e+01 1.032673e-03
#> 81       244 0.0190446456 1.072301e+01 1.058112e-03
#> 82        19 0.0017500230 1.058097e+01 1.142579e-03
#> 83        28 0.0023704707 1.053668e+01 1.170285e-03
#> 84        48 0.0040636641 1.048050e+01 1.206409e-03
#> 85        26 0.0022011514 1.044280e+01 1.231288e-03
#> 86         9 0.0009028892 1.037347e+01 1.278387e-03
#> 87        24 0.0020318320 1.036327e+01 1.285470e-03
#> 88        25 0.0021164917 1.029801e+01 1.331739e-03
#> 89        11 0.0010131712 1.017377e+01 1.424530e-03
#> 90        59 0.0049949204 1.007462e+01 1.503258e-03
#> 91        28 0.0025789813 9.854096e+00 1.694540e-03
#> 92        67 0.0056721978 9.847962e+00 1.700200e-03
#> 93        33 0.0030395137 9.828951e+00 1.717863e-03
#> 94       431 0.0336403372 9.813312e+00 1.732532e-03
#> 95         6 0.0005526389 9.758755e+00 1.784707e-03
#> 96        64 0.0054182188 9.730073e+00 1.812773e-03
#> 97        34 0.0026537621 9.419434e+00 2.146977e-03
#> 98       141 0.0129870130 9.392093e+00 2.179233e-03
#> 99       175 0.0136590696 9.391233e+00 2.180256e-03
#> 100       49 0.0041483237 9.291124e+00 2.302668e-03
#> 101       40 0.0031220731 9.261285e+00 2.340488e-03
#> 102       37 0.0028879176 9.247553e+00 2.358102e-03
#> 103        7 0.0006447453 9.156687e+00 2.478108e-03
#> 104       27 0.0024868748 9.093431e+00 2.565291e-03
#> 105       35 0.0032237266 8.993807e+00 2.708961e-03
#> 106        9 0.0007619370 8.881097e+00 2.881384e-03
#> 107        9 0.0007619370 8.881097e+00 2.881384e-03
#> 108       90 0.0076193701 8.730849e+00 3.128708e-03
#> 109       23 0.0021184489 8.683473e+00 3.211085e-03
#> 110       36 0.0030477481 8.638414e+00 3.291478e-03
#> 111       15 0.0013815971 8.631870e+00 3.303323e-03
#> 112       28 0.0028089888 8.614498e+00 3.334977e-03
#> 113       60 0.0050795801 8.365716e+00 3.823659e-03
#> 114       97 0.0075710272 8.335577e+00 3.887613e-03
#> 115      418 0.0326256634 8.329633e+00 3.900354e-03
#> 116      304 0.0237277552 8.187001e+00 4.219162e-03
#> 117       24 0.0020318320 8.122908e+00 4.370939e-03
#> 118       24 0.0022105554 8.113985e+00 4.392505e-03
#> 119       52 0.0044023027 8.098292e+00 4.430698e-03
#> 120       32 0.0029474072 8.031020e+00 4.598293e-03
#> 121       22 0.0018625127 7.933469e+00 4.852866e-03
#> 122        5 0.0005016051 7.926215e+00 4.872363e-03
#> 123       54 0.0045716221 7.876024e+00 5.009450e-03
#> 124       52 0.0044023027 7.792905e+00 5.245180e-03
#> 125       20 0.0016931934 7.722871e+00 5.452562e-03
#> 126       20 0.0018421295 7.715160e+00 5.475900e-03
#> 127       50 0.0042329834 7.469869e+00 6.274011e-03
#> 128       70 0.0059261768 7.386060e+00 6.573132e-03
#> 129       23 0.0021184489 7.365619e+00 6.648270e-03
#> 130       26 0.0023947684 7.170280e+00 7.412122e-03
#> 131        8 0.0007368518 7.143372e+00 7.524156e-03
#> 132       46 0.0042368979 7.091531e+00 7.744906e-03
#> 133       10 0.0009210648 7.037788e+00 7.980755e-03
#> 134       27 0.0022858110 6.936181e+00 8.446925e-03
#> 135      117 0.0099051812 6.902531e+00 8.607380e-03
#> 136       33 0.0030395137 6.730145e+00 9.479691e-03
#> 137        6 0.0006019262 6.580735e+00 1.030883e-02
#> 138       30 0.0025397900 6.451794e+00 1.108404e-02
#> 139        8 0.0006772773 6.421725e+00 1.127326e-02
#> 140       41 0.0032001249 6.398877e+00 1.141926e-02
#> 141        5 0.0005016051 6.336000e+00 1.183114e-02
#> 142       35 0.0032237266 6.232734e+00 1.254100e-02
#> 143       23 0.0019471724 6.205542e+00 1.273509e-02
#> 144       23 0.0021184489 6.197844e+00 1.279060e-02
#> 145        7 0.0006447453 6.134113e+00 1.325981e-02
#> 146        9 0.0008289583 6.044729e+00 1.394787e-02
#> 147       18 0.0016579166 6.023061e+00 1.412013e-02
#> 148       18 0.0015238740 6.015390e+00 1.418164e-02
#> 149       43 0.0036403657 5.982076e+00 1.445198e-02
#> 150       21 0.0017778530 5.927563e+00 1.490584e-02
#> 151       40 0.0040128411 5.865923e+00 1.543676e-02
#> 152        6 0.0006019262 5.835143e+00 1.570914e-02
#> 153       61 0.0047611614 5.744745e+00 1.653806e-02
#> 154      354 0.0299695225 5.723425e+00 1.674004e-02
#> 155       31 0.0031099518 5.564199e+00 1.833140e-02
#> 156       22 0.0022070626 5.533837e+00 1.865216e-02
#> 157       63 0.0049172651 5.509648e+00 1.891185e-02
#> 158       18 0.0015238740 5.470172e+00 1.934372e-02
#> 159       13 0.0011973842 5.469359e+00 1.935272e-02
#> 160       94 0.0086580087 5.446120e+00 1.961184e-02
#> 161        7 0.0006447453 5.424739e+00 1.985340e-02
#> 162        7 0.0007022472 5.408488e+00 2.003906e-02
#> 163       11 0.0009312563 5.396494e+00 2.017725e-02
#> 164       65 0.0055028784 5.276254e+00 2.161822e-02
#> 165       29 0.0026710878 5.222994e+00 2.229015e-02
#> 166       52 0.0052166934 5.209689e+00 2.246135e-02
#> 167       36 0.0030477481 5.207919e+00 2.248424e-02
#> 168        6 0.0005526389 5.125966e+00 2.357045e-02
#> 169        6 0.0005526389 5.125966e+00 2.357045e-02
#> 170        6 0.0006019262 5.111116e+00 2.377302e-02
#> 171       33 0.0027937690 5.089647e+00 2.406909e-02
#> 172      188 0.0146737434 4.934729e+00 2.632219e-02
#> 173       34 0.0031316202 4.875930e+00 2.723377e-02
#> 174       55 0.0042928505 4.844570e+00 2.773330e-02
#> 175       16 0.0014737036 4.799777e+00 2.846343e-02
#> 176       11 0.0010131712 4.792923e+00 2.857690e-02
#> 177       41 0.0037763655 4.767212e+00 2.900677e-02
#> 178       31 0.0026244497 4.765508e+00 2.903548e-02
#> 179       46 0.0038943447 4.714762e+00 2.990470e-02
#> 180       14 0.0011852354 4.657391e+00 3.092015e-02
#> 181       52 0.0044023027 4.647881e+00 3.109190e-02
#> 182      134 0.0123422677 4.632639e+00 3.136927e-02
#> 183       17 0.0014392144 4.605725e+00 3.186538e-02
#> 184       35 0.0029630884 4.532426e+00 3.325845e-02
#> 185       12 0.0012038523 4.528179e+00 3.334108e-02
#> 186       64 0.0049953169 4.519103e+00 3.351840e-02
#> 187       44 0.0040526849 4.518101e+00 3.353803e-02
#> 188        6 0.0005526389 4.424945e+00 3.541725e-02
#> 189        8 0.0007368518 4.405769e+00 3.581756e-02
#> 190        8 0.0008025682 4.390905e+00 3.613113e-02
#> 191       13 0.0011973842 4.323521e+00 3.758908e-02
#> 192      236 0.0184202310 4.320007e+00 3.766676e-02
#> 193       47 0.0039790044 4.285467e+00 3.843952e-02
#> 194       17 0.0015658101 4.260547e+00 3.900736e-02
#> 195        5 0.0004605324 4.119824e+00 4.238348e-02
#> 196        9 0.0008289583 4.117696e+00 4.243684e-02
#> 197        9 0.0009028892 4.102824e+00 4.281165e-02
#> 198        7 0.0006447453 4.077141e+00 4.346715e-02
#> 199       99 0.0083813071 4.050568e+00 4.415650e-02
#> 200       31 0.0028553007 4.045795e+00 4.428151e-02
#> 201       24 0.0020318320 4.015102e+00 4.509450e-02
#> 202       18 0.0016579166 3.948658e+00 4.690877e-02
#> 203       10 0.0009210648 3.865635e+00 4.928455e-02
#> 204      135 0.0114290552 3.790155e+00 5.155486e-02
#> 205        8 0.0006772773 3.776298e+00 5.198356e-02
#> 206       51 0.0039806431 3.762815e+00 5.240427e-02
#> 207       16 0.0016051364 3.754779e+00 5.265677e-02
#> 208        6 0.0005526389 3.751664e+00 5.275496e-02
#> 209       27 0.0022858110 3.717873e+00 5.383290e-02
#> 210      128 0.0117896288 3.715640e+00 5.390497e-02
#> 211       19 0.0017500230 3.616496e+00 5.720924e-02
#> 212       37 0.0034079396 3.616393e+00 5.721278e-02
#> 213      184 0.0155773789 3.588619e+00 5.817657e-02
#> 214       48 0.0048154093 3.572343e+00 5.874937e-02
#> 215       49 0.0038245395 3.480190e+00 6.210762e-02
#> 216       35 0.0029630884 3.447027e+00 6.336564e-02
#> 217       35 0.0032237266 3.439389e+00 6.365923e-02
#> 218        5 0.0004605324 3.430878e+00 6.398807e-02
#> 219        5 0.0004605324 3.430878e+00 6.398807e-02
#> 220        5 0.0004605324 3.430878e+00 6.398807e-02
#> 221        5 0.0005016051 3.420168e+00 6.440446e-02
#> 222        5 0.0005016051 3.420168e+00 6.440446e-02
#> 223      938 0.0732126132 3.392099e+00 6.550951e-02
#> 224       45 0.0045144462 3.382640e+00 6.588645e-02
#> 225       20 0.0018421295 3.361106e+00 6.675324e-02
#> 226       85 0.0066344052 3.341822e+00 6.753979e-02
#> 227       10 0.0009210648 3.303015e+00 6.915285e-02
#> 228       10 0.0009210648 3.303015e+00 6.915285e-02
#> 229       16 0.0013545547 3.286196e+00 6.986470e-02
#> 230       32 0.0032102729 3.280812e+00 7.009425e-02
#> 231       28 0.0023704707 3.249794e+00 7.143244e-02
#> 232       20 0.0018421295 3.228305e+00 7.237558e-02
#> 233       91 0.0077040298 3.149612e+00 7.594503e-02
#> 234       33 0.0030395137 3.135225e+00 7.661784e-02
#> 235       85 0.0071960718 3.114487e+00 7.759890e-02
#> 236       11 0.0010131712 3.107535e+00 7.793082e-02
#> 237       29 0.0024551304 3.100993e+00 7.824457e-02
#> 238       45 0.0038096851 3.097433e+00 7.841587e-02
#> 239       52 0.0044023027 3.042428e+00 8.111450e-02
#> 240       34 0.0034109149 3.030506e+00 8.171251e-02
#> 241       15 0.0015048154 2.988392e+00 8.386340e-02
#> 242       15 0.0013815971 2.978562e+00 8.437421e-02
#> 243       22 0.0018625127 2.948540e+00 8.595518e-02
#> 244       12 0.0011052777 2.934399e+00 8.671095e-02
#> 245        7 0.0006447453 2.845112e+00 9.165219e-02
#> 246        5 0.0004605324 2.777219e+00 9.561408e-02
#> 247        5 0.0004605324 2.777219e+00 9.561408e-02
#> 248       23 0.0019471724 2.775414e+00 9.572191e-02
#> 249        5 0.0004232983 2.773940e+00 9.581006e-02
#> 250        5 0.0004232983 2.773940e+00 9.581006e-02
#> 251        5 0.0005016051 2.767870e+00 9.617406e-02
#> 252      166 0.0140535049 2.725284e+00 9.877057e-02
#> 253       21 0.0017778530 2.704481e+00 1.000666e-01
#> 254       21 0.0021067416 2.693537e+00 1.007559e-01
#> 255       36 0.0033158331 2.669523e+00 1.022867e-01
#> 256       13 0.0011973842 2.662106e+00 1.027646e-01
#> 257       13 0.0011973842 2.662106e+00 1.027646e-01
#> 258       20 0.0016931934 2.626789e+00 1.050740e-01
#> 259       49 0.0041483237 2.618802e+00 1.056041e-01
#> 260       92 0.0077886895 2.543055e+00 1.107802e-01
#> 261      127 0.0099125820 2.541388e+00 1.108972e-01
#> 262      193 0.0150640025 2.482155e+00 1.151444e-01
#> 263       12 0.0011052777 2.452341e+00 1.173497e-01
#> 264       30 0.0027631943 2.424775e+00 1.194304e-01
#> 265      137 0.0115983745 2.418207e+00 1.199321e-01
#> 266      116 0.0098205215 2.402359e+00 1.211524e-01
#> 267      105 0.0088892652 2.400064e+00 1.213303e-01
#> 268       13 0.0011973842 2.320013e+00 1.277189e-01
#> 269       37 0.0034079396 2.308191e+00 1.286938e-01
#> 270       18 0.0015238740 2.305629e+00 1.289061e-01
#> 271       18 0.0015238740 2.305629e+00 1.289061e-01
#> 272      121 0.0094442710 2.244571e+00 1.340841e-01
#> 273       27 0.0022858110 2.243992e+00 1.341343e-01
#> 274       15 0.0013815971 2.167227e+00 1.409803e-01
#> 275        8 0.0007368518 2.096738e+00 1.476138e-01
#> 276        8 0.0007368518 2.096738e+00 1.476138e-01
#> 277        8 0.0007368518 2.096738e+00 1.476138e-01
#> 278       12 0.0010159160 2.069535e+00 1.502673e-01
#> 279       12 0.0010159160 2.069535e+00 1.502673e-01
#> 280       12 0.0011052777 2.066416e+00 1.505751e-01
#> 281       20 0.0018421295 2.038236e+00 1.533875e-01
#> 282       37 0.0034079396 2.019521e+00 1.552881e-01
#> 283       16 0.0014737036 1.998660e+00 1.574383e-01
#> 284       68 0.0057568574 1.995864e+00 1.577291e-01
#> 285       20 0.0018421295 1.995644e+00 1.577520e-01
#> 286       16 0.0014737036 1.981657e+00 1.592160e-01
#> 287        6 0.0005526389 1.942205e+00 1.634293e-01
#> 288        6 0.0005526389 1.942205e+00 1.634293e-01
#> 289        6 0.0005526389 1.942205e+00 1.634293e-01
#> 290        9 0.0008289583 1.939712e+00 1.636998e-01
#> 291       47 0.0043290043 1.904336e+00 1.675938e-01
#> 292       13 0.0011973842 1.891226e+00 1.690637e-01
#> 293       31 0.0028553007 1.874051e+00 1.710118e-01
#> 294       28 0.0025789813 1.864225e+00 1.721379e-01
#> 295       13 0.0013041734 1.850948e+00 1.736731e-01
#> 296       13 0.0013041734 1.850948e+00 1.736731e-01
#> 297       26 0.0023947684 1.850830e+00 1.736869e-01
#> 298      157 0.0122541367 1.847996e+00 1.740166e-01
#> 299       13 0.0011973842 1.843540e+00 1.745366e-01
#> 300       17 0.0014392144 1.828293e+00 1.763292e-01
#> 301       17 0.0015658101 1.824643e+00 1.767616e-01
#> 302       10 0.0010032103 1.795733e+00 1.802292e-01
#> 303       22 0.0020263425 1.759776e+00 1.846525e-01
#> 304       18 0.0018057785 1.698651e+00 1.924645e-01
#> 305       28 0.0021854511 1.693585e+00 1.931291e-01
#> 306       15 0.0015048154 1.692939e+00 1.932140e-01
#> 307       20 0.0018421295 1.690191e+00 1.935758e-01
#> 308       20 0.0018421295 1.690191e+00 1.935758e-01
#> 309       11 0.0011035313 1.679084e+00 1.950462e-01
#> 310       26 0.0023947684 1.664903e+00 1.969426e-01
#> 311       14 0.0012894907 1.663125e+00 1.971819e-01
#> 312        8 0.0006772773 1.609894e+00 2.045067e-01
#> 313        8 0.0008025682 1.604718e+00 2.052359e-01
#> 314        5 0.0004605324 1.602297e+00 2.055780e-01
#> 315        5 0.0004605324 1.602297e+00 2.055780e-01
#> 316        5 0.0005016051 1.595646e+00 2.065213e-01
#> 317       19 0.0019060995 1.582013e+00 2.084710e-01
#> 318       12 0.0009366219 1.579033e+00 2.089000e-01
#> 319       19 0.0017500230 1.573436e+00 2.097087e-01
#> 320       71 0.0060108364 1.530832e+00 2.159877e-01
#> 321       11 0.0011035313 1.526257e+00 2.166751e-01
#> 322       11 0.0011035313 1.526257e+00 2.166751e-01
#> 323       11 0.0011035313 1.526257e+00 2.166751e-01
#> 324       11 0.0010131712 1.520063e+00 2.176099e-01
#> 325       15 0.0013815971 1.514093e+00 2.185156e-01
#> 326       25 0.0025080257 1.502033e+00 2.203588e-01
#> 327       13 0.0011973842 1.496379e+00 2.212294e-01
#> 328       18 0.0015238740 1.481257e+00 2.235780e-01
#> 329       20 0.0018421295 1.471391e+00 2.251264e-01
#> 330       20 0.0018421295 1.471391e+00 2.251264e-01
#> 331       24 0.0022105554 1.466846e+00 2.258440e-01
#> 332        6 0.0005079580 1.426324e+00 2.323653e-01
#> 333       19 0.0017500230 1.424514e+00 2.326619e-01
#> 334        6 0.0006019262 1.422053e+00 2.330657e-01
#> 335       14 0.0014044944 1.406915e+00 2.355691e-01
#> 336      166 0.0129566032 1.394961e+00 2.375692e-01
#> 337       21 0.0019342360 1.381355e+00 2.398704e-01
#> 338       10 0.0009210648 1.380909e+00 2.399465e-01
#> 339       10 0.0009210648 1.380909e+00 2.399465e-01
#> 340       12 0.0012038523 1.352672e+00 2.448116e-01
#> 341       12 0.0010159160 1.349061e+00 2.454423e-01
#> 342       12 0.0011052777 1.346487e+00 2.458931e-01
#> 343       12 0.0011052777 1.346487e+00 2.458931e-01
#> 344       12 0.0011052777 1.346487e+00 2.458931e-01
#> 345       34 0.0028784287 1.338987e+00 2.472127e-01
#> 346       44 0.0037250254 1.290132e+00 2.560236e-01
#> 347       11 0.0010131712 1.288982e+00 2.562357e-01
#> 348       17 0.0014392144 1.285381e+00 2.569010e-01
#> 349       36 0.0033158331 1.282675e+00 2.574023e-01
#> 350       11 0.0011035313 1.280935e+00 2.577253e-01
#> 351       16 0.0013545547 1.276972e+00 2.584628e-01
#> 352      246 0.0192007493 1.249702e+00 2.636094e-01
#> 353       36 0.0036115570 1.239950e+00 2.654808e-01
#> 354       17 0.0014392144 1.218663e+00 2.696232e-01
#> 355       24 0.0022105554 1.185751e+00 2.761882e-01
#> 356        8 0.0007368518 1.176242e+00 2.781223e-01
#> 357        8 0.0008025682 1.169566e+00 2.794905e-01
#> 358        8 0.0008025682 1.169566e+00 2.794905e-01
#> 359       24 0.0022105554 1.165295e+00 2.803701e-01
#> 360       41 0.0034710464 1.148591e+00 2.838442e-01
#> 361       46 0.0042368979 1.148457e+00 2.838725e-01
#> 362       13 0.0011973842 1.138567e+00 2.859554e-01
#> 363       47 0.0047150883 1.130676e+00 2.876313e-01
#> 364       13 0.0013041734 1.130502e+00 2.876683e-01
#> 365       19 0.0016085337 1.117262e+00 2.905088e-01
#> 366       14 0.0014044944 1.100200e+00 2.942223e-01
#> 367       14 0.0014044944 1.100200e+00 2.942223e-01
#> 368       14 0.0010927256 1.099686e+00 2.943351e-01
#> 369        5 0.0004605324 1.098724e+00 2.945462e-01
#> 370        5 0.0004605324 1.098724e+00 2.945462e-01
#> 371        5 0.0004605324 1.098724e+00 2.945462e-01
#> 372        5 0.0004232983 1.096797e+00 2.949701e-01
#> 373        5 0.0004232983 1.096797e+00 2.949701e-01
#> 374       14 0.0012894907 1.094034e+00 2.955791e-01
#> 375        5 0.0005016051 1.093410e+00 2.957168e-01
#> 376        5 0.0005016051 1.093410e+00 2.957168e-01
#> 377        5 0.0005016051 1.093410e+00 2.957168e-01
#> 378        5 0.0005016051 1.093410e+00 2.957168e-01
#> 379        9 0.0008289583 1.081573e+00 2.983459e-01
#> 380       14 0.0011852354 1.073394e+00 3.001802e-01
#> 381       81 0.0074606245 1.060328e+00 3.031403e-01
#> 382       20 0.0018421295 1.039972e+00 3.078280e-01
#> 383       10 0.0010032103 1.037896e+00 3.083113e-01
#> 384       10 0.0010032103 1.037896e+00 3.083113e-01
#> 385       10 0.0008465967 1.034978e+00 3.089923e-01
#> 386       10 0.0008465967 1.034978e+00 3.089923e-01
#> 387       10 0.0008465967 1.034978e+00 3.089923e-01
#> 388       10 0.0008465967 1.034978e+00 3.089923e-01
#> 389       10 0.0009210648 1.032935e+00 3.094704e-01
#> 390       15 0.0013815971 1.020686e+00 3.123563e-01
#> 391       15 0.0015048154 1.005530e+00 3.159761e-01
#> 392       10 0.0010032103 9.947788e-01 3.185772e-01
#> 393       16 0.0014737036 9.708182e-01 3.244762e-01
#> 394       16 0.0013545547 9.679485e-01 3.251923e-01
#> 395        6 0.0006019262 9.679395e-01 3.251946e-01
#> 396        6 0.0006019262 9.679395e-01 3.251946e-01
#> 397       44 0.0037250254 9.661000e-01 3.256548e-01
#> 398       11 0.0011035313 9.261168e-01 3.358740e-01
#> 399       16 0.0016051364 9.254290e-01 3.360535e-01
#> 400       22 0.0020263425 9.221121e-01 3.369210e-01
#> 401       22 0.0020263425 9.221121e-01 3.369210e-01
#> 402       11 0.0011035313 9.124320e-01 3.394700e-01
#> 403       18 0.0015238740 8.821306e-01 3.476188e-01
#> 404        7 0.0006447453 8.742199e-01 3.497897e-01
#> 405        7 0.0006447453 8.742199e-01 3.497897e-01
#> 406       12 0.0011052777 8.733251e-01 3.500364e-01
#> 407        7 0.0007022472 8.688880e-01 3.512633e-01
#> 408       12 0.0012038523 8.666125e-01 3.518948e-01
#> 409       17 0.0015658101 8.506386e-01 3.563717e-01
#> 410       49 0.0045132173 8.436500e-01 3.583550e-01
#> 411      144 0.0112394630 8.396641e-01 3.594928e-01
#> 412       60 0.0050795801 8.371128e-01 3.602238e-01
#> 413       12 0.0011052777 8.084445e-01 3.685805e-01
#> 414        8 0.0007368518 7.940483e-01 3.728788e-01
#> 415       25 0.0023026619 7.864635e-01 3.751716e-01
#> 416       14 0.0012894907 7.753432e-01 3.785692e-01
#> 417       43 0.0043138042 7.727667e-01 3.793625e-01
#> 418       53 0.0044869624 7.717140e-01 3.796873e-01
#> 419       14 0.0014044944 7.686124e-01 3.806467e-01
#> 420       54 0.0049737497 7.575198e-01 3.841058e-01
#> 421       55 0.0046562817 7.380364e-01 3.902906e-01
#> 422       13 0.0013041734 7.332132e-01 3.918437e-01
#> 423        9 0.0008289583 7.278248e-01 3.935892e-01
#> 424        9 0.0008289583 7.278248e-01 3.935892e-01
#> 425       23 0.0021184489 7.271835e-01 3.937976e-01
#> 426        9 0.0007619370 7.258842e-01 3.942206e-01
#> 427        9 0.0009028892 7.224748e-01 3.953333e-01
#> 428       33 0.0030395137 7.179120e-01 3.968297e-01
#> 429       20 0.0016931934 6.957263e-01 4.042234e-01
#> 430       16 0.0013545547 6.955806e-01 4.042727e-01
#> 431       20 0.0018421295 6.931880e-01 4.050821e-01
#> 432       10 0.0009210648 6.722019e-01 4.122850e-01
#> 433        5 0.0004605324 6.670836e-01 4.140703e-01
#> 434        5 0.0004605324 6.670836e-01 4.140703e-01
#> 435        5 0.0004605324 6.670836e-01 4.140703e-01
#> 436       10 0.0010032103 6.668428e-01 4.141545e-01
#> 437        5 0.0004232983 6.656215e-01 4.145823e-01
#> 438        5 0.0004232983 6.656215e-01 4.145823e-01
#> 439       14 0.0011852354 6.640905e-01 4.151196e-01
#> 440       14 0.0011852354 6.640905e-01 4.151196e-01
#> 441       14 0.0011852354 6.640905e-01 4.151196e-01
#> 442        5 0.0005016051 6.630975e-01 4.154685e-01
#> 443        5 0.0005016051 6.630975e-01 4.154685e-01
#> 444       14 0.0012894907 6.620641e-01 4.158322e-01
#> 445       21 0.0019342360 6.523760e-01 4.192645e-01
#> 446        9 0.0009028892 6.214608e-01 4.305050e-01
#> 447       11 0.0011035313 6.194556e-01 4.312497e-01
#> 448       11 0.0011035313 6.194556e-01 4.312497e-01
#> 449        9 0.0007619370 6.192564e-01 4.313238e-01
#> 450        9 0.0007619370 6.192564e-01 4.313238e-01
#> 451       22 0.0018625127 6.183934e-01 4.316449e-01
#> 452        9 0.0008289583 6.177400e-01 4.318883e-01
#> 453        9 0.0008289583 6.177400e-01 4.318883e-01
#> 454        9 0.0008289583 6.177400e-01 4.318883e-01
#> 455       19 0.0017500230 6.083752e-01 4.354002e-01
#> 456       41 0.0034710464 6.066645e-01 4.360464e-01
#> 457       29 0.0024551304 5.990429e-01 4.389435e-01
#> 458        6 0.0005526389 5.867561e-01 4.436763e-01
#> 459        6 0.0005526389 5.867561e-01 4.436763e-01
#> 460        6 0.0005526389 5.867561e-01 4.436763e-01
#> 461       23 0.0019471724 5.855320e-01 4.441521e-01
#> 462       32 0.0027091094 5.841325e-01 4.446970e-01
#> 463       23 0.0021184489 5.830072e-01 4.451360e-01
#> 464        6 0.0006019262 5.827609e-01 4.452322e-01
#> 465        6 0.0006019262 5.827609e-01 4.452322e-01
#> 466       32 0.0029474072 5.811038e-01 4.458800e-01
#> 467       12 0.0012038523 5.786074e-01 4.468587e-01
#> 468       16 0.0014737036 5.590780e-01 4.546320e-01
#> 469       24 0.0020318320 5.558034e-01 4.559561e-01
#> 470       21 0.0016390884 5.556398e-01 4.560224e-01
#> 471       24 0.0022105554 5.532832e-01 4.569793e-01
#> 472      334 0.0282763292 5.454514e-01 4.601822e-01
#> 473       10 0.0010032103 5.411295e-01 4.619650e-01
#> 474       10 0.0010032103 5.411295e-01 4.619650e-01
#> 475       10 0.0010032103 5.411295e-01 4.619650e-01
#> 476       22 0.0020263425 5.402804e-01 4.623166e-01
#> 477       10 0.0008465967 5.389305e-01 4.628763e-01
#> 478       10 0.0009210648 5.374183e-01 4.635046e-01
#> 479       25 0.0023026619 5.262648e-01 4.681815e-01
#> 480       14 0.0012894907 5.171685e-01 4.720520e-01
#> 481       26 0.0022011514 5.041104e-01 4.776996e-01
#> 482       36 0.0030477481 5.010690e-01 4.790308e-01
#> 483       48 0.0044211108 4.980597e-01 4.803539e-01
#> 484       70 0.0064474533 4.937124e-01 4.822760e-01
#> 485       18 0.0018057785 4.875724e-01 4.850122e-01
#> 486       11 0.0011035313 4.786557e-01 4.890319e-01
#> 487       11 0.0010131712 4.749541e-01 4.907168e-01
#> 488       11 0.0010131712 4.749541e-01 4.907168e-01
#> 489       11 0.0010131712 4.749541e-01 4.907168e-01
#> 490        8 0.0007368518 4.743160e-01 4.910083e-01
#> 491       39 0.0030440212 4.634440e-01 4.960188e-01
#> 492       16 0.0013545547 4.628474e-01 4.962962e-01
#> 493       16 0.0013545547 4.628474e-01 4.962962e-01
#> 494       40 0.0036842590 4.622132e-01 4.965914e-01
#> 495       56 0.0047409414 4.612838e-01 4.970246e-01
#> 496      120 0.0110527770 4.384767e-01 5.078586e-01
#> 497        9 0.0008289583 4.334371e-01 5.103072e-01
#> 498        9 0.0008289583 4.334371e-01 5.103072e-01
#> 499        9 0.0007619370 4.319613e-01 5.110282e-01
#> 500        9 0.0007619370 4.319613e-01 5.110282e-01
#> 501       12 0.0012038523 4.286819e-01 5.126364e-01
#> 502       12 0.0012038523 4.286819e-01 5.126364e-01
#> 503       20 0.0016931934 4.257764e-01 5.140687e-01
#> 504       12 0.0011052777 4.249901e-01 5.144575e-01
#> 505       12 0.0011052777 4.249901e-01 5.144575e-01
#> 506      608 0.0474555105 4.151795e-01 5.193523e-01
#> 507       20 0.0016931934 3.860760e-01 5.343693e-01
#> 508       13 0.0011005757 3.856165e-01 5.346126e-01
#> 509       11 0.0010131712 3.705608e-01 5.426988e-01
#> 510       11 0.0009312563 3.690779e-01 5.435073e-01
#> 511       11 0.0011035313 3.665193e-01 5.449077e-01
#> 512       23 0.0021184489 3.569892e-01 5.501832e-01
#> 513       37 0.0031324077 3.507265e-01 5.537021e-01
#> 514       14 0.0012894907 3.500626e-01 5.540777e-01
#> 515       14 0.0012894907 3.500626e-01 5.540777e-01
#> 516       14 0.0012894907 3.500626e-01 5.540777e-01
#> 517       23 0.0021184489 3.463134e-01 5.562075e-01
#> 518       12 0.0010159160 3.443796e-01 5.573121e-01
#> 519       69 0.0053855760 3.265088e-01 5.677221e-01
#> 520       13 0.0011973842 3.244679e-01 5.689349e-01
#> 521       25 0.0021164917 3.243532e-01 5.690032e-01
#> 522        5 0.0004605324 3.239954e-01 5.692164e-01
#> 523        5 0.0004605324 3.239954e-01 5.692164e-01
#> 524        5 0.0004605324 3.239954e-01 5.692164e-01
#> 525        5 0.0004232983 3.230062e-01 5.698066e-01
#> 526        5 0.0004232983 3.230062e-01 5.698066e-01
#> 527        5 0.0004232983 3.230062e-01 5.698066e-01
#> 528      155 0.0131222486 3.222665e-01 5.702487e-01
#> 529       25 0.0021164917 3.215616e-01 5.706707e-01
#> 530        5 0.0005016051 3.213289e-01 5.708101e-01
#> 531        5 0.0005016051 3.213289e-01 5.708101e-01
#> 532        5 0.0005016051 3.213289e-01 5.708101e-01
#> 533        5 0.0005016051 3.213289e-01 5.708101e-01
#> 534        5 0.0005016051 3.213289e-01 5.708101e-01
#> 535        5 0.0005016051 3.213289e-01 5.708101e-01
#> 536       38 0.0032170674 3.181776e-01 5.727049e-01
#> 537       63 0.0053335591 3.167289e-01 5.735801e-01
#> 538       26 0.0022011514 3.113807e-01 5.768342e-01
#> 539       26 0.0023947684 3.072828e-01 5.793524e-01
#> 540       16 0.0014737036 2.965639e-01 5.860446e-01
#> 541        8 0.0006772773 2.920967e-01 5.888801e-01
#> 542        8 0.0006772773 2.920967e-01 5.888801e-01
#> 543        8 0.0006772773 2.920967e-01 5.888801e-01
#> 544        8 0.0007368518 2.911004e-01 5.895163e-01
#> 545        8 0.0007368518 2.911004e-01 5.895163e-01
#> 546        8 0.0007368518 2.911004e-01 5.895163e-01
#> 547        8 0.0007368518 2.911004e-01 5.895163e-01
#> 548        8 0.0007368518 2.911004e-01 5.895163e-01
#> 549        8 0.0007368518 2.911004e-01 5.895163e-01
#> 550        8 0.0007368518 2.911004e-01 5.895163e-01
#> 551        8 0.0007368518 2.911004e-01 5.895163e-01
#> 552       15 0.0013815971 2.892348e-01 5.907113e-01
#> 553       15 0.0012698950 2.877379e-01 5.916738e-01
#> 554        6 0.0005526389 2.830007e-01 5.947412e-01
#> 555        6 0.0005526389 2.830007e-01 5.947412e-01
#> 556        6 0.0005526389 2.830007e-01 5.947412e-01
#> 557        6 0.0005526389 2.830007e-01 5.947412e-01
#> 558        6 0.0005526389 2.830007e-01 5.947412e-01
#> 559        6 0.0006019262 2.803249e-01 5.964884e-01
#> 560        6 0.0006019262 2.803249e-01 5.964884e-01
#> 561       31 0.0026244497 2.696172e-01 6.035887e-01
#> 562        9 0.0009028892 2.525718e-01 6.152700e-01
#> 563        9 0.0009028892 2.525718e-01 6.152700e-01
#> 564        9 0.0008289583 2.501101e-01 6.169975e-01
#> 565        9 0.0008289583 2.501101e-01 6.169975e-01
#> 566        9 0.0008289583 2.501101e-01 6.169975e-01
#> 567        9 0.0008289583 2.501101e-01 6.169975e-01
#> 568       18 0.0016579166 2.496207e-01 6.173423e-01
#> 569        7 0.0007022472 2.489740e-01 6.177984e-01
#> 570        7 0.0007022472 2.489740e-01 6.177984e-01
#> 571        7 0.0007022472 2.489740e-01 6.177984e-01
#> 572        7 0.0007022472 2.489740e-01 6.177984e-01
#> 573       20 0.0018421295 2.292274e-01 6.320973e-01
#> 574       20 0.0016931934 2.277129e-01 6.332249e-01
#> 575        8 0.0007368518 2.269226e-01 6.338151e-01
#> 576       20 0.0016931934 2.267506e-01 6.339438e-01
#> 577       20 0.0016931934 2.267506e-01 6.339438e-01
#> 578        8 0.0006772773 2.259226e-01 6.345638e-01
#> 579        8 0.0008025682 2.242280e-01 6.358371e-01
#> 580       10 0.0008465967 2.197611e-01 6.392220e-01
#> 581       10 0.0009210648 2.187731e-01 6.399763e-01
#> 582       10 0.0009210648 2.187731e-01 6.399763e-01
#> 583       21 0.0016390884 2.169844e-01 6.413473e-01
#> 584       35 0.0029630884 2.152810e-01 6.426593e-01
#> 585      122 0.0103284795 2.112756e-01 6.457695e-01
#> 586        9 0.0008289583 2.069037e-01 6.492051e-01
#> 587        9 0.0008289583 2.069037e-01 6.492051e-01
#> 588        9 0.0007619370 2.059001e-01 6.500000e-01
#> 589       23 0.0021184489 2.047739e-01 6.508948e-01
#> 590        9 0.0009028892 2.041997e-01 6.513521e-01
#> 591       11 0.0009312563 1.950247e-01 6.587671e-01
#> 592       11 0.0010131712 1.940410e-01 6.595744e-01
#> 593       11 0.0010131712 1.940410e-01 6.595744e-01
#> 594       11 0.0010131712 1.940410e-01 6.595744e-01
#> 595     1269 0.0990477677 1.918802e-01 6.613564e-01
#> 596       25 0.0023026619 1.915018e-01 6.616696e-01
#> 597       10 0.0009210648 1.903719e-01 6.626073e-01
#> 598       10 0.0009210648 1.903719e-01 6.626073e-01
#> 599       10 0.0009210648 1.903719e-01 6.626073e-01
#> 600       24 0.0022105554 1.799736e-01 6.713959e-01
#> 601       12 0.0012038523 1.764590e-01 6.744342e-01
#> 602       12 0.0012038523 1.764590e-01 6.744342e-01
#> 603       12 0.0012038523 1.764590e-01 6.744342e-01
#> 604       41 0.0037763655 1.752106e-01 6.755219e-01
#> 605       25 0.0025080257 1.746806e-01 6.759851e-01
#> 606       42 0.0042134831 1.746329e-01 6.760268e-01
#> 607       12 0.0011052777 1.740267e-01 6.765577e-01
#> 608       12 0.0011052777 1.740267e-01 6.765577e-01
#> 609       12 0.0011052777 1.740267e-01 6.765577e-01
#> 610       12 0.0010159160 1.636552e-01 6.858133e-01
#> 611       12 0.0012038523 1.619372e-01 6.873792e-01
#> 612       12 0.0012038523 1.619372e-01 6.873792e-01
#> 613       13 0.0011005757 1.584746e-01 6.905648e-01
#> 614       13 0.0011973842 1.574994e-01 6.914693e-01
#> 615       27 0.0022858110 1.569768e-01 6.919553e-01
#> 616       13 0.0011973842 1.544840e-01 6.942866e-01
#> 617       14 0.0012894907 1.436222e-01 7.047063e-01
#> 618       14 0.0014044944 1.428651e-01 7.054491e-01
#> 619       29 0.0026710878 1.422782e-01 7.060266e-01
#> 620       15 0.0013815971 1.318062e-01 7.165666e-01
#> 621       16 0.0016051364 1.281624e-01 7.203450e-01
#> 622       54 0.0042147986 1.273397e-01 7.212064e-01
#> 623       43 0.0039605784 1.253866e-01 7.232641e-01
#> 624       33 0.0027937690 1.223321e-01 7.265187e-01
#> 625       56 0.0051579626 1.175120e-01 7.317487e-01
#> 626       17 0.0014392144 1.137204e-01 7.359476e-01
#> 627       17 0.0015658101 1.127622e-01 7.370211e-01
#> 628       20 0.0018421295 1.097962e-01 7.403763e-01
#> 629       18 0.0018057785 1.073512e-01 7.431802e-01
#> 630       18 0.0015238740 1.059320e-01 7.448239e-01
#> 631       20 0.0015610365 9.404075e-02 7.591017e-01
#> 632       20 0.0016931934 9.289141e-02 7.605330e-01
#> 633        5 0.0004605324 9.162142e-02 7.621259e-01
#> 634        5 0.0004605324 9.162142e-02 7.621259e-01
#> 635        5 0.0004605324 9.162142e-02 7.621259e-01
#> 636        5 0.0004605324 9.162142e-02 7.621259e-01
#> 637        5 0.0004605324 9.162142e-02 7.621259e-01
#> 638        5 0.0004232983 9.111252e-02 7.627675e-01
#> 639        5 0.0004232983 9.111252e-02 7.627675e-01
#> 640        5 0.0005016051 9.026594e-02 7.638393e-01
#> 641        5 0.0005016051 9.026594e-02 7.638393e-01
#> 642        5 0.0005016051 9.026594e-02 7.638393e-01
#> 643        5 0.0005016051 9.026594e-02 7.638393e-01
#> 644       21 0.0019342360 8.643790e-02 7.687554e-01
#> 645       21 0.0019342360 8.643790e-02 7.687554e-01
#> 646       27 0.0022858110 8.609945e-02 7.691957e-01
#> 647       27 0.0022858110 8.609945e-02 7.691957e-01
#> 648       28 0.0025789813 8.482101e-02 7.708674e-01
#> 649        6 0.0005526389 7.974880e-02 7.776381e-01
#> 650        6 0.0005526389 7.974880e-02 7.776381e-01
#> 651        6 0.0005526389 7.974880e-02 7.776381e-01
#> 652        6 0.0005526389 7.974880e-02 7.776381e-01
#> 653        6 0.0005526389 7.974880e-02 7.776381e-01
#> 654        6 0.0005526389 7.974880e-02 7.776381e-01
#> 655        6 0.0005526389 7.974880e-02 7.776381e-01
#> 656        6 0.0005526389 7.974880e-02 7.776381e-01
#> 657        6 0.0005079580 7.923617e-02 7.783352e-01
#> 658        6 0.0005079580 7.923617e-02 7.783352e-01
#> 659        6 0.0006019262 7.838376e-02 7.794998e-01
#> 660        6 0.0006019262 7.838376e-02 7.794998e-01
#> 661        7 0.0007022472 7.639028e-02 7.822501e-01
#> 662        7 0.0007022472 7.639028e-02 7.822501e-01
#> 663        7 0.0007022472 7.639028e-02 7.822501e-01
#> 664        7 0.0007022472 7.639028e-02 7.822501e-01
#> 665        7 0.0007022472 7.639028e-02 7.822501e-01
#> 666        7 0.0006447453 7.517710e-02 7.839429e-01
#> 667        7 0.0006447453 7.517710e-02 7.839429e-01
#> 668        7 0.0006447453 7.517710e-02 7.839429e-01
#> 669        7 0.0006447453 7.517710e-02 7.839429e-01
#> 670        7 0.0006447453 7.517710e-02 7.839429e-01
#> 671       33 0.0030395137 7.515248e-02 7.839774e-01
#> 672        7 0.0006447453 7.085241e-02 7.900997e-01
#> 673        7 0.0007022472 6.947780e-02 7.920986e-01
#> 674      233 0.0181860756 6.678974e-02 7.960696e-01
#> 675        8 0.0008025682 6.451088e-02 7.995033e-01
#> 676        8 0.0008025682 6.451088e-02 7.995033e-01
#> 677        8 0.0008025682 6.451088e-02 7.995033e-01
#> 678        8 0.0007368518 6.394017e-02 8.003734e-01
#> 679        8 0.0006772773 6.378678e-02 8.006079e-01
#> 680        8 0.0006772773 6.342006e-02 8.011698e-01
#> 681        8 0.0007368518 6.330750e-02 8.013427e-01
#> 682        8 0.0007368518 6.330750e-02 8.013427e-01
#> 683        8 0.0007368518 6.330750e-02 8.013427e-01
#> 684        8 0.0007368518 6.330750e-02 8.013427e-01
#> 685        8 0.0007368518 6.330750e-02 8.013427e-01
#> 686       54 0.0049737497 6.306660e-02 8.017131e-01
#> 687       46 0.0042368979 5.969459e-02 8.069793e-01
#> 688        9 0.0008289583 5.841684e-02 8.090159e-01
#> 689        9 0.0009028892 5.560770e-02 8.135779e-01
#> 690        9 0.0009028892 5.560770e-02 8.135779e-01
#> 691        9 0.0009028892 5.560770e-02 8.135779e-01
#> 692        9 0.0009028892 5.560770e-02 8.135779e-01
#> 693        9 0.0007024664 5.546734e-02 8.138090e-01
#> 694        9 0.0007619370 5.488927e-02 8.147641e-01
#> 695        9 0.0008289583 5.441414e-02 8.155530e-01
#> 696        9 0.0008289583 5.441414e-02 8.155530e-01
#> 697        9 0.0008289583 5.441414e-02 8.155530e-01
#> 698        9 0.0008289583 5.441414e-02 8.155530e-01
#> 699        9 0.0008289583 5.441414e-02 8.155530e-01
#> 700       10 0.0009210648 5.390362e-02 8.164048e-01
#> 701       10 0.0009210648 5.390362e-02 8.164048e-01
#> 702       10 0.0008465967 5.337602e-02 8.172895e-01
#> 703       10 0.0010032103 5.250026e-02 8.187684e-01
#> 704       11 0.0010131712 5.014798e-02 8.228060e-01
#> 705       10 0.0010032103 4.868866e-02 8.253610e-01
#> 706       10 0.0009210648 4.750493e-02 8.274632e-01
#> 707       10 0.0009210648 4.750493e-02 8.274632e-01
#> 708       12 0.0011052777 4.697510e-02 8.284130e-01
#> 709       34 0.0031316202 4.587985e-02 8.303944e-01
#> 710       11 0.0011035313 4.315853e-02 8.354270e-01
#> 711       11 0.0011035313 4.315853e-02 8.354270e-01
#> 712       11 0.0011035313 4.315853e-02 8.354270e-01
#> 713       11 0.0011035313 4.315853e-02 8.354270e-01
#> 714       11 0.0010131712 4.198463e-02 8.376491e-01
#> 715       11 0.0010131712 4.198463e-02 8.376491e-01
#> 716       11 0.0010131712 4.198463e-02 8.376491e-01
#> 717       14 0.0014044944 4.046966e-02 8.405651e-01
#> 718       76 0.0064341348 3.936938e-02 8.427187e-01
#> 719       16 0.0014737036 3.805431e-02 8.453341e-01
#> 720       16 0.0013545547 3.750424e-02 8.464420e-01
#> 721       13 0.0013041734 3.487607e-02 8.518556e-01
#> 722       13 0.0013041734 3.487607e-02 8.518556e-01
#> 723       13 0.0011005757 3.418034e-02 8.533237e-01
#> 724       13 0.0011005757 3.418034e-02 8.533237e-01
#> 725       13 0.0011973842 3.372184e-02 8.542997e-01
#> 726       13 0.0011973842 3.372184e-02 8.542997e-01
#> 727       20 0.0018421295 3.258101e-02 8.567584e-01
#> 728       20 0.0018421295 3.258101e-02 8.567584e-01
#> 729       14 0.0010927256 3.156101e-02 8.589946e-01
#> 730       20 0.0020064205 3.108173e-02 8.600582e-01
#> 731       14 0.0012894907 3.055199e-02 8.612436e-01
#> 732       15 0.0015048154 2.897457e-02 8.648378e-01
#> 733       15 0.0015048154 2.897457e-02 8.648378e-01
#> 734       15 0.0015048154 2.897457e-02 8.648378e-01
#> 735       15 0.0013815971 2.784002e-02 8.674855e-01
#> 736       16 0.0012488292 2.648569e-02 8.707198e-01
#> 737       16 0.0013545547 2.594036e-02 8.720460e-01
#> 738       29 0.0026710878 2.572801e-02 8.725664e-01
#> 739       17 0.0015658101 2.344629e-02 8.783022e-01
#> 740       18 0.0018057785 2.274836e-02 8.801133e-01
#> 741       18 0.0018057785 2.274836e-02 8.801133e-01
#> 742       60 0.0050795801 2.023828e-02 8.868736e-01
#> 743       20 0.0018421295 1.861790e-02 8.914676e-01
#> 744       20 0.0018421295 1.861790e-02 8.914676e-01
#> 745       65 0.0055028784 1.770418e-02 8.941483e-01
#> 746       84 0.0065563534 1.494491e-02 9.027015e-01
#> 747      143 0.0111614112 3.358538e-03 9.537862e-01
#> 748       75 0.0063494751 2.194502e-03 9.626364e-01
#> 749       61 0.0051642398 1.789690e-03 9.662558e-01
#> 750       66 0.0060790274 1.610683e-03 9.679868e-01
#> 751       69 0.0058415171 1.603440e-03 9.680589e-01
#> 752       40 0.0033863867 1.180673e-03 9.725893e-01
#> 753       37 0.0031324077 1.093493e-03 9.736204e-01
#> 754       26 0.0022011514 7.734560e-04 9.778128e-01
#> 755       18 0.0015238740 5.403264e-04 9.814549e-01
#> 756       17 0.0014392144 5.111630e-04 9.819622e-01
#> 757       16 0.0013545547 4.819945e-04 9.824844e-01
#> 758       20 0.0020064205 4.793435e-04 9.825326e-01
#> 759       15 0.0012698950 4.528212e-04 9.830226e-01
#> 760       12 0.0011052777 4.135219e-04 9.837759e-01
#> 761       12 0.0011052777 4.135219e-04 9.837759e-01
#> 762       14 0.0014044944 3.392523e-04 9.853048e-01
#> 763        9 0.0008289583 3.143638e-04 9.858540e-01
#> 764        9 0.0008289583 3.143638e-04 9.858540e-01
#> 765        9 0.0008289583 3.143638e-04 9.858540e-01
#> 766       12 0.0012038523 2.925177e-04 9.863543e-01
#> 767       12 0.0012038523 2.925177e-04 9.863543e-01
#> 768       11 0.0008585701 2.855114e-04 9.865187e-01
#> 769        8 0.0006772773 2.484689e-04 9.874235e-01
#> 770        8 0.0006772773 2.484689e-04 9.874235e-01
#> 771        7 0.0006447453 2.482278e-04 9.874297e-01
#> 772       10 0.0010032103 2.457643e-04 9.874922e-01
#> 773        9 0.0009028892 2.223805e-04 9.881020e-01
#> 774        7 0.0005926177 2.192559e-04 9.881859e-01
#> 775        6 0.0005526389 2.151506e-04 9.882970e-01
#> 776        6 0.0005526389 2.151506e-04 9.882970e-01
#> 777        8 0.0008025682 1.989921e-04 9.887451e-01
#> 778        8 0.0008025682 1.989921e-04 9.887451e-01
#> 779        5 0.0004605324 1.820674e-04 9.892343e-01
#> 780        5 0.0004605324 1.820674e-04 9.892343e-01
#> 781        5 0.0004605324 1.820674e-04 9.892343e-01
#> 782        5 0.0004605324 1.820674e-04 9.892343e-01
#> 783        5 0.0004605324 1.820674e-04 9.892343e-01
#> 784        7 0.0007022472 1.755989e-04 9.894272e-01
#> 785        7 0.0007022472 1.755989e-04 9.894272e-01
#> 786        7 0.0007022472 1.755989e-04 9.894272e-01
#> 787        7 0.0007022472 1.755989e-04 9.894272e-01
#> 788        5 0.0004232983 1.608150e-04 9.898821e-01
#> 789        5 0.0005016051 1.287984e-04 9.909451e-01
#> 790        5 0.0005016051 1.287984e-04 9.909451e-01
#> 791        5 0.0005016051 1.287984e-04 9.909451e-01
#> 792        5 0.0005016051 1.287984e-04 9.909451e-01
#> 793        6 0.0006019262 1.990504e-25 1.000000e+00
#> 794        6 0.0006019262 1.990504e-25 1.000000e+00
#> 795        6 0.0006019262 1.990504e-25 1.000000e+00
#> 796        6 0.0006019262 1.990504e-25 1.000000e+00
#> 797        6 0.0006019262 1.990504e-25 1.000000e+00
#> 798        6 0.0006019262 1.990504e-25 1.000000e+00
#> 799        9 0.0007619370 1.871051e-25 1.000000e+00
#> 800        9 0.0007619370 1.871051e-25 1.000000e+00
#> 801       11 0.0011035313 1.538890e-25 1.000000e+00
#> 802       11 0.0011035313 1.538890e-25 1.000000e+00
#> 803        7 0.0005926177 1.126621e-25 1.000000e+00
#> 804        6 0.0005526389 1.124714e-25 1.000000e+00
#> 805        6 0.0005526389 1.124714e-25 1.000000e+00
#> 806        6 0.0005526389 1.124714e-25 1.000000e+00
#> 807        6 0.0005526389 1.124714e-25 1.000000e+00
#> 808       12 0.0012038523 1.036765e-25 1.000000e+00
#> 809       13 0.0011973842 9.162355e-26 1.000000e+00
#> 810       11 0.0009312563 8.587986e-26 1.000000e+00
#> 811       10 0.0008465967 8.043283e-26 1.000000e+00
#> 812       10 0.0008465967 8.043283e-26 1.000000e+00
#> 813        9 0.0009028892 7.273870e-26 1.000000e+00
#> 814        9 0.0009028892 7.273870e-26 1.000000e+00
#> 815        9 0.0009028892 7.273870e-26 1.000000e+00
#> 816       10 0.0009210648 7.167333e-26 1.000000e+00
#> 817        9 0.0008289583 6.726854e-26 1.000000e+00
#> 818       11 0.0011035313 3.908265e-26 1.000000e+00
#> 819        6 0.0005526389 3.011378e-26 1.000000e+00
#> 820        6 0.0005526389 3.011378e-26 1.000000e+00
#> 821        6 0.0005526389 3.011378e-26 1.000000e+00
#> 822        6 0.0005526389 3.011378e-26 1.000000e+00
#> 823        6 0.0005526389 3.011378e-26 1.000000e+00
#> 824       15 0.0013815971 2.874724e-26 1.000000e+00
#> 825       15 0.0013815971 2.874724e-26 1.000000e+00
#> 826       23 0.0019471724 2.872199e-26 1.000000e+00
#> 827       17 0.0017054575 2.706295e-26 1.000000e+00
#> 828        7 0.0006447453 2.395383e-26 1.000000e+00
#> 829        7 0.0006447453 2.395383e-26 1.000000e+00
#> 830        7 0.0006447453 2.395383e-26 1.000000e+00
#> 831        7 0.0006447453 2.395383e-26 1.000000e+00
#> 832        7 0.0006447453 2.395383e-26 1.000000e+00
#> 833        7 0.0006447453 2.395383e-26 1.000000e+00
#> 834        7 0.0006447453 2.395383e-26 1.000000e+00
#> 835        7 0.0006447453 2.395383e-26 1.000000e+00
#> 836        7 0.0006447453 2.395383e-26 1.000000e+00
#> 837       58 0.0049102608 2.183860e-26 1.000000e+00
#> 838       19 0.0017500230 1.666634e-26 1.000000e+00
#> 839       16 0.0014737036 1.364595e-26 1.000000e+00
#> 840       11 0.0010131712 8.261665e-27 1.000000e+00
#> 841       24 0.0024077047 6.564453e-27 1.000000e+00
#> 842        8 0.0008025682 6.223453e-27 1.000000e+00
#> 843        8 0.0008025682 6.223453e-27 1.000000e+00
#> 844        8 0.0008025682 6.223453e-27 1.000000e+00
#> 845        8 0.0008025682 6.223453e-27 1.000000e+00
#> 846       36 0.0030477481 9.215813e-28 1.000000e+00
#> 847       14 0.0012894907 7.212564e-28 1.000000e+00
#> 848       14 0.0012894907 7.212564e-28 1.000000e+00
#> 849        8 0.0008025682 1.588303e-28 1.000000e+00
#> 850        7 0.0006447453 1.503294e-28 1.000000e+00
#> 851        7 0.0006447453 1.503294e-28 1.000000e+00
#> 852        7 0.0006447453 1.503294e-28 1.000000e+00
#> 853       13 0.0013041734 1.463781e-28 1.000000e+00
#> 854        7 0.0005926177 1.403269e-28 1.000000e+00
#> 855        7 0.0005926177 1.403269e-28 1.000000e+00
#> 856       10 0.0009210648 1.294833e-28 1.000000e+00
#> 857        8 0.0006772773 1.260015e-28 1.000000e+00
#> 858       21 0.0017778530 1.038377e-28 1.000000e+00
#> 859        8 0.0007368518 1.029604e-28 1.000000e+00
#> 860        8 0.0007368518 1.029604e-28 1.000000e+00
#> 861        8 0.0007368518 1.029604e-28 1.000000e+00
#> 862       30 0.0030096308 9.862472e-29 1.000000e+00
#> 863        7 0.0007022472 8.934208e-29 1.000000e+00
#> 864        7 0.0007022472 8.934208e-29 1.000000e+00
#> 865        7 0.0007022472 8.934208e-29 1.000000e+00
#> 866        6 0.0006019262 7.894263e-29 1.000000e+00
#> 867        6 0.0006019262 7.894263e-29 1.000000e+00
#> 868        6 0.0006019262 7.894263e-29 1.000000e+00
#> 869        6 0.0006019262 7.894263e-29 1.000000e+00
#> 870        5 0.0005016051 7.322728e-29 1.000000e+00
#> 871        5 0.0005016051 7.322728e-29 1.000000e+00
#> 872        5 0.0005016051 7.322728e-29 1.000000e+00
#> 873        5 0.0005016051 7.322728e-29 1.000000e+00
#> 874        5 0.0005016051 7.322728e-29 1.000000e+00
#> 875        5 0.0005016051 7.322728e-29 1.000000e+00
#> 876        5 0.0005016051 7.322728e-29 1.000000e+00
#> 877        5 0.0005016051 7.322728e-29 1.000000e+00
#> 878       86 0.0072807315 6.645610e-29 1.000000e+00
#> 879        8 0.0006772773 5.254585e-29 1.000000e+00
#> 880        8 0.0006772773 5.254585e-29 1.000000e+00
#> 881        5 0.0004605324 4.691133e-29 1.000000e+00
#> 882        5 0.0004605324 4.691133e-29 1.000000e+00
#> 883        5 0.0004605324 4.691133e-29 1.000000e+00
#> 884        5 0.0004605324 4.691133e-29 1.000000e+00
#> 885       31 0.0028553007 2.981182e-29 1.000000e+00
#> 886       12 0.0012038523 2.828515e-29 1.000000e+00
#> 887       20 0.0020064205 2.855389e-29 1.000000e+00
#> 888        7 0.0007022472 2.253915e-29 1.000000e+00
#> 889        7 0.0007022472 2.253915e-29 1.000000e+00
#> 890        7 0.0007022472 2.253915e-29 1.000000e+00
#> 891        6 0.0005079580 1.661403e-29 1.000000e+00
#> 892        6 0.0005079580 1.661403e-29 1.000000e+00
#> 893        6 0.0005079580 1.661403e-29 1.000000e+00
#> 894        6 0.0005079580 1.661403e-29 1.000000e+00
#> 895       19 0.0016085337 1.112179e-29 1.000000e+00
#> 896       19 0.0016085337 1.112179e-29 1.000000e+00
#> 897       23 0.0021184489 1.111552e-29 1.000000e+00
#> 898        9 0.0008289583 7.585450e-30 1.000000e+00
#> 899        9 0.0008289583 7.585450e-30 1.000000e+00
#> 900       10 0.0010032103 7.410389e-30 1.000000e+00
#> 901       22 0.0018625127 3.784498e-30 1.000000e+00
#> 902       14 0.0012894907 3.025776e-30 1.000000e+00
#> 903        9 0.0009028892 3.479019e-30 1.000000e+00
#> 904        9 0.0009028892 3.479019e-30 1.000000e+00
#> 905        5 0.0004232983 3.910003e-31 1.000000e+00
#> 906        5 0.0004232983 3.910003e-31 1.000000e+00
#> 907        5 0.0004232983 3.910003e-31 1.000000e+00
#> 908       10 0.0008465967 0.000000e+00 1.000000e+00
#> 909       10 0.0008465967 0.000000e+00 1.000000e+00
```

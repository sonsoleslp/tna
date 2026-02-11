# Print Detailed Information on the Pruning Results

Print Detailed Information on the Pruning Results

## Usage

``` r
pruning_details(x, ...)

# S3 method for class 'tna'
pruning_details(x, ...)

# S3 method for class 'group_tna'
pruning_details(x, ...)
```

## Arguments

- x:

  A `tna` or `group_tna` object.

- ...:

  Ignored.

## Value

A `data.frame` containing the removed edges if `x` is a `tna` object, or
a `list` of `data.frame` objects in the case of `group_tna` object.

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
[`print.tna_bootstrap()`](http://sonsoles.me/tna/reference/print.tna_bootstrap.md),
[`print.tna_permutation()`](http://sonsoles.me/tna/reference/print.tna_permutation.md),
[`print.tna_reliability()`](http://sonsoles.me/tna/reference/print.tna_reliability.md),
[`print.tna_stability()`](http://sonsoles.me/tna/reference/print.tna_stability.md),
[`prune()`](http://sonsoles.me/tna/reference/prune.md),
[`reliability()`](http://sonsoles.me/tna/reference/reliability.md),
[`reprune()`](http://sonsoles.me/tna/reference/reprune.md),
[`summary.group_tna_bootstrap()`](http://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md),
[`summary.tna_bootstrap()`](http://sonsoles.me/tna/reference/summary.tna_bootstrap.md)

## Examples

``` r
model <- tna(group_regulation)
pruned_threshold <- prune(model, method = "threshold", threshold = 0.1)
pruning_details(pruned_threshold)
#> **Pruning Details**
#> 
#> Method used: User-specified threshold (0.1)
#> Number of removed edges: 49
#> Number of retained edges: 29
#> 
#> **Removed edges**
#> 
#>          from         to       weight
#> 1    cohesion      adapt 0.0029498525
#> 2   consensus      adapt 0.0047400853
#> 3  coregulate      adapt 0.0162436548
#> 4     discuss      adapt 0.0713743356
#> 5     emotion      adapt 0.0024673951
#> 6     monitor      adapt 0.0111653873
#> 7        plan      adapt 0.0009745006
#> 8    cohesion   cohesion 0.0271386431
#> 9   consensus   cohesion 0.0148522673
#> 10 coregulate   cohesion 0.0360406091
#> 11    discuss   cohesion 0.0475828904
#> 12    monitor   cohesion 0.0558269365
#> 13       plan   cohesion 0.0251745980
#> 14  synthesis   cohesion 0.0337423313
#> 15  consensus  consensus 0.0820034761
#> 16      adapt coregulate 0.0216110020
#> 17 coregulate coregulate 0.0233502538
#> 18    discuss coregulate 0.0842824601
#> 19    emotion coregulate 0.0341910469
#> 20    monitor coregulate 0.0579204466
#> 21       plan coregulate 0.0172161767
#> 22  synthesis coregulate 0.0444785276
#> 23      adapt    discuss 0.0589390963
#> 24   cohesion    discuss 0.0595870206
#> 25       plan    discuss 0.0678902063
#> 26  synthesis    discuss 0.0628834356
#> 27  consensus    emotion 0.0726813083
#> 28    emotion    emotion 0.0768417342
#> 29    monitor    emotion 0.0907187718
#> 30  synthesis    emotion 0.0705521472
#> 31      adapt    monitor 0.0333988212
#> 32   cohesion    monitor 0.0330383481
#> 33  consensus    monitor 0.0466108390
#> 34 coregulate    monitor 0.0862944162
#> 35    discuss    monitor 0.0222728423
#> 36    emotion    monitor 0.0363059570
#> 37    monitor    monitor 0.0181437544
#> 38       plan    monitor 0.0755237941
#> 39  synthesis    monitor 0.0122699387
#> 40      adapt       plan 0.0157170923
#> 41    discuss       plan 0.0116426221
#> 42    emotion       plan 0.0997532605
#> 43  synthesis       plan 0.0751533742
#> 44   cohesion  synthesis 0.0035398230
#> 45  consensus  synthesis 0.0075841365
#> 46 coregulate  synthesis 0.0187817259
#> 47    emotion  synthesis 0.0028198802
#> 48    monitor  synthesis 0.0160502442
#> 49       plan  synthesis 0.0017865844
```

# Summarize Split-Half Reliability Results

Returns every metric of a split-half reliability analysis as a tidy
table. [`print()`](https://rdrr.io/r/base/print.html) shows four of
them.

## Usage

``` r
# S3 method for class 'tna_reliability'
summary(object, ...)
```

## Arguments

- object:

  A `tna_reliability` object.

- ...:

  Ignored.

## Value

A `data.frame` with one row per metric (and per model type, in the
column `model_type`, when
[`reliability()`](https://sonsoles.me/tna/reference/reliability.md) was
run with several `types`). The columns are `category` (the kind of
metric: weight deviations, correlations, dissimilarities, similarities,
or pattern similarities), `metric`, and the `mean`, `sd`, `median`,
`min`, `max`, and quartiles (`q25`, `q75`) of the metric over the
splits.

## See also

Validation functions
[`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md),
[`deprune()`](https://sonsoles.me/tna/reference/deprune.md),
[`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
[`markov_order()`](https://sonsoles.me/tna/reference/markov_order.md),
[`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md),
[`permutation_test.group_tna()`](https://sonsoles.me/tna/reference/permutation_test.group_tna.md),
[`plot.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/plot.group_tna_bootstrap.md),
[`plot.group_tna_permutation()`](https://sonsoles.me/tna/reference/plot.group_tna_permutation.md),
[`plot.group_tna_stability()`](https://sonsoles.me/tna/reference/plot.group_tna_stability.md),
[`plot.tna_bootstrap()`](https://sonsoles.me/tna/reference/plot.tna_bootstrap.md),
[`plot.tna_markov_order()`](https://sonsoles.me/tna/reference/plot.tna_markov_order.md),
[`plot.tna_permutation()`](https://sonsoles.me/tna/reference/plot.tna_permutation.md),
[`plot.tna_reliability()`](https://sonsoles.me/tna/reference/plot.tna_reliability.md),
[`plot.tna_stability()`](https://sonsoles.me/tna/reference/plot.tna_stability.md),
[`print.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/print.group_tna_bootstrap.md),
[`print.group_tna_permutation()`](https://sonsoles.me/tna/reference/print.group_tna_permutation.md),
[`print.group_tna_stability()`](https://sonsoles.me/tna/reference/print.group_tna_stability.md),
[`print.summary.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/print.summary.group_tna_bootstrap.md),
[`print.summary.tna_bootstrap()`](https://sonsoles.me/tna/reference/print.summary.tna_bootstrap.md),
[`print.summary.tna_markov_order()`](https://sonsoles.me/tna/reference/print.summary.tna_markov_order.md),
[`print.tna_bootstrap()`](https://sonsoles.me/tna/reference/print.tna_bootstrap.md),
[`print.tna_markov_order()`](https://sonsoles.me/tna/reference/print.tna_markov_order.md),
[`print.tna_permutation()`](https://sonsoles.me/tna/reference/print.tna_permutation.md),
[`print.tna_reliability()`](https://sonsoles.me/tna/reference/print.tna_reliability.md),
[`print.tna_stability()`](https://sonsoles.me/tna/reference/print.tna_stability.md),
[`prune()`](https://sonsoles.me/tna/reference/prune.md),
[`pruning_details()`](https://sonsoles.me/tna/reference/pruning_details.md),
[`reliability()`](https://sonsoles.me/tna/reference/reliability.md),
[`reprune()`](https://sonsoles.me/tna/reference/reprune.md),
[`summary.group_tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md),
[`summary.tna_bootstrap()`](https://sonsoles.me/tna/reference/summary.tna_bootstrap.md),
[`summary.tna_markov_order()`](https://sonsoles.me/tna/reference/summary.tna_markov_order.md),
[`summary.tna_permutation()`](https://sonsoles.me/tna/reference/summary.tna_permutation.md),
[`summary.tna_stability()`](https://sonsoles.me/tna/reference/summary.tna_stability.md)

## Examples

``` r
# Small number of iterations for CRAN
rel <- reliability(tna(engagement), iter = 20)
summary(rel)
#>                category               metric        mean           sd
#> 1     Weight Deviations      Mean Abs. Diff. 0.006925683 0.0021740554
#> 2     Weight Deviations    Median Abs. Diff. 0.005815115 0.0018860179
#> 3     Weight Deviations            RMS Diff. 0.008791284 0.0031033005
#> 4     Weight Deviations       Max Abs. Diff. 0.017236643 0.0071525632
#> 5     Weight Deviations Rel. Mean Abs. Diff. 0.020777048 0.0065221663
#> 6     Weight Deviations             CV Ratio 0.998093752 0.0096255517
#> 7          Correlations              Pearson 0.999566764 0.0003043435
#> 8          Correlations             Spearman 0.991666667 0.0085498196
#> 9          Correlations              Kendall 0.972222222 0.0284993987
#> 10         Correlations             Distance 0.999478559 0.0004160147
#> 11      Dissimilarities            Euclidean 0.026373851 0.0093099014
#> 12      Dissimilarities            Manhattan 0.062331143 0.0195664990
#> 13      Dissimilarities             Canberra 0.175987485 0.0550973223
#> 14      Dissimilarities          Bray-Curtis 0.010388524 0.0032610832
#> 15      Dissimilarities            Frobenius 0.021534159 0.0076015027
#> 16         Similarities               Cosine 0.999794528 0.0001392238
#> 17         Similarities              Jaccard 0.979456170 0.0063905339
#> 18         Similarities                 Dice 0.989611476 0.0032610832
#> 19         Similarities              Overlap 0.989611476 0.0032610832
#> 20         Similarities                   RV 0.999457702 0.0005285213
#> 21 Pattern Similarities       Rank Agreement 1.000000000 0.0000000000
#> 22 Pattern Similarities       Sign Agreement 1.000000000 0.0000000000
#>         median         min         max         q25         q75
#> 1  0.007148844 0.002720523 0.011385118 0.005754553 0.008057217
#> 2  0.006185627 0.001977020 0.008887887 0.004316660 0.007231775
#> 3  0.008633584 0.003464574 0.014676759 0.006831449 0.011164687
#> 4  0.016120906 0.006355130 0.033416235 0.013566060 0.020153482
#> 5  0.021446533 0.008161569 0.034155355 0.017263660 0.024171650
#> 6  1.000661979 0.980468590 1.012948225 0.990946456 1.004457450
#> 7  0.999646713 0.998846112 0.999933464 0.999455650 0.999748173
#> 8  0.991666667 0.983333333 1.000000000 0.983333333 1.000000000
#> 9  0.972222222 0.944444444 1.000000000 0.944444444 1.000000000
#> 10 0.999624279 0.998516358 0.999939595 0.999342316 0.999757301
#> 11 0.025900752 0.010393723 0.044030277 0.020494346 0.033494062
#> 12 0.064339600 0.024484708 0.102466065 0.051790981 0.072514949
#> 13 0.168682993 0.105256586 0.310162421 0.135607814 0.196924406
#> 14 0.010723267 0.004080785 0.017077678 0.008631830 0.012085825
#> 15 0.021147876 0.008486439 0.035950570 0.016733563 0.027347787
#> 16 0.999830880 0.999473056 0.999970187 0.999719760 0.999885465
#> 17 0.978781008 0.966418145 0.991871601 0.976117446 0.982884233
#> 18 0.989276733 0.982922322 0.995919215 0.987914175 0.991368170
#> 19 0.989276733 0.982922322 0.995919215 0.987914175 0.991368170
#> 20 0.999599658 0.997832151 0.999999319 0.999318927 0.999800060
#> 21 1.000000000 1.000000000 1.000000000 1.000000000 1.000000000
#> 22 1.000000000 1.000000000 1.000000000 1.000000000 1.000000000
```

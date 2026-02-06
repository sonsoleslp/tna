# Compare Sequences Between Groups

Performs comprehensive sequence comparison analysis between groups. All
patterns of the sequences (subsequences of specific length) are
extracted from all sequences in each group. For two groups, the
proportions of the patterns between the groups are compared with a
z-test for proportions. For more than two groups, the Chi-squared test
is applied. Both tests are applied within patterns of the same length.

## Usage

``` r
compare_sequences(x, ...)

# Default S3 method
compare_sequences(x, group, sub, min_freq = 5L, correction = "bonferroni", ...)

# S3 method for class 'group_tna'
compare_sequences(x, sub, min_freq = 5L, correction = "bonferroni", ...)
```

## Arguments

- x:

  A `group_tna` object or a `data.frame` containing sequence data in
  wide format.

- ...:

  Not used.

- group:

  A `vector` indicating the group assignment of each row of the
  data/sequence. Must have the same length as the number of
  rows/sequences of `x`. Alternatively, a single `character` string
  giving the column name of the data that defines the group when `x` is
  a wide format `data.frame` or a `tna_data` object.

- sub:

  An `integer` vector of subsequence lengths to analyze. The default is
  `2:5`.

- min_freq:

  An `integer` giving the minimum number of times that a specific
  pattern has to be observed to be included in the analysis. The default
  is `5`.

- correction:

  A `character` string naming the multiple comparison correction method
  (default: `"bonferroni"`). Supports all
  [stats::p.adjust](https://rdrr.io/r/stats/p.adjust.html) methods:
  `"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`,
  `"fdr"`, `"none"`.

## Value

A `tna_sequence_comparison` object, which is a `data.frame` with columns
giving the names of the pattern, pattern frequencies, pattern
proportions (within patterns of the same length), test statistic values
and p-values of the tests.

## See also

Model comparison functions
[`compare()`](http://sonsoles.me/tna/reference/compare.md),
[`compare.group_tna()`](http://sonsoles.me/tna/reference/compare.group_tna.md),
[`plot.tna_comparison()`](http://sonsoles.me/tna/reference/plot.tna_comparison.md),
[`plot.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md),
[`plot_compare.group_tna()`](http://sonsoles.me/tna/reference/plot_compare.group_tna.md),
[`print.tna_comparison()`](http://sonsoles.me/tna/reference/print.tna_comparison.md),
[`print.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)

## Examples

``` r
group <- c(rep("High", 1000), rep("Low", 1000))
comp <- compare_sequences(group_regulation, group)
```

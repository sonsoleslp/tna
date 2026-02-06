# Print a TNA Summary

Print a TNA Summary

## Usage

``` r
# S3 method for class 'summary.tna'
print(x, ...)
```

## Arguments

- x:

  A `summary.tna` object.

- ...:

  Ignored.

## Value

A `summary.tna` object (invisibly) containing the TNA model network
metrics and values.

## See also

Basic functions
[`build_model()`](http://sonsoles.me/tna/reference/build_model.md),
[`hist.group_tna()`](http://sonsoles.me/tna/reference/hist.group_tna.md),
[`hist.tna()`](http://sonsoles.me/tna/reference/hist.tna.md),
[`plot.group_tna()`](http://sonsoles.me/tna/reference/plot.group_tna.md),
[`plot.tna()`](http://sonsoles.me/tna/reference/plot.tna.md),
[`plot_frequencies()`](http://sonsoles.me/tna/reference/plot_frequencies.md),
[`plot_frequencies.group_tna()`](http://sonsoles.me/tna/reference/plot_frequencies.group_tna.md),
[`plot_mosaic()`](http://sonsoles.me/tna/reference/plot_mosaic.md),
[`plot_mosaic.group_tna()`](http://sonsoles.me/tna/reference/plot_mosaic.group_tna.md),
[`plot_mosaic.tna_data()`](http://sonsoles.me/tna/reference/plot_mosaic.tna_data.md),
[`print.group_tna()`](http://sonsoles.me/tna/reference/print.group_tna.md),
[`print.summary.group_tna()`](http://sonsoles.me/tna/reference/print.summary.group_tna.md),
[`print.tna()`](http://sonsoles.me/tna/reference/print.tna.md),
[`summary.group_tna()`](http://sonsoles.me/tna/reference/summary.group_tna.md),
[`summary.tna()`](http://sonsoles.me/tna/reference/summary.tna.md),
[`tna-package`](http://sonsoles.me/tna/reference/tna-package.md)

## Examples

``` r
model <- tna(group_regulation)
print(summary(model))
#> # A tibble: 13 × 2
#>    metric                         value
#>  * <chr>                          <dbl>
#>  1 Node Count                  9   e+ 0
#>  2 Edge Count                  7.8 e+ 1
#>  3 Network Density             1   e+ 0
#>  4 Mean Distance               4.72e- 2
#>  5 Mean Out-Strength           1   e+ 0
#>  6 SD Out-Strength             8.07e- 1
#>  7 Mean In-Strength            1   e+ 0
#>  8 SD In-Strength              6.80e-17
#>  9 Mean Out-Degree             8.67e+ 0
#> 10 SD Out-Degree               7.07e- 1
#> 11 Centralization (Out-Degree) 1.56e- 2
#> 12 Centralization (In-Degree)  1.56e- 2
#> 13 Reciprocity                 9.86e- 1
```

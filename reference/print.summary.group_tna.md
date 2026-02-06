# Print a Summary of a Grouped Transition Network Analysis Model

Print a Summary of a Grouped Transition Network Analysis Model

## Usage

``` r
# S3 method for class 'summary.group_tna'
print(x, ...)
```

## Arguments

- x:

  A `summary.group_tna` object.

- ...:

  Arguments passed to the `tibble` print method

## Value

`x` (invisibly).

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
[`print.summary.tna()`](http://sonsoles.me/tna/reference/print.summary.tna.md),
[`print.tna()`](http://sonsoles.me/tna/reference/print.tna.md),
[`summary.group_tna()`](http://sonsoles.me/tna/reference/summary.group_tna.md),
[`summary.tna()`](http://sonsoles.me/tna/reference/summary.tna.md),
[`tna-package`](http://sonsoles.me/tna/reference/tna-package.md)

## Examples

``` r
model <- group_model(engagement_mmm)
print(summary(model))
#> # A tibble: 13 × 4
#>    metric                      `Cluster 1` `Cluster 2` `Cluster 3`
#>  * <chr>                             <dbl>       <dbl>       <dbl>
#>  1 Node Count                        3           3           3    
#>  2 Edge Count                        9           8           8    
#>  3 Network Density                   1           1           1    
#>  4 Mean Distance                     0.111       0.239       0.302
#>  5 Mean Out-Strength                 1           1           1    
#>  6 SD Out-Strength                   0.214       0.353       0.472
#>  7 Mean In-Strength                  1           1           1    
#>  8 SD In-Strength                    0           0           0    
#>  9 Mean Out-Degree                   3           2.67        2.67 
#> 10 SD Out-Degree                     0           0.577       0.577
#> 11 Centralization (Out-Degree)       0           0.25        0.25 
#> 12 Centralization (In-Degree)        0           0.25        0.25 
#> 13 Reciprocity                       1           0.8         0.8  
```

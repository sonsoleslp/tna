# Print a `group_tna` Object

Print a `group_tna` Object

## Usage

``` r
# S3 method for class 'group_tna'
print(x, ...)
```

## Arguments

- x:

  A `group_tna` object.

- ...:

  Arguments passed to
  [`print.tna()`](http://sonsoles.me/tna/reference/print.tna.md).

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
[`print.summary.group_tna()`](http://sonsoles.me/tna/reference/print.summary.group_tna.md),
[`print.summary.tna()`](http://sonsoles.me/tna/reference/print.summary.tna.md),
[`print.tna()`](http://sonsoles.me/tna/reference/print.tna.md),
[`summary.group_tna()`](http://sonsoles.me/tna/reference/summary.group_tna.md),
[`summary.tna()`](http://sonsoles.me/tna/reference/summary.tna.md),
[`tna-package`](http://sonsoles.me/tna/reference/tna-package.md)

## Examples

``` r
model <- group_model(engagement_mmm)
print(model)
#> Cluster 1 :
#> State Labels : 
#> 
#>    Active, Average, Disengaged 
#> 
#> Transition Probability Matrix :
#> 
#>                Active    Average Disengaged
#> Active     0.85985688 0.08919748 0.05094565
#> Average    0.31210322 0.54208478 0.14581200
#> Disengaged 0.04791061 0.16179397 0.79029542
#> 
#> Initial Probabilities : 
#> 
#>     Active    Average Disengaged 
#>  0.3397762  0.3234995  0.3367243 
#> 
#> Cluster 2 :
#> State Labels : 
#> 
#>    Active, Average, Disengaged 
#> 
#> Transition Probability Matrix :
#> 
#>                Active   Average Disengaged
#> Active     0.84090909 0.1590909  0.0000000
#> Average    0.09259259 0.6296296  0.2777778
#> Disengaged 0.15555556 0.5111111  0.3333333
#> 
#> Initial Probabilities : 
#> 
#>     Active    Average Disengaged 
#> 0.75000000 0.08333333 0.16666667 
#> 
#> Cluster 3 :
#> State Labels : 
#> 
#>    Active, Average, Disengaged 
#> 
#> Transition Probability Matrix :
#> 
#>               Active   Average Disengaged
#> Active     0.5833333 0.1250000 0.29166667
#> Average    0.1527778 0.8194444 0.02777778
#> Disengaged 0.0000000 0.6000000 0.40000000
#> 
#> Initial Probabilities : 
#> 
#>     Active    Average Disengaged 
#>          0          0          1 
#> 
```

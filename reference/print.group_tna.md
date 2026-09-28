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
  [`print.tna()`](https://sonsoles.me/tna/reference/print.tna.md).

## Value

`x` (invisibly).

## See also

Basic functions
[`build_model()`](https://sonsoles.me/tna/reference/build_model.md),
[`hist.group_tna()`](https://sonsoles.me/tna/reference/hist.group_tna.md),
[`hist.tna()`](https://sonsoles.me/tna/reference/hist.tna.md),
[`plot.group_tna()`](https://sonsoles.me/tna/reference/plot.group_tna.md),
[`plot.tna()`](https://sonsoles.me/tna/reference/plot.tna.md),
[`plot_frequencies()`](https://sonsoles.me/tna/reference/plot_frequencies.md),
[`plot_frequencies.group_tna()`](https://sonsoles.me/tna/reference/plot_frequencies.group_tna.md),
[`plot_mosaic()`](https://sonsoles.me/tna/reference/plot_mosaic.md),
[`plot_mosaic.group_tna()`](https://sonsoles.me/tna/reference/plot_mosaic.group_tna.md),
[`plot_mosaic.tna_data()`](https://sonsoles.me/tna/reference/plot_mosaic.tna_data.md),
[`print.summary.group_tna()`](https://sonsoles.me/tna/reference/print.summary.group_tna.md),
[`print.summary.tna()`](https://sonsoles.me/tna/reference/print.summary.tna.md),
[`print.tna()`](https://sonsoles.me/tna/reference/print.tna.md),
[`summary.group_tna()`](https://sonsoles.me/tna/reference/summary.group_tna.md),
[`summary.tna()`](https://sonsoles.me/tna/reference/summary.tna.md),
[`tna-package`](https://sonsoles.me/tna/reference/tna-package.md)

## Examples

``` r
model <- group_model(engagement_mmm)
print(model)
#> Cluster 1 :
#> State Labels : 
#> 
#>    Disengaged, Engaged, Moderate 
#> 
#> Transition Probability Matrix :
#> 
#>            Disengaged   Engaged  Moderate
#> Disengaged 0.68079470 0.1582781 0.1609272
#> Engaged    0.02027027 0.6616424 0.3180873
#> Moderate   0.12065157 0.1242408 0.7551077
#> 
#> Initial Probabilities : 
#> 
#> Disengaged    Engaged   Moderate 
#>  0.2210884  0.1802721  0.5986395 
#> 
#> Cluster 2 :
#> State Labels : 
#> 
#>    Disengaged, Engaged, Moderate 
#> 
#> Transition Probability Matrix :
#> 
#>            Disengaged    Engaged    Moderate
#> Disengaged  0.9112066 0.08771930 0.001074114
#> Engaged     0.3083110 0.67158177 0.020107239
#> Moderate    0.1182994 0.05360444 0.828096118
#> 
#> Initial Probabilities : 
#> 
#> Disengaged    Engaged   Moderate 
#> 0.49411765 0.01176471 0.49411765 
#> 
#> Cluster 3 :
#> State Labels : 
#> 
#>    Disengaged, Engaged, Moderate 
#> 
#> Transition Probability Matrix :
#> 
#>            Disengaged   Engaged   Moderate
#> Disengaged 0.87538657 0.1111515 0.01346189
#> Engaged    0.06246451 0.6652470 0.27228847
#> Moderate   0.18153446 0.1498049 0.66866060
#> 
#> Initial Probabilities : 
#> 
#> Disengaged    Engaged   Moderate 
#> 0.07462687 0.24626866 0.67910448 
#> 
```

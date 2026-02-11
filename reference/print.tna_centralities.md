# Print Centrality Measures

Print Centrality Measures

## Usage

``` r
# S3 method for class 'tna_centralities'
print(x, ...)
```

## Arguments

- x:

  A `centralities` object.

- ...:

  Ignored.

## Value

`x` (invisibly).

## See also

Centrality measure functions
[`betweenness_network()`](http://sonsoles.me/tna/reference/betweenness_network.md),
[`centralities()`](http://sonsoles.me/tna/reference/centralities.md),
[`plot.group_tna_centralities()`](http://sonsoles.me/tna/reference/plot.group_tna_centralities.md),
[`plot.tna_centralities()`](http://sonsoles.me/tna/reference/plot.tna_centralities.md),
[`print.group_tna_centralities()`](http://sonsoles.me/tna/reference/print.group_tna_centralities.md)

## Examples

``` r
model <- tna(group_regulation)
cm <- centralities(model)
print(cm)
#> # A tibble: 9 × 10
#>   state    OutStrength InStrength ClosenessIn ClosenessOut Closeness Betweenness
#> * <fct>          <dbl>      <dbl>       <dbl>        <dbl>     <dbl>       <dbl>
#> 1 adapt          1          0.345     0.00834       0.0152    0.0248           1
#> 2 cohesion       0.973      0.812     0.0138        0.0124    0.0265           0
#> 3 consens…       0.918      2.67      0.0351        0.0125    0.0383          30
#> 4 coregul…       0.977      0.567     0.0155        0.0150    0.0210           0
#> 5 discuss        0.805      1.19      0.0196        0.0131    0.0271          16
#> 6 emotion        0.923      0.894     0.0141        0.0121    0.0231           5
#> 7 monitor        0.982      0.346     0.00758       0.0137    0.0193           0
#> 8 plan           0.626      1.19      0.0274        0.0115    0.0274           9
#> 9 synthes…       1          0.192     0.00997       0.0158    0.0243           7
#> # ℹ 3 more variables: BetweennessRSP <dbl>, Diffusion <dbl>, Clustering <dbl>
```

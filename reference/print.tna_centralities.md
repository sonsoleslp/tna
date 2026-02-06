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
#> 1 adapt          1          0.345      13.4           2.33     18.5           17
#> 2 cohesion       0.973      0.812       3.65          2.79     13.8            0
#> 3 consens…       0.918      2.67        0.798         4.34     11.5            0
#> 4 coregul…       0.977      0.567       4.55          2.31      5.97           5
#> 5 discuss        0.805      1.19        1.95          2.68      7.31           0
#> 6 emotion        0.923      0.894       1.57          3.13     14.5            0
#> 7 monitor        0.982      0.346       6.24          2.21      7.76           3
#> 8 plan           0.626      1.19        5.47          2.91     17.6           10
#> 9 synthes…       1          0.192      12.3           2.18     15.9           14
#> # ℹ 3 more variables: BetweennessRSP <dbl>, Diffusion <dbl>, Clustering <dbl>
```

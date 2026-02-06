# Print Centrality Measures

Print Centrality Measures

## Usage

``` r
# S3 method for class 'group_tna_centralities'
print(x, ...)
```

## Arguments

- x:

  A `group_tna_centralities` object.

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
[`print.tna_centralities()`](http://sonsoles.me/tna/reference/print.tna_centralities.md)

## Examples

``` r
model <- group_model(engagement_mmm)
cm <- centralities(model)
print(cm)
#> # A tibble: 9 × 11
#>   group     state      OutStrength InStrength ClosenessIn ClosenessOut Closeness
#> * <chr>     <fct>            <dbl>      <dbl>       <dbl>        <dbl>     <dbl>
#> 1 Cluster 1 Active           0.140      0.360        4.14        7.14       7.29
#> 2 Cluster 1 Average          0.458      0.251        4.42        2.95       4.42
#> 3 Cluster 1 Disengaged       0.210      0.197        5.08        5.40       5.40
#> 4 Cluster 2 Active           0.159      0.248        4.03        1.68       4.03
#> 5 Cluster 2 Average          0.370      0.670        2.11        2.7        2.93
#> 6 Cluster 2 Disengaged       0.667      0.278        1.40        2.13       2.48
#> 7 Cluster 3 Active           0.417      0.153        1.10        3.6        3.6 
#> 8 Cluster 3 Average          0.181      0.725        1.38        5.54       6.55
#> 9 Cluster 3 Disengaged       0.6        0.319        5.54        0.739      5.54
#> # ℹ 4 more variables: Betweenness <dbl>, BetweennessRSP <dbl>, Diffusion <dbl>,
#> #   Clustering <dbl>
```

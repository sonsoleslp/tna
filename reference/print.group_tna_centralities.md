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
[`betweenness_network()`](https://sonsoles.me/tna/reference/betweenness_network.md),
[`centralities()`](https://sonsoles.me/tna/reference/centralities.md),
[`plot.group_tna_centralities()`](https://sonsoles.me/tna/reference/plot.group_tna_centralities.md),
[`plot.tna_centralities()`](https://sonsoles.me/tna/reference/plot.tna_centralities.md),
[`print.tna_centralities()`](https://sonsoles.me/tna/reference/print.tna_centralities.md)

## Examples

``` r
model <- group_model(engagement_mmm)
cm <- centralities(model)
print(cm)
#> # A tibble: 9 × 12
#>   group     state      OutStrength InStrength ClosenessIn ClosenessOut Closeness
#> * <chr>     <fct>            <dbl>      <dbl>       <dbl>        <dbl>     <dbl>
#> 1 Cluster 1 Disengaged      0.319      0.141      0.0507        0.0798    0.0798
#> 2 Cluster 1 Engaged         0.338      0.283      0.0696        0.0686    0.106 
#> 3 Cluster 1 Moderate        0.245      0.479      0.107         0.0612    0.107 
#> 4 Cluster 2 Disengaged      0.0888     0.427      0.0855        0.0138    0.0855
#> 5 Cluster 2 Engaged         0.328      0.141      0.0333        0.0189    0.0669
#> 6 Cluster 2 Moderate        0.172      0.0212     0.00902       0.0369    0.0496
#> 7 Cluster 3 Disengaged      0.125      0.244      0.0681        0.0462    0.0689
#> 8 Cluster 3 Engaged         0.335      0.261      0.0638        0.0778    0.0789
#> 9 Cluster 3 Moderate        0.331      0.286      0.0612        0.0821    0.109 
#> # ℹ 5 more variables: Betweenness <dbl>, BetweennessRSP <dbl>, Diffusion <dbl>,
#> #   Clustering <dbl>, PageRank <dbl>
```

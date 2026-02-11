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
#> 1 Cluster 1 Active           0.140      0.360      0.0794       0.0342    0.0794
#> 2 Cluster 1 Average          0.458      0.251      0.0575       0.0994    0.107 
#> 3 Cluster 1 Disengaged       0.210      0.197      0.0401       0.0642    0.0642
#> 4 Cluster 2 Active           0.159      0.248      0.0608       0.0618    0.0787
#> 5 Cluster 2 Average          0.370      0.670      0.121        0.0734    0.121 
#> 6 Cluster 2 Disengaged       0.667      0.278      0.0742       0.119     0.119 
#> 7 Cluster 3 Active           0.417      0.153      0.0678       0.117     0.117 
#> 8 Cluster 3 Average          0.181      0.725      0.148        0.0605    0.148 
#> 9 Cluster 3 Disengaged       0.6        0.319      0.0746       0.101     0.196 
#> # ℹ 4 more variables: Betweenness <dbl>, BetweennessRSP <dbl>, Diffusion <dbl>,
#> #   Clustering <dbl>
```

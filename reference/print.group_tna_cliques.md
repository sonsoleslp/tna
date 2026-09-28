# Print Found Cliques

Print Found Cliques

## Usage

``` r
# S3 method for class 'group_tna_cliques'
print(x, ...)
```

## Arguments

- x:

  A `group_tna_cliques` object.

- ...:

  Arguments passed to
  [`print.tna_cliques()`](https://sonsoles.me/tna/reference/print.tna_cliques.md).

## Value

`x` (invisibly).

## See also

Clique-related functions
[`cliques()`](https://sonsoles.me/tna/reference/cliques.md),
[`plot.group_tna_cliques()`](https://sonsoles.me/tna/reference/plot.group_tna_cliques.md),
[`plot.tna_cliques()`](https://sonsoles.me/tna/reference/plot.tna_cliques.md),
[`print.tna_cliques()`](https://sonsoles.me/tna/reference/print.tna_cliques.md)

## Examples

``` r
model <- group_model(engagement_mmm)
cliq <- cliques(model, size = 2)
print(cliq)
#> Cluster 1 :
#> Number of 2-cliques = 3 (weight threshold = 0)
#> Showing 3 cliques starting from clique number 1
#> 
#> Clique 1
#>            Engaged  Moderate
#> Engaged  0.6616424 0.3180873
#> Moderate 0.1242408 0.7551077
#> 
#> Clique 2
#>            Disengaged   Engaged
#> Disengaged 0.68079470 0.1582781
#> Engaged    0.02027027 0.6616424
#> 
#> Clique 3
#>            Disengaged  Moderate
#> Disengaged  0.6807947 0.1609272
#> Moderate    0.1206516 0.7551077
#> 
#> Cluster 2 :
#> Number of 2-cliques = 3 (weight threshold = 0)
#> Showing 3 cliques starting from clique number 1
#> 
#> Clique 1
#>             Engaged   Moderate
#> Engaged  0.67158177 0.02010724
#> Moderate 0.05360444 0.82809612
#> 
#> Clique 2
#>            Disengaged   Engaged
#> Disengaged  0.9112066 0.0877193
#> Engaged     0.3083110 0.6715818
#> 
#> Clique 3
#>            Disengaged    Moderate
#> Disengaged  0.9112066 0.001074114
#> Moderate    0.1182994 0.828096118
#> 
#> Cluster 3 :
#> Number of 2-cliques = 3 (weight threshold = 0)
#> Showing 3 cliques starting from clique number 1
#> 
#> Clique 1
#>            Engaged  Moderate
#> Engaged  0.6652470 0.2722885
#> Moderate 0.1498049 0.6686606
#> 
#> Clique 2
#>            Disengaged   Engaged
#> Disengaged 0.87538657 0.1111515
#> Engaged    0.06246451 0.6652470
#> 
#> Clique 3
#>            Disengaged   Moderate
#> Disengaged  0.8753866 0.01346189
#> Moderate    0.1815345 0.66866060
#> 
```

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
  [`print.tna_cliques()`](http://sonsoles.me/tna/reference/print.tna_cliques.md).

## Value

`x` (invisibly).

## See also

Clique-related functions
[`cliques()`](http://sonsoles.me/tna/reference/cliques.md),
[`plot.group_tna_cliques()`](http://sonsoles.me/tna/reference/plot.group_tna_cliques.md),
[`plot.tna_cliques()`](http://sonsoles.me/tna/reference/plot.tna_cliques.md),
[`print.tna_cliques()`](http://sonsoles.me/tna/reference/print.tna_cliques.md)

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
#>              Average Disengaged
#> Average    0.5420848  0.1458120
#> Disengaged 0.1617940  0.7902954
#> 
#> Clique 2
#>            Active    Average
#> Active  0.8598569 0.08919748
#> Average 0.3121032 0.54208478
#> 
#> Clique 3
#>                Active Disengaged
#> Active     0.85985688 0.05094565
#> Disengaged 0.04791061 0.79029542
#> 
#> Cluster 2 :
#> Number of 2-cliques = 2 (weight threshold = 0)
#> Showing 2 cliques starting from clique number 1
#> 
#> Clique 1
#>              Average Disengaged
#> Average    0.6296296  0.2777778
#> Disengaged 0.5111111  0.3333333
#> 
#> Clique 2
#>             Active   Average
#> Active  0.84090909 0.1590909
#> Average 0.09259259 0.6296296
#> 
#> Cluster 3 :
#> Number of 2-cliques = 2 (weight threshold = 0)
#> Showing 2 cliques starting from clique number 1
#> 
#> Clique 1
#>              Average Disengaged
#> Average    0.8194444 0.02777778
#> Disengaged 0.6000000 0.40000000
#> 
#> Clique 2
#>            Active   Average
#> Active  0.5833333 0.1250000
#> Average 0.1527778 0.8194444
#> 
```

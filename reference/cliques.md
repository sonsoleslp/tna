# Identify Cliques in a Transition Network

This function identifies cliques of a specified size in a transition
network. It searches for cliques, i.e., complete subgraphs where every
pair of nodes is connected, of size `n` in the transition matrix for the
specified cluster in the `tna` object.

## Usage

``` r
cliques(x, ...)

# S3 method for class 'tna'
cliques(x, size = 2, threshold = 0, sum_weights = FALSE, ...)

# S3 method for class 'group_tna'
cliques(x, size = 2, threshold = 0, sum_weights = FALSE, ...)
```

## Arguments

- x:

  A `tna` or a `group_tna` object.

- ...:

  Ignored.

- size:

  An `integer` specifying the size of the cliques to identify. Defaults
  to `2` (dyads).

- threshold:

  A `numeric` value that sets the minimum edge weight for an edge to be
  considered in the clique. Edges below this value are ignored. Defaults
  to `0`.

- sum_weights:

  A `logical` value specifying whether the sum of the weights should be
  above the `threshold` instead of individual weights of the directed
  edges. Defaults to `FALSE`.

## Value

A `tna_cliques` object which is a `list` of two elements:

- `weights` is a `matrix` of the edge weights in the clique.

- `inits` is a `numeric` vector of initial weights for the clique.

If `x` is a `group_tna` object, a `group_tna_cliques` object is returned
instead, which is a `list` or `tna_cliques` objects.

## See also

Clique-related functions
[`plot.group_tna_cliques()`](http://sonsoles.me/tna/reference/plot.group_tna_cliques.md),
[`plot.tna_cliques()`](http://sonsoles.me/tna/reference/plot.tna_cliques.md),
[`print.group_tna_cliques()`](http://sonsoles.me/tna/reference/print.group_tna_cliques.md),
[`print.tna_cliques()`](http://sonsoles.me/tna/reference/print.tna_cliques.md)

## Examples

``` r
model <- tna(group_regulation)

# Find  2-cliques (dyads)
cliq <- cliques(model, size = 2)

model <- group_tna(engagement_mmm)
cliques(model)
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

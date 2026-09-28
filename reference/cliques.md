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
[`plot.group_tna_cliques()`](https://sonsoles.me/tna/reference/plot.group_tna_cliques.md),
[`plot.tna_cliques()`](https://sonsoles.me/tna/reference/plot.tna_cliques.md),
[`print.group_tna_cliques()`](https://sonsoles.me/tna/reference/print.group_tna_cliques.md),
[`print.tna_cliques()`](https://sonsoles.me/tna/reference/print.tna_cliques.md)

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

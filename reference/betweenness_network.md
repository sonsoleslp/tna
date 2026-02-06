# Build and Visualize a Network with Edge Betweenness

This function builds a network from a transition matrix in a `tna`
object and computes edge betweenness for the network.

## Usage

``` r
betweenness_network(x, directed)

# S3 method for class 'tna'
betweenness_network(x, directed)
```

## Arguments

- x:

  A `tna` object.

- directed:

  A `logical` value. If `TRUE`, the network is considered directed.

## Value

A `tna` object where the edge weights are edge betweenness values.

## See also

Centrality measure functions
[`centralities()`](http://sonsoles.me/tna/reference/centralities.md),
[`plot.group_tna_centralities()`](http://sonsoles.me/tna/reference/plot.group_tna_centralities.md),
[`plot.tna_centralities()`](http://sonsoles.me/tna/reference/plot.tna_centralities.md),
[`print.group_tna_centralities()`](http://sonsoles.me/tna/reference/print.group_tna_centralities.md),
[`print.tna_centralities()`](http://sonsoles.me/tna/reference/print.tna_centralities.md)

## Examples

``` r
model <- tna(group_regulation)
betweenness_network(model)
#> State Labels : 
#> 
#>    adapt, cohesion, consensus, coregulate, discuss, emotion, monitor, plan, synthesis 
#> 
#> Edge Betweenness Matrix :
#> 
#>            adapt cohesion consensus coregulate discuss emotion monitor plan
#> adapt          0        0         1          1       0       0       0    3
#> cohesion       0        0         0          6       0       0       2    0
#> consensus      0        0         0          0       0       0       0    4
#> coregulate     8        1         0          0       0       0       0    0
#> discuss        6        0         1          0       0       0       0    0
#> emotion        0        0         0          0       1       0       0    0
#> monitor        0        0         0          0       7       0       0    0
#> plan          11        3         2          3       0       3       3    8
#> synthesis      4        4         3          0       5       6       3    0
#>            synthesis
#> adapt              0
#> cohesion           3
#> consensus          0
#> coregulate         1
#> discuss            1
#> emotion            7
#> monitor           10
#> plan               0
#> synthesis          0
#> 
#> Initial Probabilities : 
#> 
#>      adapt   cohesion  consensus coregulate    discuss    emotion    monitor 
#>     0.0115     0.0605     0.2140     0.0190     0.1755     0.1515     0.1440 
#>       plan  synthesis 
#>     0.2045     0.0195 
```

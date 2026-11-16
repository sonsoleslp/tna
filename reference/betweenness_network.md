# Build and Visualize a Network with Edge Betweenness

This function builds a network from a transition matrix in a `tna`
object and computes edge betweenness for the network.

## Usage

``` r
betweenness_network(x, directed = TRUE, invert = TRUE)

# S3 method for class 'tna'
betweenness_network(x, directed = TRUE, invert = TRUE)

# S3 method for class 'group_tna'
betweenness_network(x, directed = TRUE, invert = TRUE)
```

## Arguments

- x:

  A `tna` or a `group_tna` object.

- directed:

  A `logical` value. If `TRUE`, the network is considered directed.

- invert:

  A `logical` value indicating whether the weights should be inverted
  for distance-based measures. The default is `TRUE`.

## Value

A `tna` object where the edge weights are edge betweenness values. For a
`group_tna` object, a `group_tna` object of such networks, one per
group.

## Details

The returned model remembers the model it was built from (its type and
scaling) and the values of `directed` and `invert`. The resampling
methods
[`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md),
[`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md), and
[`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md)
use this information: every resample rebuilds the original transition
network from the resampled sequences and then recomputes edge
betweenness. For example,
[`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)
applied to two edge betweenness networks tests the differences in edge
betweenness between the two groups. As for any network of a grouped
model with scaled edge weights, the edge betweenness networks of such a
model are not resampled.

## See also

Centrality measure functions
[`centralities()`](https://sonsoles.me/tna/reference/centralities.md),
[`plot.group_tna_centralities()`](https://sonsoles.me/tna/reference/plot.group_tna_centralities.md),
[`plot.tna_centralities()`](https://sonsoles.me/tna/reference/plot.tna_centralities.md),
[`print.group_tna_centralities()`](https://sonsoles.me/tna/reference/print.group_tna_centralities.md),
[`print.tna_centralities()`](https://sonsoles.me/tna/reference/print.tna_centralities.md)

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
#> adapt          0        2         6          0       0       1       0    0
#> cohesion       0        0         7          0       0       1       0    0
#> consensus      0        0         0          8      15       0       0   15
#> coregulate     0        0         0          0       4       2       1    1
#> discuss        0        0         7          0       0       2       0    0
#> emotion        0        6         7          0       0       0       0    0
#> monitor        0        0         0          0       5       2       0    1
#> plan           0        0         5          0       0       5       7    0
#> synthesis      9        0         6          0       0       0       0    0
#>            synthesis
#> adapt              0
#> cohesion           0
#> consensus          0
#> coregulate         0
#> discuss           15
#> emotion            0
#> monitor            0
#> plan               0
#> synthesis          0
#> 
#> Initial Probabilities : 
#> 
#>      adapt   cohesion  consensus coregulate    discuss    emotion    monitor 
#>     0.0115     0.0605     0.2140     0.0190     0.1755     0.1515     0.1440 
#>       plan  synthesis 
#>     0.2045     0.0195 

# Compare edge betweenness between groups
models <- group_tna(
  group_regulation,
  group = rep(c("first", "second"), each = 1000)
)
# Small number of iterations for CRAN
permutation_test(betweenness_network(models), iter = 20)
#> first vs. second :
#> # A tibble: 81 × 4
#>    edge_name           diff_true effect_size p_value
#>    <chr>                   <dbl>       <dbl>   <dbl>
#>  1 adapt -> adapt              0        NA    1     
#>  2 cohesion -> adapt           0        NA    1     
#>  3 consensus -> adapt          0        NA    1     
#>  4 coregulate -> adapt         0        NA    1     
#>  5 discuss -> adapt           -9        NA    0.0476
#>  6 emotion -> adapt            0        NA    1     
#>  7 monitor -> adapt            0        NA    1     
#>  8 plan -> adapt               0        NA    1     
#>  9 synthesis -> adapt          6        11.8  0.0476
#> 10 adapt -> cohesion          -2        NA    0.0476
#> # ℹ 71 more rows
#> 
```

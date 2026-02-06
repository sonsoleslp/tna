# Calculate Centrality Measures for a Transition Matrix

Calculates several centrality measures. See 'Details' for information
about the measures.

## Usage

``` r
centralities(x, loops = FALSE, normalize = FALSE, measures)

# S3 method for class 'tna'
centralities(x, loops = FALSE, normalize = FALSE, measures)

# S3 method for class 'matrix'
centralities(x, loops = FALSE, normalize = FALSE, measures)

# S3 method for class 'group_tna'
centralities(x, loops = FALSE, normalize = FALSE, measures)
```

## Arguments

- x:

  A `tna` object, a `group_tna` object, or a square `matrix`
  representing edge weights.

- loops:

  A `logical` value indicating whether to include loops in the network
  when computing the centrality measures (default is `FALSE`).

- normalize:

  A `logical` value indicating whether the centralities should be
  normalized (default is `FALSE`).

- measures:

  A `character` vector indicating which centrality measures should be
  computed. If missing, all available measures are returned. See
  'Details' for available measures. The elements are partially matched
  ignoring case.

## Value

A `tna_centralities` object which is a tibble (`tbl_df`). containing
centrality measures for each state.

## Details

The following measures are provided:

- `OutStrength`: Outgoing strength centrality, calculated using
  [`igraph::strength()`](https://r.igraph.org/reference/strength.html)
  with `mode = "out"`. It measures the total weight of the outgoing
  edges from each node.

- `InStrength`: Incoming strength centrality, calculated using
  [`igraph::strength()`](https://r.igraph.org/reference/strength.html)
  with `mode = "in"`. It measures the total weight of the incoming edges
  to each node.

- `ClosenessIn`: Closeness centrality (incoming), calculated using
  [`igraph::closeness()`](https://r.igraph.org/reference/closeness.html)
  with `mode = "in"`. It measures how close a node is to all other nodes
  based on the incoming paths.

- `ClosenessOut`: Closeness centrality (outgoing), calculated using
  [`igraph::closeness()`](https://r.igraph.org/reference/closeness.html)
  with `mode = "out"`. It measures how close a node is to all other
  nodes based on the outgoing paths.

- `Closeness`: Closeness centrality (overall), calculated using
  [`igraph::closeness()`](https://r.igraph.org/reference/closeness.html)
  with `mode = "all"`. It measures how close a node is to all other
  nodes based on both incoming and outgoing paths.

- `Betweenness`: Betweenness centrality defined by the number of
  geodesics calculated using
  [`igraph::betweenness()`](https://r.igraph.org/reference/betweenness.html).

- `BetweennessRSP`: Betweenness centrality based on randomized shortest
  paths (Kivimäki et al. 2016). It measures the extent to which a node
  lies on the shortest paths between other nodes.

- `Diffusion`: Diffusion centrality of Banerjee et.al. (2014). It
  measures the influence of a node in spreading information through the
  network.

- `Clustering`: Signed clustering coefficient of Zhang and
  Horvath (2005) based on the symmetric adjacency matrix (sum of the
  adjacency matrix and its transpose). It measures the degree to which
  nodes tend to cluster together.

## See also

Centrality measure functions
[`betweenness_network()`](http://sonsoles.me/tna/reference/betweenness_network.md),
[`plot.group_tna_centralities()`](http://sonsoles.me/tna/reference/plot.group_tna_centralities.md),
[`plot.tna_centralities()`](http://sonsoles.me/tna/reference/plot.tna_centralities.md),
[`print.group_tna_centralities()`](http://sonsoles.me/tna/reference/print.group_tna_centralities.md),
[`print.tna_centralities()`](http://sonsoles.me/tna/reference/print.tna_centralities.md)

## Examples

``` r
model <- tna(group_regulation)

# Centrality measures including loops in the network
centralities(model)
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

# Centrality measures excluding loops in the network
centralities(model, loops = FALSE)
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

# Centrality measures normalized
centralities(model, normalize = TRUE)
#> # A tibble: 9 × 10
#>   state    OutStrength InStrength ClosenessIn ClosenessOut Closeness Betweenness
#> * <fct>          <dbl>      <dbl>       <dbl>        <dbl>     <dbl>       <dbl>
#> 1 adapt          1         0.0618      1            0.0690     1           1    
#> 2 cohesion       0.927     0.250       0.226        0.281      0.623       0    
#> 3 consens…       0.781     1           0            1          0.438       0    
#> 4 coregul…       0.938     0.151       0.297        0.0578     0           0.294
#> 5 discuss        0.479     0.403       0.0917       0.230      0.106       0    
#> 6 emotion        0.795     0.284       0.0611       0.439      0.681       0    
#> 7 monitor        0.952     0.0623      0.432        0.0121     0.142       0.176
#> 8 plan           0         0.405       0.371        0.338      0.924       0.588
#> 9 synthes…       1         0           0.910        0          0.790       0.824
#> # ℹ 3 more variables: BetweennessRSP <dbl>, Diffusion <dbl>, Clustering <dbl>
```

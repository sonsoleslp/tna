# Calculate Centrality Measures for a Transition Matrix

Calculates several centrality measures. See 'Details' for information
about the measures.

## Usage

``` r
centralities(x, loops = FALSE, normalize = FALSE, invert = TRUE, measures)

# S3 method for class 'tna'
centralities(x, loops = FALSE, normalize = FALSE, invert = TRUE, measures)

# S3 method for class 'matrix'
centralities(x, loops = FALSE, normalize = FALSE, invert = TRUE, measures)

# S3 method for class 'group_tna'
centralities(x, loops = FALSE, normalize = FALSE, measures)
```

## Arguments

- x:

  A `tna` object, a `group_tna` object, or a square `matrix`
  representing edge weights.

- loops:

  A `logical` value indicating whether to include loops in the network
  when computing the centrality measures. The default is `FALSE`.

- normalize:

  A `logical` value indicating whether the centralities should be
  normalized. The default is `FALSE`.

- invert:

  A `logical` value indicating whether the weights should be inverted
  for distance-based measures. The default is `TRUE`.

- measures:

  A `character` vector indicating which centrality measures should be
  computed. If missing, all available measures are returned. See
  'Details' for the available measures.

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
#> 1 adapt          1          0.345     0.00834       0.0152    0.0248           1
#> 2 cohesion       0.973      0.812     0.0138        0.0124    0.0265           0
#> 3 consens…       0.918      2.67      0.0351        0.0125    0.0383          30
#> 4 coregul…       0.977      0.567     0.0155        0.0150    0.0210           0
#> 5 discuss        0.805      1.19      0.0196        0.0131    0.0271          16
#> 6 emotion        0.923      0.894     0.0141        0.0121    0.0231           5
#> 7 monitor        0.982      0.346     0.00758       0.0137    0.0193           0
#> 8 plan           0.626      1.19      0.0274        0.0115    0.0274           9
#> 9 synthes…       1          0.192     0.00997       0.0158    0.0243           7
#> # ℹ 3 more variables: BetweennessRSP <dbl>, Diffusion <dbl>, Clustering <dbl>

# Centrality measures excluding loops in the network
centralities(model, loops = FALSE)
#> # A tibble: 9 × 10
#>   state    OutStrength InStrength ClosenessIn ClosenessOut Closeness Betweenness
#> * <fct>          <dbl>      <dbl>       <dbl>        <dbl>     <dbl>       <dbl>
#> 1 adapt          1          0.345     0.00834       0.0152    0.0248           1
#> 2 cohesion       0.973      0.812     0.0138        0.0124    0.0265           0
#> 3 consens…       0.918      2.67      0.0351        0.0125    0.0383          30
#> 4 coregul…       0.977      0.567     0.0155        0.0150    0.0210           0
#> 5 discuss        0.805      1.19      0.0196        0.0131    0.0271          16
#> 6 emotion        0.923      0.894     0.0141        0.0121    0.0231           5
#> 7 monitor        0.982      0.346     0.00758       0.0137    0.0193           0
#> 8 plan           0.626      1.19      0.0274        0.0115    0.0274           9
#> 9 synthes…       1          0.192     0.00997       0.0158    0.0243           7
#> # ℹ 3 more variables: BetweennessRSP <dbl>, Diffusion <dbl>, Clustering <dbl>

# Centrality measures normalized
centralities(model, normalize = TRUE)
#> # A tibble: 9 × 10
#>   state    OutStrength InStrength ClosenessIn ClosenessOut Closeness Betweenness
#> * <fct>          <dbl>      <dbl>       <dbl>        <dbl>     <dbl>       <dbl>
#> 1 adapt          1         0.0618      0.0276        0.855    0.290       0.0333
#> 2 cohesion       0.927     0.250       0.226         0.205    0.382       0     
#> 3 consens…       0.781     1           1             0.240    1           1     
#> 4 coregul…       0.938     0.151       0.289         0.816    0.0928      0     
#> 5 discuss        0.479     0.403       0.436         0.365    0.412       0.533 
#> 6 emotion        0.795     0.284       0.237         0.128    0.202       0.167 
#> 7 monitor        0.952     0.0623      0             0.508    0           0     
#> 8 plan           0         0.405       0.721         0        0.428       0.3   
#> 9 synthes…       1         0           0.0867        1        0.265       0.233 
#> # ℹ 3 more variables: BetweennessRSP <dbl>, Diffusion <dbl>, Clustering <dbl>
```

# Community Detection for Transition Networks

This function detects communities within the transition networks
(represented by the `tna` object). It uses various algorithms to find
communities in the graph representation of transitions. For a
`group_tna` object, the communities are detected in the network of each
cluster. The function uses the `igraph` package to convert the
transition matrices into graphs and then applies community detection
algorithms (e.g., Walktrap, Fast Greedy, Label Propagation, Infomap,
Edge Betweenness, Leading Eigenvector, and Spin Glass).

## Usage

``` r
communities(x, methods, gamma)

# S3 method for class 'tna'
communities(x, methods, gamma = 1)

# S3 method for class 'group_tna'
communities(x, methods, gamma = 1)
```

## Arguments

- x:

  A `tna` or a `group_tna` object.

- methods:

  A `character` vector of community detection algorithms to apply to the
  network. The supported options are:

  - `"walktrap"`: A community detection method using short random walks.

  - `"fast_greedy"`: A method based on modularity optimization.

  - `"label_prop"`: A method that uses label propagation.

  - `"infomap"`: A method that uses information flow to detect
    communities.

  - `"edge_betweenness"`: A method that uses edge betweenness to find
    communities.

  - `"leading_eigen"`: A method using the leading eigenvector of the
    modularity matrix.

  - `"spinglass"`: A method based on the spinglass model.

  If not provided, all methods are applied.

- gamma:

  A `numeric` value depicting a parameter that affects the behavior of
  certain algorithms like the Spin Glass method. Defaults to `1`.

## Value

An object of class `tna_communities` which is a `list` with the
elements:

- `counts`: A named `integer` vector with the number of communities
  found by each algorithm.

- `assignments`: A `data.frame` with one row per state, the column
  `state`, and one column per algorithm giving the community (an
  `integer`) that the state is assigned to.

If `x` is a `group_tna` object, a `group_tna_communities` object is
returned instead, which is a `list` of `tna_communities` objects.

## See also

Community detection functions
[`plot.group_tna_communities()`](https://sonsoles.me/tna/reference/plot.group_tna_communities.md),
[`plot.tna_communities()`](https://sonsoles.me/tna/reference/plot.tna_communities.md),
[`print.group_tna_communities()`](https://sonsoles.me/tna/reference/print.group_tna_communities.md),
[`print.tna_communities()`](https://sonsoles.me/tna/reference/print.tna_communities.md)

Cluster-related functions
[`cluster_data()`](https://sonsoles.me/tna/reference/cluster_data.md),
[`group_model()`](https://sonsoles.me/tna/reference/group_model.md),
[`mmm_stats()`](https://sonsoles.me/tna/reference/mmm_stats.md),
[`print.tna_clustering()`](https://sonsoles.me/tna/reference/print.tna_clustering.md),
[`rename_groups()`](https://sonsoles.me/tna/reference/rename_groups.md)

## Examples

``` r
model <- tna(group_regulation)
comm <- communities(model)
```

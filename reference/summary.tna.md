# Calculate Summary of Network Metrics for a Transition Network

This function calculates a variety of network metrics for a `tna`
object. It computes key metrics such as node and edge counts, network
density, mean distance, strength measures, degree centrality, and
reciprocity.

## Usage

``` r
# S3 method for class 'tna'
summary(object, ...)
```

## Arguments

- object:

  A `tna` object.

- ...:

  Ignored.

## Value

A named `list` containing the following network metrics (invisibly):

- `node_count`: The total number of nodes.

- `edge_count`: The total number of edges.

- `network_Density`: The density of the network.

- `mean_distance`: The mean shortest path length.

- `mean_out_strength`: The mean out-strength of nodes.

- `sd_out_strength`: The standard deviation of out-strength.

- `mean_in_strength`: The mean in-strength of nodes.

- `sd_in_strength`: The standard deviation of in-strength.

- `mean_out_degree`: The mean out-degree of nodes.

- `sd_out_degree`: The standard deviation of out-degree.

- `centralization_out_degree`: The centralization of out-degree.

- `centralization_in_degree`: The centralization of in-degree.

- `reciprocity`: The reciprocity of the network.

## Details

The function extracts the `igraph` network and computes the following
network metrics:

- Node count: Total number of nodes in the network.

- Edge count: Total number of edges in the network.

- Network density: Proportion of possible edges that are present in the
  network.

- Mean distance: The average shortest path length between nodes.

- Mean and standard deviation of out-strength and in-strength: Measures
  of the total weight of outgoing and incoming edges for each node.

- Mean and standard deviation of out-degree: The number of outgoing
  edges from each node.

- Centralization of out-degree and in-degree: Measures of how
  centralized the network is based on the degrees of nodes.

- Reciprocity: The proportion of edges that are reciprocated (i.e.,
  mutual edges between nodes).

A summary of the metrics is printed to the console.

## See also

Basic functions
[`build_model()`](http://sonsoles.me/tna/reference/build_model.md),
[`hist.group_tna()`](http://sonsoles.me/tna/reference/hist.group_tna.md),
[`hist.tna()`](http://sonsoles.me/tna/reference/hist.tna.md),
[`plot.group_tna()`](http://sonsoles.me/tna/reference/plot.group_tna.md),
[`plot.tna()`](http://sonsoles.me/tna/reference/plot.tna.md),
[`plot_frequencies()`](http://sonsoles.me/tna/reference/plot_frequencies.md),
[`plot_frequencies.group_tna()`](http://sonsoles.me/tna/reference/plot_frequencies.group_tna.md),
[`plot_mosaic()`](http://sonsoles.me/tna/reference/plot_mosaic.md),
[`plot_mosaic.group_tna()`](http://sonsoles.me/tna/reference/plot_mosaic.group_tna.md),
[`plot_mosaic.tna_data()`](http://sonsoles.me/tna/reference/plot_mosaic.tna_data.md),
[`print.group_tna()`](http://sonsoles.me/tna/reference/print.group_tna.md),
[`print.summary.group_tna()`](http://sonsoles.me/tna/reference/print.summary.group_tna.md),
[`print.summary.tna()`](http://sonsoles.me/tna/reference/print.summary.tna.md),
[`print.tna()`](http://sonsoles.me/tna/reference/print.tna.md),
[`summary.group_tna()`](http://sonsoles.me/tna/reference/summary.group_tna.md),
[`tna-package`](http://sonsoles.me/tna/reference/tna-package.md)

## Examples

``` r
model <- tna(group_regulation)
summary(model)
#> # A tibble: 13 × 2
#>    metric                         value
#>  * <chr>                          <dbl>
#>  1 Node Count                  9   e+ 0
#>  2 Edge Count                  7.8 e+ 1
#>  3 Network Density             1   e+ 0
#>  4 Mean Distance               4.72e- 2
#>  5 Mean Out-Strength           1   e+ 0
#>  6 SD Out-Strength             8.07e- 1
#>  7 Mean In-Strength            1   e+ 0
#>  8 SD In-Strength              6.80e-17
#>  9 Mean Out-Degree             8.67e+ 0
#> 10 SD Out-Degree               7.07e- 1
#> 11 Centralization (Out-Degree) 1.56e- 2
#> 12 Centralization (In-Degree)  1.56e- 2
#> 13 Reciprocity                 9.86e- 1
```

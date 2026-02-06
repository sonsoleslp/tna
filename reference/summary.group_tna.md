# Calculate Summary of Network Metrics for a grouped Transition Network

This function calculates a variety of network metrics for a `tna`
object. It computes key metrics such as node and edge counts, network
density, mean distance, strength measures, degree centrality, and
reciprocity.

## Usage

``` r
# S3 method for class 'group_tna'
summary(object, combined = TRUE, ...)
```

## Arguments

- object:

  A `group_tna` object.

- combined:

  A logical indicating whether the summary results should be combined
  into a single data frame for all clusters (defaults to `TRUE`)

- ...:

  Ignored

## Value

A `summary.group_tna` object which is a `list` of `list`s or a combined
`data.frame` containing the following network metrics:

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

The function extracts the `igraph` network for each cluster and computes
the following network metrics:

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
[`summary.tna()`](http://sonsoles.me/tna/reference/summary.tna.md),
[`tna-package`](http://sonsoles.me/tna/reference/tna-package.md)

## Examples

``` r
group <- c(rep("High", 1000), rep("Low", 1000))
model <- group_model(group_regulation, group = group)
summary(model)
#> # A tibble: 13 × 3
#>    metric                          High      Low
#>  * <chr>                          <dbl>    <dbl>
#>  1 Node Count                  9   e+ 0 9   e+ 0
#>  2 Edge Count                  7.6 e+ 1 7.5 e+ 1
#>  3 Network Density             1   e+ 0 1   e+ 0
#>  4 Mean Distance               4.23e- 2 5.60e- 2
#>  5 Mean Out-Strength           1   e+ 0 1   e+ 0
#>  6 SD Out-Strength             9.14e- 1 7.19e- 1
#>  7 Mean In-Strength            1   e+ 0 1   e+ 0
#>  8 SD In-Strength              7.85e-17 3.93e-17
#>  9 Mean Out-Degree             8.44e+ 0 8.33e+ 0
#> 10 SD Out-Degree               1.13e+ 0 8.66e- 1
#> 11 Centralization (Out-Degree) 4.69e- 2 6.25e- 2
#> 12 Centralization (In-Degree)  4.69e- 2 6.25e- 2
#> 13 Reciprocity                 9.57e- 1 9.41e- 1
```

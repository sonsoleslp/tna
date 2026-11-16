# Calculate Summary of Network Metrics for a grouped Transition Network

This function calculates a variety of network metrics for each network
of a `group_tna` object. It computes key metrics such as node and edge
counts, network density, mean distance, strength measures, degree
centrality, and reciprocity.

## Usage

``` r
# S3 method for class 'group_tna'
summary(object, combined = TRUE, ...)
```

## Arguments

- object:

  A `group_tna` object.

- combined:

  A `logical` value indicating whether the summary results should be
  combined into a single table for all clusters. The default is `TRUE`.

- ...:

  Ignored.

## Value

A `summary.group_tna` object. When `combined = TRUE`, a tibble with one
row per metric (see
[`summary.tna()`](https://sonsoles.me/tna/reference/summary.tna.md)),
the column `metric`, and one column of values per cluster. When
`combined = FALSE`, a named `list` with the
[`summary.tna()`](https://sonsoles.me/tna/reference/summary.tna.md)
table of each cluster.

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
[`build_model()`](https://sonsoles.me/tna/reference/build_model.md),
[`hist.group_tna()`](https://sonsoles.me/tna/reference/hist.group_tna.md),
[`hist.tna()`](https://sonsoles.me/tna/reference/hist.tna.md),
[`plot.group_tna()`](https://sonsoles.me/tna/reference/plot.group_tna.md),
[`plot.tna()`](https://sonsoles.me/tna/reference/plot.tna.md),
[`plot_frequencies()`](https://sonsoles.me/tna/reference/plot_frequencies.md),
[`plot_frequencies.group_tna()`](https://sonsoles.me/tna/reference/plot_frequencies.group_tna.md),
[`plot_mosaic()`](https://sonsoles.me/tna/reference/plot_mosaic.md),
[`plot_mosaic.group_tna()`](https://sonsoles.me/tna/reference/plot_mosaic.group_tna.md),
[`plot_mosaic.tna_data()`](https://sonsoles.me/tna/reference/plot_mosaic.tna_data.md),
[`print.group_tna()`](https://sonsoles.me/tna/reference/print.group_tna.md),
[`print.summary.group_tna()`](https://sonsoles.me/tna/reference/print.summary.group_tna.md),
[`print.summary.tna()`](https://sonsoles.me/tna/reference/print.summary.tna.md),
[`print.tna()`](https://sonsoles.me/tna/reference/print.tna.md),
[`summary.tna()`](https://sonsoles.me/tna/reference/summary.tna.md),
[`tna-package`](https://sonsoles.me/tna/reference/tna-package.md)

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
#>  6 SD Out-Strength             7.85e-17 3.93e-17
#>  7 Mean In-Strength            1   e+ 0 1   e+ 0
#>  8 SD In-Strength              9.14e- 1 7.19e- 1
#>  9 Mean Out-Degree             8.44e+ 0 8.33e+ 0
#> 10 SD Out-Degree               1.13e+ 0 8.66e- 1
#> 11 Centralization (Out-Degree) 4.69e- 2 6.25e- 2
#> 12 Centralization (In-Degree)  4.69e- 2 6.25e- 2
#> 13 Reciprocity                 9.57e- 1 9.41e- 1
```

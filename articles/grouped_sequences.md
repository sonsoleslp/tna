# Using grouped sequence data with tna

TNA supports the analysis of transition networks constructed from
grouped sequence data. Groups can be defined in several ways, but
mainly: using a pre-existing grouping variable in the data (e.g., a
demographic or experimental condition), or by clustering the sequences
themselves based on their similarity. This vignette demonstrates both
approaches using the `group_regulation_long` dataset.

First, we load the packages we will use for this example.

``` r

library("tna")
library("tibble")
library("dplyr")
library("gt")
```

## Data preparation

We import the data in long format and prepare it for analysis. The
[`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)
function converts the long-format event log into wide-format sequences.
The unused columns in the dataset are stored in the metadata of
`prepared` and we can use them later on.

``` r

data("group_regulation_long", package = "tna")
prepared <- prepare_data(group_regulation_long,
                         actor = "Actor",
                         action = "Action",
                         time = "Time")
```

## Groups from a pre-existing variable

When the data contains a grouping variable, we can build separate TNA
models for each group directly. Here, the `"Achiever"` column in the
metadata splits the sequences into two groups (high vs. low achievers).

``` r

layout(t(1:2))
achievers <- group_tna(prepared, group = "Achiever")
plot(achievers)
#> Registered S3 method overwritten by 'cograph':
#>   method             from
#>   plot.tna_bootstrap tna
```

![](grouped_sequences_files/figure-html/unnamed-chunk-4-1.png)

### Comparing groups

The
[`plot_compare()`](https://sonsoles.me/tna/reference/plot_compare.md)
function visualizes the difference network between the two groups. Green
edges and donut segments indicate that the first group (High achievers)
has higher values, while red indicates the opposite (Low achievers have
higher values).

``` r

plot_compare(achievers)
```

![](grouped_sequences_files/figure-html/unnamed-chunk-5-1.png)

### Permutation test

A permutation test can be used to assess whether the observed
differences between the two groups are statistically significant.

``` r

permutation_test_results <- permutation_test(achievers)
plot(permutation_test_results)
```

![](grouped_sequences_files/figure-html/unnamed-chunk-6-1.png)

### Subsequence comparison

We can also compare the frequency of subsequences across groups. Here we
look at subsequences of length 3 to 5, keeping only those that appear at
least 5 times, and apply FDR correction for multiple comparisons.

``` r

subsequence_comparison  <- compare_sequences(achievers,
                                                  sub = 3:5,
                                                  min_freq = 5,
                                                  correction = "fdr")
plot(subsequence_comparison, cells = TRUE)
```

![](grouped_sequences_files/figure-html/unnamed-chunk-7-1.png)

## Groups from sequence clustering

When no pre-existing grouping variable is available, we can cluster the
sequences based on their pairwise dissimilarity. The
[`cluster_sequences()`](https://sonsoles.me/tna/reference/cluster_data.md).

``` r

clustering_results <- cluster_sequences(prepared, k = 3)
```

To choose an appropriate number of clusters, we can plot the silhouette
score for different values of *k*. Higher silhouette values indicate
better-separated clusters.

``` r

plot(
  2:8,
  sapply(2:8, \(k) cluster_sequences(prepared, k = k)$silhouette),
  type = "b",
  xlab = "Number of clusters (k)",
  ylab = "Silhouette",
  xaxt = "n"
)
```

![](grouped_sequences_files/figure-html/unnamed-chunk-9-1.png)

Once we have chosen *k*, we build the grouped TNA model using the
cluster assignments.

``` r

tna_model_clus <- group_tna(prepared, group = clustering_results$assignments)
```

``` r

layout(matrix(1:4, byrow = T, ncol = 2))
plot(tna_model_clus)
```

![](grouped_sequences_files/figure-html/unnamed-chunk-11-1.png)

### Summarizing the cluster-specific models

We can summarize the cluster-specific models to compare their overall
characteristics.

``` r

summary(tna_model_clus) |>
  gt() |>
  fmt_number(decimals = 2)
```

| metric                      | Group 1 | Group 2 | Group 3 |
|-----------------------------|---------|---------|---------|
| Node Count                  | 9.00    | 9.00    | 9.00    |
| Edge Count                  | 78.00   | 77.00   | 77.00   |
| Network Density             | 1.00    | 1.00    | 1.00    |
| Mean Distance               | 0.04    | 0.05    | 0.05    |
| Mean Out-Strength           | 1.00    | 1.00    | 1.00    |
| SD Out-Strength             | 0.76    | 0.84    | 0.81    |
| Mean In-Strength            | 1.00    | 1.00    | 1.00    |
| SD In-Strength              | 0.00    | 0.00    | 0.00    |
| Mean Out-Degree             | 8.67    | 8.56    | 8.56    |
| SD Out-Degree               | 0.71    | 0.73    | 0.73    |
| Centralization (Out-Degree) | 0.02    | 0.03    | 0.03    |
| Centralization (In-Degree)  | 0.02    | 0.03    | 0.03    |
| Reciprocity                 | 0.99    | 0.97    | 0.97    |

Initial probabilities show which states are most common at the start of
the sequences in each cluster.

``` r

mat <- sapply(
  tna_model_clus,
  \(x) setNames(x$inits, x$labels)
)

df <- data.frame(label = rownames(mat), mat, row.names = NULL)

gt(df, rowname_col = "label") |> fmt_percent(columns = -label)
```

|            | Group.1 | Group.2 | Group.3 |
|------------|---------|---------|---------|
| adapt      | 1.01%   | 1.01%   | 1.47%   |
| cohesion   | 6.05%   | 6.68%   | 5.55%   |
| consensus  | 17.02%  | 18.42%  | 30.18%  |
| coregulate | 1.46%   | 2.83%   | 1.79%   |
| discuss    | 17.69%  | 19.43%  | 15.82%  |
| emotion    | 15.68%  | 13.97%  | 15.33%  |
| monitor    | 15.12%  | 12.75%  | 14.68%  |
| plan       | 23.63%  | 22.87%  | 13.87%  |
| synthesis  | 2.35%   | 2.02%   | 1.31%   |

The full transition probability matrices can also be inspected for each
cluster.

``` r

transitions <- lapply(
  tna_model_clus,
  function(x) {
    x$weights |>
      data.frame() |>
      rownames_to_column("From\\To") |>
      gt() |>
      fmt_percent()
  }
)

transitions[[1]] |> tab_header(title = names(tna_model_clus)[1])
```

| Group 1 |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|
| From\To | adapt | cohesion | consensus | coregulate | discuss | emotion | monitor | plan | synthesis |
| adapt | 0.00% | 33.33% | 43.09% | 1.63% | 4.07% | 12.20% | 4.88% | 0.81% | 0.00% |
| cohesion | 0.52% | 2.09% | 47.64% | 11.26% | 8.90% | 12.83% | 3.40% | 12.83% | 0.52% |
| consensus | 0.85% | 0.93% | 8.24% | 20.51% | 22.07% | 7.61% | 4.20% | 34.50% | 1.09% |
| coregulate | 1.31% | 4.70% | 11.49% | 2.09% | 31.07% | 19.84% | 7.57% | 19.06% | 2.87% |
| discuss | 7.40% | 5.25% | 31.94% | 7.72% | 19.72% | 11.68% | 1.82% | 0.86% | 13.61% |
| emotion | 0.31% | 33.23% | 34.46% | 3.06% | 9.95% | 7.20% | 3.22% | 8.27% | 0.31% |
| monitor | 1.40% | 6.72% | 15.13% | 5.88% | 38.66% | 10.36% | 2.80% | 18.49% | 0.56% |
| plan | 0.09% | 1.83% | 28.58% | 1.19% | 7.31% | 16.62% | 9.13% | 34.98% | 0.27% |
| synthesis | 28.00% | 3.33% | 44.67% | 6.00% | 5.33% | 8.00% | 2.00% | 2.67% | 0.00% |

``` r

transitions[[2]] |> tab_header(title = names(tna_model_clus)[2])
```

| Group 2 |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|
| From\To | adapt | cohesion | consensus | coregulate | discuss | emotion | monitor | plan | synthesis |
| adapt | 0.00% | 24.06% | 52.36% | 2.83% | 6.60% | 10.38% | 3.30% | 0.47% | 0.00% |
| cohesion | 0.15% | 2.51% | 52.22% | 11.39% | 4.73% | 11.54% | 3.40% | 13.91% | 0.15% |
| consensus | 0.40% | 1.42% | 9.16% | 17.85% | 18.72% | 7.08% | 4.78% | 39.89% | 0.69% |
| coregulate | 1.31% | 3.33% | 14.88% | 2.38% | 25.95% | 16.55% | 10.00% | 23.57% | 2.02% |
| discuss | 7.19% | 4.03% | 32.78% | 8.55% | 17.78% | 10.66% | 2.91% | 1.30% | 14.81% |
| emotion | 0.00% | 31.96% | 32.56% | 3.44% | 10.05% | 7.73% | 4.12% | 9.88% | 0.26% |
| monitor | 1.39% | 5.37% | 16.98% | 5.89% | 38.47% | 9.36% | 1.73% | 18.54% | 2.25% |
| plan | 0.19% | 2.63% | 30.17% | 1.74% | 6.95% | 14.83% | 7.03% | 36.27% | 0.19% |
| synthesis | 22.73% | 3.85% | 46.85% | 4.20% | 6.29% | 6.29% | 1.40% | 8.39% | 0.00% |

``` r

transitions[[3]] |> tab_header(title = names(tna_model_clus)[3])
```

| Group 3 |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|
| From\To | adapt | cohesion | consensus | coregulate | discuss | emotion | monitor | plan | synthesis |
| adapt | 0.00% | 27.01% | 45.40% | 1.72% | 6.32% | 13.79% | 2.30% | 3.45% | 0.00% |
| cohesion | 0.31% | 3.30% | 48.51% | 12.87% | 5.49% | 10.83% | 3.14% | 15.07% | 0.47% |
| consensus | 0.35% | 1.87% | 7.04% | 18.90% | 17.07% | 7.30% | 4.78% | 42.05% | 0.65% |
| coregulate | 2.14% | 3.35% | 12.85% | 2.41% | 27.04% | 16.60% | 7.63% | 26.77% | 1.20% |
| discuss | 6.91% | 5.27% | 31.48% | 8.76% | 21.30% | 9.76% | 1.71% | 1.21% | 13.60% |
| emotion | 0.49% | 32.75% | 29.90% | 3.63% | 10.49% | 7.94% | 3.33% | 11.18% | 0.29% |
| monitor | 0.60% | 5.01% | 15.23% | 5.61% | 35.67% | 7.82% | 1.20% | 27.25% | 1.60% |
| plan | 0.00% | 2.71% | 28.06% | 1.94% | 6.39% | 13.67% | 7.40% | 39.71% | 0.12% |
| synthesis | 21.30% | 2.78% | 47.69% | 3.70% | 6.94% | 7.41% | 0.46% | 9.72% | 0.00% |

### Pruning with bootstrap

Just like ordinary TNA models, we can retain only the statistically
robust edges.

``` r

cluster_boot <- bootstrap(tna_model_clus)
```

``` r

layout(matrix(1:4, byrow = T, ncol = 2))
plot(cluster_boot)
```

![](grouped_sequences_files/figure-html/unnamed-chunk-16-1.png)

### Centrality measures

Centrality measures can be computed for each cluster to identify which
states play central roles in each group’s transition dynamics.

``` r

centrality_measures <- c(
  "BetweennessRSP",
  "Closeness",
  "InStrength",
  "OutStrength"
)
centralities_per_cluster <- centralities(
  tna_model_clus,
  measures = centrality_measures
)
plot(
  centralities_per_cluster, ncol = 4,
  colors = c("purple", "orange", "pink")
)
```

![](grouped_sequences_files/figure-html/unnamed-chunk-17-1.png)

### Subsequence comparison across clusters

Finally, we can compare subsequence frequencies across the clusters,
just as we did for the pre-existing groups above.

``` r

subsequence_comparison  <- compare_sequences(tna_model_clus, sub = 3:5, min_freq = 5, correction = "fdr")
plot(subsequence_comparison, cells = TRUE)
```

![](grouped_sequences_files/figure-html/unnamed-chunk-18-1.png)

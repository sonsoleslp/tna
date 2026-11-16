# Clustering via Dissimilarity Matrix based on String Distances

Performs clustering using specified dissimilarity measures and
clustering methods. The rows of the data are first converted to strings
and compared using the dissimilarity measures available in the
`stringdist` package.

## Usage

``` r
cluster_data(
  data,
  k,
  dissimilarity = "hamming",
  method = "pam",
  na_syms = c("*", "%"),
  weighted = FALSE,
  lambda = 1,
  ...
)

cluster_sequences(
  data,
  k,
  dissimilarity = "hamming",
  method = "pam",
  na_syms = c("*", "%"),
  weighted = FALSE,
  lambda = 1,
  ...
)
```

## Arguments

- data:

  A `data.frame`, a `matrix` in wide format, or a `tna_data` object (in
  which case, the sequence data is extracted automatically).

- k:

  An `integer` giving the number of clusters.

- dissimilarity:

  A `character` string specifying the dissimilarity measure. The
  available options are: `"osa"`, `"lv"`, `"dl"`, `"hamming"`, `"lcs"`,
  `"qgram"`, `"cosine"`, `"jaccard"`, and `"jw"`. See
  [stringdist::stringdist-metrics](https://rdrr.io/pkg/stringdist/man/stringdist-metrics.html)
  for more information on these measures.

- method:

  A `character` string specifying clustering method. The available
  methods are `"pam"`, `"ward.D"`, `"ward.D2"`,
  `"complete"`,`"average"`, `"single"`, `"mcquitty"`, `"median"`, and
  `"centroid"`. See
  [`cluster::pam()`](https://rdrr.io/pkg/cluster/man/pam.html) and
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html) for more
  information on these methods.

- na_syms:

  A `character` vector of symbols or factor levels to convert to
  explicit missing values.

- weighted:

  A `logical` value indicating whether the dissimilarity measure should
  be weighted (the default is `FALSE` for no weighting). If `TRUE`,
  earlier observations of the sequences receive a greater weight in the
  distance calculation with an exponential decay. Currently only
  supported for the Hamming distance.

- lambda:

  A `numeric` value defining the strength of the decay when
  `weighted = TRUE`. The default is `1.0`.

- ...:

  Additional arguments passed to
  [`stringdist::stringdist()`](https://rdrr.io/pkg/stringdist/man/stringdist.html).

## Value

A `tna_clustering` object which is a `list` containing:

- `data`: The original data.

- `k`: The number of clusters.

- `assignments`: An `integer` vector of cluster assignments, one per
  sequence (row of `data`).

- `silhouette`: The average silhouette width of the clustering.

- `sizes`: An `integer` vector of cluster sizes.

- `method`: The clustering method used.

- `distance`: The dissimilarity matrix (a `dist` object).

The result can be passed to
[`group_model()`](https://sonsoles.me/tna/reference/group_model.md) to
build a TNA model for each cluster.

## References

Kaufman, L., & Rousseeuw, P. J. (1990). *Finding Groups in Data: An
Introduction to Cluster Analysis*. Wiley.
[doi:10.1002/9780470316801](https://doi.org/10.1002/9780470316801)

Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the
interpretation and validation of cluster analysis. *Journal of
Computational and Applied Mathematics*, 20, 53–65.
[doi:10.1016/0377-0427(87)90125-7](https://doi.org/10.1016/0377-0427%2887%2990125-7)

## See also

Cluster-related functions
[`communities()`](https://sonsoles.me/tna/reference/communities.md),
[`group_model()`](https://sonsoles.me/tna/reference/group_model.md),
[`mmm_stats()`](https://sonsoles.me/tna/reference/mmm_stats.md),
[`print.tna_clustering()`](https://sonsoles.me/tna/reference/print.tna_clustering.md),
[`rename_groups()`](https://sonsoles.me/tna/reference/rename_groups.md)

## Examples

``` r
data <- data.frame(
  T1 = c("A", "B", "A", "C", "A", "B"),
  T2 = c("B", "A", "B", "A", "C", "A"),
  T3 = c("C", "C", "A", "B", "B", "C")
)

# PAM clustering with the Hamming distance (the defaults)
result <- cluster_sequences(data, k = 2)
print(result)
#> Clustering method: pam 
#> Number of clusters: 2 
#> Silhouette score: 0.4345238 
#> Cluster sizes:
#> 1 2 
#> 3 3 

# Hierarchical clustering with the optimal string alignment distance
result <- cluster_sequences(data, k = 2, dissimilarity = "osa",
                            method = "complete")
```

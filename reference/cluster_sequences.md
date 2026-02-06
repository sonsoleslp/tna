# Cluster Sequences via Dissimilarity Matrix based on String Distances

Performs clustering on sequence data using specified dissimilarity
measures and clustering methods. The sequences are first converted to
strings and compared using the `stringdist` package.

## Usage

``` r
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

# S3 method for class 'tna_clustering'
print(x, ...)
```

## Arguments

- data:

  A `data.frame` or a `matrix` where the rows are sequences and the
  columns are time points.

- k:

  An `integer` giving the number of clusters.

- dissimilarity:

  A `character` string specifying the dissimilarity measure. The
  available options are: `"osa"`, `"lv"`, `"dl"`, `"hamming"`,
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

- x:

  A `tna_clustering` object.

## Value

A `tna_clustering` object which is a `list` containing:

- `data`: The original data.

- `k`: The number of clusters.

- `assignments`: An `integer` vector of cluster assignments.

- `silhouette`: Silhouette score measuring clustering quality.

- `sizes`: An `integer` vector of cluster sizes.

- `method`: The clustering method used.

- `distance`: The distance matrix.

## Examples

``` r
data <- data.frame(
  T1 = c("A", "B", "A", "C", "A", "B"),
  T2 = c("B", "A", "B", "A", "C", "A"),
  T3 = c("C", "C", "A", "B", "B", "C")
)

# PAM clustering with optimal string alignment (default)
result <- cluster_sequences(data, k = 2)
print(result)
#> Clustering method: pam 
#> Number of clusters: 2 
#> Silhouette score: 0.4345238 
#> Cluster sizes:
#> 1 2 
#> 3 3 
```

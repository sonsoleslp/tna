# Print the Results of Clustering

Print the Results of Clustering

## Usage

``` r
# S3 method for class 'tna_clustering'
print(x, ...)
```

## Arguments

- x:

  A `tna_clustering` object.

- ...:

  Additional arguments passed to the generic print method.

## Value

`x` (invisibly).

## See also

Cluster-related functions
[`cluster_data()`](https://sonsoles.me/tna/reference/cluster_data.md),
[`communities()`](https://sonsoles.me/tna/reference/communities.md),
[`group_model()`](https://sonsoles.me/tna/reference/group_model.md),
[`mmm_stats()`](https://sonsoles.me/tna/reference/mmm_stats.md),
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
```

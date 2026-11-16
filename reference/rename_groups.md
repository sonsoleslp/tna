# Rename Groups

Rename Groups

## Usage

``` r
rename_groups(x, new_names)
```

## Arguments

- x:

  A `group_tna` object.

- new_names:

  A `character` vector containing one name per cluster.

## Value

The `group_tna` object `x` with its elements (groups) renamed to
`new_names`.

## See also

Cluster-related functions
[`cluster_data()`](https://sonsoles.me/tna/reference/cluster_data.md),
[`communities()`](https://sonsoles.me/tna/reference/communities.md),
[`group_model()`](https://sonsoles.me/tna/reference/group_model.md),
[`mmm_stats()`](https://sonsoles.me/tna/reference/mmm_stats.md),
[`print.tna_clustering()`](https://sonsoles.me/tna/reference/print.tna_clustering.md)

## Examples

``` r
model <- group_model(engagement_mmm)
model_renamed <- rename_groups(model, c("A", "B", "C"))
```

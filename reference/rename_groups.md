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

A renamed `group_tna` object.

## See also

Cluster-related functions
[`communities()`](http://sonsoles.me/tna/reference/communities.md),
[`group_model()`](http://sonsoles.me/tna/reference/group_model.md),
[`mmm_stats()`](http://sonsoles.me/tna/reference/mmm_stats.md)

## Examples

``` r
model <- group_model(engagement_mmm)
model_renamed <- rename_groups(model, c("A", "B", "C"))
```

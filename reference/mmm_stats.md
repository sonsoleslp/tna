# Retrieve Statistics from a Mixture Markov Model (MMM)

Retrieve Statistics from a Mixture Markov Model (MMM)

## Usage

``` r
mmm_stats(x, level = 0.05)

# S3 method for class 'tna_mmm'
mmm_stats(x, level = 0.05)

# S3 method for class 'mhmm'
mmm_stats(x, level = 0.05)
```

## Arguments

- x:

  A `mhmm` object (from the `seqHMM` package) or a `tna_mmm` object
  built by
  [`random_tna_mmm()`](https://sonsoles.me/tna/reference/random_tna_mmm.md).

- level:

  A `numeric` value representing the significance level for hypothesis
  testing and confidence intervals. Defaults to `0.05`.

## Value

A `data.frame` object.

## See also

Cluster-related functions
[`communities()`](https://sonsoles.me/tna/reference/communities.md),
[`group_model()`](https://sonsoles.me/tna/reference/group_model.md),
[`rename_groups()`](https://sonsoles.me/tna/reference/rename_groups.md)

## Examples

``` r
mmm_stats(engagement_mmm)
#>     cluster    variable  estimate std_error   ci_lower ci_upper   z_value
#> 1 Cluster 2 (Intercept) 1.1880881 0.9856987 -0.7438459 3.120022 1.2053258
#> 2 Cluster 3 (Intercept) 0.9495465 2.1560282 -3.2761911 5.175284 0.4404147
#>     p_value
#> 1 0.2280775
#> 2 0.6596368
```

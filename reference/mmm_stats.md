# Retrieve Statistics from a Mixture Markov Model (MMM)

Retrieve Statistics from a Mixture Markov Model (MMM)

## Usage

``` r
mmm_stats(x, level = 0.05)

# S3 method for class 'mhmm'
mmm_stats(x, level = 0.05)
```

## Arguments

- x:

  A `mhmm` object.

- level:

  A `numeric` value representing the significance level for hypothesis
  testing and confidence intervals. Defaults to `0.05`.

## Value

A `data.frame` object.

## See also

Cluster-related functions
[`communities()`](http://sonsoles.me/tna/reference/communities.md),
[`group_model()`](http://sonsoles.me/tna/reference/group_model.md),
[`rename_groups()`](http://sonsoles.me/tna/reference/rename_groups.md)

## Examples

``` r
mmm_stats(engagement_mmm)
#>     cluster    variable  estimate std_error  ci_lower  ci_upper   z_value
#> 1 Cluster 2 (Intercept) -2.640329 0.1274911 -2.890207 -2.390451 -20.70991
#> 2 Cluster 3 (Intercept) -4.512131 0.3157501 -5.130990 -3.893272 -14.29020
#>   p_value
#> 1       0
#> 2       0
```

# Print Detected Communities

Print Detected Communities

## Usage

``` r
# S3 method for class 'group_tna_communities'
print(x, ...)
```

## Arguments

- x:

  A `group_tna_communities` object.

- ...:

  Arguments passed to
  [`print.tna_communities()`](http://sonsoles.me/tna/reference/print.tna_communities.md).

## Value

`x` (invisibly).

## See also

Community detection functions
[`communities()`](http://sonsoles.me/tna/reference/communities.md),
[`plot.group_tna_communities()`](http://sonsoles.me/tna/reference/plot.group_tna_communities.md),
[`plot.tna_communities()`](http://sonsoles.me/tna/reference/plot.tna_communities.md),
[`print.tna_communities()`](http://sonsoles.me/tna/reference/print.tna_communities.md)

## Examples

``` r
model <- group_model(engagement_mmm)
comm <- communities(model)
print(comm)
#> Cluster 1 :
#> Number of communities found by each algorithm
#> 
#>         walktrap      fast_greedy       label_prop          infomap 
#>                1                3                3                1 
#> edge_betweenness    leading_eigen        spinglass 
#>                3                2                1 
#> 
#> Community assignments
#> 
#>        state walktrap fast_greedy label_prop infomap edge_betweenness
#> 1     Active        1           1          1       1                1
#> 2    Average        1           2          2       1                2
#> 3 Disengaged        1           3          3       1                3
#>   leading_eigen spinglass
#> 1             1         1
#> 2             1         1
#> 3             2         1
#> 
#> Cluster 2 :
#> Number of communities found by each algorithm
#> 
#>         walktrap      fast_greedy       label_prop          infomap 
#>                1                2                3                1 
#> edge_betweenness    leading_eigen        spinglass 
#>                2                2                1 
#> 
#> Community assignments
#> 
#>        state walktrap fast_greedy label_prop infomap edge_betweenness
#> 1     Active        1           2          1       1                1
#> 2    Average        1           1          2       1                2
#> 3 Disengaged        1           1          3       1                2
#>   leading_eigen spinglass
#> 1             1         1
#> 2             2         1
#> 3             2         1
#> 
#> Cluster 3 :
#> Number of communities found by each algorithm
#> 
#>         walktrap      fast_greedy       label_prop          infomap 
#>                1                3                3                1 
#> edge_betweenness    leading_eigen        spinglass 
#>                3                2                1 
#> 
#> Community assignments
#> 
#>        state walktrap fast_greedy label_prop infomap edge_betweenness
#> 1     Active        1           1          1       1                1
#> 2    Average        1           2          2       1                2
#> 3 Disengaged        1           3          3       1                3
#>   leading_eigen spinglass
#> 1             1         1
#> 2             2         1
#> 3             2         1
#> 
```

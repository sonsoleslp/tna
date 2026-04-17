# Print Detected Communities

Print Detected Communities

## Usage

``` r
# S3 method for class 'tna_communities'
print(x, ...)
```

## Arguments

- x:

  A `tna_communities` object.

- ...:

  Additional arguments passed to the generic `print` method.

## Value

`x` (invisibly).

## See also

Community detection functions
[`communities()`](http://sonsoles.me/tna/reference/communities.md),
[`plot.group_tna_communities()`](http://sonsoles.me/tna/reference/plot.group_tna_communities.md),
[`plot.tna_communities()`](http://sonsoles.me/tna/reference/plot.tna_communities.md),
[`print.group_tna_communities()`](http://sonsoles.me/tna/reference/print.group_tna_communities.md)

## Examples

``` r
model <- tna(group_regulation)
comm <- communities(model)
print(comm)
#> Number of communities found by each algorithm
#> 
#>         walktrap      fast_greedy       label_prop          infomap 
#>                1                3                1                1 
#> edge_betweenness    leading_eigen        spinglass 
#>                1                3                2 
#> 
#> Community assignments
#> 
#>        state walktrap fast_greedy label_prop infomap edge_betweenness
#> 1      adapt        1           1          1       1                1
#> 2   cohesion        1           1          1       1                1
#> 3  consensus        1           1          1       1                1
#> 4 coregulate        1           2          1       1                1
#> 5    discuss        1           2          1       1                1
#> 6    emotion        1           1          1       1                1
#> 7    monitor        1           2          1       1                1
#> 8       plan        1           3          1       1                1
#> 9  synthesis        1           1          1       1                1
#>   leading_eigen spinglass
#> 1             1         2
#> 2             1         2
#> 3             2         2
#> 4             3         1
#> 5             3         1
#> 6             1         2
#> 7             2         1
#> 8             2         2
#> 9             3         2
```

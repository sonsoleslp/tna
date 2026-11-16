# Coerce a `tna` Object into an `igraph` Object.

Coerce a `tna` Object into an `igraph` Object.

## Usage

``` r
# S3 method for class 'tna'
as.igraph(x, mode = "directed", ...)
```

## Arguments

- x:

  A `tna` object.

- mode:

  Character scalar, specifies how igraph should interpret the supplied
  matrix. See also the `weighted` argument, the interpretation depends
  on that too. Possible values are: `directed`, `undirected`, `upper`,
  `lower`, `max`, `min`, `plus`. See details below.

- ...:

  Ignored.

## Value

An `igraph` object with one vertex per state and one weighted edge per
non-zero element of the weight matrix of `x`.

## See also

Helper functions
[`as.igraph.group_tna()`](https://sonsoles.me/tna/reference/as.igraph.group_tna.md),
[`as.igraph.matrix()`](https://sonsoles.me/tna/reference/as.igraph.matrix.md)

## Examples

``` r
as.igraph(tna(group_regulation))
#> IGRAPH 8366b10 DNW- 9 78 -- 
#> + attr: name (v/c), weight (e/n)
#> + edges from 8366b10 (vertex names):
#>  [1] adapt     ->cohesion   adapt     ->consensus  adapt     ->coregulate
#>  [4] adapt     ->discuss    adapt     ->emotion    adapt     ->monitor   
#>  [7] adapt     ->plan       cohesion  ->adapt      cohesion  ->cohesion  
#> [10] cohesion  ->consensus  cohesion  ->coregulate cohesion  ->discuss   
#> [13] cohesion  ->emotion    cohesion  ->monitor    cohesion  ->plan      
#> [16] cohesion  ->synthesis  consensus ->adapt      consensus ->cohesion  
#> [19] consensus ->consensus  consensus ->coregulate consensus ->discuss   
#> [22] consensus ->emotion    consensus ->monitor    consensus ->plan      
#> + ... omitted several edges
```

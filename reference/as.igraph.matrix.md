# Coerce a Weight Matrix into an `igraph` Object.

Coerce a Weight Matrix into an `igraph` Object.

## Usage

``` r
# S3 method for class 'matrix'
as.igraph(x, mode = "directed", ...)
```

## Arguments

- x:

  A `matrix` of edge weights.

- mode:

  Character scalar, specifies how igraph should interpret the supplied
  matrix. See also the `weighted` argument, the interpretation depends
  on that too. Possible values are: `directed`, `undirected`, `upper`,
  `lower`, `max`, `min`, `plus`. See details below.

- ...:

  Ignored.

## Value

An `igraph` object with one vertex per row of `x` and one weighted edge
per non-zero element of `x`.

## See also

Helper functions
[`as.igraph.group_tna()`](https://sonsoles.me/tna/reference/as.igraph.group_tna.md),
[`as.igraph.tna()`](https://sonsoles.me/tna/reference/as.igraph.tna.md)

## Examples

``` r
weights <- matrix(c(0, 0.6, 0.4, 0.5, 0, 0.5, 0.2, 0.8, 0), 3, 3)
as.igraph(weights)
#> IGRAPH 9ed4503 D-W- 3 6 -- 
#> + attr: weight (e/n)
#> + edges from 9ed4503:
#> [1] 1->2 1->3 2->1 2->3 3->1 3->2
```

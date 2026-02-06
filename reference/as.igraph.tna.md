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

An `igraph` object.

## See also

Helper functions
[`as.igraph.group_tna()`](http://sonsoles.me/tna/reference/as.igraph.group_tna.md),
[`as.igraph.matrix()`](http://sonsoles.me/tna/reference/as.igraph.matrix.md)

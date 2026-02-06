# Bootstrap Cliques of Transition Networks from Sequence Data

Bootstrap Cliques of Transition Networks from Sequence Data

## Usage

``` r
bootstrap_cliques(x, size, threshold, iter, level, consistency_range)

# S3 method for class 'tna'
bootstrap_cliques(
  x,
  size = 2L,
  threshold = 0,
  iter = 1000,
  level = 0.05,
  consistency_range = c(0.75, 1.25)
)
```

## Arguments

- x:

  A `tna` or a `group_tna` object.

- size:

  An `integer` specifying the size of the cliques to identify. Defaults
  to `2` (dyads).

- threshold:

  A `numeric` value that sets the minimum edge weight for an edge to be
  considered in the clique. Edges below this value are ignored. Defaults
  to `0`.

- iter:

  An `integer` specifying the number of bootstrap samples to draw.
  Defaults to `1000`.

- level:

  A `numeric` value representing the significance level for hypothesis
  testing and confidence intervals. Defaults to `0.05`.

- consistency_range:

  A `numeric` vector of length 2. Determines how much the edge weights
  may deviate (multiplicatively) from their observed values (below and
  above) before they are considered insignificant. The default is
  `c(0.75, 1.25)` which corresponds to a symmetric 25% deviation range.
  Used only when `method = "stability"`.

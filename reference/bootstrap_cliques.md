# Bootstrap Cliques of Transition Networks from Sequence Data

Bootstrap the edge weights of all cliques of a given size in a `tna`
model, producing per-clique mean weights, p-values, confidence
intervals, and consistency-range bounds.

## Usage

``` r
bootstrap_cliques(
  x,
  size,
  threshold,
  iter,
  level,
  consistency_range,
  seed = NULL
)

# S3 method for class 'tna'
bootstrap_cliques(
  x,
  size = 2L,
  threshold = 0,
  iter = 1000,
  level = 0.05,
  consistency_range = c(0.75, 1.25),
  seed = NULL
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

- seed:

  A single `numeric` random seed for reproducible resampling, or `NULL`
  (the default) to use the current RNG state.

## Value

A `data.frame` (also of class `tna_bootstrap_cliques`) with one row per
clique and the columns `clique`, `mean_weight`, `p_values`, `sig`,
`cr_lower`, `cr_upper`, `ci_lower`, `ci_upper`.

## Examples

``` r
model <- tna(group_regulation)
# Small number of iterations for CRAN
boot_cliq <- bootstrap_cliques(model, size = 2, iter = 10)
```

# Import One-Hot Data

Import One-Hot Data

## Usage

``` r
import_onehot(
  data,
  cols,
  actor,
  session,
  window_size = 1L,
  window_type = "tumbling",
  aggregate = FALSE
)
```

## Arguments

- data:

  A `data.frame` in wide format.

- cols:

  An `expression` giving a tidy selection of columns to be considered as
  one-hot data.

- actor:

  An optional `character` string giving the column name of `data`
  containing the actor identifiers.

- session:

  An optional `character` string giving the column name of `data`
  containing the session identifiers.

- window_size:

  An `integer` specifying the window size for grouping.

- window_type:

  A `character` string. Either `"tumbling"` (the default) for
  non-overlapping windows or `"sliding"` for one-step sliding window.

- aggregate:

  A `logical` value that determines how multiple occurrences of the same
  event within a window are processed. Option `TRUE` aggregates multiple
  occurrences into a single occurrence. Option `FALSE` keeps all
  occurrences (the default).

## Value

The processed data as a `data.frame`.

## See also

Other data:
[`import_data()`](http://sonsoles.me/tna/reference/import_data.md),
[`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md),
[`print.tna_data()`](http://sonsoles.me/tna/reference/print.tna_data.md),
[`simulate.group_tna()`](http://sonsoles.me/tna/reference/simulate.group_tna.md),
[`simulate.tna()`](http://sonsoles.me/tna/reference/simulate.tna.md)

## Examples

``` r
d <- data.frame(
  actor = gl(100, 5),
  session = gl(10, 50),
  feature1 = rbinom(500, 1, prob = 0.33),
  feature2 = rbinom(500, 1, prob = 0.25),
  feature3 = rbinom(500, 1, prob = 0.50)
)
onehot1 <- import_onehot(d, feature1:feature3)
onehot2 <- import_onehot(d, feature1:feature3, "actor", "session")
```

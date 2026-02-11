# Plot State Frequencies as a Mosaic Between Two Groups

Plot State Frequencies as a Mosaic Between Two Groups

## Usage

``` r
# S3 method for class 'tna_data'
plot_mosaic(x, group, label = "Group", ...)
```

## Arguments

- x:

  A `tna_data` object.

- group:

  A `character` string giving the column name of the (meta) data to
  contrast the frequencies with or a vector of group indicators with the
  the same length as the number of rows in the sequence data.

- label:

  An optional `character` string that specifies a label for the grouping
  variable when `group` is not a column name of the data.

- ...:

  Ignored.

## Value

A `ggplot` object.

## See also

Basic functions
[`build_model()`](http://sonsoles.me/tna/reference/build_model.md),
[`hist.group_tna()`](http://sonsoles.me/tna/reference/hist.group_tna.md),
[`hist.tna()`](http://sonsoles.me/tna/reference/hist.tna.md),
[`plot.group_tna()`](http://sonsoles.me/tna/reference/plot.group_tna.md),
[`plot.tna()`](http://sonsoles.me/tna/reference/plot.tna.md),
[`plot_frequencies()`](http://sonsoles.me/tna/reference/plot_frequencies.md),
[`plot_frequencies.group_tna()`](http://sonsoles.me/tna/reference/plot_frequencies.group_tna.md),
[`plot_mosaic()`](http://sonsoles.me/tna/reference/plot_mosaic.md),
[`plot_mosaic.group_tna()`](http://sonsoles.me/tna/reference/plot_mosaic.group_tna.md),
[`print.group_tna()`](http://sonsoles.me/tna/reference/print.group_tna.md),
[`print.summary.group_tna()`](http://sonsoles.me/tna/reference/print.summary.group_tna.md),
[`print.summary.tna()`](http://sonsoles.me/tna/reference/print.summary.tna.md),
[`print.tna()`](http://sonsoles.me/tna/reference/print.tna.md),
[`summary.group_tna()`](http://sonsoles.me/tna/reference/summary.group_tna.md),
[`summary.tna()`](http://sonsoles.me/tna/reference/summary.tna.md),
[`tna-package`](http://sonsoles.me/tna/reference/tna-package.md)

## Examples

``` r
d <- data.frame(
  time = rep(1:5, rep = 4),
  group = rep(1:4, each = 5),
  event = sample(LETTERS[1:3], 20, replace = TRUE)
)
sequence_data <- prepare_data(
  d,
  time = "time",
  actor = "group",
  action = "event"
)
#> ── Preparing Data ──────────────────────────────────────────────────────────────
#> ℹ Input data dimensions: 20 rows, 3 columns
#> ℹ First few time values: 1, 2, and 3
#> ℹ Detected <numeric> time values: treating as Unix timestamp.
#> ℹ Number of values to parse: 20
#> ℹ Sample values: 1, 2, and 3
#> ℹ Sample of parsed times: 1970-01-01 00:00:01, 1970-01-01 00:00:02, and
#>   1970-01-01 00:00:03
#> ℹ Time threshold for new session: 900 seconds
#> ℹ Total number of sessions: 4
#> ℹ Number of unique users: 4
#> ℹ Total number of actions: 20
#> ℹ Maximum sequence length: 5 actions
#> ℹ Time range: 1970-01-01 00:00:01 to 1970-01-01 00:00:05
plot_mosaic(sequence_data, group = "group")

```

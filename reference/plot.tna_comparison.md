# Plot the Comparison of Two TNA Models or Matrices

Plot the Comparison of Two TNA Models or Matrices

## Usage

``` r
# S3 method for class 'tna_comparison'
plot(
  x,
  type = "heatmap",
  population = "difference",
  method = "pearson",
  name_x = "x",
  name_y = "y",
  ...
)
```

## Arguments

- x:

  A `tna_comparison` object.

- type:

  A `character` string naming the type of plot to produce. The available
  options are `"heatmap"` (the default), `"scatterplot"`,
  `"centrality_heatmap"`, and `"weight_density"`.

- population:

  A `"character"` string naming the population for which to produce the
  heatmaps, i.e, one of `"x"`, `"y"`, or `"difference"` for the
  differences. Ignored for `type = "scatterplot"`. Defaults to `"diff"`.

- method:

  A `character` string naming the correlation coefficient to use when
  plotting a scatterplot. The available options are `"pearson"` (the
  default), `"kendall"`, `"spearman"`, and `"distance"`. The final
  option is the distance correlation coefficient of Szekely, Rizzo, and
  Bakirov (2007). See also the `energy` package for further information
  on this measure.

- name_x:

  An optional `character` string to use as the name of the first
  population in the plots. The default is `"x"`.

- name_y:

  An optional `character` string to use as the name of the second
  population in the plots. The default is `"y"`.

- ...:

  Ignored.

## Value

A `ggplot` object.

## References

Szekely, G.J., Rizzo, M.L., and Bakirov, N.K. (2007), Measuring and
Testing Dependence by Correlation of Distances, *Annals of Statistics*,
35(6), 2769-2794. doi:10.1214/009053607000000505

## See also

Model comparison functions
[`compare()`](http://sonsoles.me/tna/reference/compare.md),
[`compare.group_tna()`](http://sonsoles.me/tna/reference/compare.group_tna.md),
[`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md),
[`plot.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md),
[`plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md),
[`plot_compare.group_tna()`](http://sonsoles.me/tna/reference/plot_compare.group_tna.md),
[`print.tna_comparison()`](http://sonsoles.me/tna/reference/print.tna_comparison.md),
[`print.tna_sequence_comparison()`](http://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)

## Examples

``` r
model_x <- tna(group_regulation[1:200, ])
model_y <- tna(group_regulation[1001:1200, ])
comp <- compare(model_x, model_y)
plot(comp)

```

# Changelog

## tna 1.2.1

- The `test` argument of
  [`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md)
  is now `TRUE` by default for backward compatibility.
- Fixed an issue with
  [`bootstrap()`](http://sonsoles.me/tna/reference/bootstrap.md) when
  the model contained weights that were zero.
- Fixed issues with several distance measures in
  [`cluster_sequences()`](http://sonsoles.me/tna/reference/cluster_data.md).

## tna 1.2.0

CRAN release: 2026-02-12

- Added the function
  [`reliability()`](http://sonsoles.me/tna/reference/reliability.md) for
  reliability analysis of `tna` models.
- The
  [`import_onehot()`](http://sonsoles.me/tna/reference/import_onehot.md)
  function has been redesigned with new features.
- Replaced the Z tests and chi-squared tests in
  [`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md)
  with a permutation test.
- Removed the chi-squared test from
  [`plot_mosaic()`](http://sonsoles.me/tna/reference/plot_mosaic.md).
- The `correct` argument of
  [`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md)
  has been renamed to `adjust` to align with other similar arguments.
- Added several new arguments to the `plot` method of `tna` objects for
  plotting heterogeneous networks.
- The `plot` method for `communities` now uses the first available
  method by default.
- The `stringdist` package has been moved to Suggests.
- Added additional checks for missing columns when using tidy selections
  in various functions that were previously silently ignored.

## tna 1.1.0

CRAN release: 2025-10-18

- Added the function
  [`cluster_sequences()`](http://sonsoles.me/tna/reference/cluster_data.md)
  to cluster sequence data using string distance measures of the
  `stringdist` package.
- Added the function
  [`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md)
  to compare the frequencies of patterns between groups for sequence
  data.
- [`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md)
  now accepts a character vector for the `actor` argument.

## tna 1.0.0

CRAN release: 2025-07-15

- CRAN release

## tna 0.7.0

- Added the function
  [`import_onehot()`](http://sonsoles.me/tna/reference/import_onehot.md)
  to read one-hot data.
- The function
  [`plot_sequences()`](http://sonsoles.me/tna/reference/plot_sequences.md)
  gains the argument `ncol` for selecting the number of columns of the
  facets.

## tna 0.6.1

- The `group` column is now automatically removed from `cols` in
  [`group_model()`](http://sonsoles.me/tna/reference/group_model.md).

## tna 0.6.0

- Added a new method
  [`plot_sequences()`](http://sonsoles.me/tna/reference/plot_sequences.md)
  for sequence index plots and state distribution plots.

## tna 0.5.1

- Fixed the handling of missing values in
  [`group_model()`](http://sonsoles.me/tna/reference/group_model.md)
  with `stslist` objects.
- Fixed an issue with
  [`plot.group_tna_permutation()`](http://sonsoles.me/tna/reference/plot.group_tna_permutation.md)
  that resulted in plotting arguments not being passed to `qgraph`.

## tna 0.5.0

CRAN release: 2025-05-26

- Added a new dataset `group_regulation_long`.

## tna 0.4.9

- Switched to `matrix` as the internal data format used by TNA models
  for performance improvements across all functions.

## tna 0.4.8

- Fixed an issue with
  [`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md)
  that resulted in excessive console output.

## tna 0.4.7

- Added the function
  [`import_data()`](http://sonsoles.me/tna/reference/import_data.md) to
  read wide format sequence data into long format.

## tna 0.4.6

- Added the function
  [`plot_frequencies()`](http://sonsoles.me/tna/reference/plot_frequencies.md)
  that can be used to plot the state frequency distribution for both
  `tna` and `group_tna` objects.

## tna 0.4.5

- The function
  [`permutation_test()`](http://sonsoles.me/tna/reference/permutation_test.md)
  is now a method for both ungrouped
  ([`build_model()`](http://sonsoles.me/tna/reference/build_model.md))
  and grouped
  ([`group_model()`](http://sonsoles.me/tna/reference/group_model.md))
  models. For grouped models, the function performs the test between
  every unique pair of groups.
- A new argument `adjust` has been added for
  [`permutation_test()`](http://sonsoles.me/tna/reference/permutation_test.md)
  to optionally adjust p-values using `p.adjust`. By default, the
  p-values are not adjusted (`adjust = "none"`).
- A new argument `groupwise` has been added for
  [`group_model()`](http://sonsoles.me/tna/reference/group_model.md).
  When `FALSE` (the default), scaling methods listed in `scaling` are
  performed globally over the groups. When `TRUE`, the scaling is
  performed within each group instead (this was the default behavior in
  previous versions of the package).
- Added a [`simulate()`](https://rdrr.io/r/stats/simulate.html) method
  for `tna` objects. For models with `type = "relative"`, this function
  simulates sequence data based on the initial probabilities and
  transition probability matrix.

## tna 0.4.4

- The
  [`plot.tna_centralities()`](http://sonsoles.me/tna/reference/plot.tna_centralities.md)
  and
  [`plot.group_tna_centralities()`](http://sonsoles.me/tna/reference/plot.group_tna_centralities.md)
  functions now plot the centralities in the same order as provided in
  the `measures` argument.
- The [`plot.tna()`](http://sonsoles.me/tna/reference/plot.tna.md) and
  [`plot_model()`](http://sonsoles.me/tna/reference/plot_model.md)
  functions now use the median edge weight as the default value for the
  `cut` argument.
- Fixed the `from` and `to` columns in
  [`bootstrap()`](http://sonsoles.me/tna/reference/bootstrap.md) output,
  which were inverted from the true edge direction.
- A plot method has been added for the
  [`bootstrap()`](http://sonsoles.me/tna/reference/bootstrap.md) output,
  which plots the corresponding network where non-significant edges have
  been pruned.

## tna 0.4.3

- The
  [`permutation_test()`](http://sonsoles.me/tna/reference/permutation_test.md)
  function now properly checks that its arguments `x` and `y` can be
  compared.
- The p-value calculations of
  [`permutation_test()`](http://sonsoles.me/tna/reference/permutation_test.md)
  and [`bootstrap()`](http://sonsoles.me/tna/reference/bootstrap.md)
  have been adjusted by adding 1 to both the number of
  permutations/bootstrap samples and the number of extreme events so
  that these estimates are never zero. The documentation has also been
  clarified regarding p-values emphasizing that these are estimates
  only.

## tna 0.4.2

- The
  [`plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md)
  function now supports `negCol` and `posCol` for specifying the color
  of the positive and negative differences in transition and initial
  probabilities.
- The [`plot_mosaic()`](http://sonsoles.me/tna/reference/plot_mosaic.md)
  function now plots the x-axis on the top and rotates the labels 90
  degrees only when there are more than three groups.

## tna 0.4.1

- The `detailed` argument of
  [`estimate_centrality_stability()`](http://sonsoles.me/tna/reference/estimate_centrality_stability.md)
  has been removed. Previously this argument had no effect on the output
  of the function.
- Removed several duplicated entries in the documentation.

## tna 0.4.0

CRAN release: 2025-03-01

- The
  [`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md)
  function now produces an object of class `tna_data`, which can be
  directly used as an argument to
  [`build_model()`](http://sonsoles.me/tna/reference/build_model.md) and
  other methods.
- The
  [`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md)
  function now supports `order` when used together with `time` and
  `actor`.
- The
  [`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md)
  function gains the `unused_fn` argument of
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  to process any extra columns. The default is to keep all columns and
  use the first value.
- Added the function
  [`compare()`](http://sonsoles.me/tna/reference/compare.md) to compare
  `tna` models and weight matrices. This function produces an object of
  class `tna_comparison` which has
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
- Added the function
  [`plot_mosaic()`](http://sonsoles.me/tna/reference/plot_mosaic.md)
  which can be used to produce mosaic plots of transition counts for
  frequency-based transition network models and to contrast the state
  counts between groups.
- Fixed an issue with
  [`plot.tna_communities()`](http://sonsoles.me/tna/reference/plot.tna_communities.md)
  which now checks for the availability of a particular community
  detection method before plotting.
- Made several arguments in the plot methods of the package accessible
  to the user.

## tna 0.3.2

- `event2sequence()` has been renamed to
  [`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md).
  The function is now also more general and can process more date
  formats.
- Added a `method` argument to
  [`bootstrap()`](http://sonsoles.me/tna/reference/bootstrap.md). The
  new default option `"stability"` implements a bootstrapping scheme
  where the edge weights are compared against a range of “consistent”
  weights (see the documentation for details). The old functionality can
  be accessed with `method = "threshold"`.
- Fixed an issue with `permutatation_test()` when `x` and `y` had a
  differing number of columns.
- Community detection methods can now be selected using the `methods`
  argument in
  [`communities()`](http://sonsoles.me/tna/reference/communities.md).
- The [`build_model()`](http://sonsoles.me/tna/reference/build_model.md)
  function has gained the argument `cols` which can be used to subset
  the columns of the data for `stslist` and `data.frame` inputs.
- Removed all `verbose` arguments in favor of
  `options(rlib_message_verbosity = "quiet").` and
  `options(rlib_warning_verbosity = "quiet")`.

## tna 0.3.1

- Fixed an issue when checking the validity of `character` type
  arguments.
- Improved the
  [`bootstrap()`](http://sonsoles.me/tna/reference/bootstrap.md)
  function to determine edge significance based on deviation from the
  observed value, rather than a fixed threshold.
- Added a helper function `event2sequence()` to parse event data into
  sequence data.

## tna 0.3.0

CRAN release: 2024-12-11

- Added support for grouped sequence data (clusters).

## tna 0.2.0

- Added bootstrapping, permutation test, and centrality stability
  functionalities.

## tna 0.1.0

- Initial version.

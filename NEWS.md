# tna 1.0.0
* CRAN release
* The function `plot_sequences()` gains the argument `ncol` for selecting the number of columns of the facets.

# tna 0.6.0

* Added a new method `plot_sequences()` for sequence index plots and state distribution plots.

# tna 0.5.1

* Fixed the handling of missing values in `group_model()` with `stslist` objects.
* Fixed an issue with `plot.group_tna_permutation()` that resulted in plotting arguments not being passed to `qgraph`.

# tna 0.5.0

* Added a new dataset `group_regulation_long`.

# tna 0.4.9

* Switched to `matrix` as the internal data format used by TNA models for performance improvements across all functions.

# tna 0.4.8

* Fixed an issue with `prepare_data()` that resulted in excessive console output.

# tna 0.4.7

* Added the function `import_data()` to read wide format sequence data into long format.

# tna 0.4.6

* Added the function `plot_frequencies()` that can be used to plot the state frequency distribution for both `tna` and `group_tna` objects.

# tna 0.4.5

* The function `permutation_test()` is now a method for both ungrouped (`build_model()`) and grouped (`group_model()`) models. For grouped models, the function performs the test between every unique pair of groups. 
* A new argument `adjust` has been added for `permutation_test()` to optionally adjust p-values using `p.adjust`. By default, the p-values are not adjusted (`adjust = "none"`).
* A new argument `groupwise` has been added for `group_model()`. When `FALSE` (the default), scaling methods listed in `scaling` are performed globally over the groups. When `TRUE`, the scaling is performed within each group instead (this was the default behavior in previous versions of the package).
* Added a `simulate()` method for `tna` objects. For models with `type = "relative"`, this function simulates sequence data based on the initial probabilities and transition probability matrix.

# tna 0.4.4

* The `plot.tna_centralities()` and `plot.group_tna_centralities()` functions now plot the centralities in the same order as provided in the `measures` argument.
* The `plot.tna()` and `plot_model()` functions now use the median edge weight as the default value for the `cut` argument.
* Fixed the `from` and `to` columns in `bootstrap()` output, which were inverted from the true edge direction.
* A plot method has been added for the `bootstrap()` output, which plots the corresponding network where non-significant edges have been pruned.

# tna 0.4.3

* The `permutation_test()` function now properly checks that its arguments `x` and `y` can be compared.
* The p-value calculations of `permutation_test()` and `bootstrap()` have been adjusted by adding 1 to both the number of permutations/bootstrap samples and the number of extreme events so that these estimates are never zero. The documentation has also been clarified regarding p-values emphasizing that these are estimates only.

# tna 0.4.2

* The `plot_compare()` function now supports `negCol` and `posCol` for specifying the color of the positive and negative differences in transition and initial probabilities.
* The `plot_mosaic()` function now plots the x-axis on the top and rotates the labels 90 degrees only when there are more than three groups.

# tna 0.4.1

* The `detailed` argument of `estimate_centrality_stability()` has been removed. Previously this argument had no effect on the output of the function.
* Removed several duplicated entries in the documentation.

# tna 0.4.0

* The `prepare_data()` function now produces an object of class `tna_data`, which can be directly used as an argument to `build_model()` and other methods.
* The `prepare_data()` function now supports `order` when used together with `time` and `actor`.
* The `prepare_data()` function gains the `unused_fn` argument of `tidyr::pivot_wider()` to process any extra columns. The default is to keep all columns and use the first value.
* Added the function `compare()` to compare `tna` models and weight matrices. This function produces an object of class `tna_comparison` which has `print()` and `plot()` methods.
* Added the function `plot_mosaic()` which can be used to produce mosaic plots of transition counts for frequency-based transition network models and to contrast the state counts between groups.
* Fixed an issue with `plot.tna_communities()` which now checks for the availability of a particular community detection method before plotting.
* Made several arguments in the plot methods of the package accessible to the user.

# tna 0.3.2

* `event2sequence()` has been renamed to `prepare_data()`. The function is now also more general and can process more date formats.
* Added a `method` argument to `bootstrap()`. The new default option `"stability"` implements a bootstrapping scheme where the edge weights are compared against a range of "consistent" weights (see the documentation for details). The old functionality can be accessed with `method = "threshold"`.
* Fixed an issue with `permutatation_test()` when `x` and `y` had a differing number of columns.
* Community detection methods can now be selected using the `methods` argument in `communities()`.
* The `build_model()` function has gained the argument `cols` which can be used to subset the columns of the data for `stslist` and `data.frame` inputs.
* Removed all `verbose` arguments in favor of `options(rlib_message_verbosity = "quiet").` and `options(rlib_warning_verbosity = "quiet")`.

# tna 0.3.1

* Fixed an issue when checking the validity of `character` type arguments.
* Improved the `bootstrap()` function to determine edge significance based on
  deviation from the observed value, rather than a fixed threshold.
* Added a helper function `event2sequence()` to parse event data into sequence data.

# tna 0.3.0

* Added support for grouped sequence data (clusters).

# tna 0.2.0

* Added bootstrapping, permutation test, and centrality stability functionalities.

# tna 0.1.0

* Initial version.

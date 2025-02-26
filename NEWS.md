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

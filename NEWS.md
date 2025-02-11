# tna 0.3.2

* `event2sequence()` has been renamed to `prepare_data()`. The function is now also more general and handles more date formats.
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

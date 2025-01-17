# tna 0.3.2

* `event2sequence()` has been renamed to `prepare_data()`. The function is now also more general and handles more date formats.

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

# Build a Grouped Transition Network Analysis Model

This function constructs a transition network analysis (TNA) model for
each group from a given sequence, wide-format dataframe or a mixture
Markov model.

## Usage

``` r
group_model(x, ...)

# Default S3 method
group_model(
  x,
  group,
  type = "relative",
  scaling = character(0L),
  groupwise = FALSE,
  cols = tidyselect::everything(),
  params = list(),
  na.rm = TRUE,
  ...
)

# S3 method for class 'mhmm'
group_model(
  x,
  type = "relative",
  scaling = character(0L),
  groupwise = FALSE,
  params = list(),
  na.rm = TRUE,
  ...
)

# S3 method for class 'tna_clustering'
group_model(
  x,
  type = "relative",
  scaling = character(0L),
  groupwise = FALSE,
  params = list(),
  na.rm = TRUE,
  ...
)

group_tna(x, ...)

group_ftna(x, ...)

group_ctna(x, ...)

group_atna(x, ...)
```

## Arguments

- x:

  An `stslist` object describing a sequence of events or states to be
  used for building the Markov model. The argument `x` also accepts
  `data.frame` objects in wide format, and `tna_data` objects. This can
  also be the output of clustering from
  [`cluster_sequences()`](http://sonsoles.me/tna/reference/cluster_sequences.md).

- ...:

  Ignored.

- group:

  A `vector` indicating the group assignment of each row of the
  data/sequence. Must have the same length as the number of
  rows/sequences of `x`. Alternatively, a single `character` string
  giving the column name of the data that defines the group when `x` is
  a wide format `data.frame` or a `tna_data` object. If not provided,
  each row of the data forms a cluster. Not used when `x` is a mixture
  Markov model or a clustering result.

- type:

  A `character` string describing the weight matrix type. Currently
  supports the following types:

  - `"relative"` for relative frequencies (probabilities, the default)

  - `"frequency"` for frequencies.

  - `"co-occurrence"` for co-occurrences.

  - `"n-gram"` for n-gram transitions. Captures higher-order transitions
    by considering sequences of n states, useful for identifying longer
    patterns.

  - `"gap"` allows transitions between non-adjacent states, with
    transitions weighted by the gap size.

  - `"window"` creates transitions between all states within a sliding
    window, capturing local relationships (several sequences together).

  - `"reverse"` considers the sequences in reverse order (resulting in
    what is called a reply network in some contexts). The resulting
    weight matrix is the transpose of the `"frequency"` option.

  - `"attention"` aggregates all downstream pairs of states with an
    exponential decay for the gap between states. The parameter `lambda`
    can be used to control the decay rate (the default is 1)-

- scaling:

  A `character` vector describing how to scale the weights defined by
  `type`. When a vector is provided, the scaling options are applied in
  the respective order. For example, `c("rank", "minmax")` would first
  compute the ranks, then scale them to the unit interval using min-max
  normalization. An empty vector corresponds to no scaling. Currently
  supports the following options:

  - `"minmax"` performs min-max normalization to scale the weights to
    the unit interval. Note that if the smallest weight is positive, it
    will be zero after scaling.

  - `"max"` Multiplies the weights by the reciprocal of the largest
    weight to scale the weights to the unit interval. This options
    preserves positive ranks, unlike `"minmax"` when all weights are
    positive.

  - `"rank"` Computes the ranks of the weights using
    [`base::rank()`](https://rdrr.io/r/base/rank.html) with
    `ties.method = "average"`.

- groupwise:

  A `logical` value that indicates whether scaling methods should be
  applied by group (`TRUE`) or globally (`FALSE`, the default).

- cols:

  An `expression` giving a tidy selection of the columns that should be
  considered as sequence data. The default is all columns. The columns
  are automatically determined for `tna_data` objects. The `group`
  column is automatically removed from these columns if provided.

- params:

  A `list` of additional arguments for models of specific `type`. The
  potential elements of this list are:

  - `n_gram`: An `integer` for n-gram transitions specifying the number
    of adjacent events. The default value is 2.

  - `max_gap`: An `integer` for the gap-allowed transitions specifying
    the largest allowed gap size. The default is 1.

  - `window_size`: An `integer` for the sliding window transitions
    specifying the window size. The default is 2.

  - `weighted`: A `logical` value. If `TRUE`, the transitions are
    weighted by the inverse of the sequence length. Can be used for
    frequency, co-occurrence and reverse model types. The default is
    `FALSE`.

  - `direction`: A `character` string specifying the direction of
    attention for models of `type = "attention"`. The available options
    are `"backward"`, `"forward"`, and `"both"`, for backward attention,
    forward attention, and bidirectional attention, respectively. The
    default is `"forward"`.

  - `decay`: A `function` that specifies the decay of the weights
    between two time points at a specific distance. The function should
    take three arguments: `i`, `j` and `lambda`, where `i` and `j` are
    `numeric` vectors of time values, and `lambda` is a `numeric` value
    for the decay rate. The function should return a `numeric` vector of
    weights. The default is
    `function(i, j, lambda) exp(-abs(i - j) / lambda)`.

  - `lambda`: A `numeric` value for the decay rate. The default is 1.

  - `time`: A `matrix` or a `data.frame` providing the time values for
    each sequence and at time index. For `tna_data` objects, this can
    also be a logical value, where `TRUE` will use the `time_data`
    element of `x` for the time values. `Date` values are converted to
    `numeric`.

  - `duration`: A `matrix` or a `data.frame` providing the time spent in
    each state for each sequence and time index. This is an alternative
    to `time`.

- na.rm:

  A `logical` value that determines if observations with `NA` value in
  `group` be removed. If `FALSE`, an additional category for `NA` values
  will be added. The default is `FALSE` and a warning is issued if `NA`
  values are detected.

## Value

An object of class `group_tna` which is a `list` containing one element
per cluster. Each element is a `tna` object.

## See also

Cluster-related functions
[`communities()`](http://sonsoles.me/tna/reference/communities.md),
[`mmm_stats()`](http://sonsoles.me/tna/reference/mmm_stats.md),
[`rename_groups()`](http://sonsoles.me/tna/reference/rename_groups.md)

## Examples

``` r
# Manually specified groups
group <- c(rep("High", 1000), rep("Low", 1000))
model <- group_model(group_regulation, group = group)

# Groups defined by a mixed Markov model
model <- group_model(engagement_mmm)

model <- group_tna(group_regulation, group = gl(2, 1000))

model <- group_ftna(group_regulation, group = gl(2, 1000))

model <- group_ctna(group_regulation, group = gl(2, 1000))

model <- group_atna(group_regulation, group = gl(2, 1000))
```

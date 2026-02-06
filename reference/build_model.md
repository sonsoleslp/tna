# Build a Transition Network Analysis Model

Construct a transition network analysis (TNA) model from sequence data.
The function takes a data set of sequence of events or states as input
and builds a TNA model. It extracts the edge weights and initial
probabilities from the data along with the state labels. The function
also accepts weight matrices and initial state probabilities directly.

## Usage

``` r
build_model(
  x,
  type = "relative",
  scaling = character(0L),
  cols = tidyselect::everything(),
  params = list(),
  inits,
  begin_state,
  end_state
)

# Default S3 method
build_model(
  x,
  type = "relative",
  scaling = character(0L),
  cols = tidyselect::everything(),
  params = list(),
  inits,
  begin_state,
  end_state
)

# S3 method for class 'matrix'
build_model(
  x,
  type = "relative",
  scaling = character(0L),
  cols = tidyselect::everything(),
  params = list(),
  inits,
  begin_state,
  end_state
)

# S3 method for class 'stslist'
build_model(
  x,
  type = "relative",
  scaling = character(0L),
  cols = tidyselect::everything(),
  params = list(),
  inits,
  begin_state,
  end_state
)

# S3 method for class 'data.frame'
build_model(
  x,
  type = "relative",
  scaling = character(0L),
  cols = tidyselect::everything(),
  params = list(),
  inits,
  begin_state,
  end_state
)

# S3 method for class 'tna_data'
build_model(
  x,
  type = "relative",
  scaling = character(0L),
  cols = tidyselect::everything(),
  params = list(),
  inits,
  begin_state,
  end_state
)

# S3 method for class 'tsn'
build_model(
  x,
  type = "relative",
  scaling = character(0L),
  cols = tidyselect::everything(),
  params = list(),
  inits,
  begin_state,
  end_state
)

# S3 method for class 'tsn_ews'
build_model(
  x,
  type = "relative",
  scaling = character(0L),
  cols = tidyselect::everything(),
  params = list(),
  inits,
  begin_state,
  end_state
)

tna(x, ...)

ftna(x, ...)

ctna(x, ...)

atna(x, ...)

tsn(x, ...)
```

## Arguments

- x:

  A `stslist` (from `TraMineR`), `data.frame`, a `matrix`, or a
  `tna_data` object (see
  [`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md)).
  For `stslist` and `data.frame` objects `x` should describe a sequence
  of events or states to be used for building the Markov model. If `x`
  is a matrix, it is assumed that the element on row `i` and column `j`
  is the weight of the edge representing the transition from state `i`
  to state `j`. If `x` is a `data.frame`, then it must be in wide format
  (see `cols` on how to define columns for the time points).

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

- cols:

  An `expression` giving a tidy selection of columns that should be
  considered as sequence data. By default, all columns are used. Ignored
  for `matrix`, `tna_data` and `tsn` type `x`.

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

- inits:

  An optional `numeric` vector of initial state probabilities for each
  state. The vector will be scaled to unity. Ignored if `x` is not a
  `matrix`.

- begin_state:

  A `character` string for an additional begin state. This state is
  added as the first observation for every sequence to signify the
  beginning of the sequence

- end_state:

  A `character` string for an additional end state. This state is added
  as the last observation for every sequence to siginify the end of the
  sequence.

- ...:

  Ignored. For the `build_model` aliases (e.g., `tna`), this argument
  matches the actual arguments to `build_model` beside `x`.

## Value

An object of class `tna` which is a `list` containing the following
elements:

- `weights`: An adjacency `matrix` of the model (weight matrix).

- `inits`: A `numeric` vector of initial values for each state. For
  `matrix` type `x`, this element will be `NULL` if `inits` is not
  directly provided

- `labels`: A `character` vector of the state labels, or `NULL` if there
  are no labels.

- `data`: The original sequence data that has been converted to an
  internal format used by the package when `x` is a `stslist` or a
  `data.frame` object. Otherwise `NULL`.

## See also

Basic functions
[`hist.group_tna()`](http://sonsoles.me/tna/reference/hist.group_tna.md),
[`hist.tna()`](http://sonsoles.me/tna/reference/hist.tna.md),
[`plot.group_tna()`](http://sonsoles.me/tna/reference/plot.group_tna.md),
[`plot.tna()`](http://sonsoles.me/tna/reference/plot.tna.md),
[`plot_frequencies()`](http://sonsoles.me/tna/reference/plot_frequencies.md),
[`plot_frequencies.group_tna()`](http://sonsoles.me/tna/reference/plot_frequencies.group_tna.md),
[`plot_mosaic()`](http://sonsoles.me/tna/reference/plot_mosaic.md),
[`plot_mosaic.group_tna()`](http://sonsoles.me/tna/reference/plot_mosaic.group_tna.md),
[`plot_mosaic.tna_data()`](http://sonsoles.me/tna/reference/plot_mosaic.tna_data.md),
[`print.group_tna()`](http://sonsoles.me/tna/reference/print.group_tna.md),
[`print.summary.group_tna()`](http://sonsoles.me/tna/reference/print.summary.group_tna.md),
[`print.summary.tna()`](http://sonsoles.me/tna/reference/print.summary.tna.md),
[`print.tna()`](http://sonsoles.me/tna/reference/print.tna.md),
[`summary.group_tna()`](http://sonsoles.me/tna/reference/summary.group_tna.md),
[`summary.tna()`](http://sonsoles.me/tna/reference/summary.tna.md),
[`tna-package`](http://sonsoles.me/tna/reference/tna-package.md)

## Examples

``` r
model <- build_model(group_regulation)
print(model)
#> State Labels : 
#> 
#>    adapt, cohesion, consensus, coregulate, discuss, emotion, monitor, plan, synthesis 
#> 
#> Transition Probability Matrix :
#> 
#>                   adapt   cohesion  consensus coregulate    discuss    emotion
#> adapt      0.0000000000 0.27308448 0.47740668 0.02161100 0.05893910 0.11984283
#> cohesion   0.0029498525 0.02713864 0.49793510 0.11917404 0.05958702 0.11563422
#> consensus  0.0047400853 0.01485227 0.08200348 0.18770738 0.18802338 0.07268131
#> coregulate 0.0162436548 0.03604061 0.13451777 0.02335025 0.27360406 0.17208122
#> discuss    0.0713743356 0.04758289 0.32118451 0.08428246 0.19488737 0.10579600
#> emotion    0.0024673951 0.32534367 0.32040888 0.03419105 0.10186817 0.07684173
#> monitor    0.0111653873 0.05582694 0.15910677 0.05792045 0.37543615 0.09071877
#> plan       0.0009745006 0.02517460 0.29040117 0.01721618 0.06789021 0.14682475
#> synthesis  0.2346625767 0.03374233 0.46625767 0.04447853 0.06288344 0.07055215
#>               monitor       plan   synthesis
#> adapt      0.03339882 0.01571709 0.000000000
#> cohesion   0.03303835 0.14100295 0.003539823
#> consensus  0.04661084 0.39579712 0.007584137
#> coregulate 0.08629442 0.23908629 0.018781726
#> discuss    0.02227284 0.01164262 0.140976968
#> emotion    0.03630596 0.09975326 0.002819880
#> monitor    0.01814375 0.21563154 0.016050244
#> plan       0.07552379 0.37420822 0.001786584
#> synthesis  0.01226994 0.07515337 0.000000000
#> 
#> Initial Probabilities : 
#> 
#>      adapt   cohesion  consensus coregulate    discuss    emotion    monitor 
#>     0.0115     0.0605     0.2140     0.0190     0.1755     0.1515     0.1440 
#>       plan  synthesis 
#>     0.2045     0.0195 

model <- tna(group_regulation)

model <- ftna(group_regulation)

model <- ctna(group_regulation)

model <- atna(group_regulation)
```

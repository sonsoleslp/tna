# Changelog

## tna 1.5.8

- [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md):
  comparing two models with scaled edge weights gives a message (class
  `tna_message_scaling`). Each network is scaled on its own, so
  comparing scaled weights is not warranted; the test still runs.

## tna 1.5.7

- [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md):
  the CS-coefficient is the largest passing proportion, whatever the
  order of `drop_prop`.
- [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md):
  `adjust` corrects over the edges tested (present in either network),
  not every cell.

## tna 1.5.6

- [`group_model()`](https://sonsoles.me/tna/reference/group_model.md),
  [`group_tna()`](https://sonsoles.me/tna/reference/group_model.md),
  [`group_ftna()`](https://sonsoles.me/tna/reference/group_model.md) and
  [`group_ctna()`](https://sonsoles.me/tna/reference/group_model.md):
  window parameters in `params` and windows recorded by
  [`import_onehot()`](https://sonsoles.me/tna/reference/import_onehot.md)
  had no effect, so a grouped model of the same call as
  [`tna()`](https://sonsoles.me/tna/reference/build_model.md) gave
  different weights. They are now used, as in
  [`build_model()`](https://sonsoles.me/tna/reference/build_model.md).

## tna 1.5.5

- NEWS.md wording.

## tna 1.5.4

Corrections to numerical results. Results from the functions below can
differ from earlier versions.

- [`bootstrap_cliques()`](https://sonsoles.me/tna/reference/bootstrap_cliques.md)
  resampled from the first sequences only, as many as there were cliques
  (for example, 22 of 2,000 sequences), so its p-values and confidence
  intervals were invalid. Each bootstrap sample now draws as many
  sequences as the data has.
- [`reliability()`](https://sonsoles.me/tna/reference/reliability.md)
  counted the transitions of the model it was given for every requested
  type, so `types` only switched the row normalization on or off:
  `"co-occurrence"`, `"gap"`, `"attention"`, and the other types all
  gave the frequency result. Each type is now estimated as
  [`build_model()`](https://sonsoles.me/tna/reference/build_model.md)
  defines it. The default `types` is the type of the model (it was
  `"relative"` for every model).
- [`compare()`](https://sonsoles.me/tna/reference/compare.md): the
  Canberra distance omitted the edges present in only one network (each
  contributes 1), and the distance correlation was reported squared. The
  states of the two networks are matched by name, so two networks
  listing the same states in a different order are compared correctly;
  networks with different states, or a matrix without state names, are
  an error of class `tna_error_states` (they were compared by position).
- [`summary()`](https://rdrr.io/r/base/summary.html) of a model reported
  the standard deviation of the in-strength as that of the out-strength
  and vice versa (the means are equal by construction).
- [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md):
  a subsample in which the correlation is undefined (the measure is
  constant in it) was dropped from the share of draws above `threshold`,
  which could raise the CS-coefficient. It now counts as a draw below
  the threshold.
- `BetweennessRSP` counted the correction for walks absorbed at a target
  once for every state, also for states that cannot reach it. In a
  network that is not strongly connected (for example, with a state that
  is never entered) the values were wrong. Only sources that reach the
  target are counted; strongly connected networks are unchanged.
- [`simulate()`](https://sonsoles.me/tna/reference/simulate.md):
  `zero_row` was replaced by the value of `format`, so
  `zero_row = "uniform"` had no effect for frequency models, and states
  that are never left (such as a state that only ends sequences) moved
  arbitrarily in relative models. `zero_row` now applies to both model
  types. `max_len = 1` and `na_range` values with equal ends or a zero
  lower end no longer fail.
- `prune(method = "disparity")` set the absent edges of a state with no
  outgoing or no incoming edges to `NA`. They stay 0.
- [`build_model()`](https://sonsoles.me/tna/reference/build_model.md) of
  a wide `data.frame` discarded `window_size`, `window_span`, and
  `windowed` given in `params`. They are now used.
- Backward (and bidirectional) attention paired every time point with
  itself, adding a self-loop of full weight at every observation. Only
  earlier time points are attended to, as forward attention uses only
  later ones.
- [`group_model()`](https://sonsoles.me/tna/reference/group_model.md)
  kept the unused factor levels of `group`, so a level with no sequences
  became an empty network with `NaN` initial probabilities. Unused
  levels are dropped.
- [`print()`](https://rdrr.io/r/base/print.html) of a model without
  initial probabilities (for example,
  [`build_model()`](https://sonsoles.me/tna/reference/build_model.md) of
  a matrix) failed.
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) of a model
  failed when `node_size` or `vsize` was given, and
  `plot_sequences(show_n = FALSE)` failed.
- [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)
  and
  [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md)
  took the observed network from the weights of the model but rebuilt
  the resamples from the sequence data, so a pruned model was compared
  with unpruned resamples (a model tested against its own pruned version
  showed significant differences). The observed network is now rebuilt
  from the sequence data too, as in
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md).
  Results for unpruned models are unchanged.
- `end_state` was not added when no sequence filled the last column of
  the data (for example, in a subset of the rows, or with an empty
  trailing column). It is now always added after the last observation.
- [`build_model()`](https://sonsoles.me/tna/reference/build_model.md) of
  a `tsn` object ordered the time points by first appearance in the
  data, not by time, which could reverse transitions. They are now
  ordered by time.
- `cliques(sum_weights = TRUE)` applied `threshold` to each direction
  before summing them, so a pair whose summed weight exceeded the
  threshold could be missed. The threshold now applies to the sum, as
  documented. This also affects
  [`bootstrap_cliques()`](https://sonsoles.me/tna/reference/bootstrap_cliques.md)
  with a positive `threshold`.
- `import_onehot(window_type = "sliding")` widened the windows of width
  3 or more (each lag was applied to the already widened rows), and
  treated width 1 as width 2. Each window now covers exactly
  `window_size` rows, and the incomplete windows at the start are
  dropped. Width 2 is unchanged.
- [`plot_associations()`](https://sonsoles.me/tna/reference/plot_associations.md)
  drew every residual in the reverse direction (from target to source).
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) of grouped
  communities colored every state of a group with the same color. Each
  community now has its own color, as for a single group.

## tna 1.5.3

- Documentation revision. The help pages describe what the functions
  return as they do now:
  [`summary()`](https://rdrr.io/r/base/summary.html) of a model (a table
  of 13 metrics, not a list),
  [`communities()`](https://sonsoles.me/tna/reference/communities.md),
  [`cliques()`](https://sonsoles.me/tna/reference/cliques.md),
  [`compare()`](https://sonsoles.me/tna/reference/compare.md),
  [`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md)
  (the default `sub` is `1:5`),
  [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md),
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md) (with
  its `model` element),
  [`reliability()`](https://sonsoles.me/tna/reference/reliability.md),
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md),
  [`import_data()`](https://sonsoles.me/tna/reference/import_data.md),
  [`import_onehot()`](https://sonsoles.me/tna/reference/import_onehot.md),
  [`simulate()`](https://sonsoles.me/tna/reference/simulate.md),
  [`plot_compare()`](https://sonsoles.me/tna/reference/plot_compare.md),
  and
  [`pruning_details()`](https://sonsoles.me/tna/reference/pruning_details.md).
  The default of `na.rm` in
  [`group_model()`](https://sonsoles.me/tna/reference/group_model.md) is
  `TRUE`, as in the code, and a missing `group` is an error.
  [`prune()`](https://sonsoles.me/tna/reference/prune.md) documents its
  four methods, including that only `"threshold"` and `"lowest"` keep
  the network weakly connected, and
  [`reprune()`](https://sonsoles.me/tna/reference/reprune.md) has its
  own help page.
- Examples no longer reach into result objects with `$` or subset the
  data with brackets; `print(<tna_data>)` examples use the `data`
  argument (they used `which`, which was ignored).
  [`centralities()`](https://sonsoles.me/tna/reference/centralities.md),
  [`prune()`](https://sonsoles.me/tna/reference/prune.md), and
  [`cluster_sequences()`](https://sonsoles.me/tna/reference/cluster_data.md)
  cite the methods they implement.
- Vignettes and articles: the tables of the getting-started vignette,
  the grouped-sequences vignette, and the README use
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) instead of
  `$weights`, `$inits`, and `$centralities$stats`; the clusters are
  built with `group_tna(<clustering>)`;
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md) calls
  no longer pass a `threshold` that the default method ignores; the time
  threshold of
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)
  is in seconds; the frequency and attention articles restore their
  [`options()`](https://rdrr.io/r/base/options.html), and the frequency
  article compares the scaled weights with
  [`compare()`](https://sonsoles.me/tna/reference/compare.md).

## tna 1.5.2

- [`summary()`](https://rdrr.io/r/base/summary.html) of a permutation
  test returns the centrality results with `what = "centralities"` (the
  edges remain the default).
- New [`summary()`](https://rdrr.io/r/base/summary.html) method for
  [`reliability()`](https://sonsoles.me/tna/reference/reliability.md)
  results: all 22 split-half metrics, with their category, as a tidy
  table ([`print()`](https://rdrr.io/r/base/print.html) shows four).
- New article “Validating a transition network” covering
  [`markov_order()`](https://sonsoles.me/tna/reference/markov_order.md),
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md) and
  bootstrap pruning,
  [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
  [`reliability()`](https://sonsoles.me/tna/reference/reliability.md),
  [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)
  (including edge betweenness),
  [`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md),
  and the `actor` argument for nested data.
- Every vignette and article ends with a list of the package articles,
  the TNA tutorials and book chapters, and the Dynalytics resources
  (package sites, tools, CarmNotes, and selected articles).

## tna 1.5.1

- Small examples on a subset of the data come before the `\donttest{}`
  blocks of
  [`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md),
  [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
  [`plot_frequencies()`](https://sonsoles.me/tna/reference/plot_frequencies.md)
  for groups, and the print, plot and summary methods of sequence
  comparisons and stability results. The
  [`as.igraph()`](https://r.igraph.org/reference/as.igraph.html) methods
  have examples.
- [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)
  for groups and the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods of
  bootstrap and reliability results document what they return.
- The vignette “A showcase of the main tna functions” restores the
  [`options()`](https://rdrr.io/r/base/options.html) it changes.
- The plots of the README are no longer part of the package, which
  reduces the installed size from 6.3 to 4.1 MB.

## tna 1.5.0

- [`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md)
  now rejects arguments it does not use. A misspelled argument such as
  `correction = "fdr"` (the argument is `adjust`) was silently ignored
  and the default Bonferroni correction applied; the vignette on grouped
  sequences had this mistake and now uses `adjust = "fdr"` as its text
  describes.

- Fixed
  [`build_model()`](https://sonsoles.me/tna/reference/build_model.md)
  and
  [`group_model()`](https://sonsoles.me/tna/reference/group_model.md)
  for data with a single time point: the sequences were joined into one
  row, which created transitions that do not exist (and
  [`group_model()`](https://sonsoles.me/tna/reference/group_model.md)
  failed).

- Fixed
  [`group_model()`](https://sonsoles.me/tna/reference/group_model.md)
  and [`group_tna()`](https://sonsoles.me/tna/reference/group_model.md)
  with a `character` `group`: when the data did not start with the
  alphabetically first group, every network was labelled with another
  group’s name. Groups keep their order of first appearance and now hold
  their own sequences.

- Fixed scaling across groups (`groupwise = FALSE`, the default)
  dropping the state names of the weight matrices, which made
  [`communities()`](https://sonsoles.me/tna/reference/communities.md)
  and [`compare()`](https://sonsoles.me/tna/reference/compare.md) fail.

- Fixed
  [`centralities()`](https://sonsoles.me/tna/reference/centralities.md)
  naming the states `1`, `2`, … instead of their labels when the only
  measures requested were `Diffusion` or `BetweennessRSP` (whose
  computations return unnamed values); the centrality table of
  `permutation_test(measures = "Diffusion")` had the same problem.

- New [`summary()`](https://rdrr.io/r/base/summary.html) methods for
  `tna_permutation`, `group_tna_permutation`, `tna_stability`, and
  `group_tna_stability` objects return tidy tables: one row per edge
  (`from`, `to`, `diff_true`, `effect_size`, `p_value`) and one row per
  measure (`measure`, `cs_coefficient`).

- Permutation effect sizes are `NA` where the permuted differences do
  not vary, instead of `Inf` or `NaN`
  ([`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)
  for edges and centralities, and
  [`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md)).
  Whether they vary is judged against the magnitude of the compared
  values, so the rule does not depend on the measurement scale.

- Grouped models with scaled edge weights (`group_model(scaling = )`,
  with either value of `groupwise`) and the networks they contain are no
  longer resampled:
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md),
  [`bootstrap_cliques()`](https://sonsoles.me/tna/reference/bootstrap_cliques.md),
  [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md),
  [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
  [`reliability()`](https://sonsoles.me/tna/reference/reliability.md),
  and
  [`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md)
  raise an error of class `tna_error_scaling`. Scaling across groups
  depends on every group, so a resample of one group cannot reproduce
  it. Before,
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md) and
  [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)
  refused such a group as a whole, but
  [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md)
  on the group, and every resampling function on one of its networks
  (for example `bootstrap(model[[1]])`), rebuilt the resamples without
  the scaling and compared them with the scaled network. Resampling of
  single scaled models (`tna(x, scaling = )`) is unchanged.

- Fixed `centralities(measures = "BetweennessRSP")` returning a single
  row for a network with a state that has no outgoing transitions;
  `BetweennessRSP` is `NA` for every state there.

- [`summary()`](https://rdrr.io/r/base/summary.html) of an
  [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md)
  result keeps its columns when nothing could be estimated, and a
  grouped summary no longer fails when one group has no results.

- New
  [`markov_order()`](https://sonsoles.me/tna/reference/markov_order.md)
  tests whether sequences are described by a first-order Markov chain,
  the assumption of a transition network, or depend on states further
  back. Each order is tested against the order below with the
  likelihood-ratio statistic (Anderson & Goodman, 1957) and a
  conditional permutation test that shuffles the next states within each
  context. The orders are tested in sequence, and the results also
  report the log-likelihood, AIC, and BIC of the multi-order model of
  each order (Scholtes, 2017). Methods for `tna` and `group_tna`
  objects, with [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws two
  panels: the log-likelihood, AIC, and BIC across the orders with the
  selected order marked, and the permutation null density of each order
  with the observed statistic (`panel = "ic"` or `"permutation"` draws
  one; unknown arguments are errors). Because the tests stop at the
  first order that does not reject, a dependence that skips an order is
  not selected: a warning of class `tna_warning_markov_order` and a
  printed note flag a higher order whose test rejects and AIC or BIC
  choosing another order. The same warning is raised when `iter` is too
  small for any p-value to fall below `level`.

- [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md)
  gains the argument `edges`: with `edges = TRUE`, the stability of the
  edge weights is estimated alongside the centralities and returned as
  the element `Edges`. Only the edges present in the network are
  compared, because an edge absent from the data cannot appear in a
  subsample and would only add agreeing zeros.

- `Diffusion` centrality is now always normalized to the range from 0 to
  1 (min-max), whatever the value of `normalize`, in
  [`centralities()`](https://sonsoles.me/tna/reference/centralities.md)
  and in everything that uses it (`permutation_test(measures = )`,
  [`compare()`](https://sonsoles.me/tna/reference/compare.md),
  [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
  plots). Its raw values grow with powers of the edge weights and exceed
  `1e30` for frequency models. Where diffusion does not vary across the
  states (a relative model with `loops = TRUE`), it is `NaN`.

- Fixed resampling of edge betweenness networks. The resampling
  functions rebuilt a model from
  [`betweenness_network()`](https://sonsoles.me/tna/reference/betweenness_network.md)
  with all weights equal to zero, so
  [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)
  gave the smallest possible p-value and an infinite effect size to
  every edge whose betweenness differed,
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md)
  returned no edges, every CS-coefficient of
  [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md)
  was 0, and
  [`reliability()`](https://sonsoles.me/tna/reference/reliability.md)
  compared empty networks.
  [`betweenness_network()`](https://sonsoles.me/tna/reference/betweenness_network.md)
  now records the model it was built from. In
  [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md),
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md), and
  [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
  every resample rebuilds that model and recomputes edge betweenness, so
  [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)
  on two edge betweenness networks tests their differences in edge
  betweenness;
  [`reliability()`](https://sonsoles.me/tna/reference/reliability.md)
  uses the transitions of that model.
  [`betweenness_network()`](https://sonsoles.me/tna/reference/betweenness_network.md)
  also has a method for `group_tna` objects, so
  `permutation_test(betweenness_network(models))` compares edge
  betweenness between all groups in one call.

  Edge betweenness networks created by earlier versions must be rebuilt
  to be resampled, and
  [`betweenness_network()`](https://sonsoles.me/tna/reference/betweenness_network.md)
  refuses a network that is already an edge betweenness network.

## tna 1.4.0

- New `actor` argument for sequences nested in actors (for example,
  several sessions per student) in every resampling method:
  [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md),
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md),
  [`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md),
  [`reliability()`](https://sonsoles.me/tna/reference/reliability.md),
  [`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md),
  and `prune(method = "bootstrap")` through
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md).
  `actor` is a column name of the data the model was built from, or a
  vector with one identifier per sequence. With one sequence per actor,
  every method gives the same result as without `actor`.
  - Permutation tests reassign whole actors between groups and shuffle
    the sequences of actors present in several groups within the actor
    (Good, 2005). A warning is given when the actors allow too few
    distinct permutations to reach the significance level.
  - Bootstraps resample whole actors with replacement (cluster
    bootstrap; Davison & Hinkley, 1997).
  - Case-dropping
    ([`estimate_cs()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md))
    drops whole actors, and split-half reliability puts all sequences of
    an actor in the same half.
- Models keep the non-sequence columns of their data (one row per
  sequence), so `actor` can name any of them. This includes models built
  from
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)
  output, grouped models, and
  [`cluster_data()`](https://sonsoles.me/tna/reference/cluster_data.md)
  of
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)
  output. With `concat > 1`, the columns that are constant within each
  concatenated block are kept. Grouped models record the rows of each
  group, so an `actor` vector for the full data applies to every group,
  including groups from mixture models.

## tna 1.3.1

CRAN release: 2026-09-28

- Fixed
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)
  emitting a dplyr grouping message when computing sessions per user;
  the summarise now drops its grouping explicitly.

## tna 1.3.0

- Fixed case-insensitive argument matching to preserve canonical option
  names, allowing
  [`cluster_data()`](https://sonsoles.me/tna/reference/cluster_data.md)
  and
  [`cluster_sequences()`](https://sonsoles.me/tna/reference/cluster_data.md)
  to pass `"ward.D"` and `"ward.D2"` correctly to
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html).
- Made state ordering deterministic across system locales for individual
  and grouped models.
- Fixed numerical instability in randomized-shortest-path betweenness
  and prevented normalization from amplifying floating-point noise in
  constant centrality measures.
- Added deterministic timezone handling to
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md):
  UTC is the default, explicit offsets are honored, fractional seconds
  are retained, and offset-free timestamps can use a user-specified
  Olson timezone.
- `prepare_data(time_threshold = FALSE)` now disables gap-based session
  splitting while retaining timestamp parsing and event ordering.
- [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)
  now supports explicit session columns and collision-safe composite
  actor/session identities, including high-cardinality groupings.
  Disabled time splitting now bypasses gap computation and keeps
  missing-time events in their actor-session sequence.
- Documented the `time_data` component returned by
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md).
- [`prune()`](https://sonsoles.me/tna/reference/prune.md) now warns
  about inactive method-specific arguments and treats a supplied
  bootstrap object as a request for bootstrap pruning.
- Added reproducible `seed` arguments to network and clique
  bootstrapping.
- Added weighted PageRank to
  [`centralities()`](https://sonsoles.me/tna/reference/centralities.md).
- [`plot.tna_permutation()`](https://sonsoles.me/tna/reference/plot.tna_permutation.md)
  now dispatches to the dedicated permutation renderer of
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html),
  so permutation-specific options such as `show_nonsig`, `show_stars`,
  `show_effect`, `edge_nonsig_color`, `edge_nonsig_style`, and
  `edge_nonsig_alpha` are honored.
- Fixed qgraph-style plotting arguments (such as `edge.label.cex`) being
  ignored by matrix-based network plots (e.g. permutation and disparity
  plots).
- Added a `bins` argument to
  [`plot.tna_reliability()`](https://sonsoles.me/tna/reference/plot.tna_reliability.md)
  for controlling the number of histogram bins.

## tna 1.2.3

CRAN release: 2026-04-26

- Resolved CRAN warning “Data files with namespace references not in the
  recursive strong package dependencies” by rebuilding `engagement_mmm`
  as a `tna_mmm` object that carries no foreign class references. The
  new sibling S3 methods
  [`group_model.tna_mmm()`](https://sonsoles.me/tna/reference/group_model.md)
  and
  [`mmm_stats.tna_mmm()`](https://sonsoles.me/tna/reference/mmm_stats.md)
  preserve all existing example behaviour without requiring `seqHMM` at
  example-run time. Real `mhmm` objects from `seqHMM` continue to work
  via the original `*.mhmm` methods.
- Added
  [`random_tna()`](https://sonsoles.me/tna/reference/random_tna.md),
  [`random_group_tna()`](https://sonsoles.me/tna/reference/random_group_tna.md),
  [`random_tna_mmm()`](https://sonsoles.me/tna/reference/random_tna_mmm.md),
  and
  [`list_random_state_pools()`](https://sonsoles.me/tna/reference/list_random_state_pools.md)
  for constructing synthetic TNA models with no external dependencies.
- Lowered the minimum required R version from 4.4.0 to 4.1.0 (the floor
  set by the native `|>` pipe used in the package).

## tna 1.2.2

CRAN release: 2026-04-15

- Fix bug in `plot.tna_sequence_comparison`

## tna 1.2.1

- The `test` argument of
  [`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md)
  is now `TRUE` by default for backward compatibility.
- Fixed an issue with
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md) when
  the model contained weights that were zero.
- Fixed issues with several distance measures in
  [`cluster_sequences()`](https://sonsoles.me/tna/reference/cluster_data.md).
- Fixed edge ordering in
  [`betweenness_network()`](https://sonsoles.me/tna/reference/betweenness_network.md).
- The packages no longer depends on `qgraph`.
- Network plotting functionality now relies on `cograph` instead.

## tna 1.2.0

CRAN release: 2026-02-12

- Added the function
  [`reliability()`](https://sonsoles.me/tna/reference/reliability.md)
  for reliability analysis of `tna` models.
- The
  [`import_onehot()`](https://sonsoles.me/tna/reference/import_onehot.md)
  function has been redesigned with new features.
- Replaced the Z tests and chi-squared tests in
  [`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md)
  with a permutation test.
- Removed the chi-squared test from
  [`plot_mosaic()`](https://sonsoles.me/tna/reference/plot_mosaic.md).
- The `correct` argument of
  [`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md)
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
  [`cluster_sequences()`](https://sonsoles.me/tna/reference/cluster_data.md)
  to cluster sequence data using string distance measures of the
  `stringdist` package.
- Added the function
  [`compare_sequences()`](https://sonsoles.me/tna/reference/compare_sequences.md)
  to compare the frequencies of patterns between groups for sequence
  data.
- [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)
  now accepts a character vector for the `actor` argument.

## tna 1.0.0

CRAN release: 2025-07-15

- CRAN release

## tna 0.7.0

- Added the function
  [`import_onehot()`](https://sonsoles.me/tna/reference/import_onehot.md)
  to read one-hot data.
- The function
  [`plot_sequences()`](https://sonsoles.me/tna/reference/plot_sequences.md)
  gains the argument `ncol` for selecting the number of columns of the
  facets.

## tna 0.6.1

- The `group` column is now automatically removed from `cols` in
  [`group_model()`](https://sonsoles.me/tna/reference/group_model.md).

## tna 0.6.0

- Added a new method
  [`plot_sequences()`](https://sonsoles.me/tna/reference/plot_sequences.md)
  for sequence index plots and state distribution plots.

## tna 0.5.1

- Fixed the handling of missing values in
  [`group_model()`](https://sonsoles.me/tna/reference/group_model.md)
  with `stslist` objects.
- Fixed an issue with
  [`plot.group_tna_permutation()`](https://sonsoles.me/tna/reference/plot.group_tna_permutation.md)
  that resulted in plotting arguments not being passed to `qgraph`.

## tna 0.5.0

CRAN release: 2025-05-26

- Added a new dataset `group_regulation_long`.

## tna 0.4.9

- Switched to `matrix` as the internal data format used by TNA models
  for performance improvements across all functions.

## tna 0.4.8

- Fixed an issue with
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)
  that resulted in excessive console output.

## tna 0.4.7

- Added the function
  [`import_data()`](https://sonsoles.me/tna/reference/import_data.md) to
  read wide format sequence data into long format.

## tna 0.4.6

- Added the function
  [`plot_frequencies()`](https://sonsoles.me/tna/reference/plot_frequencies.md)
  that can be used to plot the state frequency distribution for both
  `tna` and `group_tna` objects.

## tna 0.4.5

- The function
  [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)
  is now a method for both ungrouped
  ([`build_model()`](https://sonsoles.me/tna/reference/build_model.md))
  and grouped
  ([`group_model()`](https://sonsoles.me/tna/reference/group_model.md))
  models. For grouped models, the function performs the test between
  every unique pair of groups.
- A new argument `adjust` has been added for
  [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)
  to optionally adjust p-values using `p.adjust`. By default, the
  p-values are not adjusted (`adjust = "none"`).
- A new argument `groupwise` has been added for
  [`group_model()`](https://sonsoles.me/tna/reference/group_model.md).
  When `FALSE` (the default), scaling methods listed in `scaling` are
  performed globally over the groups. When `TRUE`, the scaling is
  performed within each group instead (this was the default behavior in
  previous versions of the package).
- Added a [`simulate()`](https://sonsoles.me/tna/reference/simulate.md)
  method for `tna` objects. For models with `type = "relative"`, this
  function simulates sequence data based on the initial probabilities
  and transition probability matrix.

## tna 0.4.4

- The
  [`plot.tna_centralities()`](https://sonsoles.me/tna/reference/plot.tna_centralities.md)
  and
  [`plot.group_tna_centralities()`](https://sonsoles.me/tna/reference/plot.group_tna_centralities.md)
  functions now plot the centralities in the same order as provided in
  the `measures` argument.
- The [`plot.tna()`](https://sonsoles.me/tna/reference/plot.tna.md) and
  [`plot_model()`](https://sonsoles.me/tna/reference/plot_model.md)
  functions now use the median edge weight as the default value for the
  `cut` argument.
- Fixed the `from` and `to` columns in
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md)
  output, which were inverted from the true edge direction.
- A plot method has been added for the
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md)
  output, which plots the corresponding network where non-significant
  edges have been pruned.

## tna 0.4.3

- The
  [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)
  function now properly checks that its arguments `x` and `y` can be
  compared.
- The p-value calculations of
  [`permutation_test()`](https://sonsoles.me/tna/reference/permutation_test.md)
  and [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md)
  have been adjusted by adding 1 to both the number of
  permutations/bootstrap samples and the number of extreme events so
  that these estimates are never zero. The documentation has also been
  clarified regarding p-values emphasizing that these are estimates
  only.

## tna 0.4.2

- The
  [`plot_compare()`](https://sonsoles.me/tna/reference/plot_compare.md)
  function now supports `negCol` and `posCol` for specifying the color
  of the positive and negative differences in transition and initial
  probabilities.
- The
  [`plot_mosaic()`](https://sonsoles.me/tna/reference/plot_mosaic.md)
  function now plots the x-axis on the top and rotates the labels 90
  degrees only when there are more than three groups.

## tna 0.4.1

- The `detailed` argument of
  [`estimate_centrality_stability()`](https://sonsoles.me/tna/reference/estimate_centrality_stability.md)
  has been removed. Previously this argument had no effect on the output
  of the function.
- Removed several duplicated entries in the documentation.

## tna 0.4.0

CRAN release: 2025-03-01

- The
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)
  function now produces an object of class `tna_data`, which can be
  directly used as an argument to
  [`build_model()`](https://sonsoles.me/tna/reference/build_model.md)
  and other methods.
- The
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)
  function now supports `order` when used together with `time` and
  `actor`.
- The
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)
  function gains the `unused_fn` argument of
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  to process any extra columns. The default is to keep all columns and
  use the first value.
- Added the function
  [`compare()`](https://sonsoles.me/tna/reference/compare.md) to compare
  `tna` models and weight matrices. This function produces an object of
  class `tna_comparison` which has
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
- Added the function
  [`plot_mosaic()`](https://sonsoles.me/tna/reference/plot_mosaic.md)
  which can be used to produce mosaic plots of transition counts for
  frequency-based transition network models and to contrast the state
  counts between groups.
- Fixed an issue with
  [`plot.tna_communities()`](https://sonsoles.me/tna/reference/plot.tna_communities.md)
  which now checks for the availability of a particular community
  detection method before plotting.
- Made several arguments in the plot methods of the package accessible
  to the user.

## tna 0.3.2

- `event2sequence()` has been renamed to
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md).
  The function is now also more general and can process more date
  formats.
- Added a `method` argument to
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md). The
  new default option `"stability"` implements a bootstrapping scheme
  where the edge weights are compared against a range of “consistent”
  weights (see the documentation for details). The old functionality can
  be accessed with `method = "threshold"`.
- Fixed an issue with `permutatation_test()` when `x` and `y` had a
  differing number of columns.
- Community detection methods can now be selected using the `methods`
  argument in
  [`communities()`](https://sonsoles.me/tna/reference/communities.md).
- The
  [`build_model()`](https://sonsoles.me/tna/reference/build_model.md)
  function has gained the argument `cols` which can be used to subset
  the columns of the data for `stslist` and `data.frame` inputs.
- Removed all `verbose` arguments in favor of
  `options(rlib_message_verbosity = "quiet").` and
  `options(rlib_warning_verbosity = "quiet")`.

## tna 0.3.1

- Fixed an issue when checking the validity of `character` type
  arguments.
- Improved the
  [`bootstrap()`](https://sonsoles.me/tna/reference/bootstrap.md)
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

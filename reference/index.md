# Package index

## Basic functions

- [`tna-package`](http://sonsoles.me/tna/reference/tna-package.md) :

  The `tna` Package.

- [`build_model()`](http://sonsoles.me/tna/reference/build_model.md)
  [`tna()`](http://sonsoles.me/tna/reference/build_model.md)
  [`ftna()`](http://sonsoles.me/tna/reference/build_model.md)
  [`ctna()`](http://sonsoles.me/tna/reference/build_model.md)
  [`atna()`](http://sonsoles.me/tna/reference/build_model.md)
  [`tsn()`](http://sonsoles.me/tna/reference/build_model.md) : Build a
  Transition Network Analysis Model

- [`print(`*`<tna>`*`)`](http://sonsoles.me/tna/reference/print.tna.md)
  :

  Print a `tna` Object

- [`plot(`*`<tna>`*`)`](http://sonsoles.me/tna/reference/plot.tna.md) :
  Plot a Transition Network Analysis Model

- [`hist(`*`<tna>`*`)`](http://sonsoles.me/tna/reference/hist.tna.md) :
  Plot a Histogram of Edge Weights in the Network

- [`sna()`](http://sonsoles.me/tna/reference/sna.md) : Build a Social
  Network Analysis Model

- [`print(`*`<summary.tna>`*`)`](http://sonsoles.me/tna/reference/print.summary.tna.md)
  : Print a TNA Summary

- [`plot_frequencies()`](http://sonsoles.me/tna/reference/plot_frequencies.md)
  : Plot the Frequency Distribution of States

- [`plot_frequencies(`*`<group_tna>`*`)`](http://sonsoles.me/tna/reference/plot_frequencies.group_tna.md)
  : Plot the Frequency Distribution of States

- [`plot_mosaic()`](http://sonsoles.me/tna/reference/plot_mosaic.md) :
  Create a Mosaic Plot of Transitions or Events

- [`plot_mosaic(`*`<tna_data>`*`)`](http://sonsoles.me/tna/reference/plot_mosaic.tna_data.md)
  : Plot State Frequencies as a Mosaic Between Two Groups

- [`plot_associations()`](http://sonsoles.me/tna/reference/plot_associations.md)
  : Plot an Association Network

- [`print(`*`<group_tna>`*`)`](http://sonsoles.me/tna/reference/print.group_tna.md)
  :

  Print a `group_tna` Object

- [`plot(`*`<group_tna>`*`)`](http://sonsoles.me/tna/reference/plot.group_tna.md)
  : Plot a Grouped Transition Network Analysis Model

- [`summary(`*`<group_tna>`*`)`](http://sonsoles.me/tna/reference/summary.group_tna.md)
  : Calculate Summary of Network Metrics for a grouped Transition
  Network

- [`hist(`*`<group_tna>`*`)`](http://sonsoles.me/tna/reference/hist.group_tna.md)
  :

  Plot a Histogram of Edge Weights for a `group_tna` Object.

- [`plot_mosaic(`*`<group_tna>`*`)`](http://sonsoles.me/tna/reference/plot_mosaic.group_tna.md)
  : Plot State Frequencies as a Mosaic Between Two Groups

- [`print(`*`<summary.group_tna>`*`)`](http://sonsoles.me/tna/reference/print.summary.group_tna.md)
  : Print a Summary of a Grouped Transition Network Analysis Model

- [`plot_sequences()`](http://sonsoles.me/tna/reference/plot_sequences.md)
  : Create a Sequence Index Plot or a Distribution Plot

## Importing/generating data

- [`import_data()`](http://sonsoles.me/tna/reference/import_data.md) :
  Import Wide Format Sequence Data as Long Format Sequence Data
- [`import_onehot()`](http://sonsoles.me/tna/reference/import_onehot.md)
  : Import One-Hot Data
- [`prepare_data()`](http://sonsoles.me/tna/reference/prepare_data.md) :
  Compute User Sessions from Event Data
- [`print(`*`<tna_data>`*`)`](http://sonsoles.me/tna/reference/print.tna_data.md)
  : Print a TNA Data Object
- [`simulate(`*`<tna>`*`)`](http://sonsoles.me/tna/reference/simulate.tna.md)
  : Simulate Data from a Transition Network Analysis Model
- [`simulate(`*`<group_tna>`*`)`](http://sonsoles.me/tna/reference/simulate.group_tna.md)
  : Simulate Data from a Group Transition Network Analysis Model
- [`summary(`*`<tna>`*`)`](http://sonsoles.me/tna/reference/summary.tna.md)
  : Calculate Summary of Network Metrics for a Transition Network

## Centralities

- [`centralities()`](http://sonsoles.me/tna/reference/centralities.md) :
  Calculate Centrality Measures for a Transition Matrix
- [`betweenness_network()`](http://sonsoles.me/tna/reference/betweenness_network.md)
  : Build and Visualize a Network with Edge Betweenness
- [`print(`*`<tna_centralities>`*`)`](http://sonsoles.me/tna/reference/print.tna_centralities.md)
  : Print Centrality Measures
- [`plot(`*`<tna_centralities>`*`)`](http://sonsoles.me/tna/reference/plot.tna_centralities.md)
  : Plot Centrality Measures
- [`print(`*`<group_tna_centralities>`*`)`](http://sonsoles.me/tna/reference/print.group_tna_centralities.md)
  : Print Centrality Measures
- [`plot(`*`<group_tna_centralities>`*`)`](http://sonsoles.me/tna/reference/plot.group_tna_centralities.md)
  : Plot Centrality Measures

## Communities

- [`communities()`](http://sonsoles.me/tna/reference/communities.md) :
  Community Detection for Transition Networks
- [`print(`*`<tna_communities>`*`)`](http://sonsoles.me/tna/reference/print.tna_communities.md)
  : Print Detected Communities
- [`plot(`*`<tna_communities>`*`)`](http://sonsoles.me/tna/reference/plot.tna_communities.md)
  : Plot Communities
- [`print(`*`<group_tna_communities>`*`)`](http://sonsoles.me/tna/reference/print.group_tna_communities.md)
  : Print Detected Communities
- [`plot(`*`<group_tna_communities>`*`)`](http://sonsoles.me/tna/reference/plot.group_tna_communities.md)
  : Plot Detected Communities

## Cliques

- [`cliques()`](http://sonsoles.me/tna/reference/cliques.md) : Identify
  Cliques in a Transition Network
- [`print(`*`<tna_cliques>`*`)`](http://sonsoles.me/tna/reference/print.tna_cliques.md)
  : Print Found Cliques of a TNA Network
- [`plot(`*`<tna_cliques>`*`)`](http://sonsoles.me/tna/reference/plot.tna_cliques.md)
  : Plot Cliques of a TNA Network
- [`print(`*`<group_tna_cliques>`*`)`](http://sonsoles.me/tna/reference/print.group_tna_cliques.md)
  : Print Found Cliques
- [`plot(`*`<group_tna_cliques>`*`)`](http://sonsoles.me/tna/reference/plot.group_tna_cliques.md)
  : Plot Found Cliques

## Clusters

- [`cluster_data()`](http://sonsoles.me/tna/reference/cluster_data.md)
  [`cluster_sequences()`](http://sonsoles.me/tna/reference/cluster_data.md)
  : Clustering via Dissimilarity Matrix based on String Distances
- [`mmm_stats()`](http://sonsoles.me/tna/reference/mmm_stats.md) :
  Retrieve Statistics from a Mixture Markov Model (MMM)
- [`print(`*`<tna_clustering>`*`)`](http://sonsoles.me/tna/reference/print.tna_clustering.md)
  : Print the Results of Clustering

## Comparison

- [`compare()`](http://sonsoles.me/tna/reference/compare.md) : Compare
  Two Matrices or TNA Models with Comprehensive Metrics
- [`compare(`*`<group_tna>`*`)`](http://sonsoles.me/tna/reference/compare.group_tna.md)
  : Compare Grouped TNA Models with Comprehensive Metrics
- [`compare_sequences()`](http://sonsoles.me/tna/reference/compare_sequences.md)
  : Compare Sequences Between Groups
- [`print(`*`<tna_comparison>`*`)`](http://sonsoles.me/tna/reference/print.tna_comparison.md)
  : Print Comparison Results
- [`print(`*`<tna_sequence_comparison>`*`)`](http://sonsoles.me/tna/reference/print.tna_sequence_comparison.md)
  : Print a Comparison of Sequences
- [`plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md) :
  Plot the Difference Network Between Two Models
- [`plot_compare(`*`<group_tna>`*`)`](http://sonsoles.me/tna/reference/plot_compare.group_tna.md)
  : Plot the Difference Network Between Two Groups
- [`plot(`*`<tna_comparison>`*`)`](http://sonsoles.me/tna/reference/plot.tna_comparison.md)
  : Plot the Comparison of Two TNA Models or Matrices
- [`plot(`*`<tna_sequence_comparison>`*`)`](http://sonsoles.me/tna/reference/plot.tna_sequence_comparison.md)
  : Plot a Sequence Comparison

## Validation

- [`permutation_test()`](http://sonsoles.me/tna/reference/permutation_test.md)
  : Compare Two Networks from Sequence Data using a Permutation Test

- [`permutation_test(`*`<group_tna>`*`)`](http://sonsoles.me/tna/reference/permutation_test.group_tna.md)
  : Compare Networks using a Permutation Test

- [`print(`*`<tna_permutation>`*`)`](http://sonsoles.me/tna/reference/print.tna_permutation.md)
  : Print Permutation Test Results

- [`print(`*`<group_tna_permutation>`*`)`](http://sonsoles.me/tna/reference/print.group_tna_permutation.md)
  : Print Permutation Test Results

- [`plot(`*`<tna_permutation>`*`)`](http://sonsoles.me/tna/reference/plot.tna_permutation.md)
  : Plot the Significant Differences from a Permutation Test

- [`plot(`*`<group_tna_permutation>`*`)`](http://sonsoles.me/tna/reference/plot.group_tna_permutation.md)
  : Plot Permutation Test Results

- [`print(`*`<tna_bootstrap>`*`)`](http://sonsoles.me/tna/reference/print.tna_bootstrap.md)
  : Print Bootstrap Results

- [`print(`*`<summary.tna_bootstrap>`*`)`](http://sonsoles.me/tna/reference/print.summary.tna_bootstrap.md)
  : Print a Bootstrap Summary

- [`print(`*`<summary.group_tna_bootstrap>`*`)`](http://sonsoles.me/tna/reference/print.summary.group_tna_bootstrap.md)
  : Print a Bootstrap Summary for a Grouped Transition Network Model

- [`print(`*`<tna_stability>`*`)`](http://sonsoles.me/tna/reference/print.tna_stability.md)
  : Print Centrality Stability Results

- [`plot(`*`<tna_stability>`*`)`](http://sonsoles.me/tna/reference/plot.tna_stability.md)
  : Plot Centrality Stability Results

- [`print(`*`<group_tna_bootstrap>`*`)`](http://sonsoles.me/tna/reference/print.group_tna_bootstrap.md)
  :

  Print `group_tna` Bootstrap Results

- [`print(`*`<group_tna_stability>`*`)`](http://sonsoles.me/tna/reference/print.group_tna_stability.md)
  : Print Centrality Stability Results

- [`plot(`*`<group_tna_stability>`*`)`](http://sonsoles.me/tna/reference/plot.group_tna_stability.md)
  : Plot Centrality Stability Results

- [`estimate_cs()`](http://sonsoles.me/tna/reference/estimate_centrality_stability.md)
  [`estimate_centrality_stability()`](http://sonsoles.me/tna/reference/estimate_centrality_stability.md)
  : Estimate Centrality Stability

- [`prune()`](http://sonsoles.me/tna/reference/prune.md) : Prune a
  Transition Network based on Transition Probabilities

- [`deprune()`](http://sonsoles.me/tna/reference/deprune.md)
  [`reprune(`*`<tna>`*`)`](http://sonsoles.me/tna/reference/deprune.md)
  : Restore a Pruned Transition Network Analysis Model

- [`reprune()`](http://sonsoles.me/tna/reference/reprune.md) : Restore
  Previous Pruning of a Transition Network Analysis Model

- [`pruning_details()`](http://sonsoles.me/tna/reference/pruning_details.md)
  : Print Detailed Information on the Pruning Results

- [`bootstrap()`](http://sonsoles.me/tna/reference/bootstrap.md) :
  Bootstrap Transition Networks from Sequence Data

- [`bootstrap_cliques()`](http://sonsoles.me/tna/reference/bootstrap_cliques.md)
  : Bootstrap Cliques of Transition Networks from Sequence Data

- [`plot(`*`<tna_bootstrap>`*`)`](http://sonsoles.me/tna/reference/plot.tna_bootstrap.md)
  : Plot a Bootstrapped Transition Network Analysis Model

- [`plot(`*`<group_tna_bootstrap>`*`)`](http://sonsoles.me/tna/reference/plot.group_tna_bootstrap.md)
  : Plot a Bootstrapped Grouped Transition Network Analysis Model

- [`summary(`*`<tna_bootstrap>`*`)`](http://sonsoles.me/tna/reference/summary.tna_bootstrap.md)
  : Summarize Bootstrap Results

- [`summary(`*`<group_tna_bootstrap>`*`)`](http://sonsoles.me/tna/reference/summary.group_tna_bootstrap.md)
  : Summarize Bootstrap Results for a Grouped Transition Network

- [`reliability()`](http://sonsoles.me/tna/reference/reliability.md) :
  Assess Model Reliability

- [`print(`*`<tna_reliability>`*`)`](http://sonsoles.me/tna/reference/print.tna_reliability.md)
  : Print Reliability Analysis Results

- [`plot(`*`<tna_reliability>`*`)`](http://sonsoles.me/tna/reference/plot.tna_reliability.md)
  : Plot Reliability Analysis Results

## Groups

- [`group_model()`](http://sonsoles.me/tna/reference/group_model.md)
  [`group_tna()`](http://sonsoles.me/tna/reference/group_model.md)
  [`group_ftna()`](http://sonsoles.me/tna/reference/group_model.md)
  [`group_ctna()`](http://sonsoles.me/tna/reference/group_model.md)
  [`group_atna()`](http://sonsoles.me/tna/reference/group_model.md) :
  Build a Grouped Transition Network Analysis Model
- [`rename_groups()`](http://sonsoles.me/tna/reference/rename_groups.md)
  : Rename Groups

## Helpers

- [`as.igraph(`*`<group_tna>`*`)`](http://sonsoles.me/tna/reference/as.igraph.group_tna.md)
  :

  Coerce a Specific Group from a `group_tna` Object into an `igraph`
  Object.

- [`as.igraph(`*`<matrix>`*`)`](http://sonsoles.me/tna/reference/as.igraph.matrix.md)
  :

  Coerce a Weight Matrix into an `igraph` Object.

- [`as.igraph(`*`<tna>`*`)`](http://sonsoles.me/tna/reference/as.igraph.tna.md)
  :

  Coerce a `tna` Object into an `igraph` Object.

## Datasets

- [`engagement`](http://sonsoles.me/tna/reference/engagement.md) :
  Example Data on Student Engagement

- [`engagement_mmm`](http://sonsoles.me/tna/reference/engagement_mmm.md)
  :

  Example Mixed Markov Model Fitted to the `engagement` Data

- [`group_regulation`](http://sonsoles.me/tna/reference/group_regulation.md)
  : Example Wide Data on Group Regulation

- [`group_regulation_long`](http://sonsoles.me/tna/reference/group_regulation_long.md)
  : Example Long Data on Group Regulation

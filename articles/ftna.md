# Frequency-based TNA

## FTNA tutorial

``` r

# Install 'tna' package from CRAN if needed (uncomment if required).
# install.packages("tna")

# Load packages
library("tna")

# Load example data provided within the 'tna' package, 
# representing group regulatory interactions
data(group_regulation)

# Run FTNA on 'group_regulation' data using raw counts of 
# transitions ("absolute" type) and print the result
model <- ftna(group_regulation)

# Print the output to inspect the model
print(model)
#> State Labels : 
#> 
#>    adapt, cohesion, consensus, coregulate, discuss, emotion, monitor, plan, synthesis 
#> 
#> Transition Frequency Matrix :
#> 
#>            adapt cohesion consensus coregulate discuss emotion monitor plan
#> adapt          0      139       243         11      30      61      17    8
#> cohesion       5       46       844        202     101     196      56  239
#> consensus     30       94       519       1188    1190     460     295 2505
#>            synthesis
#> adapt              0
#> cohesion           6
#> consensus         48
#>  [ reached 'max' / getOption("max.print") -- omitted 6 rows ]
#> 
#> Initial Probabilities : 
#> 
#>      adapt   cohesion  consensus coregulate    discuss    emotion    monitor 
#>      0.011      0.060      0.214      0.019      0.175      0.151      0.144 
#>       plan  synthesis 
#>      0.204      0.019
```

``` r

# Calculate the Transition Network Analysis (TNA) on the group_regulation 
# data with scaled weights between 0 and 1
model_scaled <- ftna(group_regulation, scaling = "minmax")
print(model_scaled) # Print the FTNA model with scaled weights
#> State Labels : 
#> 
#>    adapt, cohesion, consensus, coregulate, discuss, emotion, monitor, plan, synthesis 
#> 
#> Transition Frequency Matrix :
#> 
#>             adapt cohesion consensus coregulate discuss emotion monitor   plan
#> adapt      0.0000   0.0555     0.097     0.0044   0.012   0.024  0.0068 0.0032
#> cohesion   0.0020   0.0184     0.337     0.0806   0.040   0.078  0.0224 0.0954
#> consensus  0.0120   0.0375     0.207     0.4743   0.475   0.184  0.1178 1.0000
#>            synthesis
#> adapt         0.0000
#> cohesion      0.0024
#> consensus     0.0192
#>  [ reached 'max' / getOption("max.print") -- omitted 6 rows ]
#> 
#> Initial Probabilities : 
#> 
#>      adapt   cohesion  consensus coregulate    discuss    emotion    monitor 
#>      0.011      0.060      0.214      0.019      0.175      0.151      0.144 
#>       plan  synthesis 
#>      0.204      0.019
```

### Plotting

Scaling should change the units of the weights, not their relative
order. Plotting the absolute weights against the scaled weights checks
this: [`compare()`](https://sonsoles.me/tna/reference/compare.md) pairs
the edges of the two models and its scatterplot shows the Pearson
correlation of the weights.

``` r

# Plotting the two weights together to see if the scaling distorts the data
plot(
  compare(model, model_scaled),
  type = "scatterplot",
  name_x = "Absolute",
  name_y = "Scaled"
)
```

![](ftna_files/figure-html/unnamed-chunk-4-1.png)

``` r

# Calculate the Transition Network Analysis (TNA) on the `group_regulation` 
# data with ranked weights
model_ranked <- ftna(group_regulation, scaling = "rank")
print(model_ranked) # Print the FTNA model with ranked weights
```

Ranking keeps the order of the weights but not their distances, so the
relation with the absolute weights is monotone but not linear. The
Spearman correlation measures that agreement in order.

``` r

# Compare the absolute and the ranked weights
plot(
  compare(model, model_ranked),
  type = "scatterplot",
  method = "spearman",
  name_x = "Absolute",
  name_y = "Ranked"
)
```

![](ftna_files/figure-html/unnamed-chunk-6-1.png)

### Pruning

``` r

layout(matrix(1:4, ncol = 2))
# Pruning with different methods
pruned_threshold <- prune(model_scaled, method = "threshold", threshold = 0.1)
pruned_lowest <- prune(model_scaled, method = "lowest", lowest = 0.15)
pruned_disparity <- prune(model_scaled, method = "disparity", level = 0.5)

# Plotting for comparison
plot(pruned_threshold)
#> Registered S3 method overwritten by 'cograph':
#>   method             from
#>   plot.tna_bootstrap tna
plot(pruned_lowest)
plot(pruned_disparity)
plot(model_scaled)
```

![](ftna_files/figure-html/unnamed-chunk-7-1.png)

### Patterns

``` r

# Identify 2-cliques (dyads) from the FTNA model with a weight threshold, 
# excluding loops in visualization.
# A 2-clique represents a pair of nodes that are strongly connected based on 
# the specified weight threshold.
layout(matrix(1:6, ncol = 3))
cliques_of_two <- cliques(
  model_scaled,      # The FTNA model with scaled edge weights
  size = 2,          # Looking for pairs of connected nodes (dyads)
  threshold = 0.1    # Only include edges with weights greater than 0.1
)

# Print and visualize the identified 2-cliques (dyads)
print(cliques_of_two)  # Display details of 2-cliques
#> Number of 2-cliques = 8 (weight threshold = 0.1)
#> Showing 6 cliques starting from clique number 1
#> 
#> Clique 1
#>           consensus plan
#> consensus      0.21 1.00
#> plan           0.71 0.92
#> 
#> Clique 2
#>           consensus discuss
#> consensus      0.21    0.48
#> discuss        0.51    0.31
#> 
#> Clique 3
#>         discuss emotion
#> discuss    0.31   0.167
#> emotion    0.12   0.087
#> 
#> Clique 4
#>         emotion plan
#> emotion   0.087 0.11
#> plan      0.361 0.92
#> 
#> Clique 5
#>           consensus emotion
#> consensus      0.21   0.184
#> emotion        0.36   0.087
#> 
#> Clique 6
#>         monitor plan
#> monitor    0.01 0.12
#> plan       0.19 0.92
plot(cliques_of_two, ask = FALSE)   # Visualize 2-cliques in the network
```

![](ftna_files/figure-html/unnamed-chunk-8-1.png)

``` r

layout(matrix(1:6, ncol = 3))
# Identify 3-cliques (triads) from the FTNA model.
# A 3-clique is a fully connected set of three nodes, indicating a strong 
# triplet structure.
cliques_of_three <- cliques(
  model_scaled,      # The FTNA model with scaled edge weights
  size = 3,          # Looking for triplets of fully connected nodes (triads)
  threshold = 0.05   # Only include edges with weights greater than 0.05
)

# Print and visualize the identified 3-cliques (triads)
# Uncomment the code below to view the results
print(cliques_of_three) # Display details of 3-cliques
#> Number of 3-cliques = 5 (weight threshold = 0.05)
#> Showing 5 cliques starting from clique number 1
#> 
#> Clique 1
#>            consensus coregulate discuss
#> consensus       0.21      0.474    0.48
#> coregulate      0.11      0.018    0.22
#> discuss         0.51      0.133    0.31
#> 
#> Clique 2
#>           consensus discuss emotion
#> consensus      0.21    0.48   0.184
#> discuss        0.51    0.31   0.167
#> emotion        0.36    0.12   0.087
#> 
#> Clique 3
#>           consensus emotion plan
#> consensus      0.21   0.184 1.00
#> emotion        0.36   0.087 0.11
#> plan           0.71   0.361 0.92
#> 
#> Clique 4
#>           consensus monitor plan
#> consensus     0.207    0.12 1.00
#> monitor       0.091    0.01 0.12
#> plan          0.714    0.19 0.92
#> 
#> Clique 5
#>          cohesion emotion  plan
#> cohesion    0.018   0.078 0.095
#> emotion     0.368   0.087 0.113
#> plan        0.062   0.361 0.920
plot(cliques_of_three, ask = FALSE)  # Visualize 3-cliques in the network
```

![](ftna_files/figure-html/unnamed-chunk-9-1.png)

``` r

layout(matrix(1:6, ncol = 3))
# Identify 4-cliques (quadruples) from the FTNA model.
# A 4-clique includes four nodes where each node is connected to every other 
# node in the group.
# Uncomment the code below to view the results
cliques_of_four <- cliques(
  model_scaled,      # The FTNA model with scaled edge weights
  size = 4,          # Looking for quadruples of fully connected nodes (4-cliques)
  threshold = 0.03   # Only include edges with weights greater than 0.03
)

# Print and visualize the identified 4-cliques (quadruples) 
# Uncomment the code below to view the results
print(cliques_of_four)  # Display details of 4-cliques
#> Number of 4-cliques = 11 (weight threshold = 0.03)
#> Showing 6 cliques starting from clique number 1
#> 
#> Clique 1
#>            consensus coregulate emotion plan
#> consensus       0.21      0.474   0.184 1.00
#> coregulate      0.11      0.018   0.135 0.19
#> emotion         0.36      0.039   0.087 0.11
#> plan            0.71      0.042   0.361 0.92
#> 
#> Clique 2
#>            consensus coregulate discuss emotion
#> consensus       0.21      0.474    0.48   0.184
#> coregulate      0.11      0.018    0.22   0.135
#> discuss         0.51      0.133    0.31   0.167
#> emotion         0.36      0.039    0.12   0.087
#> 
#> Clique 3
#>            coregulate discuss emotion monitor
#> coregulate      0.018    0.22   0.135   0.068
#> discuss         0.133    0.31   0.167   0.035
#> emotion         0.039    0.12   0.087   0.041
#> monitor         0.033    0.21   0.052   0.010
#> 
#> Clique 4
#>            consensus coregulate discuss monitor
#> consensus      0.207      0.474    0.48   0.118
#> coregulate     0.106      0.018    0.22   0.068
#> discuss        0.507      0.133    0.31   0.035
#> monitor        0.091      0.033    0.21   0.010
#> 
#> Clique 5
#>           consensus discuss emotion monitor
#> consensus     0.207    0.48   0.184   0.118
#> discuss       0.507    0.31   0.167   0.035
#> emotion       0.363    0.12   0.087   0.041
#> monitor       0.091    0.21   0.052   0.010
#> 
#> Clique 6
#>            coregulate emotion monitor plan
#> coregulate      0.018   0.135   0.068 0.19
#> emotion         0.039   0.087   0.041 0.11
#> monitor         0.033   0.052   0.010 0.12
#> plan            0.042   0.361   0.186 0.92
plot(cliques_of_four, ask = FALSE)   # Visualize 4-cliques in the network
```

![](ftna_files/figure-html/unnamed-chunk-10-1.png)

``` r

# Identify 5-cliques (quintuples) from the FTNA model, summing edge weights.
# Here, the sum of edge weights in both directions must meet the specified 
# threshold for inclusion.
# Uncomment the code below to view the results
cliques_of_five <- cliques(
  model_scaled,      # The FTNA model with scaled edge weights
  size = 5,          # Looking for quintuples of fully connected nodes (5-cliques)
  threshold = 0.1,   # Only edges with total bidirectional weights greater than 0.1
  sum_weights = TRUE # Sum edge weight in both directions when computing  threshold
)

# Print and visualize the identified 5-cliques (quintuples)
print(cliques_of_five)  # Display details of 5-cliques
#> Number of 5-cliques = 7 (weight threshold = 0.1)
#> Showing 6 cliques starting from clique number 1
#> 
#> Clique 1
#>            consensus coregulate discuss emotion  plan
#> consensus       0.21      0.474    0.48   0.184 1.000
#> coregulate      0.11      0.018    0.22   0.135 0.188
#> discuss         0.51      0.133    0.31   0.167 0.018
#> emotion         0.36      0.039    0.12   0.087 0.113
#> plan            0.71      0.042    0.17   0.361 0.920
#> 
#> Clique 2
#>            consensus coregulate discuss monitor  plan
#> consensus      0.207      0.474    0.48   0.118 1.000
#> coregulate     0.106      0.018    0.22   0.068 0.188
#> discuss        0.507      0.133    0.31   0.035 0.018
#> monitor        0.091      0.033    0.21   0.010 0.123
#> plan           0.714      0.042    0.17   0.186 0.920
#> 
#> Clique 3
#>            cohesion consensus coregulate emotion  plan
#> cohesion      0.018      0.34      0.081   0.078 0.095
#> consensus     0.038      0.21      0.474   0.184 1.000
#> coregulate    0.028      0.11      0.018   0.135 0.188
#> emotion       0.368      0.36      0.039   0.087 0.113
#> plan          0.062      0.71      0.042   0.361 0.920
#> 
#> Clique 4
#>            cohesion coregulate discuss emotion  plan
#> cohesion      0.018      0.081    0.04   0.078 0.095
#> coregulate    0.028      0.018    0.22   0.135 0.188
#> discuss       0.075      0.133    0.31   0.167 0.018
#> emotion       0.368      0.039    0.12   0.087 0.113
#> plan          0.062      0.042    0.17   0.361 0.920
#> 
#> Clique 5
#>            cohesion consensus coregulate discuss emotion
#> cohesion      0.018      0.34      0.081    0.04   0.078
#> consensus     0.038      0.21      0.474    0.48   0.184
#> coregulate    0.028      0.11      0.018    0.22   0.135
#> discuss       0.075      0.51      0.133    0.31   0.167
#> emotion       0.368      0.36      0.039    0.12   0.087
#> 
#> Clique 6
#>           cohesion consensus discuss emotion  plan
#> cohesion     0.018      0.34    0.04   0.078 0.095
#> consensus    0.038      0.21    0.48   0.184 1.000
#> discuss      0.075      0.51    0.31   0.167 0.018
#> emotion      0.368      0.36    0.12   0.087 0.113
#> plan         0.062      0.71    0.17   0.361 0.920
plot(cliques_of_five, ask = FALSE)   # Visualize 5-cliques in the network
```

![](ftna_files/figure-html/unnamed-chunk-11-1.png)![](ftna_files/figure-html/unnamed-chunk-11-2.png)![](ftna_files/figure-html/unnamed-chunk-11-3.png)![](ftna_files/figure-html/unnamed-chunk-11-4.png)![](ftna_files/figure-html/unnamed-chunk-11-5.png)![](ftna_files/figure-html/unnamed-chunk-11-6.png)

### Graph level measures

``` r

summary(model_scaled)
#> # A tibble: 13 × 2
#>    metric                        value
#>  * <chr>                         <dbl>
#>  1 Node Count                   9     
#>  2 Edge Count                  78     
#>  3 Network Density              1     
#>  4 Mean Distance                0.0240
#>  5 Mean Out-Strength            1.13  
#>  6 SD Out-Strength              0.878 
#>  7 Mean In-Strength             1.13  
#>  8 SD In-Strength               0.886 
#>  9 Mean Out-Degree              8.67  
#> 10 SD Out-Degree                0.707 
#> 11 Centralization (Out-Degree)  0.0156
#> 12 Centralization (In-Degree)   0.0156
#> 13 Reciprocity                  0.986
```

``` r

summary(pruned_disparity)
#> # A tibble: 13 × 2
#>    metric                       value
#>  * <chr>                        <dbl>
#>  1 Node Count                   9    
#>  2 Edge Count                  39    
#>  3 Network Density              0.542
#>  4 Mean Distance                0.188
#>  5 Mean Out-Strength            0.900
#>  6 SD Out-Strength              0.673
#>  7 Mean In-Strength             0.900
#>  8 SD In-Strength               0.700
#>  9 Mean Out-Degree              4.33 
#> 10 SD Out-Degree                1.22 
#> 11 Centralization (Out-Degree)  0.234
#> 12 Centralization (In-Degree)   0.516
#> 13 Reciprocity                  0.615
```

### Node level measures

``` r

# Compute centrality measures for the FTNA model
centrality_measures <- centralities(model_scaled)

# Print the calculated centrality measures in the FTNA model
print(centrality_measures)
#> # A tibble: 9 × 11
#>   state      OutStrength InStrength ClosenessIn ClosenessOut Closeness Betweenness
#> * <fct>            <dbl>      <dbl>       <dbl>        <dbl>     <dbl>       <dbl>
#> 1 adapt            0.203      0.212     0.00967      0.00901    0.0104           0
#> 2 cohesion         0.658      0.667     0.0128       0.0176     0.0193           0
#> 3 consensus        2.32       2.34      0.0245       0.0254     0.0294          39
#> 4 coregulate       0.768      0.818     0.0196       0.0148     0.0205           0
#> 5 discuss          1.27       1.26      0.0210       0.0222     0.0272          19
#> 6 emotion          1.05       1.02      0.0159       0.0195     0.0207           7
#> 7 monitor          0.562      0.480     0.0124       0.0135     0.0154           0
#> 8 plan             1.54       1.56      0.0221       0.0236     0.0263          18
#> 9 synthesis        0.260      0.275     0.0137       0.0104     0.0147           0
#> # ℹ 4 more variables: BetweennessRSP <dbl>, Diffusion <dbl>, Clustering <dbl>,
#> #   PageRank <dbl>
plot(centrality_measures)
```

![](ftna_files/figure-html/unnamed-chunk-14-1.png)

``` r

# Convert the FTNA model to an igraph object and 
# calculate HITS (Hub and Authority) scores
hits_results <- igraph::hits_scores(as.igraph(model_scaled))
```

``` r

# Print the hub and authority scores (and the eigenvalue) to view influential nodes
print(hits_results)
#> $hub
#>      adapt   cohesion  consensus coregulate    discuss    emotion    monitor 
#>      0.056      0.240      0.955      0.256      0.377      0.305      0.189 
#>       plan  synthesis 
#>      1.000      0.074 
#> 
#> $authority
#>      adapt   cohesion  consensus coregulate    discuss    emotion    monitor 
#>      0.033      0.129      0.672      0.293      0.437      0.344      0.174 
#>       plan  synthesis 
#>      1.000      0.056 
#> 
#> $value
#> [1] 3.3
#> 
#> $options
#> $options$bmat
#> [1] "I"
#> 
#> $options$n
#> [1] 9
#> 
#> $options$which
#> [1] "LA"
#> 
#> $options$nev
#> [1] 1
#> 
#> $options$tol
#> [1] 0
#> 
#> $options$ncv
#> [1] 0
#> 
#> $options$ldv
#> [1] 0
#> 
#> $options$ishift
#> [1] 1
#> 
#> $options$maxiter
#> [1] 3000
#> 
#> $options$nb
#> [1] 1
#> 
#> $options$mode
#> [1] 1
#> 
#> $options$start
#> [1] 1
#> 
#> $options$sigma
#> [1] 0
#> 
#> $options$sigmai
#> [1] 0
#> 
#> $options$info
#> [1] 0
#> 
#> $options$iter
#> [1] 2
#> 
#> $options$nconv
#> [1] 1
#> 
#> $options$numop
#> [1] 9
#> 
#> $options$numopb
#> [1] 0
#> 
#> $options$numreo
#> [1] 8
```

### Edge level measures

``` r

edge_between <- betweenness_network(model_scaled)
plot(edge_between)
```

![](ftna_files/figure-html/unnamed-chunk-17-1.png)

### Community detection

``` r

detected_communities <- communities(model_scaled)
plot(detected_communities, method = "leading_eigen")
```

![](ftna_files/figure-html/unnamed-chunk-18-1.png)

``` r

print(detected_communities)
#> Number of communities found by each algorithm
#> 
#>         walktrap      fast_greedy       label_prop          infomap 
#>                1                3                1                1 
#> edge_betweenness    leading_eigen        spinglass 
#>                1                3                3 
#> 
#> Community assignments
#> 
#>       state walktrap fast_greedy label_prop infomap edge_betweenness leading_eigen
#> 1     adapt        1           1          1       1                1             1
#> 2  cohesion        1           2          1       1                1             3
#> 3 consensus        1           3          1       1                1             2
#>   spinglass
#> 1         1
#> 2         3
#> 3         2
#>  [ reached 'max' / getOption("max.print") -- omitted 6 rows ]
```

### Bootstrapping

``` r

# Perform bootstrapping on the FTNA model with a fixed seed for reproducibility
set.seed(265)
boot <- bootstrap(model_scaled)

# Print the combined results data frame containing
print(summary(boot))
#>         from    to weight p_value   sig cr_lower cr_upper ci_lower ci_upper
#> 2   cohesion adapt  0.002    0.51 FALSE   0.0015   0.0025   0.0004   0.0036
#> 3  consensus adapt  0.012    0.16 FALSE   0.0090   0.0150   0.0078   0.0163
#> 4 coregulate adapt  0.013    0.16 FALSE   0.0096   0.0160   0.0085   0.0177
#>  [ reached 'max' / getOption("max.print") -- omitted 75 rows ]

# View non-significant edges  which are less likely to be stable across bootstrap samples
print(boot, type = "nonsig")
#> Non-significant Edges
#> 
#>         from    to weight p_value cr_lower cr_upper ci_lower ci_upper
#> 2   cohesion adapt  0.002    0.51   0.0015   0.0025   0.0004   0.0036
#> 3  consensus adapt  0.012    0.16   0.0090   0.0150   0.0078   0.0163
#> 4 coregulate adapt  0.013    0.16   0.0096   0.0160   0.0085   0.0177
#>  [ reached 'max' / getOption("max.print") -- omitted 25 rows ]
```

### Comparing Models

``` r

# Create FTNA for the high-achievers subset (rows 1 to 1000)
Hi <- ftna(head(group_regulation, 1000), scaling = "minmax")

# Create FTNA for the low-achievers subset (rows 1001 to 2000)
Lo <- ftna(tail(group_regulation, 1000), scaling = "minmax")

# Plot a comparison of the "Hi" and "Lo" models
# The 'minimum' parameter is set to 0.0001, so edges with weights >= 0.0001 are shown
plot_compare(Hi, Lo, minimum = 0.0001)
```

![](ftna_files/figure-html/unnamed-chunk-20-1.png)

``` r


# Run a permutation test to determine statistical significance of 
# differences between "Hi" and "Lo"
# The 'iter' argument is set to 1000, meaning 1000 permutations are performed
Permutation <- permutation_test(Hi, Lo, iter = 1000)
#> `x` or `y` has scaled edge weights.
#> ℹ Each network is scaled on its own, so comparing scaled weights is not warranted.
#>   Build the models without `scaling` to compare the transition weights.

# Plot the significant differences identified in the permutation test
plot(Permutation)
```

![](ftna_files/figure-html/unnamed-chunk-20-2.png)

### Centrality stability

``` r

Centrality_stability <- estimate_centrality_stability(model_scaled, iter = 100)
plot(Centrality_stability)
```

![](ftna_files/figure-html/unnamed-chunk-21-1.png)

### More tutorials

**Package articles** · [sonsoles.me/tna](https://sonsoles.me/tna/)

- [Getting started with tna](https://sonsoles.me/tna/articles/tna.html)
  — build, plot, and summarize a transition network
- [Preparing data](https://sonsoles.me/tna/articles/prepare_data.html) —
  turn event logs into sequences with
  [`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md)
- [A showcase of the main
  functions](https://sonsoles.me/tna/articles/complete_tutorial.html) —
  models, centralities, communities, and validation
- [Grouped sequence
  data](https://sonsoles.me/tna/articles/grouped_sequences.html) — build
  and compare networks of groups
- [Cliques and
  communities](https://sonsoles.me/tna/articles/communities_and_cliques.html)
  — find the densely connected states
- [Frequency-based TNA](https://sonsoles.me/tna/articles/ftna.html) —
  model transition counts instead of probabilities
- [Attention TNA](https://sonsoles.me/tna/articles/atna.html) —
  accumulate transitions across temporal distances
- [Validating a transition
  network](https://sonsoles.me/tna/articles/model-validation.html) —
  Markov order, bootstrap, case-dropping, reliability, and permutation
  tests
- [Function reference
  sheet](https://sonsoles.me/tna/articles/new-docs.html) — every
  function with its arguments and an example

**Tutorials** · [sonsoles.me](https://sonsoles.me/)

- [A comprehensive TNA
  tutorial](https://sonsoles.me/posts/tna-tutorial/) — an updated
  walkthrough of transition network analysis
- [Data preparation](https://sonsoles.me/posts/tna-data/) — from raw
  event data to sequences
- [Comparing models](https://sonsoles.me/posts/tna-compare/) — network
  comparison of TNA models
- [Clustering](https://sonsoles.me/posts/tna-clustering/) — discover and
  analyze clusters of sequences
- [Group analysis](https://sonsoles.me/posts/tna-group/) — permutation
  tests, bootstrapping, and clusters

**Book chapters** · *Advanced Learning Analytics Methods*

- [Mapping relational dynamics with
  TNA](https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html) — a
  primer and tutorial
- [Frequency transition network
  analysis](https://lamethods.org/book2/chapters/ch16-ftna/ch16-ftna.html)
  — a primer and tutorial on FTNA
- [Mining patterns and clusters with
  TNA](https://lamethods.org/book2/chapters/ch17-tna-clusters/ch17-tna-clusters.html)
  — a heterogeneity approach

**Dynalytics** · [dynasite.org](https://dynasite.org/)

- [Package sites](https://pak.dynasite.org/) — documentation of the R
  packages of the Dynalytics framework
- [Tools](https://dynasite.org/#/tools) — interactive web apps, jamovi,
  and Python
- [CarmNotes](https://dynasite.org/carmnotes) — single-file notebooks
  that run the analyses in the browser, even offline
- [Selected articles](https://dynasite.org/#/papers) — publications on
  the framework and its methods

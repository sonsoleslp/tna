# tna — R Package Documentation

------------------------------------------------------------------------

**tna** provides tools for performing Transition Network Analysis (TNA)
to study relational dynamics. Build Markov models from sequence data,
compute centrality measures, detect communities and cliques, and
validate findings with bootstrapping and permutation tests.

**Install:**

``` r
install.packages("tna")
# or dev version:
devtools::install_github("sonsoleslp/tna")
```

Requires R \>= 4.1.0. All analysis objects have
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.

------------------------------------------------------------------------

## Building Models

### `build_model()`

Construct a transition network analysis model from sequence data.
Requires specifying `type`. For convenience, use the shortcut functions:
[`tna()`](http://sonsoles.me/tna/reference/build_model.md) = relative,
[`ftna()`](http://sonsoles.me/tna/reference/build_model.md) = frequency,
[`ctna()`](http://sonsoles.me/tna/reference/build_model.md) =
co-occurrence,
[`atna()`](http://sonsoles.me/tna/reference/build_model.md) = attention.

    build_model(x, type = "relative", scaling = character(0L), cols = everything(),
                params = list(), inits, begin_state, end_state)

| Parameter | Description                                                                                                 | Default                                      |
|-----------|-------------------------------------------------------------------------------------------------------------|----------------------------------------------|
| `x`       | `data.frame` (wide), `stslist`, `matrix`, or `tna_data`                                                     | —                                            |
| `type`    | `"relative"`, `"frequency"`, `"co-occurrence"`, `"n-gram"`, `"gap"`, `"window"`, `"reverse"`, `"attention"` | `"relative"`                                 |
| `scaling` | `"minmax"`, `"max"`, `"rank"`, or empty vector                                                              | `character(0)`                               |
| `params`  | List: `n_gram`, `max_gap`, `window_size`, `weighted`, `direction`, `decay`, `lambda`, `time`, `duration`    | [`list()`](https://rdrr.io/r/base/list.html) |

**Returns:** A `tna` object with `$weights`, `$inits`, `$labels`,
`$data`.

``` r
# Using build_model with explicit type
model <- build_model(group_regulation, type = "relative")

# Shortcut functions (preferred)
model <- tna(group_regulation)       # type = "relative"
model_f <- ftna(group_regulation)    # type = "frequency"
model_c <- ctna(group_regulation)    # type = "co-occurrence"
model_a <- atna(group_regulation)    # type = "attention"

print(model)
```

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

``` r
summary(model)
```

    #> # A tibble: 13 × 2
    #>    metric                         value
    #>  * <chr>                          <dbl>
    #>  1 Node Count                  9   e+ 0
    #>  2 Edge Count                  7.8 e+ 1
    #>  3 Network Density             1   e+ 0
    #>  4 Mean Distance               4.72e- 2
    #>  5 Mean Out-Strength           1   e+ 0
    #>  6 SD Out-Strength             8.07e- 1
    #>  7 Mean In-Strength            1   e+ 0
    #>  8 SD In-Strength              6.80e-17
    #>  9 Mean Out-Degree             8.67e+ 0
    #> 10 SD Out-Degree               7.07e- 1
    #> 11 Centralization (Out-Degree) 1.56e- 2
    #> 12 Centralization (In-Degree)  1.56e- 2
    #> 13 Reciprocity                 9.86e- 1

#### Model types

| Type              | Description                                       |
|-------------------|---------------------------------------------------|
| `"relative"`      | Transition probabilities (rows sum to 1). Default |
| `"frequency"`     | Raw transition counts                             |
| `"co-occurrence"` | Co-occurrence within sequences                    |
| `"n-gram"`        | Higher-order transitions                          |
| `"gap"`           | Non-adjacent transitions, weighted by gap         |
| `"window"`        | Transitions within sliding window                 |
| `"reverse"`       | Reverse order (reply networks)                    |
| `"attention"`     | Exponential decay-weighted downstream pairs       |

### `group_model()` / `group_tna()`

Build a TNA model for each group. Accepts manual group assignments,
`mhmm` objects, or `tna_clustering` results. Most tna functions accept
`group_tna` directly — no need to specify groups again.

    group_model(x, group, type = "relative", scaling = character(0L),
                groupwise = FALSE, cols = everything(), params = list(), na.rm = TRUE, ...)

``` r
# From mixture Markov model
model <- group_model(engagement_mmm)
print(model)
```

    #> Cluster 1 :
    #> State Labels : 
    #> 
    #>    Active, Average, Disengaged 
    #> 
    #> Transition Probability Matrix :
    #> 
    #>                Active    Average Disengaged
    #> Active     0.85985688 0.08919748 0.05094565
    #> Average    0.31210322 0.54208478 0.14581200
    #> Disengaged 0.04791061 0.16179397 0.79029542
    #> 
    #> Initial Probabilities : 
    #> 
    #>     Active    Average Disengaged 
    #>  0.3397762  0.3234995  0.3367243 
    #> 
    #> Cluster 2 :
    #> State Labels : 
    #> 
    #>    Active, Average, Disengaged 
    #> 
    #> Transition Probability Matrix :
    #> 
    #>                Active   Average Disengaged
    #> Active     0.84090909 0.1590909  0.0000000
    #> Average    0.09259259 0.6296296  0.2777778
    #> Disengaged 0.15555556 0.5111111  0.3333333
    #> 
    #> Initial Probabilities : 
    #> 
    #>     Active    Average Disengaged 
    #> 0.75000000 0.08333333 0.16666667 
    #> 
    #> Cluster 3 :
    #> State Labels : 
    #> 
    #>    Active, Average, Disengaged 
    #> 
    #> Transition Probability Matrix :
    #> 
    #>               Active   Average Disengaged
    #> Active     0.5833333 0.1250000 0.29166667
    #> Average    0.1527778 0.8194444 0.02777778
    #> Disengaged 0.0000000 0.6000000 0.40000000
    #> 
    #> Initial Probabilities : 
    #> 
    #>     Active    Average Disengaged 
    #>          0          0          1

``` r
summary(model)
```

    #> # A tibble: 13 × 4
    #>    metric                      `Cluster 1` `Cluster 2` `Cluster 3`
    #>  * <chr>                             <dbl>       <dbl>       <dbl>
    #>  1 Node Count                        3           3           3    
    #>  2 Edge Count                        9           8           8    
    #>  3 Network Density                   1           1           1    
    #>  4 Mean Distance                     0.111       0.239       0.302
    #>  5 Mean Out-Strength                 1           1           1    
    #>  6 SD Out-Strength                   0.214       0.353       0.472
    #>  7 Mean In-Strength                  1           1           1    
    #>  8 SD In-Strength                    0           0           0    
    #>  9 Mean Out-Degree                   3           2.67        2.67 
    #> 10 SD Out-Degree                     0           0.577       0.577
    #> 11 Centralization (Out-Degree)       0           0.25        0.25 
    #> 12 Centralization (In-Degree)        0           0.25        0.25 
    #> 13 Reciprocity                       1           0.8         0.8

``` r
# Manual groups
group <- c(rep("High", 1000), rep("Low", 1000))
model <- group_model(group_regulation, group = group)
```

### `sna()`

Build a social network analysis model from edge-list data (from, to,
weight).

``` r
set.seed(42)
d <- data.frame(
  from = sample(LETTERS[1:4], 100, replace = TRUE),
  to = sample(LETTERS[1:4], 100, replace = TRUE),
  weight = rexp(100)
)
model_sna <- sna(d)
```

------------------------------------------------------------------------

## Plotting

All TNA analysis objects have
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods
returning qgraph or ggplot2 objects.

### `plot()`

Plot a TNA model as a transition network.

    plot(x, labels, colors, pie, cut, show_pruned = TRUE, pruned_edge_color = "pink",
         edge.color = NA, edge.labels = TRUE, edge.label.position = 0.65,
         layout = "circle", layout_args = list(), scale_nodes, scaling_factor = 0.5,
         mar = rep(5, 4), theme = "colorblind", ...)

``` r
model <- tna(group_regulation)
plot(model)
```

![](new-docs_files/figure-html/unnamed-chunk-6-1.png)

``` r
plot(model, layout = "spring", scale_nodes = "OutStrength")
```

![](new-docs_files/figure-html/unnamed-chunk-7-1.png)

``` r
# Group model — side-by-side panels
gmodel <- group_model(engagement_mmm)
plot(gmodel)
```

![](new-docs_files/figure-html/unnamed-chunk-8-1.png)![](new-docs_files/figure-html/unnamed-chunk-8-2.png)![](new-docs_files/figure-html/unnamed-chunk-8-3.png)

### `plot_frequencies()`

Bar plot of state frequency distribution.

``` r
model <- tna(group_regulation)
plot_frequencies(model)
```

![](new-docs_files/figure-html/unnamed-chunk-9-1.png)

``` r
# Group comparison
gmodel <- group_model(engagement_mmm)
plot_frequencies(gmodel)
```

![](new-docs_files/figure-html/unnamed-chunk-10-1.png)

### `plot_mosaic()`

Mosaic plot with chi-square test results. Requires frequency model
([`ftna()`](http://sonsoles.me/tna/reference/build_model.md)).

``` r
model_f <- ftna(group_regulation)
plot_mosaic(model_f)
```

![](new-docs_files/figure-html/unnamed-chunk-11-1.png)

### `plot_sequences()`

Sequence index plots or state distribution plots.

    plot_sequences(x, cols = everything(), group, type = "index", scale = "proportion",
                   geom = "bar", include_na = FALSE, colors, na_color = "white",
                   sort_by, show_n = TRUE, border, title, legend_title, xlab, ylab,
                   tick = 5, ncol = 2L, ...)

``` r
plot_sequences(group_regulation)
```

![](new-docs_files/figure-html/unnamed-chunk-12-1.png)

``` r
plot_sequences(group_regulation, type = "distribution")
```

![](new-docs_files/figure-html/unnamed-chunk-13-1.png)

``` r
# Group comparison — pass group_tna directly
gmodel <- group_model(engagement_mmm)
plot_sequences(gmodel)
```

![](new-docs_files/figure-html/unnamed-chunk-14-1.png)

### `plot_compare()`

Difference network between two models. Green = x greater, red = y
greater.

``` r
model_a <- tna(group_regulation[1:1000, ])
model_b <- tna(group_regulation[1001:2000, ])
plot_compare(model_a, model_b)
```

![](new-docs_files/figure-html/unnamed-chunk-15-1.png)

### `plot_associations()`

Association network. Requires frequency model
([`ftna()`](http://sonsoles.me/tna/reference/build_model.md)).

``` r
model_f <- ftna(group_regulation)
plot_associations(model_f)
```

![](new-docs_files/figure-html/unnamed-chunk-16-1.png)

### `hist()`

``` r
model <- tna(group_regulation)
hist(model)
```

![](new-docs_files/figure-html/unnamed-chunk-17-1.png)

#### Additional plot methods

| Object                    | plot() produces                       |
|---------------------------|---------------------------------------|
| `tna_centralities`        | Lollipop charts by measure            |
| `tna_communities`         | Network colored by community          |
| `tna_cliques`             | Individual clique subnetworks         |
| `tna_bootstrap`           | Significant edges only                |
| `tna_permutation`         | Significant edge differences          |
| `tna_stability`           | Stability curves with CS-coefficients |
| `tna_comparison`          | Heatmap or difference viz             |
| `tna_sequence_comparison` | Pattern comparison chart              |
| `group_tna`               | Side-by-side network plots            |

------------------------------------------------------------------------

## Data Preparation

### `prepare_data()`

Convert long-format event logs into wide-format sequences.

    prepare_data(data, actor, time, action, order, time_threshold = 900,
                 custom_format = NULL, is_unix_time = FALSE,
                 unix_time_unit = "seconds", unused_fn = dplyr::first)

``` r
results <- prepare_data(
  group_regulation_long,
  actor = "Actor", time = "Time", action = "Action"
)
print(results$statistics)
```

    #> $total_sessions
    #> [1] 2000
    #> 
    #> $total_actions
    #> [1] 27533
    #> 
    #> $max_sequence_length
    #> [1] 26
    #> 
    #> $unique_users
    #> [1] 2000
    #> 
    #> $sessions_per_user
    #> # A tibble: 2,000 × 2
    #>    Actor n_sessions
    #>    <int>      <int>
    #>  1     1          1
    #>  2     2          1
    #>  3     3          1
    #>  4     4          1
    #>  5     5          1
    #>  6     6          1
    #>  7     7          1
    #>  8     8          1
    #>  9     9          1
    #> 10    10          1
    #> # ℹ 1,990 more rows
    #> 
    #> $actions_per_session
    #> # A tibble: 2,000 × 2
    #>    .session_id   n_actions
    #>    <chr>             <int>
    #>  1 1010 session1        26
    #>  2 1015 session1        26
    #>  3 1030 session1        26
    #>  4 1092 session1        26
    #>  5 1106 session1        26
    #>  6 1107 session1        26
    #>  7 1153 session1        26
    #>  8 1184 session1        26
    #>  9 1209 session1        26
    #> 10 1267 session1        26
    #> # ℹ 1,990 more rows
    #> 
    #> $time_range
    #> [1] "2025-01-01 08:01:16 UTC" "2025-01-01 13:03:20 UTC"

### `import_data()`

Transform wide-format feature data into long-format sequence data.

    import_data(data, cols, id_cols, window_size = 1, replace_zeros = TRUE)

### `import_onehot()`

Import one-hot encoded data as co-occurrence network.

    import_onehot(data, cols, window = 1L)

### `simulate()`

Simulate sequence data from a TNA model (requires `type = "relative"`).

``` r
model <- tna(group_regulation)
sim <- simulate(model, nsim = 5, seed = 123, max_len = 10)
print(sim)
```

    #>         T1         T2        T3      T4        T5       T6         T7
    #> 1     plan    emotion consensus monitor consensus  discuss  synthesis
    #> 2  monitor    emotion      plan    plan   emotion     plan  consensus
    #> 3     plan       plan      plan discuss   discuss cohesion coregulate
    #> 4  monitor coregulate      plan    plan      plan     plan    monitor
    #> 5 cohesion  consensus consensus    plan      plan  monitor    emotion
    #>           T8         T9       T10
    #> 1      adapt       plan      plan
    #> 2    discuss   cohesion consensus
    #> 3       plan  consensus      plan
    #> 4 coregulate  consensus      plan
    #> 5  consensus coregulate   monitor

------------------------------------------------------------------------

## Centrality Measures

### `centralities()`

Calculate centrality measures. Works on `tna`, `group_tna`, and
`matrix`.

    centralities(x, loops = FALSE, normalize = FALSE, measures)

| Measure                                      | Description                           |
|----------------------------------------------|---------------------------------------|
| `OutStrength`                                | Total weight of outgoing edges        |
| `InStrength`                                 | Total weight of incoming edges        |
| `ClosenessIn` / `ClosenessOut` / `Closeness` | Closeness centrality variants         |
| `Betweenness`                                | Geodesic betweenness                  |
| `BetweennessRSP`                             | Randomized shortest paths betweenness |
| `Diffusion`                                  | Diffusion centrality                  |
| `Clustering`                                 | Signed clustering coefficient         |

``` r
model <- tna(group_regulation)
cm <- centralities(model)
print(cm)
```

    #> # A tibble: 9 × 10
    #>   state    OutStrength InStrength ClosenessIn ClosenessOut Closeness Betweenness
    #> * <fct>          <dbl>      <dbl>       <dbl>        <dbl>     <dbl>       <dbl>
    #> 1 adapt          1          0.345     0.00834       0.0152    0.0248           1
    #> 2 cohesion       0.973      0.812     0.0138        0.0124    0.0265           0
    #> 3 consens…       0.918      2.67      0.0351        0.0125    0.0383          30
    #> 4 coregul…       0.977      0.567     0.0155        0.0150    0.0210           0
    #> 5 discuss        0.805      1.19      0.0196        0.0131    0.0271          16
    #> 6 emotion        0.923      0.894     0.0141        0.0121    0.0231           5
    #> 7 monitor        0.982      0.346     0.00758       0.0137    0.0193           0
    #> 8 plan           0.626      1.19      0.0274        0.0115    0.0274           9
    #> 9 synthes…       1          0.192     0.00997       0.0158    0.0243           7
    #> # ℹ 3 more variables: BetweennessRSP <dbl>, Diffusion <dbl>, Clustering <dbl>

``` r
plot(cm, ncol = 3, reorder = TRUE)
```

![](new-docs_files/figure-html/unnamed-chunk-20-1.png)

``` r
# On group_tna directly
gmodel <- group_model(engagement_mmm)
centralities(gmodel)
```

    #> # A tibble: 9 × 11
    #>   group     state      OutStrength InStrength ClosenessIn ClosenessOut Closeness
    #> * <chr>     <fct>            <dbl>      <dbl>       <dbl>        <dbl>     <dbl>
    #> 1 Cluster 1 Active           0.140      0.360      0.0794       0.0342    0.0794
    #> 2 Cluster 1 Average          0.458      0.251      0.0575       0.0994    0.107 
    #> 3 Cluster 1 Disengaged       0.210      0.197      0.0401       0.0642    0.0642
    #> 4 Cluster 2 Active           0.159      0.248      0.0608       0.0618    0.0787
    #> 5 Cluster 2 Average          0.370      0.670      0.121        0.0734    0.121 
    #> 6 Cluster 2 Disengaged       0.667      0.278      0.0742       0.119     0.119 
    #> 7 Cluster 3 Active           0.417      0.153      0.0678       0.117     0.117 
    #> 8 Cluster 3 Average          0.181      0.725      0.148        0.0605    0.148 
    #> 9 Cluster 3 Disengaged       0.6        0.319      0.0746       0.101     0.196 
    #> # ℹ 4 more variables: Betweenness <dbl>, BetweennessRSP <dbl>, Diffusion <dbl>,
    #> #   Clustering <dbl>

### `betweenness_network()`

Build network with edge betweenness as weights.

``` r
model <- tna(group_regulation)
bn <- betweenness_network(model)
print(bn)
```

    #> State Labels : 
    #> 
    #>    adapt, cohesion, consensus, coregulate, discuss, emotion, monitor, plan, synthesis 
    #> 
    #> Edge Betweenness Matrix :
    #> 
    #>            adapt cohesion consensus coregulate discuss emotion monitor plan
    #> adapt          0        0         1          1       0       0       0    3
    #> cohesion       0        0         0          6       0       0       2    0
    #> consensus      0        0         0          0       0       0       0    4
    #> coregulate     8        1         0          0       0       0       0    0
    #> discuss        6        0         1          0       0       0       0    0
    #> emotion        0        0         0          0       1       0       0    0
    #> monitor        0        0         0          0       7       0       0    0
    #> plan          11        3         2          3       0       3       3    8
    #> synthesis      4        4         3          0       5       6       3    0
    #>            synthesis
    #> adapt              0
    #> cohesion           3
    #> consensus          0
    #> coregulate         1
    #> discuss            1
    #> emotion            7
    #> monitor           10
    #> plan               0
    #> synthesis          0
    #> 
    #> Initial Probabilities : 
    #> 
    #>      adapt   cohesion  consensus coregulate    discuss    emotion    monitor 
    #>     0.0115     0.0605     0.2140     0.0190     0.1755     0.1515     0.1440 
    #>       plan  synthesis 
    #>     0.2045     0.0195

### `estimate_cs()`

Centrality stability via subset sampling.

    estimate_cs(x, loops = FALSE, normalize = FALSE,
                measures = c("InStrength", "OutStrength", "Betweenness"),
                iter = 1000, method = "pearson",
                drop_prop = seq(0.1, 0.9, by = 0.1),
                threshold = 0.7, certainty = 0.95, progressbar = FALSE)

``` r
model <- tna(group_regulation)
cs <- estimate_cs(model, measures = c("InStrength", "OutStrength"), iter = 100)
print(cs)
```

    #> Centrality Stability Coefficients
    #> 
    #>  InStrength OutStrength 
    #>         0.9         0.9

``` r
plot(cs)
```

![](new-docs_files/figure-html/unnamed-chunk-23-1.png)

------------------------------------------------------------------------

## Network Structure

### `communities()`

Detect communities using 7 igraph algorithms. Works on `tna` and
`group_tna`.

    communities(x, methods, gamma = 1)

``` r
model <- tna(group_regulation)
comm <- communities(model)
print(comm)
```

    #> Number of communities found by each algorithm
    #> 
    #>         walktrap      fast_greedy       label_prop          infomap 
    #>                1                3                1                1 
    #> edge_betweenness    leading_eigen        spinglass 
    #>                1                3                2 
    #> 
    #> Community assignments
    #> 
    #>        state walktrap fast_greedy label_prop infomap edge_betweenness
    #> 1      adapt        1           1          1       1                1
    #> 2   cohesion        1           1          1       1                1
    #> 3  consensus        1           1          1       1                1
    #> 4 coregulate        1           2          1       1                1
    #> 5    discuss        1           2          1       1                1
    #> 6    emotion        1           1          1       1                1
    #> 7    monitor        1           2          1       1                1
    #> 8       plan        1           3          1       1                1
    #> 9  synthesis        1           1          1       1                1
    #>   leading_eigen spinglass
    #> 1             1         1
    #> 2             1         1
    #> 3             2         1
    #> 4             3         2
    #> 5             3         2
    #> 6             1         1
    #> 7             2         2
    #> 8             2         2
    #> 9             3         1

``` r
plot(comm, method = "spinglass")
```

![](new-docs_files/figure-html/unnamed-chunk-24-1.png)

``` r
# On group_tna directly
gmodel <- group_model(engagement_mmm)
communities(gmodel)
```

    #> Cluster 1 :
    #> Number of communities found by each algorithm
    #> 
    #>         walktrap      fast_greedy       label_prop          infomap 
    #>                1                3                3                1 
    #> edge_betweenness    leading_eigen        spinglass 
    #>                3                2                1 
    #> 
    #> Community assignments
    #> 
    #>        state walktrap fast_greedy label_prop infomap edge_betweenness
    #> 1     Active        1           1          1       1                1
    #> 2    Average        1           2          2       1                2
    #> 3 Disengaged        1           3          3       1                3
    #>   leading_eigen spinglass
    #> 1             1         1
    #> 2             1         1
    #> 3             2         1
    #> 
    #> Cluster 2 :
    #> Number of communities found by each algorithm
    #> 
    #>         walktrap      fast_greedy       label_prop          infomap 
    #>                1                2                3                1 
    #> edge_betweenness    leading_eigen        spinglass 
    #>                2                2                1 
    #> 
    #> Community assignments
    #> 
    #>        state walktrap fast_greedy label_prop infomap edge_betweenness
    #> 1     Active        1           2          1       1                1
    #> 2    Average        1           1          2       1                2
    #> 3 Disengaged        1           1          3       1                2
    #>   leading_eigen spinglass
    #> 1             1         1
    #> 2             2         1
    #> 3             2         1
    #> 
    #> Cluster 3 :
    #> Number of communities found by each algorithm
    #> 
    #>         walktrap      fast_greedy       label_prop          infomap 
    #>                1                3                3                1 
    #> edge_betweenness    leading_eigen        spinglass 
    #>                3                2                1 
    #> 
    #> Community assignments
    #> 
    #>        state walktrap fast_greedy label_prop infomap edge_betweenness
    #> 1     Active        1           1          1       1                1
    #> 2    Average        1           2          2       1                2
    #> 3 Disengaged        1           3          3       1                3
    #>   leading_eigen spinglass
    #> 1             1         1
    #> 2             2         1
    #> 3             2         1

### `cliques()`

Identify cliques (complete subgraphs) of a given size.

    cliques(x, size = 2, threshold = 0, sum_weights = FALSE, ...)

``` r
model <- tna(group_regulation)
cliq <- cliques(model, size = 2)
print(cliq)
```

    #> Number of 2-cliques = 35 (weight threshold = 0)
    #> Showing 6 cliques starting from clique number 1
    #> 
    #> Clique 1
    #>            monitor      plan
    #> monitor 0.01814375 0.2156315
    #> plan    0.07552379 0.3742082
    #> 
    #> Clique 2
    #>            emotion    monitor
    #> emotion 0.07684173 0.03630596
    #> monitor 0.09071877 0.01814375
    #> 
    #> Clique 3
    #>            emotion       plan
    #> emotion 0.07684173 0.09975326
    #> plan    0.14682475 0.37420822
    #> 
    #> Clique 4
    #>           discuss    emotion
    #> discuss 0.1948874 0.10579600
    #> emotion 0.1018682 0.07684173
    #> 
    #> Clique 5
    #>           discuss    monitor
    #> discuss 0.1948874 0.02227284
    #> monitor 0.3754361 0.01814375
    #> 
    #> Clique 6
    #>            discuss       plan
    #> discuss 0.19488737 0.01164262
    #> plan    0.06789021 0.37420822

``` r
plot(cliq, n = 3, ask = FALSE)
```

![](new-docs_files/figure-html/unnamed-chunk-26-1.png)![](new-docs_files/figure-html/unnamed-chunk-26-2.png)![](new-docs_files/figure-html/unnamed-chunk-26-3.png)

------------------------------------------------------------------------

## Comparison

### `compare()`

Compare two TNA models with comprehensive metrics.

``` r
gmodel <- group_model(engagement_mmm)
comp <- compare(gmodel, i = 1, j = 2)
print(comp)
```

    #> Edge difference metrics
    #> # A tibble: 9 × 16
    #>   source     target     weight_x weight_y raw_difference absolute_difference
    #>   <fct>      <fct>         <dbl>    <dbl>          <dbl>               <dbl>
    #> 1 Active     Active       0.860    0.841          0.0189              0.0189
    #> 2 Average    Active       0.312    0.0926         0.220               0.220 
    #> 3 Disengaged Active       0.0479   0.156         -0.108               0.108 
    #> 4 Active     Average      0.0892   0.159         -0.0699              0.0699
    #> 5 Average    Average      0.542    0.630         -0.0875              0.0875
    #> 6 Disengaged Average      0.162    0.511         -0.349               0.349 
    #> 7 Active     Disengaged   0.0509   0              0.0509              0.0509
    #> 8 Average    Disengaged   0.146    0.278         -0.132               0.132 
    #> 9 Disengaged Disengaged   0.790    0.333          0.457               0.457 
    #> # ℹ 10 more variables: squared_difference <dbl>, relative_difference <dbl>,
    #> #   similarity_strength_index <dbl>, difference_index <dbl>,
    #> #   rank_difference <dbl>, percentile_difference <dbl>,
    #> #   logarithmic_ratio <dbl>, standardized_weight_x <dbl>,
    #> #   standardized_weight_y <dbl>, standardized_score_inflation <dbl>
    #> 
    #> Summary metrics of differences
    #> # A tibble: 22 × 3
    #>    category          metric               value
    #>    <chr>             <chr>                <dbl>
    #>  1 Weight Deviations Mean Abs. Diff.      0.166
    #>  2 Weight Deviations Median Abs. Diff.    0.108
    #>  3 Weight Deviations RMS Diff.            0.217
    #>  4 Weight Deviations Max Abs. Diff.       0.457
    #>  5 Weight Deviations Rel. Mean Abs. Diff. 0.498
    #>  6 Weight Deviations CV Ratio             1.16 
    #>  7 Correlations      Pearson              0.710
    #>  8 Correlations      Spearman             0.733
    #>  9 Correlations      Kendall              0.611
    #> 10 Correlations      Distance             0.500
    #> # ℹ 12 more rows
    #> 
    #> Network metrics
    #> # A tibble: 13 × 3
    #>    metric                          x     y
    #>    <chr>                       <dbl> <dbl>
    #>  1 Node Count                  3     3    
    #>  2 Edge Count                  9     8    
    #>  3 Network Density             1     1    
    #>  4 Mean Distance               0.111 0.239
    #>  5 Mean Out-Strength           1     1    
    #>  6 SD Out-Strength             0.214 0.353
    #>  7 Mean In-Strength            1     1    
    #>  8 SD In-Strength              0     0    
    #>  9 Mean Out-Degree             3     2.67 
    #> 10 SD Out-Degree               0     0.577
    #> 11 Centralization (Out-Degree) 0     0.25 
    #> 12 Centralization (In-Degree)  0     0.25 
    #> 13 Reciprocity                 1     0.8

``` r
plot(comp)
```

![](new-docs_files/figure-html/unnamed-chunk-27-1.png)

### `compare_sequences()`

Compare subsequence patterns between groups. Pass `group_tna` directly.

    # For group_tna (groups already defined)
    compare_sequences(x, sub, min_freq = 5L, correction = "bonferroni", ...)

``` r
gmodel <- group_model(engagement_mmm)
comp_seq <- compare_sequences(gmodel)
print(head(comp_seq, 10))
```

    #>                    pattern freq_Cluster 1 freq_Cluster 2 freq_Cluster 3
    #> 1                   Active          12264            137             25
    #> 2                  Average           5069            113             74
    #> 3               Disengaged           6804             47             22
    #> 4           Active->Active          10093            111             14
    #> 7          Average->Active           1524             10             11
    #> 8         Average->Average           2647             68             59
    #> 11     Disengaged->Average           1057             23             12
    #> 12  Disengaged->Disengaged           5163             15              8
    #> 13  Active->Active->Active           8293             87              6
    #> 22 Average->Active->Active           1248              9              8
    #>    prop_Cluster 1 prop_Cluster 2 prop_Cluster 3
    #> 1      0.50809960     0.46127946     0.20661157
    #> 2      0.21000953     0.38047138     0.61157025
    #> 3      0.28189087     0.15824916     0.18181818
    #> 4      0.43590740     0.38947368     0.12068966
    #> 7      0.06582016     0.03508772     0.09482759
    #> 8      0.11432150     0.23859649     0.50862069
    #> 11     0.04565086     0.08070175     0.10344828
    #> 12     0.22298523     0.05263158     0.06896552
    #> 13     0.37404718     0.31868132     0.05405405
    #> 22     0.05628975     0.03296703     0.07207207

``` r
plot(comp_seq)
```

![](new-docs_files/figure-html/unnamed-chunk-28-1.png)

### `permutation_test()`

Permutation tests for edge weight and centrality differences.

    permutation_test(x, y, adjust = "none", iter = 1000, paired = FALSE,
                     level = 0.05, measures = character(0), ...)

``` r
model_x <- tna(group_regulation[1:200, ])
model_y <- tna(group_regulation[1001:1200, ])
perm <- permutation_test(model_x, model_y, iter = 100)
print(perm)
```

    #> # A tibble: 81 × 4
    #>    edge_name           diff_true effect_size p_value
    #>    <chr>                   <dbl>       <dbl>   <dbl>
    #>  1 adapt -> adapt       0            NaN     1      
    #>  2 cohesion -> adapt    0.00541        0.955 0.792  
    #>  3 consensus -> adapt  -0.000679      -0.178 0.772  
    #>  4 coregulate -> adapt  0.00769        0.507 0.624  
    #>  5 discuss -> adapt    -0.130         -6.39  0.00990
    #>  6 emotion -> adapt     0.0101         1.65  0.238  
    #>  7 monitor -> adapt    -0.00480       -0.420 1      
    #>  8 plan -> adapt        0.00339        1.61  0.0198 
    #>  9 synthesis -> adapt  -0.159         -2.27  0.0297 
    #> 10 adapt -> cohesion   -0.0907        -1.08  0.327  
    #> # ℹ 71 more rows

``` r
plot(perm)
```

![](new-docs_files/figure-html/unnamed-chunk-29-1.png)

------------------------------------------------------------------------

## Validation

### `bootstrap()`

Bootstrap transition networks for confidence intervals and significance.

    bootstrap(x, iter = 1000, level = 0.05, method = "stability",
              threshold, consistency_range = c(0.75, 1.25))

``` r
model <- tna(group_regulation)
boot <- bootstrap(model, iter = 100)
print(boot)
```

    #> Significant Edges
    #> 
    #>          from         to     weight    p_value   cr_lower   cr_upper   ci_lower
    #> 5     discuss      adapt 0.07137434 0.00990099 0.05353075 0.08921792 0.06367881
    #> 9   synthesis      adapt 0.23466258 0.00990099 0.17599693 0.29332822 0.20701286
    #> 10      adapt   cohesion 0.27308448 0.00990099 0.20481336 0.34135560 0.23473489
    #> 12  consensus   cohesion 0.01485227 0.01980198 0.01113920 0.01856533 0.01205628
    #> 14    discuss   cohesion 0.04758289 0.00990099 0.03568717 0.05947861 0.04174764
    #> 15    emotion   cohesion 0.32534367 0.00990099 0.24400775 0.40667959 0.30804386
    #> 16    monitor   cohesion 0.05582694 0.04950495 0.04187020 0.06978367 0.04593055
    #> 17       plan   cohesion 0.02517460 0.02970297 0.01888095 0.03146825 0.02108138
    #> 19      adapt  consensus 0.47740668 0.00990099 0.35805501 0.59675835 0.42613878
    #> 20   cohesion  consensus 0.49793510 0.00990099 0.37345133 0.62241888 0.47662556
    #> 21  consensus  consensus 0.08200348 0.00990099 0.06150261 0.10250435 0.07584074
    #> 22 coregulate  consensus 0.13451777 0.00990099 0.10088832 0.16814721 0.11847857
    #> 23    discuss  consensus 0.32118451 0.00990099 0.24088838 0.40148064 0.30467968
    #> 24    emotion  consensus 0.32040888 0.00990099 0.24030666 0.40051110 0.30173390
    #> 25    monitor  consensus 0.15910677 0.00990099 0.11933008 0.19888346 0.14027960
    #> 26       plan  consensus 0.29040117 0.00990099 0.21780088 0.36300146 0.27953126
    #> 27  synthesis  consensus 0.46625767 0.00990099 0.34969325 0.58282209 0.43197086
    #> 29   cohesion coregulate 0.11917404 0.00990099 0.08938053 0.14896755 0.10167983
    #> 30  consensus coregulate 0.18770738 0.00990099 0.14078053 0.23463422 0.17793546
    #> 32    discuss coregulate 0.08428246 0.00990099 0.06321185 0.10535308 0.07560094
    #> 33    emotion coregulate 0.03419105 0.00990099 0.02564329 0.04273881 0.02791664
    #> 34    monitor coregulate 0.05792045 0.03960396 0.04344033 0.07240056 0.04646185
    #> 35       plan coregulate 0.01721618 0.00990099 0.01291213 0.02152022 0.01459032
    #> 38   cohesion    discuss 0.05958702 0.01980198 0.04469027 0.07448378 0.05074250
    #> 39  consensus    discuss 0.18802338 0.00990099 0.14101754 0.23502923 0.17717085
    #> 40 coregulate    discuss 0.27360406 0.00990099 0.20520305 0.34200508 0.25477661
    #> 41    discuss    discuss 0.19488737 0.00990099 0.14616553 0.24360921 0.18565030
    #> 42    emotion    discuss 0.10186817 0.00990099 0.07640113 0.12733521 0.09352769
    #> 43    monitor    discuss 0.37543615 0.00990099 0.28157711 0.46929518 0.35308960
    #> 44       plan    discuss 0.06789021 0.00990099 0.05091765 0.08486276 0.06197963
    #> 46      adapt    emotion 0.11984283 0.04950495 0.08988212 0.14980354 0.09281278
    #> 47   cohesion    emotion 0.11563422 0.00990099 0.08672566 0.14454277 0.10241874
    #> 48  consensus    emotion 0.07268131 0.00990099 0.05451098 0.09085164 0.06742980
    #> 49 coregulate    emotion 0.17208122 0.00990099 0.12906091 0.21510152 0.16052079
    #> 50    discuss    emotion 0.10579600 0.00990099 0.07934700 0.13224500 0.09579402
    #> 51    emotion    emotion 0.07684173 0.00990099 0.05763130 0.09605217 0.06896004
    #> 52    monitor    emotion 0.09071877 0.00990099 0.06803908 0.11339846 0.07887495
    #> 53       plan    emotion 0.14682475 0.00990099 0.11011856 0.18353094 0.13732065
    #> 54  synthesis    emotion 0.07055215 0.04950495 0.05291411 0.08819018 0.05178816
    #> 57  consensus    monitor 0.04661084 0.00990099 0.03495813 0.05826355 0.04201153
    #> 58 coregulate    monitor 0.08629442 0.01980198 0.06472081 0.10786802 0.07379825
    #> 59    discuss    monitor 0.02227284 0.02970297 0.01670463 0.02784105 0.01790366
    #> 60    emotion    monitor 0.03630596 0.03960396 0.02722947 0.04538245 0.02897804
    #> 62       plan    monitor 0.07552379 0.00990099 0.05664285 0.09440474 0.06877714
    #> 65   cohesion       plan 0.14100295 0.01980198 0.10575221 0.17625369 0.12295365
    #> 66  consensus       plan 0.39579712 0.00990099 0.29684784 0.49474641 0.38378502
    #> 67 coregulate       plan 0.23908629 0.00990099 0.17931472 0.29885787 0.22317909
    #> 69    emotion       plan 0.09975326 0.00990099 0.07481495 0.12469158 0.08958911
    #> 70    monitor       plan 0.21563154 0.00990099 0.16172366 0.26953943 0.19363832
    #> 71       plan       plan 0.37420822 0.00990099 0.28065616 0.46776027 0.36426884
    #> 77    discuss  synthesis 0.14097697 0.00990099 0.10573273 0.17622121 0.13191415
    #>      ci_upper
    #> 5  0.07931108
    #> 9  0.27622294
    #> 10 0.30525211
    #> 12 0.01771094
    #> 14 0.05606304
    #> 15 0.34006825
    #> 16 0.06835336
    #> 17 0.02994972
    #> 19 0.51727855
    #> 20 0.52501156
    #> 21 0.08936260
    #> 22 0.14866299
    #> 23 0.33733788
    #> 24 0.33839222
    #> 25 0.18140490
    #> 26 0.30109002
    #> 27 0.49930792
    #> 29 0.13193545
    #> 30 0.19671486
    #> 32 0.09358981
    #> 33 0.04044038
    #> 34 0.07144758
    #> 35 0.02016765
    #> 38 0.07166670
    #> 39 0.19802329
    #> 40 0.29142960
    #> 41 0.20743275
    #> 42 0.11336058
    #> 43 0.39270725
    #> 44 0.07465571
    #> 46 0.15223304
    #> 47 0.13081530
    #> 48 0.07929329
    #> 49 0.18994876
    #> 50 0.11388384
    #> 51 0.08880540
    #> 52 0.10276264
    #> 53 0.15537353
    #> 54 0.08594947
    #> 57 0.05059107
    #> 58 0.09831364
    #> 59 0.02691340
    #> 60 0.04395399
    #> 62 0.08361186
    #> 65 0.15535754
    #> 66 0.40886840
    #> 67 0.26287843
    #> 69 0.11052575
    #> 70 0.23786836
    #> 71 0.38633723
    #> 77 0.14920203
    #> 
    #> Non-significant Edges
    #> 
    #>          from         to       weight    p_value     cr_lower    cr_upper
    #> 2    cohesion      adapt 0.0029498525 0.49504950 0.0022123894 0.003687316
    #> 3   consensus      adapt 0.0047400853 0.21782178 0.0035550640 0.005925107
    #> 4  coregulate      adapt 0.0162436548 0.21782178 0.0121827411 0.020304569
    #> 6     emotion      adapt 0.0024673951 0.53465347 0.0018505464 0.003084244
    #> 7     monitor      adapt 0.0111653873 0.38613861 0.0083740405 0.013956734
    #> 8        plan      adapt 0.0009745006 0.54455446 0.0007308754 0.001218126
    #> 11   cohesion   cohesion 0.0271386431 0.18811881 0.0203539823 0.033923304
    #> 13 coregulate   cohesion 0.0360406091 0.05940594 0.0270304569 0.045050761
    #> 18  synthesis   cohesion 0.0337423313 0.22772277 0.0253067485 0.042177914
    #> 28      adapt coregulate 0.0216110020 0.37623762 0.0162082515 0.027013752
    #> 31 coregulate coregulate 0.0233502538 0.10891089 0.0175126904 0.029187817
    #> 36  synthesis coregulate 0.0444785276 0.15841584 0.0333588957 0.055598160
    #> 37      adapt    discuss 0.0589390963 0.15841584 0.0442043222 0.073673870
    #> 45  synthesis    discuss 0.0628834356 0.10891089 0.0471625767 0.078604294
    #> 55      adapt    monitor 0.0333988212 0.29702970 0.0250491159 0.041748527
    #> 56   cohesion    monitor 0.0330383481 0.07920792 0.0247787611 0.041297935
    #> 61    monitor    monitor 0.0181437544 0.23762376 0.0136078158 0.022679693
    #> 63  synthesis    monitor 0.0122699387 0.47524752 0.0092024540 0.015337423
    #> 64      adapt       plan 0.0157170923 0.45544554 0.0117878193 0.019646365
    #> 68    discuss       plan 0.0116426221 0.08910891 0.0087319666 0.014553278
    #> 72  synthesis       plan 0.0751533742 0.09900990 0.0563650307 0.093941718
    #> 74   cohesion  synthesis 0.0035398230 0.50495050 0.0026548673 0.004424779
    #> 75  consensus  synthesis 0.0075841365 0.11881188 0.0056881024 0.009480171
    #> 76 coregulate  synthesis 0.0187817259 0.11881188 0.0140862944 0.023477157
    #> 78    emotion  synthesis 0.0028198802 0.44554455 0.0021149101 0.003524850
    #> 79    monitor  synthesis 0.0160502442 0.20792079 0.0120376832 0.020062805
    #> 80       plan  synthesis 0.0017865844 0.45544554 0.0013399383 0.002233230
    #>        ci_lower    ci_upper
    #> 2  0.0008665117 0.005316437
    #> 3  0.0031587248 0.006378932
    #> 4  0.0108815165 0.022058809
    #> 6  0.0010328193 0.004554184
    #> 7  0.0065061526 0.018165027
    #> 8  0.0003211634 0.001829441
    #> 11 0.0184520277 0.036674923
    #> 13 0.0272324700 0.044221901
    #> 18 0.0227079343 0.047813053
    #> 28 0.0085574724 0.033282104
    #> 31 0.0173449885 0.029993682
    #> 36 0.0297407269 0.061312201
    #> 37 0.0389441796 0.080707895
    #> 45 0.0464826840 0.081322893
    #> 55 0.0212333932 0.048640941
    #> 56 0.0241721485 0.041718486
    #> 61 0.0120850617 0.024401871
    #> 63 0.0031941960 0.020545371
    #> 64 0.0066873732 0.027298471
    #> 68 0.0085419047 0.015023891
    #> 72 0.0565045249 0.097779945
    #> 74 0.0011616344 0.006411964
    #> 75 0.0057723386 0.009739495
    #> 76 0.0132478549 0.023734366
    #> 78 0.0010390881 0.004428049
    #> 79 0.0092485497 0.022666701
    #> 80 0.0008166263 0.003093354

``` r
plot(boot)
```

![](new-docs_files/figure-html/unnamed-chunk-30-1.png)

### `bootstrap_cliques()`

Bootstrap cliques to assess stability.

``` r
bc <- bootstrap_cliques(model, size = 2, iter = 100)
print(bc)
```

    #>                  clique mean_weight   p_values   sig    cr_lower   cr_upper
    #> 1        plan-synthesis  0.11278704 0.42574257 FALSE 0.084590283 0.14098381
    #> 2          monitor-plan  0.17087683 0.24752475 FALSE 0.128157620 0.21359603
    #> 3     monitor-synthesis  0.01161598 0.74257426 FALSE 0.008711988 0.01451998
    #> 4       emotion-monitor  0.05550255 0.61386139 FALSE 0.041626916 0.06937819
    #> 5          emotion-plan  0.17440699 0.03960396  TRUE 0.130805243 0.21800874
    #> 6     emotion-synthesis  0.03755344 0.77227723 FALSE 0.028165080 0.04694180
    #> 7       discuss-emotion  0.11984832 0.74257426 FALSE 0.089886239 0.14981040
    #> 8       discuss-monitor  0.15268503 0.94059406 FALSE 0.114513772 0.19085629
    #> 9          discuss-plan  0.16215710 0.46534653 FALSE 0.121617828 0.20269638
    #> 10    discuss-synthesis  0.09968694 0.21782178 FALSE 0.074765208 0.12460868
    #> 11   coregulate-discuss  0.14403104 0.92079208 FALSE 0.108023277 0.18003880
    #> 12   coregulate-emotion  0.07661606 0.41584158 FALSE 0.057462047 0.09577008
    #> 13   coregulate-monitor  0.04642722 0.43564356 FALSE 0.034820413 0.05803402
    #> 14      coregulate-plan  0.16346524 0.34653465 FALSE 0.122598927 0.20433154
    #> 15 coregulate-synthesis  0.02165263 0.86138614 FALSE 0.016239470 0.02706578
    #> 16 consensus-coregulate  0.10689472 0.27722772 FALSE 0.080171039 0.13361840
    #> 17    consensus-discuss  0.19652469 0.02970297  TRUE 0.147393514 0.24565586
    #> 18    consensus-emotion  0.13798385 0.09900990 FALSE 0.103487888 0.17247981
    #> 19    consensus-monitor  0.07646621 0.80198020 FALSE 0.057349657 0.09558276
    #> 20       consensus-plan  0.28560250 0.00990099  TRUE 0.214201873 0.35700312
    #> 21  consensus-synthesis  0.13896132 0.55445545 FALSE 0.104220990 0.17370165
    #> 22   cohesion-consensus  0.15548237 0.44554455 FALSE 0.116611779 0.19435297
    #> 23  cohesion-coregulate  0.05142589 0.76237624 FALSE 0.038569415 0.06428236
    #> 24     cohesion-discuss  0.08229898 0.48514851 FALSE 0.061724236 0.10287373
    #> 25     cohesion-emotion  0.13623957 0.15841584 FALSE 0.102179675 0.17029946
    #> 26     cohesion-monitor  0.03353692 0.77227723 FALSE 0.025152690 0.04192115
    #> 27        cohesion-plan  0.14188110 0.24752475 FALSE 0.106410827 0.17735138
    #> 28   cohesion-synthesis  0.01610520 0.74257426 FALSE 0.012078900 0.02013150
    #> 29       adapt-cohesion  0.07579324 0.78217822 FALSE 0.056844933 0.09474155
    #> 30      adapt-consensus  0.14103756 0.59405941 FALSE 0.105778170 0.17629695
    #> 31     adapt-coregulate  0.01530123 0.83168317 FALSE 0.011475921 0.01912653
    #> 32        adapt-discuss  0.08130020 0.97029703 FALSE 0.060975150 0.10162525
    #> 33        adapt-emotion  0.04978799 0.84158416 FALSE 0.037340992 0.06223499
    #> 34        adapt-monitor  0.01567699 0.76237624 FALSE 0.011757743 0.01959624
    #> 35           adapt-plan  0.09772495 0.31683168 FALSE 0.073293715 0.12215619
    #> 36      adapt-synthesis  0.05866564 0.74257426 FALSE 0.043999233 0.07333206
    #>       ci_lower   ci_upper
    #> 1  0.071210784 0.18253268
    #> 2  0.129040598 0.25717683
    #> 3  0.000000000 0.04923387
    #> 4  0.031250000 0.10752474
    #> 5  0.133218516 0.21150281
    #> 6  0.000000000 0.06776661
    #> 7  0.040969754 0.11360746
    #> 8  0.042664420 0.11959532
    #> 9  0.095587292 0.16019960
    #> 10 0.062500000 0.11862863
    #> 11 0.048403841 0.12795375
    #> 12 0.052807918 0.12375914
    #> 13 0.018947557 0.08008213
    #> 14 0.096851060 0.16182131
    #> 15 0.000000000 0.02569079
    #> 16 0.053195264 0.12402108
    #> 17 0.170273371 0.23990507
    #> 18 0.093726377 0.16847604
    #> 19 0.065863424 0.19096576
    #> 20 0.238441032 0.30085310
    #> 21 0.089430833 0.25135908
    #> 22 0.137539118 0.23496462
    #> 23 0.037643138 0.12877764
    #> 24 0.030658131 0.11651286
    #> 25 0.080848148 0.15909304
    #> 26 0.020552885 0.08502618
    #> 27 0.082228659 0.15288178
    #> 28 0.000000000 0.03667952
    #> 29 0.014921875 0.21155123
    #> 30 0.014781582 0.21356422
    #> 31 0.000000000 0.02960069
    #> 32 0.009609693 0.06069361
    #> 33 0.004804846 0.19125000
    #> 34 0.000000000 0.04923387
    #> 35 0.054743421 0.10782392
    #> 36 0.000000000 0.09501202

### `prune()`

Remove weak edges. Four methods available.

    prune(x, method = "threshold", threshold = 0.1, lowest = 0.05,
          level = 0.5, boot = NULL, ...)

``` r
model <- tna(group_regulation)

pruned_t <- prune(model, method = "threshold", threshold = 0.1)
pruned_p <- prune(model, method = "lowest", lowest = 0.05)
pruned_d <- prune(model, method = "disparity", level = 0.5)

pruning_details(pruned_t)
```

    #> **Pruning Details**
    #> 
    #> Method used: User-specified threshold (0.1)
    #> Number of removed edges: 49
    #> Number of retained edges: 29
    #> 
    #> **Removed edges**
    #> 
    #>          from         to       weight
    #> 1    cohesion      adapt 0.0029498525
    #> 2   consensus      adapt 0.0047400853
    #> 3  coregulate      adapt 0.0162436548
    #> 4     discuss      adapt 0.0713743356
    #> 5     emotion      adapt 0.0024673951
    #> 6     monitor      adapt 0.0111653873
    #> 7        plan      adapt 0.0009745006
    #> 8    cohesion   cohesion 0.0271386431
    #> 9   consensus   cohesion 0.0148522673
    #> 10 coregulate   cohesion 0.0360406091
    #> 11    discuss   cohesion 0.0475828904
    #> 12    monitor   cohesion 0.0558269365
    #> 13       plan   cohesion 0.0251745980
    #> 14  synthesis   cohesion 0.0337423313
    #> 15  consensus  consensus 0.0820034761
    #> 16      adapt coregulate 0.0216110020
    #> 17 coregulate coregulate 0.0233502538
    #> 18    discuss coregulate 0.0842824601
    #> 19    emotion coregulate 0.0341910469
    #> 20    monitor coregulate 0.0579204466
    #> 21       plan coregulate 0.0172161767
    #> 22  synthesis coregulate 0.0444785276
    #> 23      adapt    discuss 0.0589390963
    #> 24   cohesion    discuss 0.0595870206
    #> 25       plan    discuss 0.0678902063
    #> 26  synthesis    discuss 0.0628834356
    #> 27  consensus    emotion 0.0726813083
    #> 28    emotion    emotion 0.0768417342
    #> 29    monitor    emotion 0.0907187718
    #> 30  synthesis    emotion 0.0705521472
    #> 31      adapt    monitor 0.0333988212
    #> 32   cohesion    monitor 0.0330383481
    #> 33  consensus    monitor 0.0466108390
    #> 34 coregulate    monitor 0.0862944162
    #> 35    discuss    monitor 0.0222728423
    #> 36    emotion    monitor 0.0363059570
    #> 37    monitor    monitor 0.0181437544
    #> 38       plan    monitor 0.0755237941
    #> 39  synthesis    monitor 0.0122699387
    #> 40      adapt       plan 0.0157170923
    #> 41    discuss       plan 0.0116426221
    #> 42    emotion       plan 0.0997532605
    #> 43  synthesis       plan 0.0751533742
    #> 44   cohesion  synthesis 0.0035398230
    #> 45  consensus  synthesis 0.0075841365
    #> 46 coregulate  synthesis 0.0187817259
    #> 47    emotion  synthesis 0.0028198802
    #> 48    monitor  synthesis 0.0160502442
    #> 49       plan  synthesis 0.0017865844

``` r
plot(pruned_t)
```

![](new-docs_files/figure-html/unnamed-chunk-33-1.png)

``` r
# Restore and reapply
restored <- deprune(pruned_t)
repruned <- reprune(restored)
```

------------------------------------------------------------------------

## Clustering

### `cluster_sequences()`

Cluster sequences using string distance-based dissimilarity.

    cluster_sequences(data, k, dissimilarity = "hamming", method = "pam",
                      na_syms = c("*", "%"), weighted = FALSE, lambda = 1, ...)

``` r
result <- cluster_sequences(group_regulation[1:200, ], k = 3, dissimilarity = "osa")
print(result)
```

    #> Clustering method: pam 
    #> Number of clusters: 3 
    #> Silhouette score: 0.520094 
    #> Cluster sizes:
    #>  1  2  3 
    #> 72 64 64

``` r
# Pass directly to group_model
gmodel_clust <- group_model(result)
plot(gmodel_clust)
```

![](new-docs_files/figure-html/unnamed-chunk-35-1.png)![](new-docs_files/figure-html/unnamed-chunk-35-2.png)![](new-docs_files/figure-html/unnamed-chunk-35-3.png)

### `rename_groups()`

``` r
gmodel <- group_model(engagement_mmm)
gmodel_renamed <- rename_groups(gmodel, c("A", "B", "C"))
cat("Original:", names(group_model(engagement_mmm)), "\n")
```

    #> Original: Cluster 1 Cluster 2 Cluster 3

``` r
cat("Renamed:", names(gmodel_renamed), "\n")
```

    #> Renamed: A B C

### `mmm_stats()`

``` r
mmm_stats(engagement_mmm)
```

    #>     cluster    variable  estimate std_error  ci_lower  ci_upper   z_value
    #> 1 Cluster 2 (Intercept) -2.640329 0.1274911 -2.890207 -2.390451 -20.70991
    #> 2 Cluster 3 (Intercept) -4.512131 0.3157501 -5.130990 -3.893272 -14.29020
    #>   p_value
    #> 1       0
    #> 2       0

------------------------------------------------------------------------

## Summary & Conversion

### `summary()`

``` r
model <- tna(group_regulation)
summary(model)
```

    #> # A tibble: 13 × 2
    #>    metric                         value
    #>  * <chr>                          <dbl>
    #>  1 Node Count                  9   e+ 0
    #>  2 Edge Count                  7.8 e+ 1
    #>  3 Network Density             1   e+ 0
    #>  4 Mean Distance               4.72e- 2
    #>  5 Mean Out-Strength           1   e+ 0
    #>  6 SD Out-Strength             8.07e- 1
    #>  7 Mean In-Strength            1   e+ 0
    #>  8 SD In-Strength              6.80e-17
    #>  9 Mean Out-Degree             8.67e+ 0
    #> 10 SD Out-Degree               7.07e- 1
    #> 11 Centralization (Out-Degree) 1.56e- 2
    #> 12 Centralization (In-Degree)  1.56e- 2
    #> 13 Reciprocity                 9.86e- 1

``` r
gmodel <- group_model(engagement_mmm)
summary(gmodel)
```

    #> # A tibble: 13 × 4
    #>    metric                      `Cluster 1` `Cluster 2` `Cluster 3`
    #>  * <chr>                             <dbl>       <dbl>       <dbl>
    #>  1 Node Count                        3           3           3    
    #>  2 Edge Count                        9           8           8    
    #>  3 Network Density                   1           1           1    
    #>  4 Mean Distance                     0.111       0.239       0.302
    #>  5 Mean Out-Strength                 1           1           1    
    #>  6 SD Out-Strength                   0.214       0.353       0.472
    #>  7 Mean In-Strength                  1           1           1    
    #>  8 SD In-Strength                    0           0           0    
    #>  9 Mean Out-Degree                   3           2.67        2.67 
    #> 10 SD Out-Degree                     0           0.577       0.577
    #> 11 Centralization (Out-Degree)       0           0.25        0.25 
    #> 12 Centralization (In-Degree)        0           0.25        0.25 
    #> 13 Reciprocity                       1           0.8         0.8

### `as.igraph()`

``` r
model <- tna(group_regulation)
g <- as.igraph(model)
print(g)
```

    #> IGRAPH a762ecc DNW- 9 78 -- 
    #> + attr: name (v/c), weight (e/n)
    #> + edges from a762ecc (vertex names):
    #>  [1] adapt     ->cohesion   adapt     ->consensus  adapt     ->coregulate
    #>  [4] adapt     ->discuss    adapt     ->emotion    adapt     ->monitor   
    #>  [7] adapt     ->plan       cohesion  ->adapt      cohesion  ->cohesion  
    #> [10] cohesion  ->consensus  cohesion  ->coregulate cohesion  ->discuss   
    #> [13] cohesion  ->emotion    cohesion  ->monitor    cohesion  ->plan      
    #> [16] cohesion  ->synthesis  consensus ->adapt      consensus ->cohesion  
    #> [19] consensus ->consensus  consensus ->coregulate consensus ->discuss   
    #> [22] consensus ->emotion    consensus ->monitor    consensus ->plan      
    #> + ... omitted several edges

------------------------------------------------------------------------

**tna** v1.1.0 \| MIT License \| Mohammed Saqr, Santtu Tikka, Sonsoles
Lopez-Pernas

Reference: Saqr M., Lopez-Pernas S., et al. (2025). Transition Network
Analysis. *Proc. LAK ’25*, 351-361.
[doi:10.1145/3706468.3706513](https://doi.org/10.1145/3706468.3706513)

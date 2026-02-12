# `tna`: An R package for Transition Network Analysis

`tna` is an R package for the analysis of relational dynamics through
Transition Network Analysis (TNA). TNA provides tools for building TNA
models, plotting transition networks, calculating centrality measures,
and identifying dominant events and patterns. TNA statistical techniques
(e.g., bootstrapping and permutation tests) ensure the reliability of
observed insights and confirm that identified dynamics are meaningful.
See [(Saqr et al., 2025)](https://doi.org/10.1145/3706468.3706513) for
more details on TNA.

![](reference/figures/method.png)

## Resources

### Companion tutorials

We have also released comprehensive new tutorials for the main TNA
features:

| Tutorial                                                                               | Link                                        |
|----------------------------------------------------------------------------------------|---------------------------------------------|
| An Updated Comprehensive Tutorial on Transition Network Analysis (TNA)                 | <https://sonsoles.me/posts/tna-tutorial/>   |
| TNA Group Analysis: Analysis and Comparison of Groups                                  | <https://sonsoles.me/posts/tna-group/>      |
| TNA Clustering: Discovering and Analysis of Clusters                                   | <https://sonsoles.me/posts/tna-clustering/> |
| TNA Model Comparison:TNA Model Comparison: A Comprehensive Guide to Network Comparison | <https://sonsoles.me/posts/tna-compare/>    |
| Full reference guide on `tna` functions                                                | <https://sonsoles.me/tna/tna.html>          |

### Vignettes

Check out the `tna` R package vignettes:

| Vignette                             | Link                                                            |
|--------------------------------------|-----------------------------------------------------------------|
| Getting started with tna             | <https://sonsoles.me/tna/articles/tna.html>                     |
| A showcase of the main tna functions | <https://sonsoles.me/tna/articles/complete_tutorial.html>       |
| How to prepare data for tna          | <https://sonsoles.me/tna/articles/prepare_data.html>            |
| Frequency-based TNA                  | <https://sonsoles.me/tna/articles/ftna.html>                    |
| Attention TNA                        | <https://sonsoles.me/tna/articles/atna.html>                    |
| Finding cliques and communities      | <https://sonsoles.me/tna/articles/communities_and_cliques.html> |
| Using grouped sequence data          | <https://sonsoles.me/tna/articles/grouped_sequences.html>       |

### Book chapters

Do not forget to check out our tutorials in the “Advanced learning
analytics methods” book:

| Title                                                                                                                                                                    | Pages                                          | Tutorial                                                                                         |
|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------|--------------------------------------------------------------------------------------------------|
| Saqr, M., Lopez-Pernas, S., & Tikka, S. *Mapping Relational Dynamics with Transition Network Analysis: A Primer and Tutorial*                                            | <https://doi.org/10.1007/978-3-031-95365-1_15> | [Online tutorial](https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html)                   |
| Saqr, M., Lopez-Pernas, S., & Tikka, S. *Capturing the Breadth and Dynamics of the Temporal Processes with Frequency Transition Network Analysis: A Primer and Tutorial* | <https://doi.org/10.1007/978-3-031-95365-1_16> | [Online tutorial](https://lamethods.org/book2/chapters/ch16-ftna/ch16-ftna.html)                 |
| Lopez-Pernas, S., Tikka, S., & Saqr, M. *Mining Patterns and Clusters with Transition Network Analysis: A Heterogeneity Approach*                                        | <https://doi.org/10.1007/978-3-031-95365-1_17> | [Online tutorial](https://lamethods.org/book2/chapters/ch17-tna-clusters/ch17-tna-clusters.html) |

### Other tools

In addition to the `tna` R package, you can also try our [Shiny
app](https://sonsoleslp.shinyapps.io/tna-app/) and [Jamovi
plugin](https://github.com/sonsoleslp/jTNA).

## Installation

You can install the most recent stable version of `tna` from
[CRAN](https://cran.r-project.org/package=tna) or the development
version from [GitHub](https://github.com/) by running one of the
following:

``` r
install.packages("tna")

# install.packages("devtools")
# devtools::install_github("sonsoleslp/tna")
```

## Example

Load the package

``` r
library("tna")
```

Example data

``` r
data("group_regulation", package = "tna")
```

Build a Markov model

``` r
tna_model <- tna(group_regulation)
```

``` r
summary(tna_model)
```

| metric                      | value |
|:----------------------------|------:|
| Node Count                  |  9.00 |
| Edge Count                  | 78.00 |
| Network Density             |  1.00 |
| Mean Distance               |  0.05 |
| Mean Out-Strength           |  1.00 |
| SD Out-Strength             |  0.81 |
| Mean In-Strength            |  1.00 |
| SD In-Strength              |  0.00 |
| Mean Out-Degree             |  8.67 |
| SD Out-Degree               |  0.71 |
| Centralization (Out-Degree) |  0.02 |
| Centralization (In-Degree)  |  0.02 |
| Reciprocity                 |  0.99 |

Plot the transition network

``` r
# Default plot
plot(tna_model) 
```

![](reference/figures/README-tnaplot-1.svg)

``` r
# Optimized plot
plot(
  tna_model, cut = 0.2, minimum = 0.05, 
  edge.label.position = 0.8, edge.label.cex = 0.7
)
```

![](reference/figures/README-tnaplot-2.svg) Calculate the centrality
measures

``` r
cent <- centralities(tna_model)
```

|   state    | OutStrength | InStrength | ClosenessIn | ClosenessOut | Closeness | Betweenness | BetweennessRSP | Diffusion | Clustering |
|:----------:|------------:|-----------:|------------:|-------------:|----------:|------------:|---------------:|----------:|-----------:|
|   adapt    |       1.000 |      0.345 |       0.008 |        0.015 |     0.025 |       1.000 |          1.000 |     5.586 |      0.337 |
|  cohesion  |       0.973 |      0.812 |       0.014 |        0.012 |     0.027 |       0.000 |         19.000 |     5.209 |      0.300 |
| consensus  |       0.918 |      2.667 |       0.035 |        0.013 |     0.038 |      30.000 |        103.000 |     4.660 |      0.161 |
| coregulate |       0.977 |      0.567 |       0.016 |        0.015 |     0.021 |       0.000 |         27.000 |     5.148 |      0.306 |
|  discuss   |       0.805 |      1.188 |       0.020 |        0.013 |     0.027 |      16.000 |         53.000 |     4.628 |      0.240 |
|  emotion   |       0.923 |      0.894 |       0.014 |        0.012 |     0.023 |       5.000 |         36.000 |     5.070 |      0.290 |
|  monitor   |       0.982 |      0.346 |       0.008 |        0.014 |     0.019 |       0.000 |         11.000 |     5.157 |      0.289 |
|    plan    |       0.626 |      1.194 |       0.027 |        0.012 |     0.027 |       9.000 |         61.000 |     3.488 |      0.287 |
| synthesis  |       1.000 |      0.192 |       0.010 |        0.016 |     0.024 |       7.000 |          3.000 |     5.583 |      0.359 |

Plot the centrality measures

``` r
plot(cent, ncol = 3)
```

![](reference/figures/README-centralitiesplot-1.svg)

Estimate centrality stability

``` r
estimate_centrality_stability(tna_model)
#> Centrality Stability Coefficients
#> 
#>  InStrength OutStrength Betweenness 
#>         0.9         0.9         0.9
```

Identify and plot communities

``` r
coms <- communities(tna_model)
plot(coms)
```

![](reference/figures/README-unnamed-chunk-12-1.svg)

Find and plot cliques

``` r
cqs <- cliques(tna_model, threshold = 0.12)
plot(cqs)
```

![](reference/figures/README-unnamed-chunk-14-1.svg)

Compare high achievers (first 1000) with low achievers (last 1000)

``` r
tna_model_start_high <- tna(group_regulation[1:1000, ])
tna_model_start_low <- tna(group_regulation[1001:2000, ])
comparison <- permutation_test(
  tna_model_start_high, 
  tna_model_start_low,
  measures = c("InStrength")
)
```

Simple comparison vs. permutation test comparison

``` r
plot_compare(tna_model_start_high, tna_model_start_low)
plot(comparison)
```

![](reference/figures/README-unnamed-chunk-16-1.svg)

Compare centralities

``` r
print(comparison$centralities$stats)
```

|   state    | centrality |   diff_true | effect_size |     p_value |
|:----------:|:----------:|------------:|------------:|------------:|
|   adapt    | InStrength | -0.23693341 |   -6.746110 | 0.000999001 |
|  cohesion  | InStrength |  0.01634987 |    0.345312 | 0.720279720 |
| consensus  | InStrength |  0.53680793 |    7.777826 | 0.000999001 |
| coregulate | InStrength | -0.25275371 |   -7.385802 | 0.000999001 |
|  discuss   | InStrength | -0.09009651 |   -1.930958 | 0.046953047 |
|  emotion   | InStrength |  0.19288376 |    4.215793 | 0.000999001 |
|  monitor   | InStrength | -0.09192991 |   -3.454281 | 0.000999001 |
|    plan    | InStrength |  0.12225988 |    2.745588 | 0.007992008 |
| synthesis  | InStrength | -0.04909607 |   -3.220131 | 0.002997003 |

# Papers using TNA

- Saqr, M., Lopez-Pernas, S., Tormanen, T., Kaliisa, R., Misiejuk, K., &
  Tikka, S. (2025). Transition Network Analysis: A Novel Framework for
  Modeling, Visualizing, and Identifying the Temporal Patterns of
  Learners and Learning Processes. Proceedings of the 15th International
  Learning Analytics and Knowledge Conference (LAK ’25), 351–361. ACM.
  <https://doi.org/10.1145/3706468.3706513>
- Tikka, S., Lopez-Pernas, S., & Saqr, M. (2025). tna: An R Package for
  Transition Network Analysis. Applied Psychological Measurement (online
  ahead of print). <doi:10.1177/01466216251348840>
- López-Pernas, S., Misiejuk, K., Kaliisa, R., & Saqr, M. (2025).
  Capturing the process of students’ AI interactions when creating and
  learning complex network structures. IEEE Transactions on Learning
  Technologies, 1–13. <https://doi.org/10.1109/tlt.2025.3568599>
- Törmänen, T., Saqr, M., López-Pernas, S., Mänty, K., Suoraniemi, J.,
  Heikkala, N., & Järvenoja, H. (2025). Emotional dynamics and
  regulation in collaborative learning. Learning and Instruction,
  100, 102188. <https://doi.org/10.1016/j.learninstruc.2025.102188>
- López-Pernas, S., Misiejuk, K., Oliveira, E., & Saqr, M. (2025). The
  dynamics of the self-regulation process in student-AI interactions:
  The case of problem-solving in programming education. Proceedings of
  the 25th Koli Calling International Conference on Computing Education
  Research, 1–12. <https://doi.org/10.1145/3769994.3770043>
- Misiejuk, K., Kaliisa, R., Lopez-Pernas, S., & Saqr, M. (2026).
  Expanding the Quantitative Ethnography Toolkit with Transition Network
  Analysis: Exploring Methodological Synergies and Boundaries. In
  Advances in Quantitative Ethnography. ICQE 2025, CCIS vol. 2677.
  Springer. <https://doi.org/10.1007/978-3-032-12229-2_10>
- Lopez-Pernas, S., Misiejuk, K., Tikka, S., & Saqr, M. (2026). Role
  Dynamics in Student-AI Collaboration: A Heterogeneous Transition
  Network Analysis Approach. SSRN preprint.
  <https://doi.org/10.2139/ssrn.6082190>

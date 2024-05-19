`tna`: An R package for Transition Network Analysis
====================================================================================================

Install from Github
```R
install.packages("devtools")
library(devtools)
install_github("sonsoleslp/tna")
```
Load the library
```R
library(tna)
```

Example data
```R
data("engagement", package = "tna")
```

Build a Markov model
```R
tna_model <- build.tna(engagement)
```

Plot the transition network
```R
plot.tna(tna_model, mar = c(4,4,4,4))
```

Calculate the centrality measures
```R
calculate.centralities(tna_model$Matrix)
```
Plot the centrality measures
```R
plot.centralities(tna_model$Matrix0)
```

# Code to create `fit_mmm` object.

library("tna")
library("seqHMM")

set.seed(265)

tna_model <- tna(engagement)
n <- length(tna_model$labels)
g <- 3

trans_probs <- simulate_transition_probs(n, g)

init_probs <- list(
  c(0.70, 0.20, 0.10),
  c(0.15, 0.70, 0.15),
  c(0.10, 0.20, 0.70)
)

mmm <- build_mmm(
  engagement,
  transition_probs = trans_probs,
  initial_probs = init_probs
)

fit_mmm <- fit_model(
  mmm,
  control_em = list(restart = list(times = 100, n_optimum = 101))
)

engagement_mmm <- fit_mmm$model

usethis::use_data(
  engagement_mmm,
  overwrite = TRUE,
  compress = "xz"
)

# MMM using TNA

engagement_tna_mmm <- cluster_mmm(
  engagement,
  k = 3,
  progressbar = FALSE,
  seed = 6000,
  parallel = TRUE,
  n_starts = 100
)

usethis::use_data(
  engagement_tna_mmm,
  overwrite = TRUE,
  compress = "xz"
)

# Build the bundled `engagement_mmm` dataset.
#
# Historically this was a real seqHMM `mhmm` object, but storing seqHMM /
# TraMineR class references inside `data/*.rda` triggers a CRAN check
# warning ("Data files with namespace references not in the recursive
# strong package dependencies"). We now bundle a structurally-equivalent
# `tna_mmm` object: same fields used by `group_model.tna_mmm` and
# `mmm_stats.tna_mmm`, but no foreign class references.
#
# Re-run from the package root with:
#   source("data-raw/engagement_mmm.R")

devtools::load_all(".")

engagement_mmm <- random_tna_mmm(
  n_clusters   = 3,
  n_states     = 3,
  category     = "engagement",
  diag_boost   = 2,
  n_sequences  = 1000,
  seq_length   = 25,
  n_covariates = 1,
  seed         = 1
)

usethis::use_data(engagement_mmm, overwrite = TRUE)

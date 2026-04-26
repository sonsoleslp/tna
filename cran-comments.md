## Resubmission

This release fixes the CRAN check warnings reported for v1.2.2:

* "Found the following significant warnings: Warning: namespace 'seqHMM' is
  not available and has been replaced"
* "Data files with namespace references not in the recursive strong package
  dependencies: data/engagement_mmm.rda: seqHMM"

Both warnings stemmed from `data/engagement_mmm.rda` containing class
references to `seqHMM` and `TraMineR` (an internal `stslist`), neither of
which were strong dependencies. The dataset has been rebuilt as a `tna_mmm`
object that carries no foreign class references. Two new sibling S3
methods (`group_model.tna_mmm()`, `mmm_stats.tna_mmm()`) preserve all
existing example behaviour without requiring `seqHMM` at example-run time.
Real `seqHMM` `mhmm` objects continue to dispatch through the original
`*.mhmm` methods.

Additional cleanup:

* Removed two dead `library("seqHMM")` calls from
  `vignettes/grouped_sequences.Rmd` (the vignette body never used any
  `seqHMM` function).
* Lowered the minimum required R version from 4.4.0 to 4.1.0 (the floor
  set by the native `|>` pipe used in the package).
* Added `random_tna()`, `random_group_tna()`, `random_tna_mmm()`, and
  `list_random_state_pools()` for synthetic TNA construction.

## Test environments

* Local macOS, R 4.5.2, `R CMD check --as-cran` and `--as-cran` with
  `_R_CHECK_DEPENDS_ONLY_=true`: both PASS (only environment-only
  failures from missing local pdflatex / outdated HTML Tidy).

## R CMD check results

0 errors, 0 warnings, 0 notes (when run on a system with pdflatex and a
recent HTML Tidy installed).

## Method references

The methods implemented in this package are described in:

Saqr, M., López-Pernas, S., Törmänen, T., Kaliisa, R., Misiejuk, K., &
Tikka, S. (2025). Transition Network Analysis: A Novel Framework for
Modeling, Visualizing, and Identifying the Temporal Patterns of Learners
and Learning Processes. *Proceedings of the 15th International Learning
Analytics and Knowledge Conference (LAK '25)*, 351-361.
<doi:10.1145/3706468.3706513>

## Reverse dependencies

This package has no reverse dependencies on CRAN.

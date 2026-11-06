# Sequences nested in actors -------------------------------------------------
#
# Every resampling method (bootstrap, permutation, case-dropping, split-half)
# takes `actor`: a column name of the data the model was built from, or a
# vector with one actor identifier per sequence. The helpers below resolve it
# to identifiers and to the resampling units that respect the nesting.

#' Check the `actor` Argument
#'
#' @param actor `NULL`, a column name, or a vector of actor identifiers.
#' @param paired A `logical` value.
#' @noRd
check_actor <- function(actor, paired = FALSE) {
  if (is.null(actor)) {
    return(invisible(NULL))
  }
  stopifnot_(
    is.atomic(actor) && length(actor) > 0L,
    "Argument {.arg actor} must be a column name or a vector with one actor
     identifier per sequence."
  )
  stopifnot_(
    !paired,
    "Arguments {.arg actor} and {.arg paired = TRUE} cannot be combined:
     a paired design is the special case of one actor per pair."
  )
}

#' Is `actor` a Column Name?
#'
#' @param actor A column name or a vector of actor identifiers.
#' @noRd
is_actor_column <- function(actor) {
  is.character(actor) && length(actor) == 1L
}

#' Actor Identifier of Each Sequence of a `tna` Model
#'
#' @param x A `tna` object.
#' @param actor A column name of the non-sequence data the model was built
#' from, or a vector with one identifier per sequence of `x`.
#' @noRd
actor_ids <- function(x, actor) {
  n <- nrow(x$data)
  if (is_actor_column(actor)) {
    meta <- attr(x$data, "meta")
    stopifnot_(
      actor %in% names(meta),
      "Argument {.arg actor} must name a non-sequence column of the data
       the model was built from, but {.val {actor}} was not found."
    )
    ids <- meta[[actor]]
  } else {
    stopifnot_(
      length(actor) == n,
      "Argument {.arg actor} must have one value per sequence
       ({n}), not {length(actor)}."
    )
    ids <- actor
  }
  stopifnot_(
    !anyNA(ids),
    "Argument {.arg actor} must not contain missing values."
  )
  as.character(ids)
}

#' Split `actor` Across the Clusters of a `group_tna` Model
#'
#' A column name applies to every cluster as is. A vector has one value per
#' sequence of the data the grouped model was built from, and is split by the
#' rows each cluster received.
#'
#' @param x A `group_tna` object.
#' @param actor `NULL`, a column name, or a vector of actor identifiers.
#' @return A `list` with an `actor` argument for each cluster.
#' @noRd
group_actor <- function(x, actor) {
  if (is.null(actor) || is_actor_column(actor)) {
    return(rep(list(actor), length(x)))
  }
  n <- attr(x, "n_rows")
  stopifnot_(
    !is.null(n),
    "This grouped model does not record its rows. Rebuild it with
     {.fn group_model} to use an {.arg actor} vector."
  )
  stopifnot_(
    length(actor) == n,
    "Argument {.arg actor} must have one value per sequence of the
     grouped data ({n}), not {length(actor)}."
  )
  lapply(x, function(y) actor[attr(y$data, "rows")])
}

#' Resampling Units: the Sequence Rows of Each Actor
#'
#' @param x A `tna` object.
#' @param actor A column name or a vector of actor identifiers.
#' @return A `list` of integer row indices, one element per actor, in order
#' of first appearance.
#' @noRd
actor_units <- function(x, actor) {
  ids <- actor_ids(x, actor)
  units <- unname(split(seq_along(ids), factor(ids, levels = unique(ids))))
  stopifnot_(
    length(units) >= 2L,
    "Argument {.arg actor} must identify at least two actors."
  )
  units
}

#' Draw Bootstrap Samples of Sequence Rows
#'
#' Sequences are resampled with replacement, or whole actors when `actor` is
#' given (cluster bootstrap). With one sequence per actor both are identical.
#'
#' @param x A `tna` object.
#' @param actor `NULL`, a column name, or a vector of actor identifiers.
#' @return A `function` without arguments that returns sequence rows.
#' @noRd
bootstrap_sampler <- function(x, actor) {
  n <- nrow(x$data)
  idx <- seq_len(n)
  if (is.null(actor)) {
    return(function() sample(idx, n, replace = TRUE))
  }
  units <- actor_units(x, actor)
  k <- length(units)
  function() {
    unlist(units[sample.int(k, k, replace = TRUE)], use.names = FALSE)
  }
}

#' Log Number of Distinct Arrangements of a Label Vector
#'
#' @param labels A vector of group labels.
#' @noRd
log_arrangements <- function(labels) {
  lfactorial(length(labels)) - sum(lfactorial(table(labels)))
}

#' Precompute a Permutation Scheme that Respects Actors
#'
#' Actors with all of their sequences in one group ("pure") are reassigned
#' between the groups as whole units. Actors with sequences in more than one
#' group ("crossed") keep their group counts and shuffle the labels
#' internally (Good, 2005).
#'
#' @param ids A `character` vector of actor identifiers, one per sequence.
#' @param labels A vector of group labels, one per sequence.
#' @param level The significance level, used to warn about designs that
#' cannot produce a p-value below it.
#' @noRd
actor_design <- function(ids, labels, level) {
  n_labels <- tapply(labels, ids, n_unique)
  pure <- names(n_labels)[n_labels == 1L]
  crossed <- names(n_labels)[n_labels > 1L]
  pure_rows <- which(ids %in% pure)
  crossed_rows <- which(ids %in% crossed)
  crossed_ids <- ids[crossed_rows]
  pure_label <- labels[match(pure, ids)]
  log_perms <- log_arrangements(pure_label) +
    sum(
      vapply(
        split(labels[crossed_rows], crossed_ids),
        log_arrangements,
        numeric(1L)
      )
    )
  if (log_perms < log(1 / level)) {
    warning_(
      "The actors allow only {round(exp(log_perms))} distinct permutation{?s},
       so no p-value can fall below {signif(exp(-log_perms), 3)}."
    )
  }
  list(
    labels = labels,
    pure_rows = pure_rows,
    pure_index = match(ids[pure_rows], pure),
    pure_label = pure_label,
    crossed_rows = crossed_rows,
    crossed_ids = crossed_ids,
    crossed_sorted = crossed_rows[order(crossed_ids)]
  )
}

#' One Permutation of the Group Labels that Respects Actors
#'
#' @param design A `list` from `actor_design()`.
#' @return The permuted vector of group labels.
#' @noRd
actor_permute <- function(design) {
  perm <- design$labels
  if (length(design$pure_label) > 1L) {
    shuffled <- design$pure_label[sample.int(length(design$pure_label))]
    perm[design$pure_rows] <- shuffled[design$pure_index]
  }
  if (length(design$crossed_rows) > 0L) {
    # Rows sorted by actor in random within-actor order receive the
    # actor-sorted original labels: a uniform shuffle within each actor
    target <- design$crossed_rows[
      order(design$crossed_ids, stats::runif(length(design$crossed_rows)))
    ]
    perm[target] <- design$labels[design$crossed_sorted]
  }
  perm
}

#' Actor Identifiers of Two Models Compared by a Permutation Test
#'
#' @param x,y `tna` objects.
#' @param actor_x,actor_y `NULL`, a column name, or a vector of actor
#' identifiers for the sequences of `x` and `y`.
#' @return `NULL` or the identifiers of the sequences of `x` followed by
#' those of `y`.
#' @noRd
pair_actor_ids <- function(x, y, actor_x, actor_y) {
  onlyif(
    !is.null(actor_x),
    c(actor_ids(x, actor_x), actor_ids(y, actor_y))
  )
}

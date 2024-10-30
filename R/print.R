#' Print Centrality Measures
#'
#' @param x A `centralities` object.
#' @param ... Ignored.
#' @export
#'
print.tna_centralities <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

#' Print Detected Communities
#'
#' @param x A `tna_communities` object.
#' @param ... Ignored.
#' @export
#'
print.tna_communities <- function(x, ...) {
  stopifnot_(
    is_tna_communities(x),
    "Argument {.arg x} must be a {.cls tna_communities} object."
  )
  cat("Number of communities found by each algorithm:\n")
  print(x$counts)
  cat("\nCommunity assignments:\n")
  print(x$assignments)
  invisible(x)
}

#' Print Found Cliques
#'
#' @param x A `tna_cliques` object.
#' @param n An `integer` defining the maximum number of cliques to show.
#' The defaults is `6`.
#' @param first An `integer` giving the index of the first clique to show.
#' The default index is `1`.
#' @param digits An `integer` giving the number of digits to show for the edge
#' weights in the cliques. The default is `3`.
#' @param ... Ignored.
#' @export
#'
print.tna_cliques <- function(x, n = 6, first = 1, digits = 3, ...) {
  stopifnot_(
    is_tna_cliques(x),
    "Argument {.arg x} must be a {.cls cliques} object."
  )
  n_cliques <- length(x$weights)
  if (n_cliques == 0) {
    cat("No ", size, "-cliques were found in the network.", sep = "")
    return(invisible(x))
  }
  stopifnot_(
    checkmate::test_int(x = n, lower = 1L, upper = n_cliques),
    "Argument {.arg n} must be a single non-negative {.cls integer}
    between 1 and {n_cliques}."
  )
  check_positive(first)
  check_nonnegative(digits)
  size <- attr(x, "size")
  threshold <- attr(x, "threshold")
  cluster <- attr(x, "cluster")
  cat(
    "Number of ", size, "-cliques: ", n_cliques, " ",
    "(weight threshold = ", threshold, ", cluster = ", cluster, ")\n",
    sep = ""
  )
  max_cliques <- min(first + n - 1L, n_cliques)
  cat(
    "Showing ", max_cliques, " cliques starting from clique number ", first,
    sep = ""
  )
  cat("\n")
  for (i in seq(first, max_cliques)) {
    cat("\nClique ", i, ":\n", sep = "")
    print(round(x$weights[[i]], digits))
  }
}


#' Print a `tna` object
#'
#' @param x A `tna` object.
#' @param digits An `integer` giving the number of decimal digits to print.
#' Defaults to `3`.
#' @param generic A `logical` value. If `TRUE`, use generic print method
#' instead. Defaults to `FALSE`.
#' @param ... Ignored.
#' @export
#'
print.tna <- function(x, digits = 3, generic = FALSE, ...) {
  check_tna(x)
  check_flag(generic)
  if (generic) {
    NextMethod(generic = "print", object = x, ...)
    return()
  }
  check_nonnegative(digits)
  type <- attr(x, "type")
  mat_type <- switch(
    type,
    relative = "Transition Probability",
    scaled = "Scaled Transition Frequency",
    ranked = "Scaled Transition Frequency Rank",
    absolute = "Transition Frequency",
    betweenness = "Edge Betweenness"
  )
  cat("State Labels\n\n")
  cat(paste(x$labels, collapse = ", "), "\n")
  cat("\n", mat_type, " Matrix\n\n", sep = "")
  print(round(x$weights, digits))
  cat("\nInitial Probabilities\n\n", sep = "")
  print(round(x$inits, digits))
  invisible(x)
}

# TODO print.tna_stability

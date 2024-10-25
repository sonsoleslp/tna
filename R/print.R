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
  n_clust <- length(x)
  has_clust <- n_clust > 1
  for (i in seq_len(n_clust)) {
    onlyif(has_clust, cat("Cluster", i, "\n"))
    cat("Number of communities found by each algorithm:\n")
    print(x[[i]]$counts)
    cat("\nCommunity assignments:\n")
    print(x[[i]]$assignments)
    onlyif(has_clust, cat("\n"))
  }
  invisible(x)
}

#' Print Found Cliques
#'
#' @param x A `tna_cliques` object.
#' @param n An `integer` defining the maximum number of cliques to show.
#' The defaults is 6.
#' @param first An `integer` giving the index of the first clique to show.
#' The default index is 1.
#' @param digits An `integer` giving the number of digits to show for the edge
#' weights in the cliques. The default is 3.
#' @param ... Ignored.
#' @export
#'
print.tna_cliques <- function(x, n = 6, first = 1, digits = 3, ...) {
  stopifnot_(
    is_tna_cliques(x),
    "Argument {.arg x} must be a {.cls cliques} object."
  )
  n_cliques <- length(x$weights)
  size <- attr(x, "size")
  if (n_cliques == 0) {
    cat("No ", size, "-cliques were found in the network.", sep = "")
    return(invisible(x))
  }
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
#' @param digits Number of decimal digits to print. Defaults to 2.
#' @param generic Use generic print. Defaults to `FALSE`
#' @param ... Ignored.
#' @export
#'
print.tna <- function(x, digits = 2, generic = FALSE, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  if (generic) {
    NextMethod(generic = "print", object = x, ...)
    return()
  }
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
  n_clust <- length(x$weights)
  has_clust <- n_clust > 1
  clust_names <- names(x$weights)
  for (i in seq_len(n_clust)) {
    onlyif(has_clust, cat(paste0(clust_names[i], ":\n")))
    print(x$weights[[i]])
    onlyif(has_clust, cat("\n"))
  }
  cat("\nInitial Probabilities\n\n", sep = "")
  for (i in seq_len(n_clust)) {
    init <- x$inits[[i]]
    names(init) <- x$labels
    onlyif(has_clust, cat(paste0(clust_names[i], ":\n")))
    print(init)
    onlyif(has_clust, cat("\n"))
  }
  invisible(x)
}

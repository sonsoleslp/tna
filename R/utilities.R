#' Normalize `x` to the unit interval from 0 to 1.
#'
#' @param x A `numeric` vector.
#' @param na.rm A `logical` value indicating whether missing values
#'   should be removed.
#' @noRd
ranger <- function(x, na.rm = FALSE) {
  mi <- min(x, na.rm = na.rm)
  ma <- max(x, na.rm = na.rm)
  (x - mi) / (ma - mi)
}

#' Check Weak Connectivity of an Adjacency Matrix
#'
#' This function checks if an adjacency matrix represents a weakly connected
#' graph. A graph is considered weakly connected if there is a path between
#' any two vertices when ignoring the direction of edges.
#'
#' @param mat A square adjacency `matrix` representing the graph.
#' @return A `logical` value indicating whether the graph is
#' weakly connected (`TRUE`) or not (`FALSE`).
#' @noRd
is_weakly_connected <- function(mat) {
  n <- nrow(mat)
  visited <- logical(n)
  stack <- integer(n)
  stack[1L] <- 1
  visited[1L] <- TRUE
  len <- 1L
  while (len > 0) {
    v <- stack[len]
    len <- len - 1L
    neighbors <- which(mat[v, ] > 0 | mat[, v] > 0)
    for (u in neighbors) {
      if (!visited[u]) {
        len <- len + 1L
        visited[u] <- TRUE
        stack[len] <- u
      }
    }
  }
  all(visited)
}

#' Shorthand for `try(., silent = TRUE)`
#'
#' @param expr An \R expression to try.
#' @noRd
try_ <- function(expr) {
  try(expr, silent = TRUE)
}

#' Check that argument is an object of class `tna`
#'
#' @param x An \R object.
#' @noRd
is_tna <- function(x) {
  inherits(x, "tna")
}

#' Check that argument is an object of class `tna_centralities`
#'
#' @param x An \R object.
#' @noRd
is_tna_centralities <- function(x) {
  inherits(x, "tna_centralities")
}

#' @importFrom igraph as.igraph
#' @export
igraph::as.igraph

#' Coerce a `tna` object to an `igraph` object.
#'
#' @export
#' @inheritParams igraph::as.igraph
#' @return An `igraph` object.
as.igraph.tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "tna")
  igraph::graph_from_adjacency_matrix(
    adjmatrix = x$weights,
    mode = "directed",
    weighted = TRUE
  )
}

#' Coerce  a specific group from a `group_tna` object to an `igraph` object.
#'
#' @export
#' @inheritParams igraph::as.igraph
#' @param which The number or name of the group.
#' @return An `igraph` object.
as.igraph.group_tna <- function(x, which, ...) {
  check_missing(x)
  check_missing(which)
  check_class(x, "group_tna")
  stopifnot_(
    !is.null(x[[which]]),
    "There is no group named {which}."
  )
  as.igraph(x[[which]])
}

# Functions borrowed from the `dynamite` package --------------------------
# https://github.com/ropensci/dynamite

#' Shorthand for `if (test) yes else no`
#'
#' @param test A `logical` value of the condition to evaluate.
#' @param yes An \R object to return when `test` evaluates to `TRUE`.
#' @param no An \R object to return when `test` evaluates to `FALSE`.
#' @noRd
ifelse_ <- function(test, yes, no) {
  if (test) {
    yes
  } else {
    no
  }
}

#' Return `yes` if `test` is `TRUE`, otherwise return `NULL`
#'
#' @param test \[`logical(1)`] Condition to evaluate.
#' @param yes An \R object to return when `test` evaluates to `TRUE`.
#' @noRd
onlyif <- function(test, yes) {
  if (test) {
    yes
  } else {
    NULL
  }
}

#' Generate a Warning Message
#'
#' @param message See [cli::cli_warn()].
#' @param ... See [cli::cli_warn()].
#' @noRd
warning_ <- function(message, ...) {
  cli::cli_warn(message, ..., .envir = parent.frame())
}

#' Stop function execution unless a condition is true
#'
#' @param message See [cli::cli_abort()].
#' @param ... See [cli::cli_abort()].
#' @param call See [cli::cli_abort()].
#' @noRd
stopifnot_ <- function(cond, message, ..., call = rlang::caller_env()) {
  if (!cond) {
    cli::cli_abort(message, ..., .envir = parent.frame(), call = call)
  }
}

#' Create a Comma-separated Character String
#'
#' @param x A `character` vector.
#' @noRd
cs <- function(...) {
  paste0(c(...), collapse = ", ")
}

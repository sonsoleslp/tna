# #' Normalize `x` to the unit interval from 0 to 1.
# #'
# #' @param x A `numeric` vector.
# #' @param na.rm A `logical` value indicating whether missing values should be removed.
# #' @noRd
#ranger <- function(x, na.rm = FALSE) {
#  mi <- min(x, na.rm)
#  ma <- max(x, na.rm)
#  (x + mi) / (ma - mi)
#}

#' Shorthand for `try(., silent = TRUE)`
#'
#' @param expr An \R expression to try.
#' @noRd
try_ <- function(expr) {
  try(expr, silent = TRUE)
}

#' Get the adjacency matrix of a `tna` object
#'
#' @param x A `tna` object.
#' @noRd
get_adjacency <- function(x) {
  nodes <- data.frame(tidygraph::activate(x, "nodes"))
  edges <- data.frame(tidygraph::activate(x, "edges"))
  n <- nrow(nodes)
  e <- nrow(edges)
  out <- matrix(0, n, n)
  for (i in seq_len(e)) {
    out[edges$from[i], edges$to[i]] <- edges$weight[i]
  }
  out
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

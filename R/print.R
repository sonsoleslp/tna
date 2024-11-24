#' Print Bootstrap Results
#'
#' @export
#' @param x A `tna_bootstrap` object.
#' @param digits An `integer` giving the minimal number of
#' *significant* digits to print.
#' @param type A `character` vector giving the type of edges to print.
#' The default option `"both"` prints both statistically significant and
#' non-significant edges, `"sig"` prints only significant edges, and `"nonsig"`
#' prints only the non-significant edges.
#' @param ... Ignored.
#'
print.tna_bootstrap <- function(x, digits = getOption("digits"),
                                type = "both", ...) {
  stopifnot_(
    is_tna_bootstrap(x),
    "Argument {.arg x} must be a {.cls tna_bootstrap} object."
  )
  check_nonnegative(digits)
  method <- onlyif(is.character(type), tolower(type))
  method <- try(
    match.arg(type, c("both", "sig", "nonsig")),
    silent = TRUE
  )
  stopifnot_(
    !inherits(method, "try-error"),
    "Argument {.arg type} must be either {.val both}, {.val sig},
     or {.val nonsig}."
  )
  sig <- x$summary$sig
  edges <- x$summary |>
    dplyr::select(!sig)
  if (any(sig) && type %in% c("both", "sig")) {
    cat("Significant Edges\n\n")
    print(edges[which(sig), ], digits = digits)
  }
  if (any(sig) && any(!sig) && type == "both") {
    cat("\n")
  }
  if (any(!sig) && type %in% c("both", "nonsig")) {
    cat("Non-significant Edges\n\n")
    print(edges[which(!sig), ], digits = digits)
  }
}

#' Print a TNA Summary
#'
#' @export
#' @param x A `summary.tna` object.
#' @param ... Ignored.
#'
print.summary.tna <- function(x, ...) {
  NextMethod(generic = "print", object =  x, ...)
}

#' Print Bootstrap Summary
#'
#' @export
#' @param x A `summary.tna_bootstrap` object.
#' @param ... Ignored.
#'
print.summary.tna_bootstrap <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

#' Print Centrality Measures
#'
#' @export
#' @param x A `centralities` object.
#' @param ... Ignored.
#'
print.tna_centralities <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

#' Print Detected Communities
#'
#' @export
#' @param x A `tna_communities` object.
#' @param ... Ignored.
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
#' @export
#' @param x A `tna_cliques` object.
#' @param n An `integer` defining the maximum number of cliques to show.
#' The defaults is `6`.
#' @param first An `integer` giving the index of the first clique to show.
#' The default index is `1`.
#' @param digits An `integer` giving the minimal number of
#' *significant* digits to print.
#' @param ... Ignored.
#'
print.tna_cliques <- function(x, n = 6, first = 1,
                              digits = getOption("digits"), ...) {
  stopifnot_(
    is_tna_cliques(x),
    "Argument {.arg x} must be a {.cls tna_cliques} object."
  )
  n_cliques <- length(x$weights)
  if (n_cliques == 0) {
    cat("No ", attr(x,"size"), "-cliques were found in the network.", sep = "")
    return(invisible(x))
  }
  check_positive(n)
  check_positive(first)
  check_nonnegative(digits)
  n <- min(n, n_cliques)
  size <- attr(x, "size")
  threshold <- attr(x, "threshold")
  cluster <- attr(x, "cluster")
  cat(
    "Number of ", size, "-cliques: ", n_cliques, " ",
    "(weight threshold = ", threshold, ")\n",
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
    print(x$weights[[i]], digits)
  }
}


#' Print a `tna` object
#'
#' @export
#' @param x A `tna` object.
#' @param digits An `integer` giving the number of
#' *significant* digits to print.
#' @param generic A `logical` value. If `TRUE`, use generic print method
#' instead. Defaults to `FALSE`.
#' @param ... Ignored.
#'
print.tna <- function(x, digits = getOption("digits"), generic = FALSE, ...) {
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
  print(x$weights, digits)
  if (!is.null(x$inits)) {
    cat("\nInitial Probabilities\n\n", sep = "")
    print(x$inits, digits)
  }
  invisible(x)
}

# TODO print.tna_stability

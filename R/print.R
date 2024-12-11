#' Print a TNA Summary
#'
#' @export
#' @param x A `summary.tna` object.
#' @param ... Ignored.
#' @return A `summary.tna` object (invisibly) containing the TNA model network
#' metrics and values.
#' @examples
#' model <- tna(engagement)
#' print(summary(model))
#'
print.summary.tna <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

#' Print Bootstrap Summary
#'
#' @export
#' @param x A `summary.tna_bootstrap` object.
#' @param ... Arguments passed to the generic `print` method.
#' @return A `summary.tna_bootstrap` object (invisibly) containing the weight,
#' p-value and confidence interval of each edge.
#' @examples
#' model <- tna(engagement)
#' # Small number of iterations for CRAN
#' boot <- bootstrap(model, iter = 10)
#' print(summary(boot))
#'
print.summary.tna_bootstrap <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
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
#' @return The `tna` object passed as argument `x` (invisibly).
#' @examples
#' model <- tna(engagement)
#' print(model)
#'
print.tna <- function(x, digits = getOption("digits"), generic = FALSE, ...) {
  check_missing(x)
  check_class(x, "tna")
  check_flag(generic)
  if (generic) {
    NextMethod(generic = "print", object = x, ...)
    return()
  }
  check_nonnegative(digits)
  type <- attr(x, "type")
  mat_type <- switch(type,
    `relative` = "Transition Probability",
    `frequency` = "Transition Frequency",
    `co-occurrence` = "Co-occurrence",
    `betweenness` = "Edge Betweenness"
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
#' @return `x` (invisibly).
#' @examples
#' model <- tna(engagement)
#' # Small number of iterations for CRAN
#' boot <- bootstrap(model, iter = 10)
#' print(boot)
#'
print.tna_bootstrap <- function(x, digits = getOption("digits"),
                                type = "both", ...) {
  check_missing(x)
  check_class(x, "tna_bootstrap")
  check_nonnegative(digits)
  type <- check_match(type, c("both", "sig", "nonsig"))
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
  invisible(x)
}


#' Print Centrality Measures
#'
#' @export
#' @param x A `centralities` object.
#' @param ... Ignored.
#' @return `x` (invisibly).
#' @examples
#' model <- tna(engagement)
#' cm <- centralities(model)
#' print(cm)
#'
print.tna_centralities <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

#' Print Detected Communities
#'
#' @export
#' @param x A `tna_communities` object.
#' @param ... Ignored.
#' @return `x` (invisibly).
#' @examples
#' model <- tna(engagement)
#' comm <- communities(model)
#' print(comm)
#'
print.tna_communities <- function(x, ...) {
  check_missing(x)
  check_class(x, "tna_communities")
  cat("Number of communities found by each algorithm:\n")
  print(x$counts)
  cat("\nCommunity assignments:\n")
  print(x$assignments)
  invisible(x)
}

#' Print Found Cliques of a TNA Network
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
#' @return `x` (invisibly).
#' @examples
#' model <- tna(engagement)
#' cliq <- cliques(model, size = 2)
#' print(cliq)
#'
print.tna_cliques <- function(x, n = 6, first = 1,
                              digits = getOption("digits"), ...) {
  check_missing(x)
  check_class(x, "tna_cliques")
  n_cliques <- length(x$weights)
  size <- attr(x, "size")
  if (n_cliques == 0) {
    cat("No ", attr(x, "size"), "-cliques were found in the network.", sep = "")
    return(invisible(x))
  }
  check_positive(n)
  check_positive(first)
  check_nonnegative(digits)
  n <- min(n, n_cliques)
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
  invisible(x)
}

#' Print Centrality Stability Results
#'
#' @export
#' @param x A `tna_stability` object.
#' @param ... Ignored.
#' @return `x` (invisibly).
#' @examples
#' model <- tna(engagement)
#' # Small number of iterations and drop proportions for CRAN
#' cs <- estimate_cs(
#'   model,
#'   measures = c("InStrength", "OutStrength"),
#'   drop_prop = seq(0.3, 0.9, by = 0.2),
#'   iter = 10
#' )
#' print(cs)
#'
print.tna_stability <- function(x, ...) {
  check_missing(x)
  check_class(x, "tna_stability")
  cs_coefs <- unlist(lapply(x, "[[", "cs_coefficient"))
  names(cs_coefs) <- names(x)
  cat("Centrality Stability Coefficients\n\n")
  print(cs_coefs)
  invisible(x)
}

#' Print Permutation Test Results
#'
#' @export
#' @param x A `tna_permutation` object.
#' @param ... Additional arguments passed to the `tibble` print method.
#' @return `x` (invisibly).
#' @examples
#' model_x <- tna(group_regulation[1:200, ])
#' model_y <- tna(group_regulation[1001:1200, ])
#' # Small number of iterations for CRAN
#' perm <- permutation_test(model_x, model_y, iter = 20)
#' print(perm)
#'
print.tna_permutation <- function(x, ...) {
  check_missing(x)
  check_class(x, "tna_permutation")
  cat("Edges\n\n")
  print(tibble::as_tibble(x$edges$stats), ...)
  if (!is.null(x$centralities)) {
    cat("Centralities\n\n")
    print(x$centralities$stats, ...)
  }
  invisible(x)
}

# Clusters ----------------------------------------------------------------

#' Print a `group_tna` Object
#'
#' @export
#' @family clusters
#' @param x A `group_tna` object.
#' @param ... Arguments passed to [print.tna()].
#' @return `x` (invisibly).
#' @examples
#' model <- group_model(engagement_mmm)
#' print(model)
#'
print.group_tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  lapply(x, \(i) print(i, ...))
  invisible(x)
}

#' Print `group_tna` Bootstrap Results
#'
#' @export
#' @family clusters
#' @param x A `group_tna_bootstrap` object.
#' @param ... Arguments passed to [print.tna_bootstrap()].
#' @return `x` (invisibly).
#' @examples
#' model <- group_model(engagement_mmm)
#' # Low number of iteration for CRAN
#' boot <- bootstrap(model, iter = 10)
#' print(boot)
#'
print.group_tna_bootstrap <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna_bootstrap")
  lapply(x, \(i) print(i, ...))
  invisible(x)
}

#' Print the summary of a grouped Transition Network Analysis Model
#'
#' @export
#' @family clusters
#' @param x A `summary.group_tna` object.
#' @param ... Arguments passed to [print.summary.tna()].
#' @return `x` (invisibly).
#' @examples
#' model <- group_model(engagement_mmm)
#' print(summary(model))
#'
print.summary.group_tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "summary.group_tna")
  if (inherits(x, "data.frame")) {
    NextMethod(generic = "print", object = x, ...)
  } else {
    lapply(x, \(i) print(x = i, ...))
  }
  invisible(x)
}

#' Print Bootstrap Summary for a Grouped Transition Network
#'
#' @export
#' @family clusters
#' @param x A `summary.group_tna_bootstrap` object.
#' @param ... Arguments passed to the generic `print` method.
#' @return `x` (invisibly).
#' @examples
#' model <- group_model(engagement_mmm)
#' # Low number of iteration for CRAN
#' boot <- bootstrap(model, iter = 10)
#' print(summary(boot))
#'
print.summary.group_tna_bootstrap <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

#' Print Centrality Measures
#'
#' @export
#' @family clusters
#' @param x A `group_tna_centralities` object.
#' @param ... Ignored.
#' @return `x` (invisibly).
#' @examples
#' model <- group_model(engagement_mmm)
#' cm <- centralities(model)
#' print(cm)
#'
print.group_tna_centralities <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna_centralities")
  NextMethod(generic = "print", object = x, ...)
}

#' Print Detected Communities
#'
#' @export
#' @family clusters
#' @param x A `group_tna_communities` object.
#' @param ... Arguments passed to [print.tna_communities()].
#' @return `x` (invisibly).
#' @examples
#' model <- group_model(engagement_mmm)
#' comm <- communities(model)
#' print(comm)
#'
print.group_tna_communities <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna_communities")
  Map(
    function(y, i) {
      print(i)
      print(y, ...)
    },
    x,
    names(x)
  )
  invisible(x)
}

#' Print Found Cliques
#'
#' @export
#' @family clusters
#' @param x A `group_tna_cliques` object.
#' @param ... Arguments passed to [print.tna_cliques()].
#' @return `x` (invisibly).
#' @examples
#' model <- group_model(engagement_mmm)
#' cliq <- cliques(model, size = 2)
#' print(cliq)
#'
print.group_tna_cliques <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna_cliques")
  Map(
    function(y, i) {
      print(i)
      print(y, ...)
    },
    x,
    names(x)
  )
  invisible(x)
}

#' Print Centrality Stability Results
#'
#' @export
#' @family clusters
#' @param x A `group_tna_stability` object.
#' @param ... Arguments passed to [print.tna_stability()].
#' @return `x` (invisibly).
#' @examples
#' model <- group_model(engagement_mmm)
#' # Low number of iterations for CRAN
#' stability <- estimate_cs(
#'   model,
#'   drop_prop = c(0.3, 0.5, 0.7, 0.9),
#'   iter = 10
#' )
#' print(stability)
#'
print.group_tna_stability <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna_stability")
  Map(
    function(y, i) {
      cat(i, ":\n\n", sep = "")
      print(y, ...)
    },
    x,
    names(x)
  )
  invisible(x)
}

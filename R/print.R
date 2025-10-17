#' Print a TNA Summary
#'
#' @export
#' @family basic
#' @param x A `summary.tna` object.
#' @param ... Ignored.
#' @return A `summary.tna` object (invisibly) containing the TNA model network
#' metrics and values.
#' @examples
#' model <- tna(group_regulation)
#' print(summary(model))
#'
print.summary.tna <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

#' Print a Bootstrap Summary
#'
#' @export
#' @family validation
#' @param x A `summary.tna_bootstrap` object.
#' @param ... Arguments passed to the generic `print` method.
#' @return A `summary.tna_bootstrap` object (invisibly) containing the weight,
#' estimated p-value and confidence interval of each edge.
#' @examples
#' model <- tna(group_regulation)
#' # Small number of iterations for CRAN
#' boot <- bootstrap(model, iter = 10)
#' print(summary(boot))
#'
print.summary.tna_bootstrap <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

# #' Print a Summary of a Mixture Markov Model Fit
# #'
# #' @export
# #' @family groups
# #' @param x A `summary.tna_mmm` object.
# #' @param digits An `integer` giving the number of *significant* digits
# #'   to print.
# #' @param ... Arguments passed to the generic `print` method.
# #' @return A `summary.tna_mmm` object (invisibly) with details of the model fit.
# #' @examples
# #' sumr <- summary(engagement_tna_mmm)
# #' print(sumr)
# #'
# print.summary.tna_mmm <- function(x, digits = 3L, ...) {
#   cat("Covariate effects :\n")
#   cat("Reference cluster:", x$cluster_names[1L], "\n\n")
#   cf <- x$coefficients
#   v <- diag(x$vcov)
#   k <- ncol(x$prior)
#   q <- nrow(cf)
#   mean_prior <- colMeans(x$prior)
#   names(mean_prior) <- x$cluster_names
#   clust_tab <- table(x$assignments)
#   tab <- matrix(
#     c(
#       as.character(clust_tab),
#       as.character(round(prop.table(clust_tab), digits = digits))
#     ),
#     nrow = 2L,
#     byrow = TRUE
#   )
#   colnames(tab) <- x$cluster_names
#   rownames(tab) <- c("count", "proportion")
#   for (i in seq(2L, k)) {
#     idx <- seq((i - 2L) * q + 1L, (i - 1L) * q)
#     cat(x$cluster_names[i], ":\n")
#     d <- data.frame(
#       `Estimate` = round(cf[, i], digits = digits),
#       `Std. error` = round(sqrt(v[idx]), digits = digits),
#       check.names = FALSE
#     )
#     rownames(d) <- rownames(cf)
#     print(d, digits = digits, ...)
#     cat("\n")
#   }
#   cat("Log-likelihood:", round(x$loglik, digits = digits), "\n")
#   cat("AIC:", round(x$aic, digits = digits), "\n")
#   cat("BIC:", round(x$bic, digits = digits), "\n\n")
#   cat("Mean of prior cluster probabilities :\n")
#   print(mean_prior, digits = digits, ...)
#   cat("\nMost probable clusters :\n")
#   print.default(tab, quote = FALSE, print.gap = 2L, right = TRUE)
#   cat("\n")
#   cat("Classification table :\n")
#   cat("Mean cluster probabilities (columns) by the most probable cluster (rows)\n\n")
#   print(x$classification, digits = digits, ...)
#   invisible(x)
# }

#' Print a `tna` Object
#'
#' @export
#' @family basic
#' @param x A `tna` object.
#' @param generic A `logical` value. If `TRUE`, use generic print method
#' instead. Defaults to `FALSE`.
#' @param ... Additional arguments passed to the generic `print` methods.
#' @return The `tna` object passed as argument `x` (invisibly).
#' @examples
#' model <- tna(group_regulation)
#' print(model)
#'
print.tna <- function(x, generic = FALSE, ...) {
  check_missing(x)
  check_class(x, "tna")
  check_flag(generic)
  if (generic) {
    NextMethod(generic = "print", object = x, ...)
    return()
  }
  type <- attr(x, "type")
  mat_type <- switch(type,
    `relative` = "Transition Probability",
    `frequency` = "Transition Frequency",
    `co-occurrence` = "Co-occurrence",
    `n-gram` = "N-gram Transition",
    `gap` = "Gap-allowed Transition",
    `window` = "Sliding Window Transition",
    `betweenness` = "Edge Betweenness",
    "Edge Weight"
  )
  cat("State Labels : \n\n")
  cat("  ", paste(x$labels, collapse = ", "), "\n")
  cat("\n", mat_type, " Matrix :\n\n", sep = "")
  print(x$weights, ...)
  inits <- stats::setNames(x$inits, x$labels)
  if (!is.null(x$inits)) {
    cat("\nInitial Probabilities : \n\n", sep = "")
    print(inits, ...)
  }
  invisible(x)
}

#' Print Bootstrap Results
#'
#' @export
#' @family validation
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
#' model <- tna(group_regulation)
#' # Small number of iterations for CRAN
#' boot <- bootstrap(model, iter = 10)
#' print(boot)
#'
print.tna_bootstrap <- function(x, digits = getOption("digits"),
                                type = "both", ...) {
  check_missing(x)
  check_class(x, "tna_bootstrap")
  check_values(digits)
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
#' @family centralities
#' @param x A `centralities` object.
#' @param ... Ignored.
#' @return `x` (invisibly).
#' @examples
#' model <- tna(group_regulation)
#' cm <- centralities(model)
#' print(cm)
#'
print.tna_centralities <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

#' Print Detected Communities
#'
#' @export
#' @family communities
#' @param x A `tna_communities` object.
#' @param ... Additional arguments passed to the generic `print` method.
#' @return `x` (invisibly).
#' @examples
#' model <- tna(group_regulation)
#' comm <- communities(model)
#' print(comm)
#'
print.tna_communities <- function(x, ...) {
  check_missing(x)
  check_class(x, "tna_communities")
  cat("Number of communities found by each algorithm\n\n")
  print(x$counts, ...)
  cat("\nCommunity assignments\n\n")
  print(x$assignments, ...)
  invisible(x)
}

#' Print Comparison Results
#'
#' @export
#' @family comparison
#' @param x A `tna_comparison` object.
#' @param ... Additional arguments passed to the tibble `print` method.
#' @return `x` (invisibly).
#' @examples
#' model_x <- tna(group_regulation[1:200, ])
#' model_y <- tna(group_regulation[1001:1200, ])
#' comp <- compare(model_x, model_y)
#' print(comp)
#'
print.tna_comparison <- function(x, ...) {
  check_missing(x)
  check_class(x, "tna_comparison")
  cat("Edge difference metrics\n")
  print(x$edge_metrics, ...)
  cat("\nSummary metrics of differences\n")
  print(x$summary_metrics, ...)
  cat("\nNetwork metrics\n")
  print(x$network_metrics, ...)
  cat("\nCentrality differences\n")
  print(x$centrality_differences, ...)
  cat("\nCentrality correlations\n")
  print(x$centrality_correlations, ...)
  invisible(x)
}

#' Print Found Cliques of a TNA Network
#'
#' @export
#' @family cliques
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
#' model <- tna(group_regulation)
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
  check_values(n, strict = TRUE)
  check_values(first, strict = TRUE)
  check_values(digits)
  n <- min(n, n_cliques)
  threshold <- attr(x, "threshold")
  cluster <- attr(x, "cluster")
  cat(
    "Number of ", size, "-cliques = ", n_cliques, " ",
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
    cat("\nClique ", i, "\n", sep = "")
    print(x$weights[[i]], digits = digits)
  }
  invisible(x)
}

#' Print a TNA Data Object
#'
#' @export
#' @family data
#' @param x A `tna_data` object.
#' @param data A `character` string that defines the data to be printed
#' tibble. Accepts either `"sequence"` (default) for wide format sequence data,
#' `"meta"`, for the wide format metadata, or `"long"` for the long format
#' data.
#' @param ... Arguments passed to the tibble `print` method.
#' @return `x` (invisibly).
#' @examples
#' res <- prepare_data(group_regulation_long, action = "Action", actor = "Actor",
#' time = "Time")
#' print(res, which = "sequence")
#' print(res, which = "meta")
#' print(res, which = "long")
#'
print.tna_data <- function(x, data = "sequence", ...) {
  check_missing(x)
  check_class(x, "tna_data")
  data <- check_match(data, c("sequence", "meta", "long", "names"))
  idx <- paste0(data, "_data")
  print(x[[idx]])
}

#' Print Centrality Stability Results
#'
#' @export
#' @family validation
#' @param x A `tna_stability` object.
#' @param ... Additional arguments passed to the generic `print` method.
#' @return `x` (invisibly).
#' @examples
#' model <- tna(group_regulation)
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
  print(cs_coefs, ...)
  invisible(x)
}

#' Print Permutation Test Results
#'
#' @export
#' @family validation
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
  if (!is.null(x$centralities)) {
    cat("Edges\n\n")
  }
  print(tibble::as_tibble(x$edges$stats), ...)
  if (!is.null(x$centralities)) {
    cat("\nCentralities\n\n")
    print(x$centralities$stats, ...)
  }
  invisible(x)
}

# #' Print a Mixture Markov Model Fit
# #'
# #' @export
# #' @family clusters
# #' @param x A `tna_mmm` object.
# #' @param digits An `integer` giving the number of *significant* digits
# #'   to print.
# #' @param ... Arguments passed to the generic `print` method
# #' @return `x` (invisibly)
# #' @examples
# #' print(engagement_tna_mmm)
# #'
# print.tna_mmm <- function(x, digits = 3L, ...) {
#   cat("Mixture Markov Model\n\n")
#   cat("Data:", x$data_name, "\n")
#   cat("Number of sequences:", nrow(x$data), "\n")
#   cat("Number of time points:", ncol(x$data), "\n")
#   cat("Number of clusters:", x$k, "\n")
#   cat("States:", cs(x$states), "\n")
#   cat("Coefficients :\n")
#   print(coef.tna_mmm(x), digits = digits)
#   cat("\n")
#   cat("Cluster sizes:")
#   print(x$sizes)
#   cat("\n")
#   cat("Intial probabilities :\n")
#   print.listof(x$inits, digits = digits)
#   cat("Transition probabilities :\n")
#   print.listof(x$trans, digits = digits)
#   invisible(x)
# }

#' Print the Results of Clustering
#'
#' @export
#' @rdname cluster_sequences
print.tna_clustering <- function(x, ...) {
  cat("Clustering method:", x$method, "\n")
  cat("Number of clusters:", x$k, "\n")
  cat("Silhouette score:", x$silhouette, "\n")
  cat("Cluster sizes:\n")
  print(x$sizes, ...)
  invisible(x)
}

#' Print a Comparison of Sequences
#'
#' @export
#' @family comparison
#' @param x A `tna_sequence_comparison` object.
#' @param ... Arguments passed to the generic `print` method.
#' @return `x` (invisibly).
#' @examples
#' group <- c(rep("High", 1000), rep("Low", 1000))
#' comp <- compare_sequences(group_regulation, group)
#' print(comp)
#'
print.tna_sequence_comparison <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

# Groups ----------------------------------------------------------------

#' Print a `group_tna` Object
#'
#' @export
#' @family basic
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
  print.listof(x, ...)
  invisible(x)
}

#' Print `group_tna` Bootstrap Results
#'
#' @export
#' @family validation
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
  print.listof(x, ...)
  invisible(x)
}

#' Print a Summary of a Grouped Transition Network Analysis Model
#'
#' @export
#' @family basic
#' @param x A `summary.group_tna` object.
#' @param ... Arguments passed to the `tibble` print method
#' @return `x` (invisibly).
#' @examples
#' model <- group_model(engagement_mmm)
#' print(summary(model))
#'
print.summary.group_tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "summary.group_tna")
  NextMethod(generic = "print", object = x, ...)
}

#' Print a Bootstrap Summary for a Grouped Transition Network Model
#'
#' @export
#' @family validation
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
  check_missing(x)
  check_class(x, "summary.group_tna_bootstrap")
  NextMethod(generic = "print", object = x, ...)
}

#' Print Centrality Measures
#'
#' @export
#' @family centralities
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
#' @family communities
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
  print.listof(x, ...)
  invisible(x)
}

#' Print Found Cliques
#'
#' @export
#' @family cliques
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
  print.listof(x, ...)
  invisible(x)
}

#' Print Centrality Stability Results
#'
#' @export
#' @family validation
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
  print.listof(x, ...)
  invisible(x)
}


#' Print Permutation Test Results
#'
#' @export
#' @family validation
#' @param x A `group_tna_permutation` object.
#' @param ... Arguments passed to [print.tna_permutation()].
#' @return `x` (invisibly).
#' @examples
#' model <- group_model(engagement_mmm)
#' # Small number of iterations for CRAN
#' perm <- permutation_test(model, iter = 20)
#' print(perm)
#'
print.group_tna_permutation <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna_permutation")
  print.listof(x, ...)
  invisible(x)
}

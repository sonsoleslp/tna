#' Cluster Sequences via Dissimilarity Matrix based on String Distances
#'
#' Performs clustering on sequence data using specified dissimilarity measures
#' and clustering methods. The sequences are first converted to strings
#' and compared using the `stringdist` package.
#'
#' @param data A `data.frame` or a `matrix` where the rows are sequences and
#'   the columns are time points.
#' @param x A `tna_clustering` object.
#' @param k An `integer` giving the number of clusters.
#' @param dissimilarity A `character` string specifying the
#'   dissimilarity measure. The available options are: `"osa"`, `"lv"`, `"dl"`,
#'   `"hamming"`, `"qgram"`, `"cosine"`, `"jaccard"`, and `"jw"`. See
#'   [stringdist::stringdist-metrics] for more information on these measures.
#' @param method A `character` string specifying clustering method.
#'   The available methods are `"pam"`, `"ward.D"`, `"ward.D2"`,
#'   `"complete"`,`"average"`, `"single"`, `"mcquitty"`, `"median"`, and
#'   `"centroid"`. See [cluster::pam()] and [stats::hclust()] for more
#'   information on these methods.
#' @param na_syms A `character` vector of symbols or factor levels to convert
#'   to explicit missing values.
#' @param weighted A `logical` value indicating whether the dissimilarity
#'   measure should be weighted (the default is `FALSE` for no weighting).
#'   If `TRUE`, earlier observations of the sequences receive a greater weight
#'   in the distance calculation with an exponential decay. Currently only
#'   supported for the Hamming distance.
#' @param lambda A `numeric` value defining the strength of the decay when
#'   `weighted = TRUE`. The default is `1.0`.
#' @param ... Additional arguments passed to [stringdist::stringdist()].
#' @return A `tna_clustering` object which is a  `list` containing:
#'
#'   * `data`: The original data.
#'   * `k`: The number of clusters.
#'   * `assignments`: An `integer` vector of cluster assignments.
#'   * `silhouette`: Silhouette score measuring clustering quality.
#'   * `sizes`: An `integer` vector of cluster sizes.
#'   * `method`: The clustering method used.
#'   * `distance`: The distance matrix.
#'
#' @examples
#' data <- data.frame(
#'   T1 = c("A", "B", "A", "C", "A", "B"),
#'   T2 = c("B", "A", "B", "A", "C", "A"),
#'   T3 = c("C", "C", "A", "B", "B", "C")
#' )
#'
#' # PAM clustering with optimal string alignment (default)
#' result <- cluster_sequences(data, k = 2)
#' print(result)
#'
#' @export
cluster_sequences <- function(data, k, dissimilarity = "hamming",
                              method = "pam", na_syms = c("*", "%"),
                              weighted = FALSE, lambda = 1.0, ...) {
  stopifnot_(
    is.data.frame(data) || is.matrix(data),
    "Argument {.arg data} must be a {.cls data.frame} or a {.cls matrix}."
  )
  check_range(k, type = "integer", lower = 2L, upper = nrow(data) - 1L)
  dissimilarity <- check_match(
    dissimilarity,
    available_clustering_dissimilarities
  )
  method <- check_match(
    method,
    available_clustering_methods
  )
  stopifnot_(
    !weighted || dissimilarity == "hamming",
    "Weighting is only supported for the Hamming distance."
  )
  if (weighted && dissimilarity == "hamming") {
    dist_mat <- weighted_hamming(
      mat = seq2str(
        data,
        na_syms = na_syms,
        na_include = TRUE,
        collapse = FALSE
      ),
      lambda = lambda
    )
  } else {
    dist_mat <- stringdist::stringdistmatrix(
      a = seq2str(data, na_syms),
      method = dissimilarity,
      ...
    )
  }
  if (method == "pam") {
    clust_result <- cluster::pam(dist_mat, diss = TRUE, k = k)
    assignments <- clust_result$clustering
    silhouette_score <- clust_result$silinfo$avg.width
  } else {
    hc <- stats::hclust(dist_mat, method = method)
    assignments <- stats::cutree(hc, k = k)
    sil <- cluster::silhouette(assignments, dist = dist_mat)
    silhouette_score <- mean(sil[, 3L])
  }
  sizes <- c(table(assignments))
  structure(
    list(
      data = data,
      k = k,
      assignments = assignments,
      silhouette = silhouette_score,
      sizes = sizes,
      method = method,
      distance = dist_mat
    ),
    class = "tna_clustering"
  )
}

#' Convert a matrix of sequences to a vector of strings
#'
#' @param data A `data.frame` or a `matrix`
#' @param na_syms An optional `vector` of values to convert to `NA` values.
#' @param na_convert A `logical` value indicating whether to include missing
#'   values as an explicit value.
#' @param collapse A `logical` value indicating whether the character matrix
#'   should be collapsed to a `vector` of strings.
#' @noRd
seq2str <- function(data, na_syms, na_include = FALSE, collapse = TRUE) {
  data <- as.matrix(data)
  chr_mat <- matrix(NA_character_, nrow = nrow(data), ncol = ncol(data))
  data[data %in% na_syms] <- NA
  obs <- !is.na(data)
  u_vals <- unique(c(data))
  base <- c(letters, LETTERS, as.character(0:9))
  obs_vals <- data[obs]
  idx <- match(obs_vals, u_vals)
  chr_mat[obs] <- base[idx]
  if (na_include) {
    chr_mat[!obs] <- base[max(idx) + 1L]
  }
  if (collapse) {
    apply(
      chr_mat,
      1L,
      function(x) {
        paste0(x[!is.na(x)], collapse = "")
      }
    )
  } else {
    chr_mat
  }
}

#' Attention-Weighted Hamming Distance
#'
#' @param mat A `character` matrix
#' @param lambda A `numeric` value for the decay rate
#' @noRd
weighted_hamming <- function(mat, lambda) {
  n <- nrow(mat)
  k <- ncol(mat)
  weights <- exp(-lambda * seq(0, k - 1))
  weights <- weights / max(weights)
  d <- matrix(0.0, n, n)
  for (i in seq_len(n - 1L)) {
    for (j in seq(i + 1L, n)) {
      d[i, j] <- d[j, i] <- sum(
        (mat[i, ] != mat[j, ]) * weights
      )
    }
  }
  stats::as.dist(d)
}

available_clustering_dissimilarities <- c(
  "osa",
  "lv",
  "dl",
  "hamming",
  "qgram",
  "cosine",
  "jaccard",
  "jw"
)

available_clustering_methods <- c(
  "pam",
  "ward.D2",
  "ward.D",
  "complete",
  "average",
  "single",
  "mcquitty",
  "median",
  "centroid"
)

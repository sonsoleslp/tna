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
cluster_sequences <- function(data, k, dissimilarity = "osa",
                              method = "pam", na_syms = c("*", "%"), ...) {
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
  strings <- seq2str(data, na_syms)
  dist_mat <- stringdist::stringdistmatrix(
    a = strings,
    method = dissimilarity,
    ...
  )
  # Perform clustering
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
#' @noRd
seq2str <- function(data, na_syms) {
  data <- as.matrix(data)
  chr_mat <- matrix(NA_character_, nrow = nrow(data), ncol = ncol(data))
  data[data %in% na_syms] <- NA
  obs <- !is.na(data)
  u_vals <- unique(c(data))
  base <- c(letters, LETTERS, as.character(0:9))
  obs_vals <- data[obs]
  idx <- match(obs_vals, u_vals)
  chr_mat[obs] <- base[idx]
  apply(
    chr_mat,
    1L,
    function(x) {
      paste0(x[!is.na(x)], collapse = "")
    }
  )
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

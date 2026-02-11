#' Clustering via Dissimilarity Matrix based on String Distances
#'
#' Performs clustering using specified dissimilarity measures
#' and clustering methods. The rows of the data are first converted to strings
#' and compared using the dissimilarity measures available in the 
#' `stringdist` package.
#'
#' @param data A `data.frame` or a `matrix` in wide format.
#' @param k An `integer` giving the number of clusters.
#' @param dissimilarity A `character` string specifying the
#'   dissimilarity measure. The available options are: `"osa"`, `"lv"`, `"dl"`,
#'   `"hamming"`, `"lcs"`, `"qgram"`, `"cosine"`, `"jaccard"`, and `"jw"`.
#'   See [stringdist::stringdist-metrics] for more information on
#'   these measures.
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
#'
#' @export
cluster_data <- function(data, k, dissimilarity = "hamming",
                         method = "pam", na_syms = c("*", "%"),
                         weighted = FALSE, lambda = 1.0, ...) {
  stopifnot_(
    is.data.frame(data) || is.matrix(data),
    "Argument {.arg data} must be a {.cls data.frame} or a {.cls matrix}."
  )
  check_range(k, type = "integer", lower = 2L, upper = nrow(data) - 1L)
  dissimilarity <- check_match(dissimilarity, available_clustering_metrics)
  method <- check_match(method, available_clustering_methods)
  stopifnot_(
    !weighted || dissimilarity == "hamming",
    "Weighting is only supported for the Hamming distance."
  )
  lambda <- ifelse_(weighted, lambda, 0.0)
  strings <- seq2chr(data, na_syms)
  dist_mat <- dissimilarity_matrix(strings, dissimilarity, lambda, ...)
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

#' @export
#' @rdname cluster_data
cluster_sequences <- cluster_data

#' Convert a matrix of sequences to a character matrix of symbols
#'
#' @param data A `data.frame` or a `matrix`
#' @param na_syms An optional `vector` of values to convert to `NA` values.
#' @noRd
seq2chr <- function(data, na_syms) {
  data <- as.matrix(data)
  chr_mat <- matrix(NA_character_, nrow = nrow(data), ncol = ncol(data))
  data[data %in% na_syms] <- NA
  obs <- !is.na(data)
  last_obs <- max.col(obs, ties.method = "last")
  u_vals <- unique(c(data))
  base <- c(letters, LETTERS, as.character(0:9))
  obs_vals <- data[obs]
  idx <- match(obs_vals, u_vals)
  chr_mat[obs] <- base[idx]
  chr_mat[!obs] <- base[max(idx) + 1L]
  list(mat = chr_mat, len = last_obs)
}

get_qgram <- function(x, n, q) {
  if (n < q) {
    return(integer())
  }
  grams <- character(n - q + 1L)
  for (i in seq_along(grams)) {
    grams[i] <- paste0(x[i:(i + q - 1L)], collapse = "")
  }
  tab <- table(grams)
  counts <- as.integer(tab)
  names(counts) <- names(tab)
  counts
}

# Dissimilarity measures --------------------------------------------------

available_clustering_metrics <- c(
  "osa",
  "lv",
  "dl",
  "hamming",
  "lcs",
  "qgram",
  "cosine",
  "jaccard",
  "jw"
)

#' Dissimilarity matrix
#'
#' @param strings Output of `seq2str`.
#' @param lambda A `numeric` value for the decay rate.
#' @noRd
dissimilarity_matrix <- function(strings, dissimilarity, lambda, ...) {
  mat <- strings$mat
  len <- strings$len
  n <- nrow(mat)
  weights <- 1.0
  if (lambda > 0) {
    k <- ncol(mat)
    weights <- exp(-lambda * seq(0, k - 1))
    weights <- weights / max(weights)
  }
  qgrams <- vector(mode = "list", length = n)
  if (dissimilarity %in% c("qgram", "cosine", "jaccard")) {
    q <- list(...)$q
    q <- ifelse_(is.null(q), 2L, q)
    for (i in seq_len(n)) {
      qgrams[[i]] <- get_qgram(x = mat[i, ], n = len[i], q = q)
    }
  }
  d_fun <- switch(dissimilarity,
    osa = osa_dist,
    lv = levenshtein_dist,
    dl = damerau_levenshtein_dist,
    hamming = hamming_dist,
    lcs = lcs_dist,
    qgram = qgram_dist,
    cosine = cosine_dist,
    jaccard = jaccard_dist,
    jw = jaro_winkler_dist
  )
  d <- matrix(0.0, n, n)
  for (i in seq_len(n - 1L)) {
    for (j in seq(i + 1L, n)) {
      d[i, j] <- d_fun(
        x = mat[i, ],
        y = mat[j, ],
        n = len[i],
        m = len[j],
        qx = qgrams[[i]],
        qy = qgrams[[j]],
        weights = weights,
        ...
      )
      d[j, i] <- d[i, j]
    }
  }
  stats::as.dist(d)
}

osa_dist <- function(x, y, n, m, ...) {
  d <- matrix(0L, n + 1L, m + 1L)
  d[, 1L] <- 0:n
  d[1L, ] <- 0:m
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      cost <- 0L + 1L * (x[i] == y[j])
      d[i + 1L, j + 1L] <- min(
        d[i, j + 1L] + 1L,
        d[i + 1L, j] + 1L,
        d[i, j] + cost
      )
      if (i > 1 && j > 1 && x[i] == y[j - 1L] && x[i - 1L] == y[j]) {
        d[i + 1L, j + 1L] <- min(d[i + 1L, j + 1L], d[i - 1L, j - 1L] + 1L)
      }
    }
  }
  d[n+1L, m+1L]
}

levenshtein_dist <- function(x, y, n, m, ...) {
  d <- matrix(0L, n + 1L, m + 1L)
  d[, 1L] <- 0:n
  d[1L, ] <- 0:m
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      cost <- 0L + 1L * (x[i] == y[j])
      d[i + 1L, j + 1L] <- min(
        d[i, j + 1L] + 1L,
        d[i + 1L, j] + 1L,
        d[i, j] + cost
      )
    }
  }
  d[n + 1L, m + 1L]
}

damerau_levenshtein_dist <- function(x, y, n, m, ...) {
  d <- matrix(0L, n + 1L, m + 1L)
  d[, 1L] <- 0:n
  d[1L, ] <- 0:m
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      cost <- 0L + 1L * (x[i] == y[j])
      d[i + 1L, j + 1L] <- min(
        d[i, j + 1L] + 1L,
        d[i + 1L, j] + 1L,
        d[i, j] + cost
      )
      if (i > 1 && j > 1 && x[i] == y[j - 1L] && x[i - 1L] == y[j]) {
        d[i + 1L, j + 1L] <- min(d[i + 1L, j + 1L], d[i - 1L, j - 1L] + 1L)
      }
    }
  }
  d[n + 1L, m + 1L]
}

hamming_dist <- function(x, y, n, m, weights, ...) {
  sum((x != y) * weights)
}

lcs_dist <- function(x, y, n, m, ...) {
  lcs_len <- matrix(0L, n + 1L, m + 1L)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      if (x[i] == y[j]) {
        lcs_len[i + 1L, j + 1L] <- lcs_len[i, j] + 1L
      } else {
        lcs_len[i + 1L, j + 1L] <- max(
          lcs_len[i, j + 1L],
          lcs_len[i + 1L, j]
        )
      }
    }
  }
  max(n, m) - lcs_len[n + 1L, m + 1L]
}

qgram_dist <- function(x, y, qx, qy, ...) {
  grams <- union(names(qx), names(qy))
  x_all <- qx[grams]
  y_all <- qy[grams]
  x_all[is.na(x_all)] <- 0L
  y_all[is.na(y_all)] <- 0L
  sum(abs(x_all - y_all))
}

cosine_dist <- function(x, y, qx, qy, ...) {
  grams <- union(names(qx), names(qy))
  x_all <- qx[grams]
  y_all <- qy[grams]
  x_all[is.na(x_all)] <- 0L
  y_all[is.na(y_all)] <- 0L
  num <- sum(x_all * y_all)
  den <- sqrt(sum(x_all^2)) * sqrt(sum(y_all^2))
  if (den == 0) {
    return(1)
  }
  1.0 - num / den
}

jaccard_dist <- function(qx, qy, ...) {
  qx <- names(qx)
  qy <- names(qy)
  if (length(qx) == 0 && length(qy) == 0) {
    return(0)
  }
  1.0 - length(intersect(qx, qy)) / length(union(qx, qy))
}

jaro_dist <- function(x, y, n, m, ...) {
  if (n == 0 && m == 0) return(0)
  if (n == 0 || m == 0) return(1)
  match_dist <- floor(max(n, m) / 2) - 1
  x_match <- rep(FALSE, n)
  y_match <- rep(FALSE, m)
  matches <- 0L
  for (i in seq_len(n)) {
    start <- max(1L, i - match_dist)
    end <- min(m, i + match_dist)
    for (j in start:end) {
      if (!y_match[j] && x[i] == y[j]) {
        x_match[i] <- TRUE
        y_match[j] <- TRUE
        matches <- matches + 1L
        break
      }
    }
  }
  if (matches == 0L) {
    return(1)
  }
  trans <- 0L
  k <- 1L
  for (i in seq_len(n)) {
    if (x_match[i]) {
      while (!y_match[k]) {
        k <- k + 1L
      }
      if (x[i] != y[k]) {
        trans <- trans + 1L
      }
      k <- k + 1L
    }
  }
  trans <- trans / 2L
  jaro <- (matches / n + matches / m + (matches - trans) / matches) / 3.0
  1.0 - jaro
}

jaro_winkler_dist <- function(x, y, n, m, p = 0.1, max_l = 4L, ...) {
  jaro_d <- 1 - jaro_dist(x, y, n, m)
  l <- 0L
  for (k in seq_len(min(n, m, max_l))) {
    if (x[k] == y[k]) {
      l <- l + 1L
    } else {
      break
    }
  }
  jw <- jaro_d + l * p * (1.0 - jaro_d)
  1.0 - jw
}

# Clustering methods ------------------------------------------------------

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

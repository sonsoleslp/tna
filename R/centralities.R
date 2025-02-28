#' Calculate Centrality Measures for a Transition Matrix
#'
#' Calculates several centrality measures. See 'Details' for
#' information about the measures.
#'
#' The following measures are provided:
#'
#'   * `OutStrength`: Outgoing strength centrality, calculated using
#'     [igraph::strength()] with `mode = "out"`. It measures the total weight
#'     of the outgoing edges from each node.
#'   * `InStrength`: Incoming strength centrality, calculated using
#'     [igraph::strength()] with `mode = "in"`. It measures the total weight
#'     of the incoming edges to each node.
#'   * `ClosenessIn`: Closeness centrality (incoming), calculated using
#'     [igraph::closeness()] with `mode = "in"`. It measures how close a node
#'     is to all other nodes based on the incoming paths.
#'   * `ClosenessOut`: Closeness centrality (outgoing), calculated using
#'     [igraph::closeness()] with `mode = "out"`. It measures how close a node
#'     is to all other nodes based on the outgoing paths.
#'   * `Closeness`: Closeness centrality (overall), calculated using
#'     [igraph::closeness()] with `mode = "all"`. It measures how close a node
#'     is to all other nodes based on both incoming and outgoing paths.
#'   * `Betweenness`: Betweenness centrality defined by the number of
#'     geodesics calculated using [igraph::betweenness()].
#'   * `BetweennessRSP`: Betweenness centrality based on randomized shortest
#'     paths (Kivimäki et al. 2016). It measures the extent to which a
#'     node lies on the shortest paths between other nodes.
#'   * `Diffusion`: Diffusion centrality of Banerjee et.al. (2014).
#'     It measures the influence of a node in spreading information through
#'     the network.
#'   * `Clustering`: Signed clustering coefficient of Zhang and Horvath (2005)
#'     based on the symmetric adjacency matrix (sum of the adjacency matrix
#'     and its transpose). It measures the degree to which nodes tend to
#'     cluster together.
#'
#' @export
#' @family core
#' @rdname centralities
#' @param x A `tna` object, a `group_tna` object, or
#' a square `matrix` representing edge weights.
#' @param loops A `logical` value indicating whether to include loops in the
#'   network when computing the centrality measures (default is `FALSE`).
#' @param normalize  A `logical` value indicating whether the centralities
#'   should be normalized (default is `FALSE`).
#' @param measures A `character` vector indicating which centrality
#'   measures should be computed. If missing, all available measures are
#'   returned. See 'Details' for available measures. The elements are partially
#'   matched ignoring case.
#' @param ... Ignored.
#' @return A `tna_centralities` object which is a tibble (`tbl_df`).
#'   containing centrality measures for each state.
#' @examples
#' model <- tna(group_regulation)
#'
#' # Centrality measures including loops in the network
#' centralities(model)
#'
#' # Centrality measures excluding loops in the network
#' centralities(model, loops = FALSE)
#'
#' # Centrality measures normalized
#' centralities(model, normalize = TRUE)
#'
centralities <- function(x, loops = FALSE, normalize = FALSE, measures, ...) {
  UseMethod("centralities")
}

#' @export
#' @rdname centralities
centralities.tna <- function(x, loops = FALSE,
                             normalize = FALSE, measures, ...) {
  check_missing(x)
  check_class(x, "tna")
  out <- centralities_(
    x$weights,
    loops = loops,
    normalize = normalize,
    measures = measures
  )
  if (!is.null(x$data)) {
    attr(out, "colors") <- attr(x$data, "colors")
  }
  out
}

#' @export
#' @rdname centralities
centralities.matrix <- function(x, loops = FALSE,
                                normalize = FALSE, measures, ...) {
  check_missing(x)
  stopifnot_(
    is.matrix(x),
    "Argument {.arg x} must be a {.cls matrix}."
  )
  centralities_(x, loops, normalize, measures)
}

#' Internal function to calculate various centrality measures
#'
#' @param x An adjacency `matrix` of a directed weighted graph.
#' @inheritParams centralities
#' @noRd
centralities_ <- function(x, loops, normalize, measures) {
  check_flag(loops)
  check_flag(normalize)
  measures <- ifelse_(
    missing(measures),
    available_centrality_measures,
    measures
  )
  measures <- check_measures(measures)
  diag(x) <- ifelse_(loops, diag(x), 0)
  g <- igraph::graph_from_adjacency_matrix(
    adjmatrix = x,
    mode = "directed",
    weighted = TRUE
  )
  measures_out <- lapply(
    measures,
    function(y) {
      centrality_funs[[y]](g = g, x = x)
    }
  )
  names(measures_out) <- measures
  out <- as.data.frame(measures_out)
  if (normalize) {
    out[, measures] <- as.data.frame(lapply(out[, measures], ranger))
  }
  rn <- rownames(out)
  out <- tibble::rownames_to_column(out, "state")
  out$state <- factor(out$state, levels = rn)
  structure(
    out,
    class = c("tna_centralities", "tbl_df", "tbl", "data.frame")
  )
}

#' Compute the Diffusion Centrality Measure
#'
#' @param mat A weight `matrix`.
#' @noRd
diffusion <- function(mat) {
  s <- 0
  n <- ncol(mat)
  p <- diag(1, n, n)
  for (i in seq_len(n)) {
    p <- p %*% mat
    s <- s + p
  }
  .rowSums(s, n, n)
}

#' @export
#' @rdname estimate_centrality_stability
estimate_cs <- function(x, ...) {
  UseMethod("estimate_cs")
}

#' @export
#' @rdname estimate_centrality_stability
estimate_centrality_stability <- estimate_cs

#' Estimate Centrality Stability
#'
#' Estimates the stability of centrality measures in a network
#' using subset sampling without replacement. It allows for dropping varying
#' proportions of cases and calculates correlations between the original
#' centralities and those computed using sampled subsets.
#'
#' The function works by repeatedly resampling the data, dropping varying
#' proportions of cases, and calculating centrality measures on the subsets.
#' The correlation between the original centralities and the resampled
#' centralities is calculated for each drop proportion. The stability of each
#' centrality measure is then summarized using a centrality stability (CS)
#' coefficient, which represents the proportion of dropped cases at which
#' the correlations drop below a given threshold (default 0.7).
#'
#' The results can be visualized by plotting the output object showing the
#' stability of the centrality measures across different drop proportions,
#' along with confidence intervals. The CS-coefficients are displayed in the
#' subtitle.
#'
#' @export
#' @rdname estimate_centrality_stability
#' @param x A `tna` or a `group_tna` object representing the temporal network
#' analysis data. The object should be created from a sequence data object.
#' @param loops A `logical` value indicating whether to include loops in the
#'   network when computing the centrality measures (default is `FALSE`).
#' @param normalize A `logical` value indicating whether to normalize
#' the centrality measures. The default is `FALSE`.
#' @param measures A `character` vector of centrality measures to estimate.
#' The default measures are `"InStrength"`, `"OutStrength"`,
#' and `"Betweenness"`. See [centralities()] for a list of available centrality
#' measures.
#' @param iter An `integer` specifying the number of resamples to draw.
#' The default is 1000.
#' @param method A `character` string indicating the correlation coefficient
#' type. The default is `"pearson"`. See [stats::cor()] for details.
#' @param drop_prop A `numeric` vector specifying the proportions of
#' cases to drop in each sampling iteration. Default is a sequence from 0.1 to
#' 0.9 in increments of 0.1.
#' @param threshold A `numeric` value specifying the correlation threshold for
#' calculating the CS-coefficient. The default is 0.7.
#' @param certainty A `numeric` value specifying the desired level of certainty
#' for the CS-coefficient. Default is 0.95.
#' @param detailed A `logical` value specifying whether to return detailed
#' sampling results. If `TRUE`, detailed results are included in the output.
#' The default is `FALSE`.
#' @param progressbar A `logical` value. If `TRUE`, a progress bar is displayed
#' Defaults to `FALSE`
#' @param ... Ignored.
#'
#' @return A `tna_stability` object which is a `list` with an element for each
#' `measure` with the following elements:
#'
#' * `cs_coefficient`: The centrality stability (CS) coefficient
#'   of the measure.
#' * `correlations`: A `matrix` of correlations between the original
#'   centrality and the resampled centralities for each drop proportion.
#' * `detailed_results`: A detailed data frame of the sampled correlations,
#'   returned only if `return_detailed = TRUE`
#'
#' If `x` is a `group_tna` object, a `group_tna_stability` object is returned
#' instead, which is a `list` of `tna_stability` objects.
#'
#' @examples
#' model <- tna(group_regulation)
#' # Small number of iterations and drop proportions for CRAN
#' estimate_cs(
#'   model,
#'   drop_prop = seq(0.3, 0.9, by = 0.2),
#'   measures = c("InStrength", "OutStrength"),
#'   iter = 10
#' )
#'
estimate_cs.tna <- function(x, loops = FALSE, normalize = FALSE,
                            measures = c(
                              "InStrength", "OutStrength", "Betweenness"
                            ), iter = 1000, method = "pearson",
                            drop_prop = seq(0.1, 0.9, by = 0.1),
                            threshold = 0.7,
                            certainty = 0.95, detailed = FALSE,
                            progressbar = FALSE, ...) {
  check_tna_seq(x)
  check_flag(loops)
  check_flag(normalize)
  check_flag(progressbar)
  check_flag(detailed)
  check_values(iter, strict = TRUE)
  check_range(threshold)
  check_range(certainty)
  check_measures(measures)
  d <- x$data
  type <- attr(x, "type")
  scaling <- attr(x, "scaling")
  params <- attr(x, "params")
  model <- initialize_model(d, type, scaling, params, transitions = TRUE)
  trans <- model$trans
  a <- dim(trans)[2]
  n <- nrow(d)
  n_seq <- seq_len(n)
  n_prop <- length(drop_prop)
  centralities_orig <- centralities_(
    x = x$weights,
    loops = loops,
    normalize = normalize,
    measures = measures
  )
  sds_orig <- centralities_orig |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(measures),
        stats::sd
      )
    )
  valid_measures <- which(sds_orig > 0)
  centralities_orig <- centralities_orig[, 1L + valid_measures]
  measures <- measures[valid_measures]
  n_measures <- length(measures)
  stability <- replicate(
    n_measures,
    matrix(NA, nrow = iter, ncol = n_prop),
    simplify = FALSE
  )
  names(stability) <- measures
  if (progressbar) {
    pb <- utils::txtProgressBar(min = 0, max = n_prop * iter, style = 3)
  }
  for (i in seq_len(n_prop)) {
    prop <- drop_prop[i]
    n_drop <- floor(n * prop)
    if (n_drop == 0) {
      warning_(
        paste0("No cases dropped for proportion ", prop, ". Skipping...")
      )
      next
    }
    corr_prop <- matrix(nrow = iter, ncol = n_measures)
    for (j in seq_len(iter)) {
      keep <- sample(n_seq, n - n_drop, replace = FALSE)
      trans_sub <- trans[keep, , , drop = FALSE]
      weight_sub <- compute_weights(trans_sub, type, scaling, a)
      centralities_sub <- centralities_(
        x = weight_sub,
        loops = loops,
        normalize = normalize,
        measures = measures
      )
      sds_sub <- centralities_sub |>
        dplyr::summarize(
          dplyr::across(
            dplyr::all_of(measures),
            stats::sd
          )
        )
      corr_prop[j, ] <- vapply(
        measures,
        function(measure) {
          centrality_sub <- centralities_sub[[measure]]
          centrality_orig <- centralities_orig[[measure]]
          sd_sub <- sds_sub[measure]
          ifelse_(
            sd_sub > 0,
            stats::cor(
              centrality_sub,
              centrality_orig,
              method = method,
              use = "complete.obs"
            ),
            NA_real_
          )
        },
        numeric(1L)
      )
      if (progressbar) {
        utils::setTxtProgressBar(pb, (i - 1) * iter + j)
      }
    }
    for (k in seq_len(n_measures)) {
      measure <- measures[k]
      stability[[measure]][, i] <- corr_prop[, k]
    }
  }
  if (progressbar) {
    close(pb)
  }
  out <- list()
  # TODO handle all NA case?
  for (measure in measures) {
    cs_coef <- calculate_cs(
      stability[[measure]],
      threshold,
      certainty,
      drop_prop
    )
    out[[measure]] <- list(
      cs_coefficient = cs_coef,
      correlations = stability[[measure]]
    )
  }
  structure(
    out,
    class = "tna_stability",
    drop_prop = drop_prop,
    threshold = threshold
  )
}

#' @export
#' @rdname estimate_centrality_stability
estimate_centrality_stability.tna <- estimate_cs.tna

#' Calculate Centrality Stability
#'
#' @param corr_mat A `matrix` of correlation values
#' @inheritParams estimate_centrality_stability
#' @noRd
calculate_cs <- function(corr_mat, threshold, certainty, drop_prop) {
  prop_above <- apply(corr_mat, 2, function(x) {
    mean(x >= threshold, na.rm = TRUE)
  })
  valid_indices <- which(prop_above >= certainty)
  ifelse_(
    length(valid_indices) > 0,
    drop_prop[max(valid_indices)],
    0
  )
}

#' Compute the Randomized Shortest Path Betweenness Centrality Measure
#'
#' @param mat A weight `matrix`.
#' @param beta The beta parameter of the algorithm.
#' @noRd
rsp_bet <- function(mat, beta = 0.01) {
  n <- ncol(mat)
  D <- .rowSums(mat, m = n, n = n)
  if (any(D == 0)) {
    return(NA)
  }
  P_ref <- diag(D^-1) %*% mat
  C <- mat^-1
  C[is.infinite(C)] <- 0
  W <- P_ref * exp(-beta * C)
  Z <- solve(diag(1, n, n) - W)
  Z_recip <- Z^-1
  Z_recip[is.infinite(Z_recip)] <- 0
  Z_recip_diag <- diag(Z_recip) * diag(1, n, n)
  out <- diag(tcrossprod(Z, Z_recip - n * Z_recip_diag) %*% Z)
  out <- round(out)
  out <- out - min(out) + 1
  out
}

#' Compute the Signed Clustering Coefficient
#'
#' @param mat A weight `matrix`.
#' @noRd
wcc <- function(mat) {
  diag(mat) <- 0
  n <- ncol(mat)
  num <- diag(mat %*% mat %*% mat)
  den <- .colSums(mat, n, n)^2 - .colSums(mat^2, n, n)
  num / den
}


# Clusters ----------------------------------------------------------------

#' @export
#' @family clusters
#' @rdname centralities
centralities.group_tna <- function(x, loops = FALSE,
                                   normalize = FALSE, measures, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  # missing() does not work with lapply, need to evaluate measures here.
  measures <- ifelse_(
    missing(measures),
    available_centrality_measures,
    measures
  )
  out <- dplyr::bind_rows(
    lapply(
      x,
      function(i) {
        data.frame(
          centralities.tna(
            x = i,
            loops = loops,
            normalize = normalize,
            measures = measures,
            ...
          )
        )
      }
    ),
    .id = "group"
  )
  structure(
    out,
    class = c("group_tna_centralities", "tbl_df", "tbl", "data.frame")
  )
}

#' @export
#' @family clusters
#' @rdname estimate_centrality_stability
estimate_cs.group_tna <- function(x, loops = FALSE, normalize = FALSE,
                                  measures = c(
                                    "InStrength", "OutStrength", "Betweenness"
                                  ), iter = 1000, method = "pearson",
                                  drop_prop = seq(0.1, 0.9, by = 0.1),
                                  threshold = 0.7, certainty = 0.95,
                                  detailed = FALSE, progressbar = FALSE, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  structure(
    lapply(
      x,
      function(i) {
        estimate_centrality_stability.tna(
          i,
          loops = loops,
          normalize = normalize,
          measures = measures,
          iter = iter,
          method = method,
          drop_prop = drop_prop,
          threshold = threshold,
          certainty = certainty,
          detailed = detailed,
          progressbar = progressbar,
          ...
        )
      }
    ),
    class = "group_tna_stability"
  )
}

#' @export
#' @family clusters
#' @rdname estimate_centrality_stability
estimate_centrality_stability.group_tna <- estimate_cs.group_tna

# Available centrality measures -----------------------------------------------
available_centrality_measures <- c(
  "OutStrength",
  "InStrength",
  "ClosenessIn",
  "ClosenessOut",
  "Closeness",
  "Betweenness",
  "BetweennessRSP",
  "Diffusion",
  "Clustering"
)

# Centrality measure function wrappers ----------------------------------------

centrality_funs <- list()

centrality_funs$OutStrength <- function(g, ...) {
  igraph::strength(g, mode = "out")
}

centrality_funs$InStrength <- function(g, ...) {
  igraph::strength(g, mode = "in")
}

centrality_funs$ClosenessIn <- function(g, ...) {
  igraph::closeness(g, mode = "in")
}

centrality_funs$ClosenessOut <- function(g, ...) {
  igraph::closeness(g, mode = "out")
}

centrality_funs$Closeness <- function(g, ...) {
  igraph::closeness(g, mode = "all")
}

centrality_funs$Betweenness <- function(g, ...) {
  igraph::betweenness(g)
}

centrality_funs$BetweennessRSP <- function(x, ...) {
  rsp_bet(x)
}

centrality_funs$Diffusion <- function(x, ...) {
  diffusion(x)
}

centrality_funs$Clustering <- function(x, ...) {
  wcc(x + t(x))
}

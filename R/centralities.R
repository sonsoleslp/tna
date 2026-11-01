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
#'     the network. Diffusion is always min-max normalized to the range
#'     from 0 to 1, whatever the value of `normalize`, because its raw
#'     values grow with powers of the edge weights (for a frequency model
#'     they easily exceed `1e30`). When the raw values are equal for every
#'     state, for example in a relative transition network with
#'     `loops = TRUE` where every row sums to one, the normalized values are
#'     `NaN`: the measure does not distinguish the states.
#'   * `Clustering`: Signed clustering coefficient of Zhang and Horvath (2005)
#'     based on the symmetric adjacency matrix (sum of the adjacency matrix
#'     and its transpose). It measures the degree to which nodes tend to
#'     cluster together.
#'   * `PageRank`: Weighted PageRank centrality calculated using
#'     [igraph::page_rank()] on the directed transition network.
#'
#' @export
#' @family centralities
#' @rdname centralities
#' @param x A `tna` object, a `group_tna` object, or
#' a square `matrix` representing edge weights.
#' @param loops A `logical` value indicating whether to include loops in the
#'   network when computing the centrality measures. The default is `FALSE`.
#' @param normalize A `logical` value indicating whether the centralities
#'   should be normalized. The default is `FALSE`. `Diffusion` is always
#'   normalized (see 'Details').
#' @param invert A `logical` value indicating whether the weights should be
#'   inverted for distance-based measures. The default is `TRUE`.
#' @param measures A `character` vector indicating which centrality
#'   measures should be computed. If missing, all available measures are
#'   returned. See 'Details' for the available measures.
#' @return A `tna_centralities` object, which is a tibble (`tbl_df`) with
#'   one row per state, the column `state`, and one `numeric` column per
#'   requested measure (in the order listed in 'Details'). For a `group_tna`
#'   object, a `group_tna_centralities` object: the same table for every
#'   group stacked, with the group name in the first column `group`.
#' @references
#' Banerjee, A., Chandrasekhar, A. G., Duflo, E., & Jackson, M. O. (2014).
#' Gossip: Identifying central individuals in a social network.
#' Working Paper.
#'
#' Kivimaki, I., Lebichot, B., Saramaki, J., & Saerens, M. (2016). Two
#' betweenness centrality measures based on randomized shortest paths.
#' *Scientific Reports*, 6, 19668. \doi{10.1038/srep19668}
#'
#' Zhang, B., & Horvath, S. (2005). A general framework for weighted gene
#' co-expression network analysis. *Statistical Applications in Genetics and
#' Molecular Biology*, 4(1), Article 17. \doi{10.2202/1544-6115.1128}
#' @examples
#' model <- tna(group_regulation)
#'
#' # Centrality measures excluding loops in the network (the default)
#' centralities(model)
#'
#' # Centrality measures including loops in the network
#' centralities(model, loops = TRUE)
#'
#' # Centrality measures normalized
#' centralities(model, normalize = TRUE)
#'
centralities <- function(x, loops = FALSE, normalize = FALSE,
                         invert = TRUE, measures) {
  UseMethod("centralities")
}

#' @export
#' @rdname centralities
centralities.tna <- function(x, loops = FALSE, normalize = FALSE,
                             invert = TRUE, measures) {
  check_missing(x)
  check_class(x, "tna")
  out <- centralities_(
    x$weights,
    loops = loops,
    normalize = normalize,
    invert = invert,
    measures = measures
  )
  if (!is.null(x$data)) {
    attr(out, "colors") <- attr(x$data, "colors")
  }
  out
}

#' @export
#' @rdname centralities
centralities.matrix <- function(x, loops = FALSE, normalize = FALSE,
                                invert = TRUE, measures) {
  check_missing(x)
  stopifnot_(
    is.matrix(x),
    "Argument {.arg x} must be a {.cls matrix}."
  )
  centralities_(x, loops, normalize, invert, measures)
}

#' Internal function to calculate various centrality measures
#'
#' @param x An adjacency `matrix` of a directed weighted graph.
#' @inheritParams centralities
#' @noRd
centralities_ <- function(x, loops, normalize, invert, measures) {
  check_flag(loops)
  check_flag(normalize)
  check_flag(invert)
  measures <- measures %m% names(centrality_funs)
  measures <- check_measures(measures)
  diag(x) <- ifelse_(loops, diag(x), 0)
  g <- as.igraph(x)
  w <- ifelse_(invert, 1.0 / igraph::E(g)$weight, igraph::E(g)$weight)
  measures_out <- lapply(
    measures,
    function(y) {
      centrality_funs[[y]](g = g, x = x, w = w)
    }
  )
  names(measures_out) <- measures
  out <- as.data.frame(measures_out)
  if (normalize) {
    out[, measures] <- as.data.frame(
      lapply(out[, measures, drop = FALSE], ranger)
    )
  } else if ("Diffusion" %in% measures) {
    # Raw diffusion grows with powers of the weights, so it is always scaled
    out$Diffusion <- ranger(out$Diffusion)
  }
  # Diffusion and BetweennessRSP return unnamed vectors: name the states
  # from the weight matrix, not from the first measure computed
  rn <- ifelse_(is.null(rownames(x)), rownames(out), rownames(x))
  rownames(out) <- rn
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
estimate_cs <- function(x, loops, normalize, invert, measures, iter, method,
                        drop_prop, threshold, certainty, progressbar = NULL, 
                        detailed = NULL, actor = NULL, edges = FALSE) {
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
#' With `edges = TRUE`, the stability of the edge weights is estimated in the
#' same way: in each subsample the network is estimated again and its edge
#' weights are correlated with the edge weights of the original network
#' (Epskamp et al., 2018). Only the edges present in the original network
#' are compared, self-loops included. An edge that is absent from the
#' original data cannot appear in a subsample, so including the absent edges
#' would add pairs of zeros that always agree and inflate the correlation.
#' The `loops`, `normalize`, and `invert` arguments do not affect the edge
#' weights.
#'
#' The results can be visualized by plotting the output object showing the
#' stability of the centrality measures across different drop proportions,
#' along with confidence intervals. The CS-coefficients are displayed in the
#' subtitle.
#'
#' @export
#' @family validation
#' @rdname estimate_centrality_stability
#' @param x A `tna` or a `group_tna` object representing the temporal network
#'   analysis data. The object should be created from a sequence data object.
#'   Grouped models with scaled edge weights, and their networks, are not
#'   resampled (an error of class `tna_error_scaling`).
#' @param loops A `logical` value indicating whether to include loops in the
#'   network when computing the centrality measures. The default is `FALSE`.
#' @param normalize A `logical` value indicating whether to normalize
#'   the centrality measures. The default is `FALSE`.
#' @param invert A `logical` value indicating whether the weights should be
#'   inverted for distance-based measures. The default is `TRUE`.
#' @param measures A `character` vector of centrality measures to estimate.
#'   The default measures are `"InStrength"`, `"OutStrength"`,
#'   and `"Betweenness"`. See [centralities()] for a list of available
#'   centrality measures.
#' @param iter An `integer` specifying the number of resamples to draw.
#'   The default is 1000.
#' @param method A `character` string indicating the correlation coefficient
#'   type. The default is `"pearson"`. See [stats::cor()] for details.
#' @param drop_prop A `numeric` vector specifying the proportions of
#'   cases to drop in each sampling iteration. Default is a sequence from 0.1
#'   to 0.9 in increments of 0.1.
#' @param threshold A `numeric` value specifying the correlation threshold for
#'   calculating the CS-coefficient. The default is 0.7.
#' @param certainty A `numeric` value specifying the desired level of certainty
#'   for the CS-coefficient. Default is 0.95.
#' @param progressbar A `logical` value. If `TRUE`, a progress bar is
#'   displayed. Defaults to `FALSE`.
#' @param detailed Deprecated. This argument is ignored and will be removed in 
#'   a future version.
#' @param actor An optional actor identifier for sequences nested in actors:
#'   a `character` string naming a non-sequence column of the data the model
#'   was built from, or a vector with one identifier per sequence (per
#'   sequence of the data the grouped model was built from, for a `group_tna`
#'   object). When given, `drop_prop` is the proportion of actors dropped,
#'   and each actor's sequences are dropped together. The default `NULL`
#'   drops single sequences.
#' @param edges A `logical` value. If `TRUE`, the stability of the edge
#'   weights is also estimated and returned as the element `Edges`. The
#'   default is `FALSE`.
#' @return A `tna_stability` object which is a `list` with an element for each
#' `measure` (and an element `Edges` when `edges = TRUE`) with the following
#' elements:
#'
#' * `cs_coefficient`: The centrality stability (CS) coefficient
#'   of the measure.
#' * `correlations`: A `matrix` of correlations between the original
#'   centrality and the resampled centralities, with one row per resample
#'   (`iter` rows) and one column per drop proportion.
#'
#' The drop proportions and the correlation threshold are stored in the
#' attributes `drop_prop` and `threshold`. Use [summary.tna_stability()] for
#' the CS-coefficients as a table.
#'
#' If `x` is a `group_tna` object, a `group_tna_stability` object is returned
#' instead, which is a `list` of `tna_stability` objects.
#'
#' @examples
#' # A small subset and few iterations
#' estimate_cs(
#'   tna(head(group_regulation, 200)),
#'   drop_prop = c(0.3, 0.6),
#'   measures = "InStrength",
#'   iter = 5
#' )
#' \donttest{
#' model <- tna(group_regulation)
#' # Small number of iterations and drop proportions for CRAN
#' estimate_cs(
#'   model,
#'   drop_prop = seq(0.3, 0.9, by = 0.2),
#'   measures = c("InStrength", "OutStrength"),
#'   iter = 10
#' )
#' # Stability of the edge weights alongside the centralities
#' estimate_cs(
#'   model,
#'   drop_prop = seq(0.3, 0.9, by = 0.2),
#'   measures = "InStrength",
#'   iter = 10,
#'   edges = TRUE
#' )
#' }
#' @references
#' Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating psychological
#' networks and their accuracy: A tutorial paper. *Behavior Research Methods*,
#' 50(1), 195--212. \doi{10.3758/s13428-017-0862-1}
#'
estimate_cs.tna <- function(x, loops = FALSE, normalize = FALSE, invert = TRUE,
                            measures = c(
                              "InStrength", "OutStrength", "Betweenness"
                            ), iter = 1000, method = "pearson",
                            drop_prop = seq(0.1, 0.9, by = 0.1),
                            threshold = 0.7, certainty = 0.95,
                            progressbar = FALSE, detailed = NULL,
                            actor = NULL, edges = FALSE) {
  check_tna_seq(x)
  check_not_scaled_group(x, "Case-dropping")
  check_actor(actor)
  check_flag(edges)
  check_flag(loops)
  check_flag(normalize)
  check_flag(invert)
  check_flag(progressbar)
  check_values(iter, strict = TRUE)
  check_range(threshold, lower = 0, upper = 1)
  check_range(certainty, lower = 0, upper = 1)
  check_measures(measures)
  if (!is.null(detailed)) {
    .Deprecated(msg = "'detailed' is deprecated and will be ignored.")
  }
  d <- x$data
  type <- attr(x, "type")
  scaling <- attr(x, "scaling")
  params <- attr(x, "params")
  model <- initialize_model(d, type, scaling, params, transitions = TRUE)
  trans <- model$trans
  a <- dim(trans)[2L]
  n <- nrow(d)
  n_seq <- seq_len(n)
  # Cases are sequences, or actors with all of their sequences
  n_cases <- n
  keep_cases <- function(m) sample(n_seq, m, replace = FALSE)
  if (!is.null(actor)) {
    units <- actor_units(x, actor)
    n_cases <- length(units)
    keep_cases <- function(m) {
      unlist(units[sample.int(n_cases, m)], use.names = FALSE)
    }
  }
  n_prop <- length(drop_prop)
  n_measures <- length(measures)
  if (n_measures > 0L) {
    centralities_orig <- centralities_(
      x = x$weights,
      loops = loops,
      normalize = normalize,
      invert = invert,
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
  }
  # An edge that is absent from the full data is absent from every subsample,
  # so only the edges of the network are compared (self-loops included)
  edge_mask <- !is.na(x$weights) & x$weights != 0
  edges_orig <- x$weights[edge_mask]
  if (edges && !isTRUE(stats::sd(edges_orig) > 0)) {
    warning_(
      "The edge weights of {.arg x} do not vary, so their stability
       is not estimated.",
      class = "tna_warning_edges"
    )
    edges <- FALSE
  }
  targets <- c(measures, onlyif(edges, "Edges"))
  n_targets <- length(targets)
  stability <- replicate(
    n_targets,
    matrix(NA, nrow = iter, ncol = n_prop),
    simplify = FALSE
  )
  names(stability) <- targets
  if (progressbar) {
    cli::cli_progress_bar(
      name = "Computing centrality stability",
      total = n_prop * iter
    )
  }
  for (i in seq_len(n_prop)) {
    prop <- drop_prop[i]
    n_drop <- floor(n_cases * prop)
    if (n_drop == 0) {
      warning_(
        paste0("No cases dropped for proportion ", prop, ". Skipping...")
      )
      next
    }
    corr_prop <- matrix(nrow = iter, ncol = n_targets)
    for (j in seq_len(iter)) {
      keep <- keep_cases(n_cases - n_drop)
      trans_sub <- trans[keep, , , drop = FALSE]
      weight_sub <- compute_weights(trans_sub, type, scaling, a, params)
      corr_measures <- numeric(0L)
      if (n_measures > 0L) {
        centralities_sub <- centralities_(
          x = weight_sub,
          loops = loops,
          invert = invert,
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
        corr_measures <- vapply(
          measures,
          function(measure) {
            centrality_sub <- centralities_sub[[measure]]
            centrality_orig <- centralities_orig[[measure]]
            # NaN when a normalized measure (Diffusion) is constant
            sd_sub <- sds_sub[[measure]]
            ifelse_(
              isTRUE(sd_sub > 0),
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
      }
      corr_edges <- onlyif(
        edges,
        stability_cor(weight_sub[edge_mask], edges_orig, method)
      )
      corr_prop[j, ] <- c(corr_measures, corr_edges)
      if (progressbar) {
        cli::cli_progress_update()
      }
    }
    for (k in seq_len(n_targets)) {
      stability[[targets[k]]][, i] <- corr_prop[, k]
    }
  }
  out <- list()
  for (measure in targets) {
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
  if (progressbar) {
    cli::cli_progress_done()
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

#' Correlate Resampled Values with the Original Values
#'
#' @param sub A `numeric` vector of values from a subsample.
#' @param orig A `numeric` vector of the original values.
#' @param method See [stats::cor()].
#' @return The correlation, or `NA` when `sub` does not vary.
#' @noRd
stability_cor <- function(sub, orig, method) {
  ifelse_(
    isTRUE(stats::sd(sub, na.rm = TRUE) > 0),
    stats::cor(sub, orig, method = method, use = "complete.obs"),
    NA_real_
  )
}

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
    # Undefined with a sink state: one NA per state
    return(rep(NA_real_, n))
  }
  P_ref <- diag(D^-1) %*% mat
  C <- mat^-1
  C[is.infinite(C)] <- 0
  W <- P_ref * exp(-beta * C)
  Z <- solve(diag(1, n, n) - W)
  Z_recip <- Z^-1
  # Entries that are mathematically zero can contain finite solver noise.
  # Taking their reciprocal amplifies that noise into phantom centrality.
  Z_recip[!is.finite(Z_recip) | abs(Z) < 1e-12] <- 0
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


# Groups ----------------------------------------------------------------

#' @export
#' @rdname centralities
centralities.group_tna <- function(x, loops = FALSE, normalize = FALSE,
                                   invert = TRUE, measures) {
  check_missing(x)
  check_class(x, "group_tna")
  # missing() does not work with lapply, need to evaluate measures here.
  measures <- measures %m% names(centrality_funs)
  out <- dplyr::bind_rows(
    lapply(
      x,
      function(i) {
        data.frame(
          centralities.tna(
            x = i,
            loops = loops,
            normalize = normalize,
            invert = invert,
            measures = measures
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
#' @rdname estimate_centrality_stability
estimate_cs.group_tna <- function(x, loops = FALSE, normalize = FALSE,
                                  invert = TRUE,
                                  measures = c(
                                    "InStrength", "OutStrength", "Betweenness"
                                  ), iter = 1000, method = "pearson",
                                  drop_prop = seq(0.1, 0.9, by = 0.1),
                                  threshold = 0.7, certainty = 0.95,
                                  progressbar = FALSE, detailed = NULL,
                                  actor = NULL, edges = FALSE) {
  check_missing(x)
  check_class(x, "group_tna")
  check_not_scaled_group(x, "Case-dropping")
  check_actor(actor)
  actors <- group_actor(x, actor)
  if (!is.null(detailed)) {
    .Deprecated(msg = "'detailed' is deprecated and will be ignored.")
  }
  structure(
    Map(
      function(i, actor) {
        estimate_centrality_stability.tna(
          i,
          loops = loops,
          normalize = normalize,
          invert = invert,
          measures = measures,
          iter = iter,
          method = method,
          drop_prop = drop_prop,
          threshold = threshold,
          certainty = certainty,
          progressbar = progressbar,
          actor = actor,
          edges = edges
        )
      },
      x,
      actors
    ),
    class = "group_tna_stability"
  )
}

#' @export
#' @rdname estimate_centrality_stability
estimate_centrality_stability.group_tna <- estimate_cs.group_tna

# Centrality measure function wrappers ----------------------------------------

centrality_funs <- list()

centrality_funs$OutStrength <- function(g, ...) {
  igraph::strength(g, mode = "out")
}

centrality_funs$InStrength <- function(g, ...) {
  igraph::strength(g, mode = "in")
}

centrality_funs$ClosenessIn <- function(g, w, ...) {
  igraph::closeness(g, mode = "in", weights = w)
}

centrality_funs$ClosenessOut <- function(g, w, ...) {
  igraph::closeness(g, mode = "out", weights = w)
}

centrality_funs$Closeness <- function(g, w, ...) {
  igraph::closeness(g, mode = "all", weights = w)
}

centrality_funs$Betweenness <- function(g, w, ...) {
  igraph::betweenness(g, weights = w)
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

centrality_funs$PageRank <- function(g, ...) {
  igraph::page_rank(
    g,
    directed = TRUE,
    weights = igraph::E(g)$weight
  )$vector
}

#' Calculate Centralities for a Transition Matrix
#'
#' This function calculates several centrality measures. See 'Details' for
#' information about the measures.
#'
#' The following measures are provided:
#'
#'   * `OutStrength`\cr Outgoing strength centrality, calculated using
#'     [igraph::strength()] with `mode = "out"`. It measures the total weight
#'     of the outgoing edges from each node.
#'   * `InStrength`\cr Incoming strength centrality, calculated using
#'     [igraph::strength()] with `mode = "in"`. It measures the total weight
#'     of the incoming edges to each node.
#'   * `ClosenessIn`\cr Closeness centrality (incoming), calculated using
#'     [igraph::closeness()] with `mode = "in"`. It measures how close a node
#'     is to all other nodes based on the incoming paths.
#'   * `ClosenessOut`\cr Closeness centrality (outgoing), calculated using
#'     [igraph::closeness()] with `mode = "out"`. It measures how close a node
#'     is to all other nodes based on the outgoing paths.
#'   * `Closeness`\cr Closeness centrality (overall), calculated using
#'     [igraph::closeness()] with `mode = "all"`. It measures how close a node
#'     is to all other nodes based on both incoming and outgoing paths.
#'   * `Betweenness`\cr Betweenness centrality based on randomized shortest
#'     paths (Kivimäki et al. 2016). It measures the extent to which a
#'     node lies on the shortest paths between other nodes.
#'   * `Diffusion`\cr Diffusion centrality of Banerjee et.al. (2014).
#'     It measures the influence of a node in spreading information through
#'     the network.
#'   * `Clustering`\cr Signed clustering coefficient of Zhang and Horvath (2005)
#'     based on the symmetric adjacency matrix (sum of the adjacency matrix
#'     and its transpose). It measures the degree to which nodes tend to
#'     cluster together.
#'
#' @export
#' @family core
#' @rdname centralities
#' @param x A square matrix representing transition probabilities or adjacency,
#'   or a `tna` object.
#' @param measures A `character` vector indicating which centrality
#'   measures should be computed. If `NULL`, all available measures are
#'   returned. See 'Details' for available measures. The elements are partially
#'   matched ignoring case.
#' @param loops A `logical` value indicating whether to include loops in the
#'   network when computing the centrality measures (default is `FALSE`).
#' @param normalize  A `logical` value indicating whether the centralities
#'   should be normalized (default is `FALSE`).
#' @param cluster Index of the cluster for which to compute the centralities or
#'   `NULL` if there are no clusters or if centralities should be computed for
#'   all clusters.
#' @param ... Ignored.
#' @return A `centralities` object which is a tibble (`tbl_df`)
#'   containing centrality measures for each state.
#' @references
#' Banerjee, A., A. Chandrasekhar, E. Duflo, and M. Jackson (2014).
#' Gossip: Identifying Central Individuals in a Social Network.
#' Working Paper.
#'
#' Kivimaki, I., Lebichot, B., Saramaki, J., & Saerens, M. (2016).
#' Two betweenness centrality measures based on Randomized Shortest Paths.
#' Scientific Reports, 6, 19668.
#'
#' Zhang, B., & Horvath, S. (2005).
#' A general framework for weighted gene co-expression network analysis.
#' Statistical Applications in Genetics and Molecular Biology, 4(1).

#' @examples
#' tna_model <- build_tna(engagement)
#'
#' # Centrality measures including loops in the network
#' centralities(tna_model)
#'
#' # Centrality measures excluding loops in the network
#' centralities(tna_model, loops = FALSE)
#'
#' # Centrality measures normalized
#' centralities(tna_model, normalize = TRUE)
#'
centralities <- function(x, loops = FALSE, cluster = NULL,
                         normalize = FALSE, measures = NULL, ...) {
  UseMethod("centralities")
}

#' @export
#' @rdname centralities
centralities.tna <- function(x, loops = FALSE, cluster = NULL,
                             normalize = FALSE, measures = NULL, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  if (length(x$weights) == 1L) {
    centralities_(
      x$weights[[1]],
      loops = loops,
      normalize = normalize,
      measures = measures
    )
  } else if (length(x$weights) > 1L && !is.null(cluster)) {
    centralities_(
      x$weights[[cluster]],
      loops = loops,
      normalize = normalize,
      measures = measures
    )
  } else if (length(x$weights) > 1L) {
    centrality_list <- list()
    clusternames <- names(x$weights)
    for (i in seq_along(x$weights)){
      centrality_list[[i]] <- centralities_(
        x$weights[[i]],
        loops = loops,
        normalize = normalize,
        measures = measures
      )
      centrality_list[[i]]$Cluster <- clusternames[i]
    }
    structure(
      dplyr::bind_rows(centrality_list) |>
        dplyr::mutate(
          Cluster = factor(!!rlang::sym("Cluster"), levels = clusternames)
        ),
      class = c("tna_centralities", "tbl_df", "tbl", "data.frame")
    )
  }
}

#' @export
#' @rdname centralities
centralities.matrix <- function(x, loops = FALSE, cluster = NULL,
                                normalize = FALSE, measures = NULL, ...) {
  stopifnot_(
    is.matrix(x),
    "Argument {.arg x} must be a {.cls matrix}."
  )
  centralities_(x, loops, normalize, measures)
}

#' Internal function to calculate various centrality measures
#'
#' @param x An adjacency matrix of a directed weighted graph
#' @noRd
centralities_ <- function(x, loops, normalize, measures) {
  stopifnot_(
    checkmate::test_flag(x = loops),
    "Argument {.arg loops} must be a single {.cls logical} value."
  )
  stopifnot_(
    checkmate::test_flag(x = normalize),
    "Argument {.arg normalize} must be a single {.cls logical} value."
  )
  measures <- ifelse_(
    is.null(measures),
    available_centrality_measures,
    measures
  )
  stopifnot_(
    checkmate::test_character(
      x = measures,
      any.missing = FALSE,
      unique = TRUE,
    ),
    "Argument {.arg measures} must be a {.cls character} vector."
  )
  lower_measures <- tolower(measures)
  lower_defaults <- tolower(available_centrality_measures)
  measures_match <- pmatch(lower_measures, lower_defaults)
  no_match <- is.na(measures_match)
  invalid_measures <- measures[no_match]
  valid_measures <- measures[!no_match]
  stopifnot_(
    length(invalid_measures) == 0L,
    c(
      "Argument {.arg measures} contains invalid centrality measures:",
      `x` = "Measure{?s} {.val {invalid_measures}} {?is/are} not recognized."
    )
  )
  diag(x) <- ifelse_(loops, diag(x), 0)
  g <- igraph::graph_from_adjacency_matrix(
    adjmatrix = x,
    mode = "directed",
    weighted = TRUE
  )
  measures_out <- lapply(
    valid_measures,
    function(y) {
      centrality_funs[[y]](g = g, x = x)
    }
  )
  names(measures_out) <- valid_measures
  out <- as.data.frame(measures_out)
  if (normalize) {
    out <- out |>
      dplyr::mutate(dplyr::across(dplyr::all_of(measures), ranger))
  }
  structure(
    tibble::rownames_to_column(out, "State") |>
      dplyr::mutate(
        State = factor(!!rlang::sym("State"), levels = rownames(out))
      ),
    class = c("centralities", "tbl_df", "tbl", "data.frame")
  )
}

# TODO Is this needed? not called ever
#' #' Convert an \R Object into a `centralities` object
#' #'
#' #' @export
#' #' @param x An \R object to be converted
#' #' @rdname as.tna_centralities
#' as.tna_centralities <- function(x) {
#'   UseMethod("as.centralities")
#' }
#'
#' #' @rdname as.centralities
#' #' @export
#' as.tna_centralities.data.frame <- function(x) {
#'   structure(df, class = c("tna_centralities", "tbl_df", "tbl", "data.frame"))
#' }

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

#' Compute the Randomized Shortest Path Betweenness Centrality Measure
#'
#' @param mat A weight `matrix`.
#' @noRd
rsp_bet <- function(mat, beta = 0.01) {
  n <- ncol(mat)
  W <- mat * exp(-beta * mat^-1)
  Z <- solve(diag(1, n, n) - W)
  Zrecip <- Z^-1
  Zrecip_diag <- diag(Zrecip) * diag(1, n, n)
  out <- diag(tcrossprod(Z, Zrecip - n * Zrecip_diag) %*% Z)
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

#' Estimate Centrality Stability
#'
#' This function estimates the stability of centrality measures in a network
#' using subset sampling without replacement. It allows for dropping varying
#' proportions of cases and calculates correlations between the original
#' centralities and sampled subsets.
#'
#' @export
#' @param x A `tna` object representing the temporal network analysis data.
#' The object should be created from a sequence datad object.
#' @param cluster An `integer` specifying the cluster of the `tna` object
#' to analyze. Default is 1.
#' @param measures A `character` vector of centrality measures to estimate.
#' The default measures are `"InStrength"`, `"OutStrength"`,
#' and `"Betweenness"`.
#' @param normalize A `logical` value indicating whether to normalize
#' the centrality measures. The default is `FALSE`.
#' @param iter An `integer` specifying the number of resamples to draw.
#' The default is 1000.
#' @param method A `character` string indicating the correlation coefficient
#' type. The default is `"pearson"`. See [stats:cor()] for details.
#' @param drop_prop A `numeric` vector specifying the proportions of
#' cases to drop in each sampling iteration. Default is a sequence from 0.1 to
#' 0.9 in increments of 0.1.
#' @param threshold A `numeric` value specifying the correlation threshold for
#' calculating the CS-coefficient. THe default is 0.7.
#' @param certainty A `numeric` value specifying the desired level of certainty
#' for the CS-coefficient. Default is 0.95.
#' @param detailed A `logical` value specifying whether to return detailed
#' sampling results. If `TRUE`, detailed results are included in the output.
#' THe default is `FALSE`.
#'
#' @details
#' The function works by repeatedly resampling the data, dropping varying
#' proportions of cases, and calculating centrality measures on the subsets.
#' The correlation between the original centralities and the resampled
#' centralities is calculated for each drop proportion. The stability of each
#' centrality measure is then summarized using a centrality stability (CS)
#' coefficient, which represents the proportion of dropped cases at which
#' the correlations drop below a given threshold (default 0.7).
#'
#' The results can be visualized with an optional plot showing the stability of
#' the centrality measures across different drop proportions, along with
#' confidence intervals. The CS-coefficients are displayed in the plot's
#' subtitle.
#'
#' @return A `stability` object which is a `list` containing the following
#' components:
#'
#' * `cs_coefficient`: The centrality stability (CS) coefficient
#'   for each measure.
#' * `correlations`: A `matrix` of correlations between the original
#'   centrality and the resampled centralities for each drop proportion.
#' * `detailed_results`: A detailed data frame of the sampled correlations,
#'   returned only if `return_detailed = TRUE`.
#'
#' @examples
#' \dontrun{
#' # Assuming 'x' is a tna object
#' results <- estimate_cs(x, measures = c("InStrength", "OutStrength"))
#' }
#'
estimate_cs <- function(x, cluster = 1,
                        measures = c(
                          "InStrength", "OutStrength", "Betweenness"
                        ),
                        normalize = FALSE, iter = 1000, method = "pearson",
                        drop_prop = seq(0.1, 0.9, by = 0.1), threshold = 0.7,
                        certainty = 0.95, detailed = FALSE) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  stopifnot_(
    !is.null(x$seq),
    "Argument {.arg x} must be a {.cls tna} object created from sequence data."
  )
  d <- x$seq[[cluster]]
  model <- build_markov_model(d, transitions = TRUE)
  trans <- model$trans
  a <- dim(trans)[2]
  n <- nrow(d)
  n_prop <- length(drop_prop)
  n_measures <- length(measures)
  type <- attr(x, "type")
  centralities_orig <- centralities(
    x,
    cluster = cluster,
    normalize = normalize,
    measures = measures
  )
  stability <- replicate(
    n_measures,
    matrix(NA, nrow = iter, ncol = n_prop),
    simplify = FALSE
  )
  names(stability) <- measures
  for (i in seq_len(n_prop)) {
    prop <- drop_prop[i]
    n_drop <- floor(n * prop)
    if (n_drop == 0) {
      warning_("No cases dropped for proportion ", prop, " skipping...")
      next
    }
    corr_prop <- matrix(nrow = iter, ncol = n_measures)
    for (j in seq_len(iter)) {
      keep <- sample(seq_len(n), n - n_drop, replace = FALSE)
      trans_sub <- trans[keep, , ]
      weight_sub <- compute_weights(trans_sub, type, a)
      corr_prop[j, ] <- vapply(
        measures,
        function(measure) {
          centrality_sub <- centralities(
            weight_sub,
            normalize = normalize,
            measures = measure
          )[[measure]]
          centrality_orig <- centralities_orig[[measure]]
          cor(
            centrality_sub,
            centrality_orig,
            method = method,
            use = "complete.obs"
          )
        },
        numeric(1L)
      )
    }
    for (k in seq_len(n_measures)) {
      measure <- measures[k]
      stability[[measure]][, i] <- corr_prop[, k]
    }
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
  out

    # # Check if results are empty
    # for (measure in measures) {
    #   if (all(is.na(stability_results[[measure]]))) {
    #     cat(sprintf("Warning: All results for measure '%s' are NA.\n", measure))
    #   }
    # }

    #cat("\n=== Centrality Stability Analysis ===\n\n")


      #cat(sprintf("%s Centrality: CS-Coefficient = %.2f\n", measure, cs_coef))


    #if (return_detailed) {
    #  detailed_df <- lapply(stability_results, function(res) {
    #    data.frame(
    #      drop_proportion = rep(drop_proportions, each = n_bootstraps),
    #      correlation = as.vector(res)
    #    )
    #  })
    #  final_results$detailed_results <- detailed_df
    #}

    # if (plot) {
    #   plot_stability_results(invisible(final_results))
    # }

    # return(invisible(final_results))

  # }, error = function(e) {
  #   cat("\nError occurred:", conditionMessage(e), "\n")
  #   stop(e)
  # }, finally = {
  #   if (!is.null(cl)) {
  #     parallel::stopCluster(cl)
  #   }
  # })
}

#' @rdname estimate_cs
#' @export
estimate_centrality_stability <- estimate_cs


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

# Available centrality measures -----------------------------------------------
available_centrality_measures <- c(
  "OutStrength",
  "InStrength",
  "ClosenessIn",
  "ClosenessOut",
  "Closeness",
  "Betweenness",
  "Diffusion",
  "Clustering"
)


# Centrality function wrappers --------------------------------------------

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

centrality_funs$Betweenness <- function(x, ...) {
  rsp_bet(x)
}

centrality_funs$Diffusion <- function(x, ...) {
  diffusion(x)
}

centrality_funs$Clustering <- function(x, ...) {
  wcc(x + t(x))
}

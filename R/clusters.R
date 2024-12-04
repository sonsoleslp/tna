#' Build a grouped Transition Network Analysis Model
#'
#' This function constructs a transition network analysis (TNA) model for each cluster
#' from a given sequence, wide-formatted dataframe, or mixture Markov model.
#'
#' @export
#' @family clusters
#' @rdname group_model
#' @param x An `stslist` object describing a sequence of events or states to
#'   be used for building the Markov model. The argument `x` also accepts
#'   a `data.frame` object in wide format.
#'   (each column is a timepoint with no extra columns). Alternatively, the
#'   function accepts a mixture Markov model from the library `seqHMM`.
#' @param type A `character` string describing the weight matrix type.
#'   Currently supports `"relative"` for relative frequencies
#'   (probabilities, the default), `"scaled"` for frequencies scaled to the
#'   unit interval, `"ranked"` for ranks of the weights scaled to the unit
#'   interval, and `"absolute"` for frequencies.
#' @param ... Ignored.
#' @return An object of class `group_model` which is a `list` containing one
#'   element per cluster. Each element is also a list containing the
#'   following elements:
#'   * `weights`: An adjacency `matrix` of the model (weight matrix).
#'   * `inits`: A `numeric` vector of initial values for each state.
#'     For `matrix` type `x`, this element will be `NULL` if `inits` is not
#'     directly provided
#'   * `labels`: A `character` vector of the state labels, or `NULL` if
#'     there are no labels.
#'   * `data`: The original sequence data that has been converted to an
#'     internal format used by the package when `x` is a `stslist` or a
#'     `data.frame` object. Otherwise `NULL`.
#'
#' @examples
#' group = c(rep("High",100),rep("Low",100))
#' model <- group_model(engagement, group = group)
#' print(model)
#'
group_model <- function(x, ...) {
  UseMethod("group_model")
}

#' Build a grouped Transition Network Analysis Model providing group/cluster assignments
#'
#' @param x An `stslist` object describing a sequence of events or states to
#'   be used for building the Markov model. The argument `x` also accepts
#'   a `data.frame` object in wide format.
#'   (each column is a timepoint with no extra columns).
#' @param group A vector indicating the cluster assignment of each
#'  row of the data / sequence. Must have the same length as the number of
#'  rows/sequences of `x`.
#' @return An object of class `group_model` which is a `list` containing one
#'   element per cluster. Each element is also a list containing the
#'   following elements:
#'   * `weights`: An adjacency `matrix` of the model (weight matrix).
#'   * `inits`: A `numeric` vector of initial values for each state.
#'     For `matrix` type `x`, this element will be `NULL` if `inits` is not
#'     directly provided
#'   * `labels`: A `character` vector of the state labels, or `NULL` if
#'     there are no labels.
#'   * `data`: The original sequence data that has been converted to an
#'     internal format used by the package when `x` is a `stslist` or a
#'     `data.frame` object. Otherwise `NULL`.
#' @export
#' @family clusters
#' @rdname group_model
group_model.default <- function(x, group, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )

  stopifnot_(
    !missing(group),
    "Argument {.arg group} is missing."
  )

  stopifnot_(
    !(typeof(group) %in% c("stslist","data.frame")),
    "Argument {.arg x} has to be of type `stslist` (sequence object) or `data.frame`."
  )

  stopifnot_(
    length(group) == nrow(x),
    "Argument {.arg group} must be the same length as number of rows/sequences in {.arg x}"
  )

  if (typeof(group) != "factor") {
    group <- factor(group)
  }

  levs <- levels(group)
  clusters <- list()
  for (i in levs) {
    clusters[[i]] <- build_model(x[group == i, ], ...)
  }
  structure(clusters, class = "group_model")
}

#' Build a grouped Transition Network Analysis Model providing an MMM model from `seqHMM`
#'
#' @param x An `mhmm` object from `seqHMM`
#' @param ... Same as `tna`
#' @return A `group_model` object
#' @return An object of class `group_model` which is a `list` containing one
#'   element per cluster. Each element is also a list containing the
#'   following elements:
#'   * `weights`: An adjacency `matrix` of the model (weight matrix).
#'   * `inits`: A `numeric` vector of initial values for each state.
#'     For `matrix` type `x`, this element will be `NULL` if `inits` is not
#'     directly provided
#'   * `labels`: A `character` vector of the state labels, or `NULL` if
#'     there are no labels.
#'   * `data`: The original sequence data that has been converted to an
#'     internal format used by the package when `x` is a `stslist` or a
#'     `data.frame` object. Otherwise `NULL`.
#'
#' @export
#' @family clusters
#' @rdname group_model
group_model.mhmm <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )

  group <- (summary(x))$most_probable_cluster
  group_model.default(x$observations, group = group, ...)
}


#' Check that argument is an object of class `group_model`
#'
#' @param x An \R object.
#' @family clusters
#' @noRd
is_group_model <- function(x) {
  inherits(x, "group_model")
}

#' Check that argument is an object of class `group_model_centralities`
#'
#' @param x An \R object.
#' @family clusters
#' @noRd
is_group_model_centralities <- function(x) {
  inherits(x, "group_model_centralities")
}

#' Check that argument is an object of class `group_model_bootstrap`
#'
#' @param x An \R object.
#' @family clusters
#' @noRd
is_group_model_bootstrap <- function(x) {
  inherits(x, "group_model_bootstrap")
}

#' Check that argument is an object of class `group_model_communities`
#'
#' @param x An \R object.
#' @family clusters
#' @noRd
is_group_model_communities <- function(x) {
  inherits(x, "group_model_communities")
}


#' Check that argument is an object of class `group_model_cliques`
#'
#' @param x An \R object.
#' @family clusters
#' @noRd
is_group_model_cliques <- function(x) {
  inherits(x, "group_model_cliques")
}

#' Check that argument is an object of class `group_model_stability`
#'
#' @param x An \R object.
#' @family clusters
#' @noRd
is_group_model_stability <- function(x) {
  inherits(x, "group_model_stability")
}

#' Plot a grouped Transition Network Analysis Model
#'
#' @param x A `group_model` object.
#' @param title A title for each plot. It can be a single string (the same one
#'  will be used for all plots) or a list (one per group)
#' @param ... Same as [plot.tna()]
#' @rdname plot
#' @return NULL
#' @family clusters
#' @export
plot.group_model <- function(x, title, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )

  if (missing(title)) {
    title = names(x)
  } else if (length(title) == 1) {
    title = rep(title, length(x))
  }

  for (i in 1:length(x)){
    plot(x[[i]], title = title[i], ...)
  }
}


#' Coerce  a specific group from a `group_model` object to an `igraph` object.
#'
#' @export
#' @inheritParams igraph::as.igraph
#' @param which The number or name of group.
#' @return An `igraph` object.
as.igraph.group_model <- function(x, which){
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )
  stopifnot_(
    !is.null(x[[which]]),
    "There is no group named {.arg which}"
  )
  stopifnot_(
    !missing(which),
    "Argument {.arg which} is missing"
  )
  return (as.igraph.tna(x[[which]]))
}

#' Calculate Summary of Network Metrics for a grouped Transition Network
#'
#' This function calculates a variety of network metrics for a `tna` object.
#' It computes key metrics such as node and edge counts, network density,
#' mean distance, strength measures, degree centrality, and reciprocity.
#'
#' @export
#' @param x A `group_model` object.
#' @param combined A logical indicating whether the summary results should be
#' combined into a single dataframe for all clusters (defaults to `TRUE`)
#' @param ... Ignored
#' @rdname summary
#' @family clusters
#' @details
#' The function extracts the `igraph` network for each cluster and
#' computes the following network metrics:
#'
#'   * Node count: Total number of nodes in the network.
#'   * Edge count: Total number of edges in the network.
#'   * Network density: Proportion of possible edges that
#'     are present in the network.
#'   * Mean distance: The average shortest path length between nodes.
#'   * Mean and standard deviation of out-strength and in-strength: Measures
#'     of the total weight of outgoing and incoming edges for each node.
#'   * Mean and standard deviation of out-degree: The number of outgoing
#'     edges from each node.
#'   * Centralization of out-degree and in-degree: Measures of how
#'     centralized the network is based on the degrees of nodes.
#'   * Reciprocity: The proportion of edges that are reciprocated
#'     (i.e., mutual edges between nodes).
#'
#' A summary of the metrics is printed to the console.
#'
#' @return A `list` of `list`s or combined `data.frame` or containing the following network metrics (invisibly):
#'
#'   * `node_count`: The total number of nodes.
#'   * `edge_count`: The total number of edges.
#'   * `network_Density`: The density of the network.
#'   * `mean_distance`: The mean shortest path length.
#'   * `mean_out_strength`: The mean out-strength of nodes.
#'   * `sd_out_strength`: The standard deviation of out-strength.
#'   * `mean_in_strength`: The mean in-strength of nodes.
#'   * `sd_in_strength`: The standard deviation of in-strength.
#'   * `mean_out_degree`: The mean out-degree of nodes.
#'   * `sd_out_degree`: The standard deviation of out-degree.
#'   * `centralization_out_degree`: The centralization of out-degree.
#'   * `centralization_in_degree`: The centralization of in-degree.
#'   * `reciprocity`: The reciprocity of the network.
#'
#' @examples
#' group <- c(rep("High",100),rep("Low",100))
#' model <- group_model(engagement, group = group)
#' summary(model)
#'
summary.group_model <- function(x, combined = TRUE) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )
  if(combined == FALSE) {
    lapply(x, \(i) summary.tna(i))
  } else {
    dplyr::bind_rows(lapply(x, \(i) summary.tna(i)), .id = "Cluster") |>
      tidyr::pivot_wider(names_from = "Cluster", values_from = "value")
  }
}


#' Prune a `group_model` network based on transition probabilities
#'
#' Prunes a set of networks represented by a `group_model` object by removing
#' edges based on a specified threshold, lowest percent of non-zero edge
#' weights, or the disparity filter algorithm (Serrano et al., 2009).
#' It ensures the networks remain weakly connected.
#'
#' @export
#' @family clusters
#' @param x An object of class `group_model`
#' @param method A `character` string describing the pruning method.
#' The available options are `"threshold"`, `"lowest"`, `"bootstrap"` and
#' `"disparity"`, corresponding to the methods listed in Details. The default
#' is `"threshold"`.
#' @param threshold A numeric value specifying the edge weight threshold.
#' Edges with weights below or equal to this threshold will be considered for
#' removal.
#' @param lowest A `numeric` value specifying the lowest percentage
#' of non-zero edges. This percentage of edges with the lowest weights will be
#' considered for removal. The default is `0.05`.
#' @param level A `numeric` value representing the significance level for the
#' disparity filter. Defaults to `0.5`.
#' @param boot A `group_model_bootstrap` object to be used for pruning with method
#' `"boot"`. The method argument is ignored if this argument is supplied.
#' @param ... Arguments passed to [bootstrap()] when
#' using `method = "bootstrap"` and when a `group_model_bootstrap` is not supplied.
#' @return A pruned `group_model` object. Details on the pruning can be viewed with
#' [pruning_details()]. The original model can be restored with [deprune()].
#' @references
#' Serrano, M. A., Boguna, M., & Vespignani, A. (2009). Extracting the
#' multiscale backbone of complex weighted networks.
#' *Proceedings of the National Academy of Sciences, 106*,
#' 6483-6488. \doi{10.1073/pnas.0808904106}
#'
#' @examples
#' group <- c(rep("High",100),rep("Low",100))
#' model <- group_model(engagement, group = group)
#' pruned_threshold <- prune(model, method = "threshold", threshold = 0.1)
#' pruned_percentile <- prune(model,method = "lowest", lowest = 0.05)
#' pruned_disparity <- prune(model, method = "disparity", level = 0.5)
#'
prune.group_model <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )

  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )

  structure(
    lapply(x, \(i) prune.tna(i, ...)),
    class = "group_model"
  )
}


#' Print Detailed Information on the Pruning Results
#'
#' @rdname pruning_details
#' @export
#' @param x A pruned `group_model` object.
#' @param removed_edges Should a `data.frame` of removed edges be printed?
#' The default is `FALSE`.
#' @param ... Ignored.
pruning_details.group_model <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )

  Map(function(y, i) {print(i); pruning_details.tna(y, ...)}, x, names(x))

}

#' @family clusters
#' @rdname deprune
#' @export
deprune.group_model <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )
  structure(
    lapply(x, \(i) deprune.tna(i, ...)),
    class = "group_model"
  )
}

#' @family clusters
#' @rdname reprune
#' @export
reprune.group_model <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )
  structure(
    lapply(x, \(i) reprune.tna(i, ...)),
    class = "group_model"
  )
}

#' Print `group_model` Bootstrap Results
#'
#' @param x A `group_model_bootstrap` object.
#' @param digits An `integer` giving the minimal number of
#' *significant* digits to print.
#' @param type A `character` vector giving the type of edges to print.
#' The default option `"both"` prints both statistically significant and
#' non-significant edges, `"sig"` prints only significant edges, and `"nonsig"`
#' prints only the non-significant edges.
#' @param ... Ignored.
#'
#' @family clusters
#' @export
print.group_model_bootstrap <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model_bootstrap(x),
    "Argument {.arg x} must be of type `group_model_bootstrap`"
  )
  lapply(x, \(i) print.tna_bootstrap(i, ...))
}

#' Print the summary of a grouped Transition Network Analysis Model
#'
#' @param x A `summary.group_model` object.
#' @param ... Ignored
#' @family clusters
#' @export
print.summary.group_model  <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )
  if (is.list(x)) {
    lapply(x, \(i) print.summary.tna(i, ...))
  } else {
    print.summary.tna(x, ...)
  }
}

#' Print `group_model` Bootstrap Summary
#'
#' @param x A `summary.group_model_bootstrap` object.
#' @param ... Ignored.
#' @family clusters
#' @export
print.summary.group_model_bootstrap  <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model_bootstrap(x),
    "Argument {.arg x} must be of type `group_model_bootstrap`"
  )
  lapply(x, \(i) print.summary.tna_bootstrap(i, ...))
}

#' Print `group_model` Centrality Measures
#'
#' @param x A `group_centralities` object.
#' @param ... Ignored.
#' @family clusters
#' @export
print.group_model_centralities  <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model_centralities(x),
    "Argument {.arg x} must be of type `group_model_centralities`"
  )

  NextMethod(generic = "print", object = x, ...)
}


#' Print `group_model` Detected Communities
#'
#' @export
#' @param x A `group_model_communities` object.
#' @family clusters
#' @param ... Ignored.
print.group_model_communities  <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model_communities(x),
    "Argument {.arg x} must be of type `group_model_communities`"
  )
  Map(function(y, i) {print(i); print.tna_communities(y, ...)}, x, names(x))
}


#' Print `group_model` Found Cliques
#'
#' @export
#' @param x A `group_model_cliques` object.
#' @param n An `integer` defining the maximum number of cliques to show.
#' The defaults is `6`.
#' @param first An `integer` giving the index of the first clique to show.
#' The default index is `1`.
#' @param digits An `integer` giving the minimal number of
#' *significant* digits to print.
#' @param ... Ignored.
print.group_model_cliques  <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model_cliques(x),
    "Argument {.arg x} must be of type `group_model_cliques`"
  )
  Map(function(y, i) {print(i); print.tna_cliques(y, ...)}, x, names(x))

}

#' @family clusters
#' @export
hist.group_model <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )
  lapply(x, \(i) hist.tna(i, ...))
}

#' @family clusters
#' @export
plot.group_model_centralities <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model_centralities(x),
    "Argument {.arg x} must be of type `group_model_centralities`"
  )
  plot_centralities_multiple(x, ...)
}

#' @family clusters
#' @export
plot.group_model_cliques <- function(x, title, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model_cliques(x),
    "Argument {.arg x} must be of type `group_model_cliques`"
  )

  if (missing(title)) {
    title = names(x)
  } else if (length(title) == 1) {
    title = rep(title, length(x))
  }

  Map(function(y, i) plot.tna_cliques(y, title = i, ...), x, title)

}

#' @family clusters
#' @export
plot.group_model_stability <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model_stability(x),
    "Argument {.arg x} must be of type `group_model_stability`"
  )
  lapply(x, \(i) plot.tna_stability(i, ...))
}

#' @family clusters
#' @export
plot.group_model_communities <- function(x, title = names(x), colors = lapply(x, \(x) NULL), ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model_communities(x),
    "Argument {.arg x} must be of type `group_model_communities`"
  )
  if (is.null(colors) | (is.vector(colors) & is.atomic(colors))){
      colors = lapply(x, \(x) colors)
  }

  if (is.null(title) |
       (is.vector(title) & is.atomic(title) & (length(title) == 1))){
      title = lapply(x, \(x) title)
  }
  Map(function(y, i, j) plot.tna_communities(y, title = i, colors = j, ...), x, title, colors)

}

#' @family clusters
#' @rdname communities
#' @export
communities.group_model <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )


  structure(
    lapply(x, \(i) communities.tna(i, ...)),
    class = "group_model_communities"
  )
}

#' @family clusters
#' @export
cliques.group_model <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )
  structure(
    lapply(x, \(i) cliques.tna(i, ...)),
    class = "group_model_cliques"
  )
}

#' @family clusters
#' @rdname centralities
#' @export
centralities.group_model <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )

  grc <- dplyr::bind_rows(lapply(x, \(i) data.frame(centralities.tna(i, ...))), .id = "Group")

  structure(
    grc,
    class = c("group_model_centralities", "tbl_df", "tbl", "data.frame")
  )
}

#' @family clusters
#' @rdname estimate_cs
#' @export
estimate_centrality_stability.group_model <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )
  structure(
    lapply(x, \(i) estimate_centrality_stability.tna(i, ...)),
    class = "group_model_stability"
  )

}


#' @family clusters
#' @export
bootstrap.group_model <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} must be of type `group_model`"
  )
  structure(
    lapply(x, \(i) bootstrap.tna(i, ...)),
    class = "group_model_bootstrap"
  )
}

#' Retrieve statistics from a mixture Markov model (MMM)
#'
#' @param x An `mhmm` object
#' @return A `data.frame` object
#' @family clusters
#' @export
mmm_stats <- function(x, use_t_dist = TRUE, conf_level = 0.95) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    inherits(x, "mhmm"),
    "Argument {.arg x} must be of type `mhmm`. See `seqHMM`"
  )

  model_summary <- summary(x)
  # Extract necessary information
  coef <- model_summary$coefficients
  vcov <- model_summary$vcov

  # Initialize lists to store results
  coef_flat <- c()
  se_flat <- c()
  cluster_list <- c()
  variable_list <- c()

  # Exclude the reference cluster (assumed to be the first cluster)
  coef <- as.matrix(coef)[, -1, drop = FALSE]

  # Extract the diagonal of the vcov matrix
  vcov_diag <- sqrt(diag(vcov))

  # Flatten the coefficients and map them to the corresponding standard errors
  num_vars <- nrow(coef)
  num_clusters <- ncol(coef)

  for (cluster in seq_len(num_clusters)) {
    for (var in seq_len(num_vars)) {
      coef_flat <- c(coef_flat, coef[var, cluster])
      se_flat <- c(se_flat, vcov_diag[(cluster - 1) * num_vars + var])
      cluster_list <- c(cluster_list, colnames(coef)[cluster])
      variable_list <- c(variable_list, rownames(coef)[var])
    }
  }

  # Ensure the lengths match
  stopifnot_(length(coef_flat) == length(se_flat),
             "The lengths of the coefficients and standard errors do not match.")

  # Calculate z-value or t-value
  statistic <- coef_flat / se_flat

  # Determine degrees of freedom if using t-distribution
  if (use_t_dist && !is.null(model_summary$df.residual)) {
    df <- model_summary$df.residual
    # Calculate p-values using t-distribution
    p_value <- 2 * (1 - pt(abs(statistic), df))
    # Calculate confidence intervals
    ci_margin <- qt((1 + conf_level) / 2, df) * se_flat
  } else {
    # Calculate p-values using normal distribution
    p_value <- 2 * (1 - pnorm(abs(statistic)))
    # Calculate confidence intervals
    ci_margin <- qnorm((1 + conf_level) / 2) * se_flat
  }

  ci_lower <- coef_flat - ci_margin
  ci_upper <- coef_flat + ci_margin

  # Create a data frame with all results in the desired order
  results <- data.frame(
    Cluster = cluster_list,
    Variable = variable_list,
    Estimate = coef_flat,
    p_value = p_value,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper,
    Std_Error = se_flat,
    t_value = statistic # or z_value depending on distribution
  )
  rownames(results) <- NULL
  return(results)
}

#' Rename clusters
#'
#' @param x A `group_model` object
#' @param new_names A vector containing one name per cluster
#' @return A renamed `group_model` object
#' @family clusters
#' @export
rename_groups <- function(x, new_names) {
  stopifnot_(
    is_group_model(x),
    "Argument {.arg x} is not a `group_model` object"
  )
  stopifnot_(
    !missing(new_names),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is.vector(new_names),
    "Argument {.arg new_names} must be a vector"
  )
  stopifnot_(
    length(new_names) == length(x),
    "Argument {.arg new_names} must be the same length as {.arg x}"
  )
  names(x) <- new_names
}

#' @export
#' @rdname build_model
#' @examples
#' model <- group_tna(engagement)
#'
group_tna <- function(x, scaling = character(0L), ...) {
  check_missing(x)
  group_model(x = x, type = "relative", scaling = scaling, ...)
}

#' @export
#' @rdname build_model
#' @examples
#' model <- group_ftna(engagement)
#'
group_ftna <- function(x, scaling = character(0L), ...) {
  group_model(x = x, type = "absolute", scaling = scaling, ...)
}

#' @export
#' @rdname build_model
#' @examples
#' model <- group_ctna(engagement)
#'
group_ctna <- function(x, scaling = character(0L), ...) {
  group_model(x = x, type = "co-occurrence", scaling = scaling, ...)
}

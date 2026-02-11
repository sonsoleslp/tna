#' Plot a Histogram of Edge Weights in the Network
#'
#' @export
#' @family basic
#' @inheritParams graphics::hist
#' @param main A `character` string defining the title of the plot.
#' @param xlab A `character` string defining the vertical axis label.
#' @param ... Additional arguments passed to [graphics::hist()].
#' @return A `histogram` object of edge weights.
#' @examples
#' model <- tna(group_regulation)
#' hist(model)
#'
hist.tna <- function(x, breaks, col = "lightblue",
                     main, xlab, border = "white", ...) {
  check_missing(x)
  check_class(x, "tna")
  w <- c(x$weights)
  type <- attr(x, "type")
  xlab_missing <- missing(xlab)
  if (xlab_missing) {
    xlab <- paste0(
      "Edge Weights (",
      switch(type,
             `relative` = "Probabilities",
             `frequency` = "Frequencies",
             `co-occurrence` = "Co-occurrences"
      ),
      ")"
    )
  }
  if (missing(main)) {
    main <- ifelse_(
      xlab_missing,
      paste0("Histogram of ", xlab),
      "Histogram of Edge Weights"
    )
  }
  if (missing(breaks)) {
    breaks <- ifelse_(
      type == "relative",
      seq(0, 1, length.out = 20),
      seq(0, max(w), length.out = 20)
    )
  }
  graphics::hist(
    x = w,
    breaks = breaks,
    col = col,
    main = main,
    xlab = xlab,
    border = border,
    ...
  )
}

#' Plot a Transition Network Analysis Model
#'
#' This function plots a transition network analysis (TNA) model using
#' the `qgraph` package. The nodes in the graph represent states, with node
#' sizes corresponding to initial state probabilities. Edge labels represent
#' the edge weights of the network.
#'
#' @export
#' @family basic
#' @param x A `tna` object from [tna()].
#' @param node_list An optional `list` of two `character` vectors that define
#'   two mutually exclusive groups of node labels.
#' @param use_list_order A `logical` value. If `node_list` is provided,
#'   defines how the order of the nodes in the plot is defined. A `TRUE` value
#'   uses the order in `node_list`. Otherwise, the nodes are ranked based on
#'   edge weights and ordered according to the rank.
#' @param x_offset An optional `numeric` vector with the same number of
#'   elements as there are states. Defines a horizontal offset for each node
#'   in the plot when `node_list` is provided.
#' @param labels See [qgraph::qgraph()].
#' @param colors See [qgraph::qgraph()].
#' @param pie See [qgraph::qgraph()].
#' @param cut Edge color and width emphasis cutoff value. The default is
#'   the median of the edge weights. See [qgraph::qgraph()] for details.
#' @param show_pruned A `logical` value indicating if pruned edges removed by
#'   [prune()] should be shown in the plot.  The default is `TRUE`, and the
#'   edges are drawn as dashed with a different color to distinguish them.
#' @param pruned_edge_color A `character` string for the color to use for
#'   pruned edges when `show_pruned = TRUE`. The default is `"pink"`.
#' @param edge.color See [qgraph::qgraph()].
#' @param edge.labels See [qgraph::qgraph()].
#' @param edge.label.position See [qgraph::qgraph()].
#' @param layout One of the following:
#'   * A `character` string describing a `qgraph` layout (e.g., `"circle"`)
#'     or the name of a `igraph` layout function (e.g., `"layout_on_grid"`).
#'   * A `matrix` of node positions to use, with a row for each node and
#'     `x` and `y` columns for the node positions.
#'   * A layout function from `igraph`.
#' @param layout_args A `list` of arguments to pass to the `igraph` layout
#'   function when `layout` is a function or a character string that specifies
#'   a function name.
#' @param scale_nodes A `character` string giving the name of a centrality
#'   measure to scale the node size by. See [centralities()] for valid names.
#'   If missing (the default), uses default [qgraph::qgraph()] scaling.
#'   The value of `vsize` provided via `...` is used as baseline size.
#' @param scaling_factor A `numeric` value specifying how strongly to scale
#'   the nodes when `scale_nodes` is provided. Values
#'   between 0 and 1 will result in smaller differences and values larger
#'   than 1 will result in greater differences. The default is `0.5`.
#' @param mar See [qgraph::qgraph()].
#' @param theme See [qgraph::qgraph()].
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` plot of the transition network.
#' @examples
#' model <- tna(group_regulation)
#' plot(model)
#'
plot.tna <- function(x, node_list, use_list_order = TRUE, x_offset,
                     labels, colors, pie, cut,
                     show_pruned = TRUE, pruned_edge_color = "pink",
                     edge.color = NA, edge.labels = TRUE,
                     edge.label.position = 0.65, layout = "circle",
                     layout_args = list(), scale_nodes, scaling_factor = 0.5,
                     mar = rep(5, 4), theme = "colorblind", ...) {
  check_missing(x)
  check_class(x, "tna")
  if (missing(node_list)) {
    out <- plot_tna_(
      x, labels, colors, pie, cut, show_pruned, pruned_edge_color,
      edge.color, edge.labels, edge.label.position, layout,
      layout_args, scale_nodes, scaling_factor, mar, theme, ...
    )
  } else {
    out <- plot_htna_(
      x = x,
      node_list = node_list,
      use_list_order = use_list_order,
      x_offset = x_offset,
      layout = NULL,
      colors = NULL,
      shape = NULL,
      labels = labels,
      pie = pie,
      cut = cut,
      show_pruned = show_pruned,
      pruned_edge_color = pruned_edge_color,
      edge.color = edge.color,
      edge.labels = edge.labels,
      edge.label.position = edge.label.position,
      layout_args = list(),
      scale_nodes = scale_nodes,
      scaling_factor = scaling_factor,
      mar = mar,
      theme = theme,
      ...
    )
  }
  invisible(out)
}

plot_htna_ <- function(x, node_list, use_list_order = TRUE, x_offset,
                       layout, colors, shape, ...) {
  stopifnot_(
    length(node_list) == 2L,
    "Argument {.arg node_list} must be a {.cls list} of length 2."
  )
  stopifnot_(
    is.character(node_list[[1L]]) && is.character(node_list[[2L]]),
    "Elements of {.arg node_list} must be {.cls character} vectors."
  )
  check_flag(use_list_order)
  lab <- x$labels
  lhs <- node_list[[1L]]
  rhs <- node_list[[2L]]
  common <- intersect(rhs, lhs)
  stopifnot_(
    length(common) == 0,
    "The groups defined by {.arg node_list} must not contain common states."
  )
  lhs_idx <- match(lhs, lab)
  rhs_idx <- match(rhs, lab)
  missing <- lhs[is.na(lhs_idx)]
  stopifnot_(
    length(missing) == 0,
    "Nodes {.val {missing}} are not present in the model."
  )
  missing <- rhs[is.na(rhs_idx)]
  stopifnot_(
    length(missing) == 0,
    "Nodes {.val {missing}} are not present in the model."
  )
  n_lhs <- length(lhs_idx)
  n_rhs <- length(rhs_idx)
  n <- length(lab)
  missing <- lab[!lab %in% union(lhs, rhs)]
  stopifnot_(
    n_lhs + n_rhs == n,
    c(
      "Every state must belong to one of the
      groups defined by {.arg node_list}.",
      `x` = "No group has been specified for node{?s} {.val {missing}}."
    )
  )
  colors <- rep("lightgray", n)
  shape <- rep("circle", n)
  colors[lhs_idx] <- "#ffd89d"
  colors[rhs_idx] <- "#a68ba5"
  shape[lhs_idx] <- "circle"
  shape[rhs_idx] <- "square"
  x_pos <- rep(0, n)
  x_pos[lhs_idx] <- -0.5
  x_pos[rhs_idx] <- 0.5
  if (!missing(x_offset)) {
    stopifnot_(
      length(x_offset) == n,
      "Argument {.arg x_offset} must be of length {n} (the number of nodes)."
    )
    x_pos[lhs_idx] <- x_offset[1:n_lhs]
    x_pos[rhs_idx] <- x_offset[(n_lhs + 1):n]
  }
  y_pos <- rep(0, n)
  y_pos[lhs_idx] <- seq(1, -1, length.out = n_lhs)
  y_pos[rhs_idx] <- seq(1, -1, length.out = n_rhs)
  if (!use_list_order) {
    w <- x$weights
    edges <- w[lhs_idx, rhs_idx, drop = FALSE]
    out_str <- rowSums(edges)
    in_str <- colSums(edges)
    rank_in <- rank(-in_str)
    rank_out <- rank(-out_str)
    pos_lhs <- rowSums(edges * rank_in[col(edges)]) / out_str
    pos_rhs <- colSums(edges * rank_out) / in_str
    y_pos[lhs_idx] <- y_pos[lhs_idx][rank(pos_lhs, ties.method = "first")]
    y_pos[rhs_idx] <- y_pos[rhs_idx][rank(pos_rhs, ties.method = "first")]
  }
  layout_mat <- cbind(x = x_pos, y = y_pos)
  plot_tna_(
    x = x,
    layout = layout_mat,
    colors = colors,
    shape = shape,
    ...
  )
}

plot_tna_ <- function(x, labels, colors, pie, cut,
                      show_pruned = TRUE, pruned_edge_color = "pink",
                      edge.color = NA, edge.labels = TRUE,
                      edge.label.position = 0.65, layout = "circle",
                      layout_args = list(), scale_nodes, scaling_factor = 0.5,
                      mar = rep(5, 4), theme = "colorblind", ...) {
  check_flag(show_pruned)
  check_flag(edge.labels)
  check_range(edge.label.position, scalar = FALSE)
  layout <- check_layout(x, layout = layout, layout_args)
  vsize <- list(...)$vsize
  pie <- pie %m% x$inits
  labels <- labels %m% x$labels
  if (missing(colors)) {
    colors <- ifelse_(
      is.null(x$data),
      color_palette(length(x$labels)),
      attr(x$data, "colors")
    )
  }
  cut <- cut %m% stats::median(x$weights, na.rm = TRUE)
  lty <- 1
  if (!is.null(attr(x, "pruning")) && show_pruned) {
    lty <- x$weights
    lty[x$weights == 0] <- 2
    lty[x$weights > 0] <- 1
    edge_color_mat <- attr(x, "pruning")$original
    edge_color_mat[x$weights == 0] <- pruned_edge_color
    edge_color_mat[x$weights > 0] <- edge.color
    edge.color <- edge_color_mat
  }
  weights <- ifelse_(
    !is.null(attr(x, "pruning")) && show_pruned,
    attr(x, "pruning")$original,
    x$weights
  )
  n <- nodes(x)
  scale_nodes <- ifelse_(
    missing(scale_nodes) || isFALSE(scale_nodes),
    character(0L),
    scale_nodes
  )
  scale_nodes <- ifelse_(
    isTRUE(scale_nodes),
    "InStrength",
    scale_nodes
  )
  vsize <- ifelse_(is.null(vsize), rep(8 * exp(-n / 80)), vsize)
  if (length(scale_nodes) > 0) {
    check_string(scale_nodes)
    check_range(scaling_factor, lower = 0)
    cent <- centralities(x, measures = scale_nodes, normalize = TRUE)[[2L]]
    vsize <- vsize * (1 + cent)^scaling_factor
  }
  qgraph::qgraph(
    input = weights,
    color = colors,
    edge.color = edge.color,
    edge.labels = edge.labels,
    edge.label.position = edge.label.position,
    labels = labels,
    layout = layout,
    theme = theme,
    vsize = vsize,
    pie = pie,
    mar = mar,
    lty = lty,
    cut = cut,
    ...
  )
}

#' Plot a Bootstrapped Transition Network Analysis Model
#'
#' @export
#' @family validation
#' @param x A `tna_bootstrap` object.
#' @param ... Additional arguments passed to [plot.tna()].
#' @examples
#' model <- tna(group_regulation)
#' # Small number of iterations for CRAN
#' boot <- bootstrap(model, iter = 50)
#' plot(boot)
#'
plot.tna_bootstrap <- function(x, ...) {
  check_missing(x)
  check_class(x, "tna_bootstrap")
  plot(x$model, ...)
}

#' Plot Centrality Measures
#'
#' Plots the centrality measures of a `tna_centralities` object as a
#' lollipop chart. The resulting plot includes facets for each centrality
#' measure, showing the values for each state. The returned plot is a
#' `ggplot2` object, so it can be easily modified and styled. See
#' [centralities()] for details on the centrality measures.
#'
#' @export
#' @family centralities
#' @param x An object of class `tna_centralities`.
#' @param ncol Number of columns to use for the facets. The default is 3.
#' @param scales Either `"fixed"` or `"free_x"` (the default). If `"free_x"`,
#'   the horizontal axis is scaled individually in each facet. If `"fixed"`,
#'   the same values are used for all axes.
#' @param reorder A `logical` value indicating whether to reorder the values
#'   for each centrality in a descending order. The default is `TRUE`.
#' @param colors The colors for each node (default is the model colors
#'  if the `tna` model object is passed, otherwise `"black"`).
#' @param labels A `logical` value indicating whether to show the centrality
#'   numeric values. The default is `TRUE`.
#' @param ... Ignored.
#' @return A `ggplot` object displaying the lollipop charts for each centrality
#'   measure.
#' @examples
#' tna_model <- tna(group_regulation)
#' cm <- centralities(tna_model)
#' plot(cm, ncol = 3, reorder = TRUE)
#'
plot.tna_centralities <- function(x, reorder = TRUE, ncol = 3,
                                  scales = c("free_x", "fixed"),
                                  colors, labels = TRUE, ...) {
  check_class(x, "tna_centralities")
  plot_centralities_(x, reorder, ncol, scales, colors, NULL, labels)
}

#' Plot Cliques of a TNA Network
#'
#' @export
#' @family cliques
#' @inheritParams print.tna_cliques
#' @inheritParams plot.tna
#' @param cut See [qgraph::qgraph()].
#' @param normalize See [qgraph::qgraph()].
#' @param show_loops A `logical` value indicating whether to include loops
#'   in the plots or not.
#' @param minimum See [qgraph::qgraph()].
#' @param ask A `logical` value. When `TRUE`, show plots one by one and asks
#'   to plot the next plot in interactive mode.
#' @return `NULL` (invisibly).
#' @examples
#' model <- tna(group_regulation)
#' cliq <- cliques(model, size = 2)
#' plot(cliq, n = 1, ask = FALSE)
#'
plot.tna_cliques <- function(x, n = 6, first = 1, show_loops = FALSE,
                             edge.labels = TRUE, edge.label.position = 0.65,
                             minimum = 0.00001, mar = rep(5, 4),
                             layout = "circle", layout_args = list(),
                             cut = 0.01, normalize = TRUE,
                             ask = TRUE, colors, theme = "colorblind", ...) {
  check_class(x, "tna_cliques")
  n_cliques <- length(x$weights)
  size <- attr(x, "size")
  if (n_cliques == 0) {
    warning_("No {size}-cliques were found in the network.")
    return(invisible(NULL))
  }
  colors <- colors %m% attr(x, "colors")
  labels <- attr(x, "labels")
  max_cliques <- min(first + n - 1L, n_cliques)
  if (interactive()) { # nocov start
    op <- graphics::par(ask = ask)
    on.exit(graphics::par(op))
  } # nocov end
  for (i in seq(first, max_cliques)) {
    clique_weights <- x$weights[[i]]
    directed <- !attr(x, "sum_weights")
    diag(clique_weights) <- ifelse_(
      show_loops,
      diag(clique_weights),
      0
    )
    layout <- check_layout(
      x = clique_weights,
      layout = layout,
      args = layout_args,
      directed = directed
    )
    plot_args <- list(
      input = clique_weights,
      labels = colnames(clique_weights),
      edge.labels = edge.labels,
      edge.label.position = edge.label.position,
      directed = directed,
      mar = mar,
      minimum = minimum,
      theme = theme,
      cut = cut,
      normalize = normalize,
      layout = layout,
      color = colors[match(rownames(clique_weights), labels)],
      pie = x$inits[[i]]
    )
    plot_args <- utils::modifyList(plot_args, list(...))
    do.call(qgraph::qgraph, args = plot_args)
  }
  invisible(NULL)
}

#' Plot Communities
#'
#' This function visualizes the communities detected within a `tna` object
#' based on different community detection algorithms and their corresponding
#' color mappings.
#'
#' @export
#' @family communities
#' @param x A `communities` object generated by the `find_communities` method.
#'   Each community detection method maps nodes or points in to a specific
#'   communities.
#' @param colors A `character` vector of color values used for visualizing
#'   community assignments.
#' @param method A `character` string naming a community detection method to
#'   use for coloring the plot. The default is to use the first available 
#'   method in `x`. See [communities()] for details.
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` object in which the nodes are colored by community.
#' @examples
#' model <- tna(group_regulation)
#' comm <- communities(model)
#' plot(comm, method = "leading_eigen")
#'
plot.tna_communities <- function(x, colors, method, ...) {
  check_class(x, "tna_communities")
  available_methods <- intersect(
    names(x$assignment),
    names(supported_communities)
  )
  method <- method %m% available_methods[1L]
  check_string(method)
  stopifnot_(
    method %in% available_methods,
    "The {.val {method}} method is not available in {.arg x}."
  )
  y <- attr(x, "tna")
  colors <- colors %m% default_colors
  plot(y, colors = map_to_color(x$assignment[[method]], colors), ...)
}

#' Plot the Comparison of Two TNA Models or Matrices
#'
#' @export
#' @family comparison
#' @param x A `tna_comparison` object.
#' @param type A `character` string naming the type of plot to produce. The
#'   available options are `"heatmap"` (the default), `"scatterplot"`,
#'   `"centrality_heatmap"`, and `"weight_density"`.
#' @param population A `"character"` string naming the population for which
#'   to produce the heatmaps, i.e, one of `"x"`, `"y"`, or `"difference"` for
#'   the differences. Ignored for `type = "scatterplot"`. Defaults to `"diff"`.
#' @param method A `character` string naming the correlation coefficient to
#'   use when plotting a scatterplot. The available options are `"pearson"`
#'   (the default), `"kendall"`, `"spearman"`, and `"distance"`. The final
#'   option is the distance correlation coefficient of
#'   Szekely, Rizzo, and Bakirov (2007). See also the `energy` package for
#'   further information on this measure.
#' @param name_x An optional `character` string to use as the name of the
#'   first population in the plots. The default is `"x"`.
#' @param name_y An optional `character` string to use as the name of the
#'   second population in the plots. The default is `"y"`.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @references
#' Szekely, G.J., Rizzo, M.L., and Bakirov, N.K. (2007),
#' Measuring and Testing Dependence by Correlation of Distances,
#' *Annals of Statistics*, 35(6), 2769-2794.
#' doi:10.1214/009053607000000505
#' @examples
#' model_x <- tna(group_regulation[1:200, ])
#' model_y <- tna(group_regulation[1001:1200, ])
#' comp <- compare(model_x, model_y)
#' plot(comp)
#'
plot.tna_comparison <- function(x, type = "heatmap",
                                population = "difference", method = "pearson",
                                name_x = "x", name_y = "y", ...) {
  check_class(x, "tna_comparison")
  check_string(name_x)
  check_string(name_y)
  type <- check_match(
    type,
    c("heatmap", "scatterplot", "centrality_heatmap", "weight_density")
  )
  if (type == "heatmap") {
    population <- check_match(population, c("x", "y", "difference"))
    weight_col <- switch(
      population,
      x = "weight_x",
      y = "weight_y",
      difference = "raw_difference"
    )
    title <- switch(
      population,
      x = paste0("Heatmap ", name_x),
      y = paste0("Heatmap ", name_y),
      difference = paste0(
        "Difference Matrix Heatmap (", name_x, " vs. ", name_y, ")"
      )
    )
    edges <- x$edge_metrics[, c("source", "target", weight_col)]
    names(edges)[3] <- "value"
    return(create_heatmap(edges, title))
  }
  if (type == "scatterplot") {
    method <- check_match(
      method,
      c("pearson", "kendall", "spearman", "distance")
    )
    edges <- x$edge_metrics[, c("source", "target", "weight_x", "weight_y")]
    metric_idx <- tolower(x$summary_metrics$metric) == method
    corr <- round(x$summary_metrics$value[metric_idx], 3)
    # switch tracking does not seem to work here
    corr_subtitle <- switch( # nocov start
      method,
      pearson = bquote("Pearson's" ~ {rho} ~~ "=" ~~ .(corr)),
      spearman = bquote("Spearman's" ~ {rho} ~~ "=" ~~ .(corr)),
      kendall = bquote("Kendall's" ~ {tau} ~~ "=" ~~ .(corr)),
      distance = paste0("Distance correlation = ", corr)
    ) # nocov end
    out <-
      ggplot2::ggplot(
        edges,
        ggplot2::aes(
          !!rlang::sym("weight_x"),
          !!rlang::sym("weight_y")
        )
      ) +
      ggplot2::geom_point(alpha = 0.6, color = "blue") +
      ggplot2::geom_smooth(
        formula = y ~ x, method = "lm", color = "red", linetype = "dashed"
      ) +
      ggplot2::labs(
        title = paste0("Correlation between ", name_x, " and ", name_y),
        subtitle = corr_subtitle,
        x = paste0("Weights (", name_x, ")"),
        y = paste0("Weights (", name_y, ")")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 12, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = 10))
    return(out)
  }
  if (type == "centrality_heatmap") {
    out <-
      ggplot2::ggplot(
        x$centrality_differences,
        ggplot2::aes(
          x = !!rlang::sym("centrality"),
          y = !!rlang::sym("state"),
          fill = !!rlang::sym("difference")
        )
      ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(
        low = "blue",
        high = "red",
        mid = "white"
      ) +
      ggplot2::labs(
        title = paste0(
          "Centrality Difference Heatmap (", name_x, " vs ", name_y, ")"
        ),
        x = "Centrality Measure",
        y = "Node"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 12, face = "bold")
      )
    return(out)
  }
  if (type == "weight_density") {
    edges <- x$edge_metrics[, c("source", "target", "weight_x", "weight_y")]
    out <-
      ggplot2::ggplot(
        edges,
        ggplot2::aes(x = !!rlang::sym("weight_x"), color = name_x)
      ) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::geom_density(
        ggplot2::aes(x = !!rlang::sym("weight_y"), color = name_y),
        alpha = 0.5
      ) +
      ggplot2::labs(
        title = "Density Plot of Model Weights",
        x = "Weight",
        y = "Density",
        color = "Model"
      ) +
      ggplot2::theme_minimal()
    return(out)
  }
}

#' Plot the Significant Differences from a Permutation Test
#'
#' @export
#' @family validation
#' @param x A `tna_permutation` object.
#' @param colors See [qgraph::qgraph()].
#' @param ... Arguments passed to [plot_model()].
#' @param posCol Color for plotting edges
#'   the difference in edge weights is positive. See [qgraph::qgraph()].
#' @param negCol Color for plotting edges when
#'   the the difference in edge weights is negative. See [qgraph::qgraph()].
#' @return A `qgraph` object containing only the significant edges according
#'   to the permutation test.
#' @examples
#' model_x <- tna(group_regulation[1:200, ])
#' model_y <- tna(group_regulation[1001:1200, ])
#' # Small number of iterations for CRAN
#' perm <- permutation_test(model_x, model_y, iter = 20)
#' plot(perm)
#'
plot.tna_permutation <- function(x, colors,
                                 posCol = "#009900", negCol = "red", ...) {
  check_missing(x)
  check_class(x, "tna_permutation")
  colors <- colors %m% attr(x, "colors")
  plot_model(
    x$edges$diffs_sig,
    labels = attr(x, "labels"),
    colors = colors,
    posCol = posCol,
    negCol = negCol,
    ...
  )
}

#' Plot Centrality Stability Results
#'
#' This function visualizes the centrality stability results produced by the
#' `estimate_centrality_stability` function. It shows how different centrality
#' measures' correlations change as varying proportions of cases are dropped,
#' along with their confidence intervals (CIs).
#'
#' @export
#' @family validation
#' @param x A `tna_stability` object produced by `estimate_cs`.
#' @param level A `numeric` value representing the significance level for
#'   the confidence intervals. Defaults to `0.05`.
#' @param ... Ignored.
#'
#' @details
#' The function aggregates the results for each centrality measure across
#' multiple proportions of dropped cases (e.g., 0.1, 0.2, ..., 0.9) and
#' calculates the mean and the desired quantiles for each proportion.
#' The confidence intervals (CIs) are computed based on the quantiles
#' and displayed in the plot.
#'
#' If no valid data is available for a centrality measure
#' (e.g., missing or NA values), the function skips that measure with a warning.
#'
#' The plot includes:
#'
#' * The mean correlation for each centrality measure as a function of the
#'   proportion of dropped cases.
#' * Shaded confidence intervals representing CIs for each centrality measure.
#' * A horizontal dashed line at the threshold value used for calculating
#'   the CS-coefficient.
#' * A subtitle listing the CS-coefficients for each centrality measure.
#'
#' @return A `ggplot` object displaying the stability analysis plot.
#' @examples
#' model <- tna(group_regulation)
#' cs <- estimate_cs(model, iter = 10)
#' plot(cs)
#'
plot.tna_stability <- function(x, level = 0.05, ...) {
  check_class(x, "tna_stability")
  check_range(level, lower = 0, upper = 1)
  x$detailed_results <- NULL
  x_names <- names(x)
  drop_prop <- attr(x, "drop_prop")
  threshold <- attr(x, "threshold")
  measure_data <- vector(mode = "list", length = length(x))
  cs_subtitle <- character(length(x))
  for (i in seq_along(x)) {
    measure <- x_names[i]
    corr <- x[[measure]]$correlations
    means <- apply(corr, 2L, mean, na.rm = TRUE)
    ci_lower <- apply(
      corr,
      2L,
      stats::quantile,
      probs = level / 2.0,
      na.rm = TRUE
    )
    ci_upper <- apply(
      corr,
      2L,
      stats::quantile,
      probs = 1.0 - level / 2.0,
      na.rm = TRUE
    )
    measure_data[[i]] <- data.frame(
      measure = measure,
      proportion = drop_prop,
      correlation = means,
      lower = ci_lower,
      upper = ci_upper
    )
    cs_coef <- x[[measure]]$cs_coefficient
    cs_subtitle[i] <- paste0(
      measure,
      " CS = ",
      round(cs_coef, 2L)
    )
  }
  plot_data <- dplyr::bind_rows(measure_data)
  stopifnot_(
    nrow(plot_data) > 0,
    "No valid data to plot."
  )
  cs_subtitle <- paste0(cs_subtitle, collapse = "; ")
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = !!rlang::sym("proportion"),
      y = !!rlang::sym("correlation"),
      color = !!rlang::sym("measure")
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = !!rlang::sym("lower"),
        ymax = !!rlang::sym("upper"),
        fill = !!rlang::sym("measure")
      ),
      alpha = 0.2
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(
      yintercept = threshold,
      linetype = "dashed",
      color = "gray50"
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "solid",
      color = "gray25"
    ) +
    ggplot2::labs(
      title = "Centrality Stability Analysis",
      subtitle = paste0("CS-Coeficients: ", cs_subtitle),
      x = "Proportion of Cases Dropped",
      y = "Correlation with Original Centrality",
      color = "Centrality Measure",
      fill = "Centrality Measure"
    ) +
    ggplot2::scale_x_continuous(breaks = drop_prop) +
    ggplot2::theme_minimal() +
    ggplot2::ylim(-1, 1)
}

#' Plot a Sequence Comparison
#'
#' Visualize the differences in pattern counts by plotting the standardized
#' residuals by pattern and group.
#'
#' @export
#' @family comparison
#' @param x A `tna_sequence_comparison` object.
#' @param n An `integer` giving the number of patterns to plot.
#'   The default is `10`.
#' @param legend  A `logical` value indicating whether to show the color scale
#'   legend. The default is `TRUE`.
#' @param cells A `logical` value indicating whether to display the
#'   numeric values in each cell. The default is `TRUE`.
#' @param text_color A `character` string specifying the text color to use for
#'   the cell values. The default is `"white"`.
#' @param digits An `integer` specifying the number of digits for the cell
#'   values.
#' @param ... Not used.
#' @return A `ggplot` object.
#' @examples
#' group <- c(rep("High", 1000), rep("Low", 1000))
#' comp <- compare_sequences(group_regulation, group)
#' plot(comp)
#'
plot.tna_sequence_comparison <- function(x, n = 10, legend = TRUE,
                                         cells = TRUE, text_color = "white",
                                         digits = 2L, ...) {
  check_missing(x)
  check_class(x, "tna_sequence_comparison")
  check_values(n, strict = TRUE)
  check_flag(legend)
  check_flag(cells)
  check_string(text_color)
  check_values(digits)
  n <- min(n, length(x$pattern))
  pat <- x$pattern[seq_len(n)]
  freq_cols <- attr(x, "freq_cols")
  groups <- attr(x, "groups")
  x <- x |>
    dplyr::filter(!!rlang::sym("pattern") %in% pat) |>
    dplyr::select(dplyr::all_of(freq_cols))
  n <- sum(x)
  rs <- rowSums(x)
  cs <- colSums(x)
  expected <- outer(rs, cs) / n
  v <- function(r, c, n) c * r * (n - r) * (n - c) / n^3
  cell_var <- outer(rs, cs, v, n)
  resid <- (x - expected) / cell_var
  n <- nrow(resid)
  m <- ncol(resid)
  d <- data.frame(xmin = rep(0.0, n * m), xmax = 0.0, ymin = 0.0, ymax = 0.0)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      row <- (i - 1) * m + j
      d[row, "xmin"] <- j
      d[row, "xmax"] <- j + 1.0
      d[row, "ymin"] <- -i
      d[row, "ymax"] <- -i - 1.0
      d[row, "resid"] <- resid[i, j]
      d[row, "label"] <- round(resid[i, j], digits = digits)
    }
  }
  d$xcent <- (d$xmin + d$xmax) / 2.0
  d$ycent <- (d$ymin + d$ymax) / 2.0
  p <-
    ggplot2::ggplot(
      d,
      ggplot2::aes(
        xmin = !!rlang::sym("xmin"),
        xmax = !!rlang::sym("xmax"),
        ymin = !!rlang::sym("ymin"),
        ymax = !!rlang::sym("ymax"),
        fill = !!rlang::sym("resid")
      )
    ) +
    ggplot2::geom_rect(color = "white", show.legend = legend) +
    ggplot2::scale_fill_gradient2(
      name = "Standardized\nresidual",
      oob = bound,
      low = "#D33F6A",
      high = "#4A6FE3",
      limits = c(-4, 4),
      breaks = c(-4, -2, 0, 2, 4)
    ) +
    ggplot2::scale_x_continuous(
      breaks = unique(d$xcent),
      labels = groups,
      position = "bottom",
      expand = c(0.01, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = unique(d$ycent),
      labels = pat,
      expand = c(0.01, 0)
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text.x =  ggplot2::element_text(
        angle = ifelse(n > 3, 90, 0),
        hjust = ifelse(n > 3, 0, 0.5),
        vjust = ifelse(n > 3, 0.5, 0)
      ),
      legend.key.size = ggplot2::unit(1, "cm"),
      axis.text.y = ggplot2::element_text(hjust = 1, vjust = 0.40)
    ) +
    ggplot2::labs(x = "Groups", y = "")
  if (cells) {
    p <- p +
      ggplot2::geom_text(
        mapping = ggplot2::aes(
          x = !!rlang::sym("xcent"),
          y = !!rlang::sym("ycent"),
          label = !!rlang::sym("label")
        ),
        fontface = "bold",
        color = text_color
      )
  }
  p
}

#' Plot Reliability Analysis Results
#'
#' @export
#' @family validation
#' @param x A `tna_reliability` object.
#' @param type A `character` string specifying the plot type. The options are:
#'   `"histogram"` (default), `"density"`, or `"boxplot"`.
#' @param metric A `character` string specifying the metric to plot.
#'   The default is the median absolute difference (`"Median Abs. Diff."`).
#' @param ... Ignored
#' @examples
#' # Small number of iterations for CRAN
#' model <- tna(engagement)
#' rel <- reliability(model, iter = 20)
#' plot(rel)
#'
plot.tna_reliability <- function(x, type = "histogram",
                                 metric = "Median Abs. Diff.", ...) {
  check_missing(x)
  check_class(x, "tna_reliability")
  check_string(metric)
  type <- check_match(type, c("histogram", "density", "boxplot"))
  stopifnot_(
    !metric %in% unique(x$metrics),
    "Metric {.val {metric}} was not found in {.arg x}"
  )
  met <- metric
  d <- x$metrics |>
    dplyr::filter(metric == met)
  has_model <- "model_type" %in% names(x$metrics)
  if (has_model) {
    stats <- d |>
      dplyr::group_by(!!rlang::sym("model_type")) |>
      dplyr::summarize(
        mean = mean(!!rlang::sym("value"), na.rm = TRUE),
        sd = stats::sd(!!rlang::sym("value"), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        label = paste0(
          !!rlang::sym("model_type"),
          " (Mean: ",
          round(!!rlang::sym("mean"), 3),
          ", SD: ",
          round(!!rlang::sym("sd"), 3),
          ")"
        )
      )
    d <- d |> dplyr::left_join(stats, by = "model_type")
    group_var <- "label"
  } else {
    stats <- d |>
      dplyr::summarize(
        mean = mean(!!rlang::sym("value"), na.rm = TRUE),
        sd = stats::sd(!!rlang::sym("value"), na.rm = TRUE)
      )
    group_var <- NULL
  }
  sub <- paste0(
    "Mean: ", round(stats$mean, 3),
    " | SD: ", round(stats$sd, 3)
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(x = !!rlang::sym("value"))) +
    ggplot2::labs(
      title = paste("Reliability Distribution:", metric),
      x = metric,
      y = "Frequency"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "top",
      legend.title = ggplot2::element_blank()
    )
  if (type == "histogram") {
    if (has_model) {
      p <- p +
        ggplot2::geom_histogram(
          ggplot2::aes(
            fill = !!rlang::sym(group_var)
          ),
          position = "identity",
          alpha = 0.5,
          bins = 30,
          color = "white"
        ) +
        ggplot2::geom_vline(
          data = stats,
          ggplot2::aes(
            xintercept = !!rlang::sym("mean"),
            color = !!rlang::sym("label"),
          ),
          linetype = "dashed",
          linewidth = 1,
          show.legend = FALSE
        )
    } else {
      p <- p +
        ggplot2::geom_histogram(
          fill = "cadetblue",
          color = "white",
          alpha = 0.7,
          bins = 30
        ) +
        ggplot2::geom_vline(
          ggplot2::aes(xintercept = stats$mean),
          linetype = "dashed",
          color = "firebrick",
          linewidth = 1
        ) +
        ggplot2::labs(y = "Frequency", subtitle = sub)
    }
  } else if (type == "density") {
    if (has_model) {
      p <- p +
        ggplot2::geom_density(
          ggplot2::aes(fill = !!rlang::sym(group_var)),
          alpha = 0.4
        ) +
        ggplot2::geom_vline(
          data = stats,
          ggplot2::aes(
            xintercept = !!rlang::sym("mean"),
            color = !!rlang::sym("label")
          ),
          linetype = "dashed",
          linewidth = 1,
          show.legend = FALSE
        ) +
        ggplot2::labs(y = "Density")
    } else {
      p <- p +
        ggplot2::geom_density(fill = "cadetblue", alpha = 0.5) +
        ggplot2::geom_vline(
          ggplot2::aes(xintercept = stats$mean),
          linetype = "dashed",
          color = "firebrick",
          linewidth = 1
        ) +
        ggplot2::labs(y = "Density", subtitle = sub)
    }

  } else if (type == "boxplot") {
    if (has_model) {
      p <- ggplot2::ggplot(
        d,
        ggplot2::aes(
          x = !!rlang::sym(group_var),
          y = !!rlang::sym("value"),
          fill = !!rlang::sym(group_var))
      ) +
        ggplot2::geom_boxplot(alpha = 0.7) +
        ggplot2::labs(x = "", title = metric) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
    } else {
      p <- ggplot2::ggplot(
        d,
        ggplot2::aes(y = !!rlang::sym("value"))
      ) +
        ggplot2::geom_boxplot(fill = "cadetblue", alpha = 0.7) +
        ggplot2::labs(title = metric, subtitle = sub) +
        ggplot2::theme_minimal()
    }
  }
  p
}


#' Plot Centrality Measures
#'
#' @inheritParams plot.tna_centralities
#' @noRd
plot_centralities_ <- function(x, reorder, ncol, scales, colors,
                               palette, labels) {
  check_flag(reorder)
  check_flag(labels)
  scales <- check_match(scales, c("free_x", "fixed"))
  scales <- ifelse_(scales == "free_x", "free", "free_y")
  n <- n_unique(x$state)
  colors <- ifelse_(
    missing(colors),
    attr(x, "colors") %||% rep("black", n),
    rep(colors, length.out = n)
  )
  ifelse_(
    inherits(x, "tna_centralities"),
    plot_centralities_single(x, reorder, ncol, scales, colors, labels),
    plot_centralities_multiple(x, reorder, ncol, scales, colors, palette, labels)
  )
}

#' Plot Centralities for a Single Cluster
#'
#' @inheritParams plot.tna_centralities
#' @noRd
plot_centralities_single <- function(x, reorder, ncol, scales, colors, labels) {
  # Create some NULLs for R CMD Check
  name <- value <- NULL
  levs <- names(x)[-1L]
  x <- stats::reshape(
    as.data.frame(x),
    idvar = "state",
    ids = x[["state"]],
    times = levs,
    timevar = "name",
    drop = "state",
    varying = list(levs),
    direction = "long",
    v.names = "value"
  ) |>
    dplyr::group_by(name) |>
    dplyr::mutate(
      prop = value / sum(value, na.rm = TRUE),
      name = factor(name, levels = levs)
    )
  n_measures <- length(levs)
  state_within_name <- paste(x$state, x$name, sep = "___")
  x$state_within <- ifelse_(
    reorder,
    stats::reorder(state_within_name, x$value, FUN = identity),
    x$state
  )
  ggplot2::ggplot(x) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_col(
      ggplot2::aes(
        fill = !!rlang::sym("state"),
        x = !!rlang::sym("state_within"),
        y = !!rlang::sym("value")
      ),
      linewidth = 4
    ) +
    ggplot2::coord_flip(clip = "off") +
    onlyif(
      labels,
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0(" ", round(!!rlang::sym("value"), 2), " "),
          x = !!rlang::sym("state_within"),
          y = !!rlang::sym("value"),
          hjust = !!rlang::sym("prop") > 0.05
        ),
        vjust = 0.35,
        size = 3
      )
    ) +
    ggplot2::facet_wrap(~name, ncol = ncol, scales = scales) +
    ggplot2::scale_x_discrete(
      labels = function(y) gsub("___.+$", "", y)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      axis.text.y = ggplot2::element_text(size = 8, vjust = 0.2),
      panel.spacing = ggplot2::unit(2, "lines"),
      plot.margin = ggplot2::margin(5.5, 11, 5.5, 5.5, "points")
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("")
}

plot_centralities_multiple <- function(x, reorder, ncol, scales,
                                       colors, palette = "Set2", labels) {
  measures <- names(x)[3:ncol(x)]
  n_clusters <- n_unique(x$group)
  x$state <- factor(x$state)
  x <- x |>
    data.frame() |>
    stats::reshape(
      varying = measures,
      v.names = "value",
      timevar = "name",
      times = measures,
      direction = "long"
    )
  x$name <- factor(x$name, levels = measures)
  ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = !!rlang::sym("value"),
      y = !!rlang::sym("state"),
      color = !!rlang::sym("group"),
      fill = !!rlang::sym("group"),
      group = !!rlang::sym("group")
    )
  ) +
    ggplot2::facet_wrap("name", ncol = ncol, scales = scales) +
    ggplot2::geom_path() +
    ifelse_(
      !is.null(colors) & (n_unique(colors) == n_clusters),
      ggplot2::scale_color_manual(values = colors),
      ggplot2::scale_color_brewer(palette = palette)
    ) +
    ggplot2::geom_point(size = 2, shape = 21, stroke = NA) +
    ifelse_(
      !is.null(colors) & (n_unique(colors) == n_clusters),
      ggplot2::scale_fill_manual(values = colors),
      ggplot2::scale_fill_brewer(palette = palette)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Centrality") +
    ggplot2::ylab("") +
    ggplot2::labs(color = "", fill = "") +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      panel.spacing = ggplot2::unit(1, "lines"),
      axis.text.y = ggplot2::element_text(size = 8, vjust = 0.2),
      legend.position = "bottom"
    )
}

#' Plot the Difference Network Between Two Models
#'
#' Plots the difference network between model `x` and model `y`. The edges are
#' computed from subtracting the two models. The pie chart is the difference in
#' initial probabilities between model `x` and model `y`. Green color indicates
#' that `x`is greater than `y`and red indicates otherwise.
#'
#' @export
#' @family comparison
#' @param x A `tna` object. This is the the principal model.
#' @param y A `tna` object. This is the model subtracted from the
#'   principal model.
#' @param theme See [qgraph::qgraph()].
#' @param palette See [qgraph::qgraph()].
#' @param posCol Color for plotting edges and pie when
#'   the first group has a higher value. See [qgraph::qgraph()].
#' @param negCol Color for plotting edges and pie when
#'   the second group has a higher value. See [qgraph::qgraph()].
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` object displaying the difference network between the
#'   two models.
#' @examples
#' model_x <- tna(group_regulation[group_regulation[, 1] == "plan", ])
#' model_y <- tna(group_regulation[group_regulation[, 1] != "plan", ])
#' plot_compare(model_x, model_y)
#'
plot_compare <- function(x, ...) {
  UseMethod("plot_compare")
}

#' @export
#' @rdname plot_compare
plot_compare.tna <- function(x, y, theme = NULL, palette = "colorblind",
                             posCol = "#009900", negCol = "red", ...) {
  check_class(x, "tna")
  check_class(y, "tna")
  stopifnot_(
    all(x$labels == y$labels),
    "{.arg x} and {.arg y} must have the same labels."
  )
  pie <- abs(x$inits - y$inits)
  piesign <- ifelse(x$inits > y$inits, posCol, negCol)
  diff <- build_model_(
    x$weights - y$weights,
    type = attr(x, "type"),
    labels = x$labels,
    inits = pie
  )
  weights_abs <- abs(x$weights)
  q <- stats::quantile(weights_abs, probs = c(0.2, 0.3))
  plot.tna(
    x = diff,
    pie = pie,
    pieColor = piesign,
    palette = palette,
    theme = theme,
    posCol = posCol,
    negCol = negCol,
    ...
  )
}

#' Plot the Frequency Distribution of States
#'
#' @export
#' @family basic
#' @param x A `tna` object created from sequence data.
#' @param colors A `character` vector of colors to be used in the plot
#'   (one per label) or a single color.
#' @param width A `numeric` value for the Width of the bars. Default is 0.7,
#' @param hjust A `numeric` value for the horizontal adjustment of the labels.
#'   Default is 1.2.
#' @param show_label A `logical` value indicating whether to show a label with
#'   the frequency counts. Default is `TRUE`.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @examples
#' model <- tna(group_regulation)
#' plot_frequencies(model)
#' plot_frequencies(model, width =  0.5, colors = "pink")
#'
plot_frequencies <- function(x, ...) {
  UseMethod("plot_frequencies")
}

#' @export
#' @rdname plot_frequencies
plot_frequencies.tna <- function(x, width = 0.7, hjust = 1.2,
                                 show_label = TRUE, colors, ...) {
  check_missing(x)
  check_tna_seq(x)
  check_values(width, type = "numeric")
  check_numeric(hjust)
  check_flag(show_label)
  colors <- ifelse_(
    missing(colors),
    attr(x$data, "colors") %||% "black",
    colors
  )
  n_colors <- length(colors)
  n_labels <- length(x$labels)
  stopifnot_(
    n_colors == 1L || n_colors == n_labels,
    "The number of {.arg colors} does not match
     the number of labels in {.arg x}."
  )
  colors <- ifelse_(
    n_colors == 1L,
    rep(colors, n_labels),
    colors
  )
  tab <- table(unlist(x$data))
  d <- as.data.frame(tab)
  names(d) <- c("state", "freq")
  d[[1L]] <- factor(x$labels[d[[1L]]], levels = rev(x$labels))
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(y = !!rlang::sym("state"), x = !!rlang::sym("freq"))
  ) +
    ggplot2::geom_bar(
      ggplot2::aes(fill = !!rlang::sym("state")),
      stat = "identity",
      width = width
    )
  if (show_label) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = !!rlang::sym("freq")),
        position = ggplot2::position_dodge(width = width),
        hjust = hjust
      )
  }
  p +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::labs(y = "State", x = "Frequency") +
    ggplot2::theme(
      axis.title =  ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(color = "black"),
      text = ggplot2::element_text(color = "black")
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme(legend.position = "none")
}

#' Plot a Transition Network Model from a Matrix of Edge Weights
#'
#' @export
#' @param x A square `matrix` of edge weights.
#' @inheritParams plot.tna
#' @keywords internal
#' @return See [plot.tna()].
#' @examples
#' m <- matrix(rexp(25), 5, 5)
#' plot_model(m)
#'
plot_model <- function(x, labels, colors, cut,
                       edge.labels = TRUE, edge.label.position = 0.65,
                       layout = "circle", layout_args = list(),
                       mar = rep(5, 4), theme = "colorblind", ...) {
  stopifnot_(
    is.matrix(x) && ncol(x) == nrow(x),
    "Argument {.arg x} must be a square matrix."
  )
  nc <- ncol(x)
  labels <- labels %m% seq_len(nc)
  colors <- colors %m% color_palette(nc)
  cut <- cut %m% stats::median(x, na.rm = TRUE)
  layout <- check_layout(x, layout, layout_args)
  qgraph::qgraph(
    input = x,
    color = colors,
    edge.labels = edge.labels,
    edge.label.position = edge.label.position,
    labels = labels,
    layout = layout,
    theme = theme,
    mar = mar,
    cut = cut,
    ...
  )
}

#' Create a Mosaic Plot of Transitions or Events
#'
#' @export
#' @family basic
#' @param x A `tna` or a `group_tna` object.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @examples
#' ftna_model <- ftna(group_regulation)
#' plot_mosaic(ftna_model)
#'
plot_mosaic <- function(x, ...) {
  UseMethod("plot_mosaic")
}

#' @export
#' @rdname plot_mosaic
plot_mosaic.tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "tna")
  stopifnot_(
    attr(x, "type") %in% c("frequency", "co-occurrence"),
    "Mosaic plots are supported only for integer-valued weight matrices."
  )
  plot_mosaic_(
    as.table(t(x$weights)),
    xlab = "Incoming edges",
    ylab = "Outgoing edges"
  )
}

# from https://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2,
# Based on the code by Jake Fisher and cpsyctc.
plot_mosaic_ <- function(tab, xlab, ylab) {
  n <- nrow(tab)
  m <- ncol(tab)
  rs <- .rowSums(tab, n, m)
  widths <- c(0, cumsum(rs)) / sum(tab)
  heights <- apply(tab, 1L, function(y) c(0, cumsum(y / sum(y))))
  d <- data.frame(xmin = rep(0, n * m), xmax = 0, ymin = 0, ymax = 0)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      row <- (i - 1) * m + j
      row_offset <- (i - 1) * n * 0.0025
      col_offset <- (j - 1) * m * 0.0025
      d[row, "xmin"] <- widths[i] + row_offset
      d[row, "xmax"] <- widths[i + 1] + row_offset
      d[row, "ymin"] <- heights[j, i] + col_offset
      d[row, "ymax"] <- heights[j + 1, i] + col_offset
      d[row, "freq"] <- tab[i, j]
    }
  }
  d$row <- rep(dimnames(tab)[[1]], m)
  d$col <- rep(dimnames(tab)[[2]], each = n)
  chisq <- suppressWarnings(stats::chisq.test(tab))
  df <- chisq$parameter
  chisqval <- chisq$statistic
  d$xcent <- (d$xmin + d$xmax) / 2
  d$ycent <- (d$ymin + d$ymax) / 2
  d$stdres <- as.vector(t(chisq$stdres))
  ggplot2::ggplot(
    d,
    ggplot2::aes(
      xmin = !!rlang::sym("xmin"),
      xmax = !!rlang::sym("xmax"),
      ymin = !!rlang::sym("ymin"),
      ymax = !!rlang::sym("ymax"),
      fill = !!rlang::sym("stdres"),
    )
  ) +
  ggplot2::geom_rect(color = "black", show.legend = TRUE) +
  ggplot2::scale_fill_gradient2(
    name = "Standardized\nresidual",
    oob = bound,
    low = "#D33F6A",
    high = "#4A6FE3",
    limits = c(-4, 4),
    breaks = c(-4, -2, 0, 2, 4)
  ) +
  ggplot2::scale_x_continuous(
    breaks = unique(d$xcent),
    labels = dimnames(tab)[[1]],
    position = "top",
    expand = c(0.01, 0)
  ) +
  ggplot2::scale_y_continuous(
    breaks = d$ycent[d$xmin == 0],
    labels = dimnames(tab)[[2]],
    expand = c(0.01, 0)
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.text.x =  ggplot2::element_text(
      angle =  ifelse(n > 3, 90, 0),
      hjust =  ifelse(n > 3, 0, 0.5),
      vjust =  ifelse(n > 3, 0.5, 0)
    ),
    axis.text.y = ggplot2::element_text(hjust = 1, vjust = 0.40)
  ) +
  ggplot2::labs(x = xlab, y = ylab)
}

#' Create a Sequence Index Plot or a Distribution Plot
#'
#' @export
#' @rdname plot_sequences
#' @param x A `tna`, `group_tna`, `tna_data` or a `data.frame` object with
#'   sequence data in wide format.
#' @param cols An `expression` giving a tidy selection of column names to be
#'   treated as time points. By default, all columns will be used.
#' @param group A vector indicating the group assignment of each
#'   row of the data. Must have the same length as the
#'   number of rows of `x`. Alternatively, a single `character` string giving
#'   the column name of the data that defines the group when `x` is a wide
#'   format `data.frame` or a `tna_data` object. Used for faceting the plot.
#' @param type A `character` string for the type of plot to generate. The
#'   available options are  `"index"` (the default) for a sequence index plot,
#'   and `"distribution"` showing the distribution of the states over time.
#' @param scale A `character` string that determines the scaling of the
#'   vertical axis for distribution plots. The options are `"proportion"`
#'   (the default) and `"count"` for proportions and raw counts of states,
#'   respectively.
#' @param geom A `character` string for the type of geom to use for
#'   distribution plots. The options are `"bar"` (the default) and `"area"`.
#' @param include_na A `logical` value for whether to include missing values
#'   for distribution plots. The default is `FALSE`. If `TRUE`, the missing
#'   values are converted to a new state and included in the plot.
#' @param colors A named `character` vector mapping states to colors, or an
#'   unnamed `character` vector. If missing, a default palette is used.
#' @param na_color A `character` string giving the color to use for missing
#'   values. The default is `"white"`.
#' @param sort_by An optional `expression` giving a tidy selection of column
#' names of `x` to sort by or `"everything"`.
#' @param show_n A `logical` value for whether to add the number of
#'   observations (total or by group) to the plot title.
#' @param border A `character` string giving the color for borders. For index
#'   plots, this is the color of borders between cells (tiles). For
#'   distribution plot with `geom = "bar"`, this is the color of bar outlines.
#'   Not applicable to `geom = "area"`.
#' @param title An optional `character` string providing a title for the plot.
#' @param legend_title An optional `character` string providing a title for the
#'   legend.
#' @param xlab A `character` string giving the label for the horizontal axis.
#'   The default is `"Time"`.
#' @param ylab A `character` string giving the label for the vertical axis.
#'   The default is `"Sequence"` for index plots, and `"Proportion"` or
#'   `"Count"` based on `scale` for distribution plots.
#' @param tick An `integer` specifying the horizontal axis label interval. The
#' default value `tick = 5` shows every 5th label. Setting this to 1 will show
#' every label.
#' @param ncol Number of columns to use for the facets. The default is 2.
#' @param ... Ignored.
#' @examples
#' # Sequence index plot (default)
#' plot_sequences(
#'   group_regulation,
#'   group = rep(1:2, each = 1000),
#' )
#' # State distribution plot
#' plot_sequences(
#'   group_regulation,
#'   group = rep(1:2, each = 1000),
#'   type = "distribution",
#' )
#'
plot_sequences <- function(x, ...) {
  UseMethod("plot_sequences")
}

#' @export
#' @rdname plot_sequences
plot_sequences.tna <- function(x, group, type = "index",
                               scale = "proportion", geom = "bar",
                               include_na = FALSE, na_color = "white", sort_by,
                               show_n = TRUE, border, title, legend_title,
                               xlab, ylab, tick = 5, ncol = 2L, ...) {
  check_missing(x)
  check_tna_seq(x)
  d <- as.data.frame(x$data)
  cols <- names(d)
  colors <- attr(x$data, "colors")
  lab <- x$labels
  lev <- seq_along(lab)
  if (!missing(group)) {
    stopifnot_(
      length(group) == nrow(d),
      "Argument {.arg group} must the same length as the number of rows in
       the sequence data of {.arg x}."
    )
    d$.group <- group
    group <- ".group"
  }
  plot_sequences_(
    d, lev, lab, cols, group, type, scale, geom, include_na, colors,
    na_color, rlang::enquo(sort_by), show_n, border, title, legend_title,
    xlab, ylab, tick, ncol
  )
}

#' @export
#' @rdname plot_sequences
plot_sequences.tna_data <- function(x, group, type = "index",
                                    scale = "proportion",
                                    geom = "bar", include_na = FALSE,
                                    colors, na_color = "white", sort_by,
                                    show_n = TRUE, border, title,
                                    legend_title, xlab, ylab, tick = 5,
                                    ncol = 2L, ...) {
  check_missing(x)
  check_class(x, "tna_data")
  wide <- cbind(x$sequence_data, x$meta_data)
  plot_sequences.default(
    x = wide,
    cols = dplyr::all_of(names(x$sequence_data)),
    group = group,
    type = type,
    scale = scale,
    geom = geom,
    include_na = include_na,
    colors = colors,
    na_color = na_color,
    sort_by = rlang::enquo(sort_by),
    show_n = show_n,
    border = border,
    title = title,
    legend_title = legend_title,
    xlab = xlab,
    ylab = ylab,
    tick = tick,
    ncol = ncol,
    ...
  )
}

#' @export
#' @rdname plot_sequences
plot_sequences.default <- function(x, cols = tidyselect::everything(),
                                   group, type = "index",
                                   scale = "proportion", geom = "bar",
                                   include_na = FALSE, colors,
                                   na_color = "white", sort_by,
                                   show_n = TRUE, border, title,
                                   legend_title, xlab, ylab, tick = 5,
                                   ncol = 2L, ...) {
  check_missing(x)
  stopifnot_(
    inherits(x, "stslist") || inherits(x, "data.frame"),
    "Argument {.arg x} must be {.cls stslist} (sequence data) or a
    {.cls data.frame} or object."
  )
  # TODO can remove this if TraMineR is fixed
  x <- as.data.frame(x)
  cols <- get_cols(rlang::enquo(cols), x)
  if (!missing(group)) {
    n_group <- length(group)
    stopifnot_(
      n_group == nrow(x) || n_group == 1L,
      "Argument {.arg group} must be of length one or the same length as the
       number of rows in {.arg x}."
    )
    if (n_group == 1L) {
      stopifnot_(
        group %in% names(x),
        "Argument {.arg group} must be a column name of {.arg x}
         when of length one."
      )
    } else {
      x$.group <- group
      group <- ".group"
    }
  }
  lab <- ifelse_(
    inherits(x, "stslist"),
    attr(x, "alphabet"),
    sort(unique(unlist(x[, cols])))
  )
  lev <- seq_along(lab)
  x[cols] <- lapply(x[cols], factor, labels = lab, levels = lab)
  x[cols] <- lapply(x[cols], as.integer)
  sort_by <- sort_by %m% rlang::enquo(sort_by)
  sort_by <- rlang::as_quosure(sort_by, rlang::caller_env())
  plot_sequences_(
    x, lev, lab, cols, group, type, scale, geom, include_na, colors,
    na_color, sort_by, show_n, border, title, legend_title,
    xlab, ylab, tick, ncol
  )
}

plot_sequences_ <- function(x, lev, lab, cols, group, type, scale,
                            geom, include_na, colors, na_color, sort_by,
                            show_n, border, title, legend_title,
                            xlab, ylab, tick, ncol) {
  type <- check_match(type, c("distribution", "index"))
  scale <- check_match(scale, c("count", "proportion"))
  geom <- check_match(geom, c("area", "bar"))
  check_flag(include_na)
  check_flag(show_n)
  check_values(tick)
  has_group <- TRUE
  if (missing(group)) {
    x$.group <- 1L
    group <- ".group"
    has_group <- FALSE
  }
  x <- x |>
    dplyr::select(c(dplyr::all_of(cols), !!rlang::sym(group))) |>
    dplyr::group_by(!!rlang::sym(group))
  sort_by <- get_cols(sort_by, x)
  if (!missing(sort_by)) {
    x <- x |>
      dplyr::arrange(dplyr::across(dplyr::all_of(sort_by)), .by_group = TRUE)
  }
  x$.seq_id <- rev(seq_len(nrow(x)))
  # Remove temporary grouping used in sorting
  group <- ifelse_(has_group, group, rlang::missing_arg())
  x <- x |> dplyr::ungroup()
  long_data <- x |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols),
      names_to = "time",
      values_to = "state"
    ) |>
    dplyr::mutate(
      .seq_id = factor(!!rlang::sym(".seq_id")),
      time = factor(!!rlang::sym("time"), levels = cols),
      state = factor(!!rlang::sym("state"), levels = lev, labels = lab)
    )
  colors <- colors %m% color_palette(n_unique(long_data$state))
  if (show_n) {
    if (!missing(group)) {
      group_n <- x |>
        dplyr::group_by(!!rlang::sym(group)) |>
        dplyr::summarize(n = dplyr::n())
      title_n <- paste0(
        "{n[", seq_along(group_n$n), "] == ", group_n$n, "}",
        collapse = " * \", \" * "
      )
      title_n <- paste0("\"(\" * ", title_n, " * \")\"")
    } else {
      title_n <- paste0("\"(\" * n == ", nrow(x), "* \")\"")
    }
  }
  if (type == "index") {
    create_index_plot(
      long_data, group, colors, na_color, border,
      title, include_na, title_n, legend_title, xlab, ylab, tick, ncol
    )
  } else {
    create_distribution_plot(
      long_data, group, scale, geom, include_na, colors, na_color,
      border, title, title_n, legend_title, xlab, ylab, tick, ncol
    )
  }
}

create_index_plot <- function(x, group, colors, na_color, border,
                              title, include_na, title_n, legend_title,
                              xlab, ylab, tick, ncol) {
  title <- title %m% "Sequence Index Plot"
  xlab <- xlab %m% "Time"
  ylab <- ylab %m% "Sequence"
  legend_title <- legend_title %m% NULL
  title <- str2expression(paste0("\"", title, " \" * ", title_n))
  every_nth <- function(y) y[(seq_along(y) - 1L) %% tick == 0]
  if (!include_na) {
    x <- x |> tidyr::drop_na()
  }
  p <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = !!rlang::sym("time"),
      y = !!rlang::sym(".seq_id")
    )
  ) +
    ggplot2::geom_raster(ggplot2::aes(fill = !!rlang::sym("state"))) +
    ggplot2::scale_fill_manual(
      values = colors,
      name = legend_title,
      na.value = na_color
    ) +
    ggplot2::scale_x_discrete(breaks = every_nth) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "right"
    )
  if (!missing(border)) {
    p <- p + ggplot2::geom_tile(
      ggplot2::aes(fill = !!rlang::sym("state")),
      color = border,
      linewidth = 0.1,
      na.rm = FALSE
    )
  }
  if (!missing(group)) {
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(!!rlang::sym(group)),
      ncol = ncol,
      scales = "free_y",
    )
  }
  p
}

create_distribution_plot <- function(x, group, scale, geom, include_na,
                                     colors, na_color, border,
                                     title, title_n, legend_title,
                                     xlab, ylab, tick, ncol) {
  title <- title %m% "Sequence Distribution Plot"
  xlab <- xlab %m% "Time"
  ylab <- ifelse_(
    missing(ylab),
    ifelse_(scale == "proportion", "Proportion", "Count"),
    ylab
  )
  legend_title <- legend_title %m% NULL
  title <- str2expression(paste0("\"", title, " \" * ", title_n))
  every_nth <- function(y) y[(seq_along(y) - 1L) %% tick == 0]
  position <- ifelse_(scale == "proportion", "fill", "stack")
  if (!include_na) {
    x <- x |> tidyr::drop_na()
  }
  if (geom == "bar") {
    p <- ggplot2::ggplot(
      x,
      ggplot2::aes(
        x = !!rlang::sym("time"),
        fill = !!rlang::sym("state")
      )
    ) +
      ggplot2::geom_bar(na.rm = FALSE, width = 1, position = position) +
      ggplot2::scale_x_discrete(breaks = every_nth)
  } else if (geom == "area") {
    time_levels <- levels(x$time)
    x$time <- as.numeric(x$time)
    p <- ggplot2::ggplot(
      x,
      ggplot2::aes(
        x = !!rlang::sym("time"),
        fill = !!rlang::sym("state")
      )
    ) +
      ggplot2::geom_area(position = position, stat = "count") +
      ggplot2::scale_x_continuous(
        breaks = every_nth(seq_along(time_levels)),
        labels = every_nth(time_levels)
      )
  }
  p <- p +
    ggplot2::scale_fill_manual(
      values = colors,
      name = legend_title,
      na.value = na_color
    ) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_reverse() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "right"
    )
  if (!missing(group)) {
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(!!rlang::sym(group)),
      ncol = ncol,
      scales = "free_y"
    )
  }
  p
}

#' Create a heatmap from edgelist data
#'
#' @param data A `data.frame` with source and target columns and edge weights.
#' @param title A `character` string giving the plot title.
#' @noRd
create_heatmap <- function(data, title) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = !!rlang::sym("target"),
      y = !!rlang::sym("source"),
      fill = !!rlang::sym("value")
    )) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "blue",
      high = "red",
      mid = "white",
      limits = c(-1, 1),
      na.value = "grey50"
    ) +
    ggplot2::labs(title = title, x = "Target", y = "Source") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 12, face = "bold")
    )
}

# Groups ----------------------------------------------------------------

#' Plot a Histogram of Edge Weights for a `group_tna` Object.
#'
#' @export
#' @family basic
#' @param x A `group_tna` object.
#' @param ... Additional arguments passed to [graphics::hist()].
#' @return A `list` (invisibly) of `histogram` objects of the edge weights of
#'   each cluster.
#' @examples
#' model <- group_model(engagement_mmm)
#' hist(model)
#'
hist.group_tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  invisible(lapply(x, hist.tna, ...))
}

#' Plot a Grouped Transition Network Analysis Model
#'
#' Plots a transition network of each cluster using `qgraph`.
#'
#' @export
#' @family basic
#' @param x A `group_model` object.
#' @param title A title for each plot. It can be a single string (the same one
#'   will be used for all plots) or a list (one per group)
#' @param which An optional `integer` vector of groups to plot. By default, all
#'   groups are plotted.
#' @param ... Same as [plot.tna()].
#' @return `NULL` (invisibly).
#' @inheritDotParams plot.tna
#' @examples
#' model <- group_model(engagement_mmm)
#' plot(model, which = 1)
#'
plot.group_tna <- function(x, title, which, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  title <- title %m% names(x)
  title <- rep_len(title, length(x))
  which <- which %m% seq_along(x)
  stopifnot_(
    all(which %in% seq_along(x)),
    "Argument {.arg which} must only contain valid cluster indices."
  )
  for (i in which) {
    plot(x[[i]], title = title[i], ...)
  }
  invisible(NULL)
}

#' Plot a Bootstrapped Grouped Transition Network Analysis Model
#'
#' @export
#' @family validation
#' @param x A `group_tna_bootstrap` object.
#' @param title A `character` vector of titles to use for each plot.
#' @param ... Additional arguments passed to [plot.tna()].
#' @examples
#' model <- group_model(engagement_mmm)
#' # Small number of iterations for CRAN
#' boot <- bootstrap(model, iter = 10)
#' plot(boot)
#'
plot.group_tna_bootstrap <- function(x, title, ...) {
  check_missing(x)
  check_class(x, "group_tna_bootstrap")
  title <- title %m% names(x)
  title <- rep_len(title, length(x))
  invisible(
    Map(function(y, i) plot.tna_bootstrap(y, title = i, ...), x, title)
  )
}

#' Plot Centrality Measures
#'
#' @export
#' @family centralities
#' @param x A `group_tna_centralities` object.
#' @param palette A color palette to be applied if `colors` is not specified.
#' @inheritParams plot.tna_centralities
#' @return A `ggplot` object displaying a line chart for each centrality
#'   with one line per cluster.
#' @examples
#' model <- group_model(engagement_mmm)
#' cm <- centralities(model)
#' plot(cm)
#'
plot.group_tna_centralities <- function(x, reorder = TRUE, ncol = 3,
                                        scales = c("free_x", "fixed"),
                                        colors, palette = "Set2",
                                        labels = TRUE, ...) {
  check_missing(x)
  check_class(x, "group_tna_centralities")
  plot_centralities_(x, reorder, ncol, scales, colors, palette, labels)
}

#' Plot Found Cliques
#'
#' @export
#' @family cliques
#' @param x A `group_tna_cliques` object.
#' @param title A `character` vector of titles to use for each plot.
#' @param ... Arguments passed to [plot.tna_cliques()].
#' @return A `list` (invisibly) with one element per cluster. Each element
#'   contains a `qgraph` plot when only one clique is present per cluster,
#'   otherwise the element is `NULL`.
#' @examples
#' model <- group_model(engagement_mmm)
#' cliq <- cliques(model, size = 2)
#' plot(cliq, ask = FALSE)
#'
plot.group_tna_cliques <- function(x, title, ...) {
  check_missing(x)
  check_class(x, "group_tna_cliques")
  title <- title %m% names(x)
  title <- rep_len(title, length(x))
  invisible(
    Map(function(y, i) plot.tna_cliques(y, title = i, ...), x, title)
  )
}

#' Plot Detected Communities
#'
#' @export
#' @family communities
#' @param x A `group_tna_communities` object.
#' @param title A `character` vector of titles to use for each plot.
#' @param colors A `character` vector of colors to use.
#' @param ... Arguments passed to [plot.tna_communities()].
#' @return A `list` (invisibly) of `qgraph` objects in which the nodes are
#'   colored by community for each cluster.
#' @examples
#' model <- group_model(engagement_mmm)
#' comm <- communities(model)
#' plot(comm)
#'
plot.group_tna_communities <- function(x, title, colors, ...) {
  check_missing(x)
  check_class(x, "group_tna_communities")
  n <- length(x)
  colors <- colors %m% default_colors
  colors <- rep_len(colors, n)
  title <- title %m% names(x)
  title <- rep_len(title, n)
  invisible(
    Map(
      function(y, i, j) {
        plot(y, title = i, colors = j, ...)
      },
      x,
      title,
      colors
    )
  )
}

#' Plot Centrality Stability Results
#'
#' @export
#' @family validation
#' @param x A `group_tna_stability` object.
#' @param ... Arguments passed to [plot.tna_stability()].
#' @return A `list` (invisibly) of `ggplot` objects displaying the stability
#'   analysis plot.
#' @examples
#' model <- group_model(engagement_mmm)
#' # Low number of iterations for CRAN
#' stability <- estimate_cs(
#'   model,
#'   drop_prop = c(0.3, 0.5, 0.7, 0.9),
#'   iter = 10
#' )
#' plot(stability)
#'
plot.group_tna_stability <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna_stability")
  invisible(lapply(x, plot.tna_stability, ...))
}

#' Plot Permutation Test Results
#'
#' @export
#' @family validation
#' @param x A `group_tna_permutation` object.
#' @param title An optional `character` vector of titles for each plot.
#'   When not provided, the title shows the names of the clusters being
#'   contrasted.
#' @param ... Arguments passed to [plot.tna_permutation()].
#' @return A `list` (invisibly) of `qgraph` objects depicting the significant
#'   difference between each pair.
#' @examples
#' model <- group_tna(engagement_mmm)
#' # Small number of iterations for CRAN
#' perm <- permutation_test(model, iter = 20)
#' plot(perm)
#'
plot.group_tna_permutation <- function(x, title, ...) {
  check_missing(x)
  check_class(x, "group_tna_permutation")
  title <- title %m% names(x)
  invisible(
    lapply(
      seq_along(x),
      function(i) plot.tna_permutation(x[[i]], title = title[i], ...)
    )
  )
}

#' Plot the Difference Network Between Two Groups
#'
#' @export
#' @family comparison
#' @param x A `group_tna` object.
#' @param i An `integer` index or the name of the principal cluster as a
#' `character` string.
#' @param j An `integer` index or the name of the secondary cluster as a
#' `character` string.
#' @param ... Additional arguments passed to [plot_compare.tna()].
#' @return A `qgraph` object displaying the difference network between the
#'   two clusters
#' @examples
#' model <- group_model(engagement_mmm)
#' plot_compare(model)
#'
plot_compare.group_tna <- function(x, i = 1L, j = 2L, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  check_clusters(x, i, j)
  plot_compare(x = x[[i]], y = x[[j]], ...)
}

#' Plot the Frequency Distribution of States
#'
#' @export
#' @family basic
#' @param x A `group_tna` object.
#' @param label An optional `character` string that can be provided to specify
#'   the grouping factor name if `x` was not constructed using a column name of
#'   the original data.
#' @param colors A `character` vector of colors to be used in the plot
#'   (one per group).
#' @param palette A `character` string that specifies the palette to be used if
#'   `colors` are not passed.
#' @param width A `numeric` value for the width of the bars.
#'   The default is `0.7`.
#' @param hjust A `numeric` value for the horizontal adjustment of the labels.
#'   The default is `1.2`.
#' @param position Position of the bars:`"dodge"`, `"dodge2"`, `"fill"` or
#'   `"stack"`.
#' @param show_label A `logical` value indicating whether to show a label with
#'   the frequency counts. Default is `TRUE`.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @examples
#' model <- group_model(engagement_mmm)
#' # Default
#' plot_frequencies(model)
#' # Default labels outside and custom colors
#' plot_frequencies(
#'   model,
#'   width = 0.9,
#'   hjust = -0.3,
#'   colors = c("#218516", "#f9c22e", "#53b3cb")
#' )
#' # Stacked with no labels
#' plot_frequencies(model, position = "stack", show_label = FALSE)
#' # Fill
#' plot_frequencies(model, position = "fill", hjust = 1.1)
#'
plot_frequencies.group_tna <- function(x, label, colors, width = 0.7,
                                       palette = "Set2",
                                       show_label = TRUE, position = "dodge",
                                       hjust = 1.2, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  label <- label %m% attr(x, "label")
  combined <- combine_data(x)
  long <- tidyr::pivot_longer(combined, cols = !(!!rlang::sym(".group")))
  check_values(width, type = "numeric")
  check_numeric(hjust)
  check_flag(show_label)
  long$value <- factor(x[[1L]]$labels[long$value], levels = rev(x[[1L]]$labels))
  long$.group <- factor(long$.group)
  position <- check_match(position, c("dodge", "dodge2", "fill", "stack"))
  position <- switch(position,
    dodge = ggplot2::position_dodge(width = width),
    dodge2 = ggplot2::position_dodge2(width = width),
    stack = "stack",
    fill = "fill"
  )
  d <- long |>
    dplyr::group_by(!!rlang::sym(".group"), !!rlang::sym("value")) |>
    dplyr::summarize(freq = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(!!rlang::sym("value")))

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      y = !!rlang::sym("value"),
      x = !!rlang::sym("freq"),
      fill = !!rlang::sym(".group"))
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      colour = "black",
      position = position,
      width = width
    )
  if(show_label) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = !!rlang::sym("freq")),
        position = position,
        hjust = hjust
      )
  }
  if (!missing(colors)) {
    colors <- rep(colors, length.out = length(x))
    p <- p + ggplot2::scale_fill_manual(name = label, values = colors)
  } else {
    p <- p + ggplot2::scale_fill_brewer(name = label, palette = palette)
  }
  p +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::labs(y = "State", x = "Frequency") +
    ggplot2::theme(
      legend.position =  "bottom",
      axis.title =  ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(color = "black"),
      text = ggplot2::element_text(color = "black"))
}

#' Plot State Frequencies as a Mosaic Between Two Groups
#'
#' @export
#' @family basic
#' @param x A `tna_data` object.
#' @param group A `character` string giving the column name of the (meta) data
#'   to contrast the frequencies with or a vector of group indicators with the
#'   the same length as the number of rows in the sequence data.
#' @param label An optional `character` string that specifies a label for the
#'   grouping variable when `group` is not a column name of the data.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @examples
#' d <- data.frame(
#'   time = rep(1:5, rep = 4),
#'   group = rep(1:4, each = 5),
#'   event = sample(LETTERS[1:3], 20, replace = TRUE)
#' )
#' sequence_data <- prepare_data(
#'   d,
#'   time = "time",
#'   actor = "group",
#'   action = "event"
#' )
#' plot_mosaic(sequence_data, group = "group")
#'
plot_mosaic.tna_data <- function(x, group, label = "Group", ...) {
  check_missing(x)
  check_class(x, "tna_data")
  check_missing(group)
  check_string(label)
  n_group <- length(group)
  stopifnot_(
    n_group == nrow(x$sequence_data) || n_group == 1L,
    "Argument {.arg group} must be of length one or the same length as the
     number of rows/sequences in {.arg x}."
  )
  if (n_group == 1L) {
    stopifnot_(
      group %in% names(x$meta_data),
      "Argument {.arg group} must be a column name of the input data
       when of length one."
    )
    group <- x$meta_data[[group]]
  }
  group <- ifelse_(is.factor(group), group, factor(group))
  wide <- cbind(x$sequence_data, group)
  names(wide) <- c(names(x$sequence_data), ".group")
  long <- wide |>
    tidyr::pivot_longer(cols = !(!!rlang::sym(".group"))) |>
    tidyr::drop_na()
  tab <- table(long$.group, long$value)
  plot_mosaic_(
    tab,
    xlab = label,
    ylab = "State"
  )
}

#' Plot State Frequencies as a Mosaic Between Two Groups
#'
#' @export
#' @family basic
#' @param x A `group_tna` object.
#' @param label An optional `character` string that can be provided to specify
#'   the grouping factor name if `x` was not constructed using a column name of
#'   the original data.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @examples
#' model <- group_model(engagement, group = rep(1:3, length.out = 1000))
#' plot_mosaic(model)
#'
plot_mosaic.group_tna <- function(x, label, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  label <- label %m% attr(x, "label")
  combined <- combine_data(x)
  long <- tidyr::pivot_longer(combined, cols = !(!!rlang::sym(".group")))
  labels <- x[[1L]]$labels
  use_na <- ifelse_(attr(x, "na.rm"), "no", "ifany")
  tab <- table(long$.group, long$value, useNA = use_na)
  dimnames(tab) <- list(attr(x, "levels"), labels)
  plot_mosaic_(
    tab,
    xlab = label,
    ylab = "State"
  )
}

#' @export
#' @rdname plot_sequences
plot_sequences.group_tna <- function(x, type = "index", scale = "proportion",
                                     geom = "bar", include_na = FALSE,
                                     na_color = "white", sort_by, show_n = TRUE,
                                     border, title, legend_title, xlab, ylab,
                                     tick = 1, ncol = 2L, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  d <- combine_data(x)
  cols <- setdiff(names(d), ".group")
  group <- ".group"
  colors <- attr(x[[1L]]$data, "colors")
  lab <- x[[1L]]$labels
  lev <- seq_along(lab)
  plot_sequences_(
    d, lev, lab, cols, group, type, scale, geom, include_na, colors,
    na_color, rlang::enquo(sort_by), show_n, border, title, legend_title,
    xlab, ylab, tick, ncol
  )
}

# Associations ------------------------------------------------------------

#' Plot an Association Network
#'
#' @export
#' @rdname plot_associations
#' @param x A `tna` object.
#' @param edge.color An optional `character` vector of colors for the edges.
#'   By default, the colors are specified by the magnitude of the
#'   standardized residual.
#' @param ... Additional arguments passed to [plot_model()].
#' @return A `qgraph` plot of the network.
#' @examples
#' model <- ftna(group_regulation)
#' plot_associations(model)
#'
plot_associations <- function(x, ...) {
  UseMethod("plot_associations")
}

#' @export
#' @rdname plot_associations
plot_associations.tna <- function(x, edge.color, ...) {
  check_missing(x)
  check_class(x, "tna")
  stopifnot_(
    attr(x, "type") %in% c("frequency", "co-occurrence"),
    "Association network plots are supported only for
     integer-valued weight matrices."
  )
  tab <- as.table(t(x$weights))
  chisq <- suppressWarnings(stats::chisq.test(tab))
  res <- as.numeric(chisq$stdres)
  dim(res) <- dim(tab)
  if (missing(edge.color)) {
    edge.color <- matrix(NA_character_, ncol = ncol(res))
    edge.color[res > 4] <- "#4A6FE3"
    edge.color[res > 2 & res <= 4] <- "#9DA8E2"
    edge.color[res <= 2 & res >= -2] <- "#E2E2E2"
    edge.color[res < -2 & res >= -4] <- "#E495A5"
    edge.color[res < -4] <- "#D33F6A"
  }
  plot_model(res, edge.color = edge.color, labels = x$labels, ...)
}

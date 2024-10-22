#' Plot a Transition Network Analysis Model
#'
#' This function plots a transition network analysis (TNA) model using
#' the `qgraph` package. The nodes in the graph represent states, with node
#' sizes corresponding to initial state probabilities. Edges between nodes
#' represent the transition probabilities.
#'
#' @export
#' @param x A `tna` object from [tna::build_tna()].
#' @param cluster Index of the primary cluster to visualize.
#'   Defaults to the first cluster.
#' @param cluster2 Optional index of the secondary cluster. If specified,
#'   The difference between the transition probabilities of `cluster` and
#'   `cluster2` will be plotted.
#' @param colors See [qgraph::qgraph()].
#' @param edge.labels See [qgraph::qgraph()].
#' @param labels See [qgraph::qgraph()].
#' @param layout See [qgraph::qgraph()].
#' @param mar See [qgraph::qgraph()].
#' @param pie See [qgraph::qgraph()].
#' @param cut See [qgraph::qgraph()].
#' @param minimum See [qgraph::qgraph()].
#' @param theme See [qgraph::qgraph()].
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` plot of the transition network.
#' @family core
#' @examples
#' tna_model <- build_tna(engagement)
#' plot(tna_model)
#'
plot.tna <- function(x, cluster = 1, cluster2, colors = x$colors,
                     edge.labels = TRUE, labels = x$labels, layout = "circle",
                     mar = rep(5, 4), pie = x$inits[[cluster]],
                     cut = 0.1, minimum = 0.05, theme = "colorblind", ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  stopifnot_(
    checkmate::test_integerish(
      x = cluster,
      lower = 1,
      upper = length(x$weights),
      any.missing = FALSE,
      len = 1,
      null.ok = FALSE
    ),
    "Argument {.arg cluster} must be a single integer value between 1 and
     the number of clusters."
  )
  stopifnot_(
    missing(cluster2) || checkmate::test_integerish(
      x = cluster2,
      lower = 1,
      upper = length(x$weights),
      any.missing = FALSE,
      len = 1,
      null.ok = FALSE
    ),
    "Argument {.arg cluster2} must be a single integer value between 1 and
     the number of clusters."
  )
  cluster <- as.integer(cluster)
  cluster2 <- onlyif(!missing(cluster2), as.integer(cluster2))
  qgraph::qgraph(
    input = ifelse_(
      is.null(cluster2),
      x$weights[[cluster]],
      x$weights[[cluster]] - x$weights[[cluster2]]
    ),
    color = colors,
    minimum = minimum,
    cut = cut,
    edge.labels = edge.labels,
    labels = labels,
    layout = layout,
    pie = onlyif(is.null(cluster2), pie),
    mar = mar,
    theme = theme,
    ...
  )
}

# TODO is this needed
#' #' Plot Transition Networks for All Clusters
#' #'
#' #' This function plots the transition networks for each cluster in a `tna` object.
#' #' It iterates through the transition matrices for each cluster and generates
#' #' corresponding plots using the `plot.tna` function.
#' #'
#' #' @param x A `tna` object containing transition matrices for different clusters.
#' #' @param ... Additional arguments to be passed to the `plot.tna` function.
#' #'
#' #' @return A series of plots showing the transition networks for each cluster.
#' #' @family core
#' #' @examples
#' #' \dontrun{
#' #' # Assuming `tna_model` is a tna object containing transition matrices
#' #' plot_clusters(tna_model)
#' #' }
#' #' @export
#' plot_clusters <- function(x, ...) {
#'   stopifnot_(
#'     is_tna(x),
#'     "Argument {.arg x} must be a {.cls tna} object."
#'   )
#'   result <- list()
#'   matrices <- x$transits
#'   for (clus in seq_along(matrices)) {
#'     plot.tna(x, cluster = clus, ...)
#'   }
#' }


#' Plot Centrality Measures
#'
#' Plots the centrality measures of a `tna_centralities` object as a
#' lollipop chart. The resulting plot includes facets for each centrality
#' measure, showing the values for each state. The returned plot is a
#' `ggplot2` object, so it can be easily modified and styled. See
#' [centralities()] for details on the centrality measures.
#'
#' @export
#' @param x An object of class `tna_centralities`.
#' @param ncol Number of columns to use for the facets. The default is 3.
#' @param scales Either `"fixed"` or `"free_x"` (the default). If `"free_x"`,
#'   the horizontal axis is scaled individually in each facet. If `"fixed"`,
#'   the same values are used for all axes.
#' @param reorder A `logical` value indicating whether to reorder the values
#'   for each centrality in a descending order. The default is `TRUE`.
#' @param model An object of class `tna`.
#' @param colors The colors for each node (default is the model colors
#'  if the `tna` model object is passed, otherwise `"black"`).
#' @param labels A `logical` value indicating whether to show the centrality
#'   numeric values. The default is `TRUE`.
#' @param ... Ignored.
#' @family core
#' @return A `ggplot` object displaying the lollipop charts for each centrality
#'   measure.
#' @examples
#' tna_model <- build_tna(engagement)
#' cm <- centralities(tna_model)
#' plot(cm)
#' plot(cm, ncol = 4, reorder = TRUE)
#'
plot.tna_centralities <- function(x, model = NULL, reorder = TRUE,
                              ncol = 3, scales = c("free_x", "fixed"),
                              colors = NULL, labels = TRUE, ...) {
  stopifnot_(
    is_centralities(x),
    "Argument {.arg x} must be a {.cls tna_centralities} object."
  )
  stopifnot_(
    checkmate::test_flag(x = reorder),
    "Argument {.arg reorder} must be a single {.cls logical} value."
  )
  stopifnot_(
    checkmate::test_flag(x = labels),
    "Argument {.arg labels} must be a single {.cls logical} value."
  )
  stopifnot_(
    is.null(model) | is_tna(model),
    "Argument {.arg modes} must be a single {.cls tna} model or empty."
  )

  # TODO some default colors function here that computes them from seq
  # without using seqHMM or TraMineR
  if (is.null(colors) & is_tna(model) & !is.null(model$colors)) {
    colors <- model$colors
  } else if (is.null(colors)) {
    colors = rep("black", length.out = length(unique(x$State)))
  } else if (length(colors) == 1){
    colors = rep(colors, length.out = length(unique(x$State)))
  }
  scales <- onlyif(is.character(scales), tolower(scales))
  scales <- try(match.arg(scales, c("free_x", "fixed")), silent = TRUE)
  stopifnot_(
    !inherits(scales, "try-error"),
    "Argument {.arg scales} must be either {.val free_x} or {.val fixed}."
  )
  scales <- ifelse_(scales == "free_x", "free", "free_y")
  ifelse_(
    "Cluster" %in% names(x),
    plot_centralities_multiple(x, ncol, scales, colors, labels),
    plot_centralities_single(x, reorder, ncol, scales, colors, labels)
  )
}

plot.tna_cliques <- function(x, ...) {
  stopifnot_(
    is_cliques(x),
    "Argument {.arg x} must be a {.cls tna_cliques} object."
  )
  # TODO
}

#' Plot Centrality Stability Results
#'
#' This function visualizes the centrality stability results produced by the
#' `estimate_centrality_stability` function. It shows how different centrality
#' measures' correlations change as varying proportions of cases are dropped, along with their confidence intervals (CIs).
#'
#' @export
#' @param stability_results A list of stability results produced by `estimate_centrality_stability`. Each centrality measure contains:
#' - `correlations`: A matrix of correlations between the original centralities and resampled centralities for different proportions of dropped cases.
#' - `cs_coefficient`: The centrality stability (CS) coefficient, which represents the proportion of dropped cases where the correlation drops below the specified threshold.
#'
#' @details
#' The function aggregates the results for each centrality measure across multiple proportions of dropped cases (e.g., 0.1, 0.2, ..., 0.9) and calculates the mean and standard deviation for each proportion. The confidence intervals (CIs) are computed based on the standard deviations and displayed in the plot.
#'
#' If no valid data is available for a centrality measure (e.g., missing or NA values), the function skips that measure with a warning.
#'
#' The plot includes:
#' - The mean correlation for each centrality measure as a function of the proportion of dropped cases.
#' - Shaded confidence intervals representing the 95% CIs for each centrality measure.
#' - A horizontal dashed line at y = 0.7, indicating the typical threshold used for calculating the CS-coefficient.
#' - A subtitle listing the CS-coefficients for each centrality measure.
#'
#' @return A `ggplot` object displaying the stability analysis plot.
#'
#' @examples
#' \dontrun{
#' # Assuming 'stability_results' is a list from estimate_centrality_stability()
#' plot_stability_results(stability_results)
#' }
#'
plot.tna_stability <- function(stability_results) {
  plot_data <- data.frame()  # Initialize an empty dataframe to store all results
  cs_subtitle <- ""  # String to accumulate CS-coefficients for the subtitle

  # Iterate over all centrality measures, except detailed results
  for (measure_name in names(stability_results)) {
    if (measure_name != "detailed_results") {
      measure_results <- stability_results[[measure_name]]$correlations

      # Debugging step: check if measure_results has valid dimensions
      if (is.null(dim(measure_results)) || nrow(measure_results) == 0 || ncol(measure_results) == 0) {
        cat(sprintf("Warning: No valid data for measure '%s'. Skipping.\n", measure_name))
        next
      }

      means <- apply(measure_results, 1, mean, na.rm = TRUE)
      sds <- apply(measure_results, 1, sd, na.rm = TRUE)

      # Check for NaN/NA values in the standard deviations
      if (any(is.nan(sds)) || any(is.na(sds))) {
        cat(sprintf("Warning: NA or NaN values detected in the standard deviations for '%s'.\n", measure_name))
      }

      proportions <- seq(0.1, 0.9, by = 0.1)

      if (length(means) == length(proportions)) {
        measure_data <- data.frame(
          proportion = proportions,
          correlation = means,
          lower = means - 2 * sds,  # 95% CI lower bound
          upper = means + 2 * sds,  # 95% CI upper bound
          measure = measure_name
        )

        # Ensure no negative values for CIs
        measure_data$lower <- pmax(measure_data$lower, 0)
        measure_data$upper <- pmin(measure_data$upper, 1)

        plot_data <- rbind(plot_data, measure_data)

        # Collect CS-coefficients for the subtitle
        cs_coef <- stability_results[[measure_name]]$cs_coefficient
        cs_subtitle <- paste0(cs_subtitle, measure_name, ": CS = ", round(cs_coef, 2), "; ")
      } else {
        cat(sprintf("Warning: Length mismatch between means and proportions for '%s'.\n", measure_name))
      }
    }
  }

  if (nrow(plot_data) == 0) {
    stop("Error: No valid data to plot.")
  }

  # Remove the trailing semicolon and space from the subtitle
  cs_subtitle <- gsub("; $", "", cs_subtitle)

  # Create the ggplot with CIs (geom_ribbon) and CS-coefficients in the subtitle
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = proportion, y = correlation, color = measure)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = measure), alpha = 0.2) +  # Confidence intervals
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0.7, linetype = "dashed", color = "gray50") +
    ggplot2::labs(
      title = "Centrality Stability Analysis",
      subtitle = paste("CS-Coeficients: ", cs_subtitle),  # Include CS-coefficients as a subtitle
      x = "Proportion of Cases Dropped",
      y = "Correlation with Original Centrality",
      color = "Centrality Measure",
      fill = "Centrality Measure"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::ylim(0, 1)

  # Print the plot
  print(p)
}

plot_centralities_single <- function(x, reorder, ncol, scales, colors, labels) {
  x <- stats::reshape(
    as.data.frame(x),
    idvar = "State",
    ids = x[["State"]],
    times = names(x)[-1L],
    timevar = "name",
    drop = "State",
    varying = list(names(x)[-1L]),
    direction = "long",
    v.names = "value"
  )
  x <- ifelse_(
    reorder,
    dplyr::arrange(
      x, !!rlang::sym("name"), !!rlang::sym("value")
    ),
    dplyr::arrange(
      x, !!rlang::sym("name"), dplyr::desc(!!rlang::sym("State"))
    )
  ) |>
    dplyr::mutate(rank = dplyr::row_number())
  ggplot2::ggplot(x) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_col(
      ggplot2::aes(
        fill = !!rlang::sym("State"),
        x = !!rlang::sym("rank"),
        y = !!rlang::sym("value")
      ),
      size = 4
    ) +
    ggplot2::coord_flip(clip = "off") +
    onlyif(
      labels,
      ggplot2::geom_text(
        ggplot2::aes(
          label = round(!!rlang::sym("value"), 2),
          x = !!rlang::sym("rank"),
          y = !!rlang::sym("value")
        ),
        # vjust = 2,
        hjust = 1,
        size = 3
      )
    ) +
    ggplot2::facet_wrap(~name, ncol = ncol, scales = scales) +
    ggplot2::scale_x_continuous(
      name = NULL,
      expand = c(0, 0.5),
      breaks = x$rank,
      labels = x$State,
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      axis.text.y = ggplot2::element_text(size = 8),
      panel.spacing = ggplot2::unit(2, "lines"),
      plot.margin = ggplot2::margin(5.5, 11, 5.5, 5.5, "points")
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("")
}

plot_centralities_multiple <- function(x, ncol, scales, colors, labels) {
  measures <- names(x)[names(x) %in% valid_measures]
  n_clusters <- length(unique(x$Cluster))
  dplyr::mutate(x, Cluster = factor(!!rlang::sym("Cluster"))) |>
    data.frame() |>
    stats::reshape(
      varying = themeasures,
      v.names = "value",
      timevar = "name",
      times = themeasures,
      direction = "long"
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!rlang::sym("value"),
        y = !!rlang::sym("State"),
        color = !!rlang::sym("Cluster"),
        fill = !!rlang::sym("Cluster"),
        group = !!rlang::sym("Cluster")
      )
    ) +
    ggplot2::facet_wrap("name", ncol = 4) +
    ggplot2::geom_path() +
    ifelse_(
      length(unique(colors)) == n_clusters,
      ggplot2::scale_color_manual(values = colors),
      ggplot2::scale_color_discrete()
    ) +
    ggplot2::geom_point(size = 2, shape = 21, stroke = NA) +
    ifelse_(
      length(unique(colors)) == n_clusters,
      ggplot2::scale_fill_manual(values = colors),
      ggplot2::scale_fill_discrete()
    ) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Centrality") +
    ggplot2::ylab("") +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(1, "lines"),
      legend.position = "bottom"
    )
}

#' Plot Communities
#'
#' This function visualizes the communities detected within a `tna` object
#' based on different community detection algorithms and their corresponding
#' color mappings.
#'
#' @export
#' @param x A `tna` object, which represents a transition network analysis
#' object containing data to be plotted. The object should include a component `weights`
#' that can be iterated over to plot individual communities.
#' @param y A `communities` object generated by the `find_communities` method.
#' Each community detection method maps nodes or points in `x`  to specific
#' communities.
#' @param cluster Index of the cluster for which to produce the plot. Defaults
#' to the first cluster.
#' @param community A `character` string naming a community detection method to
#' use for coloring the plot. This can be one of the following:
#'
#' * `"walktrap"`: A community detection method using short random walks.
#' * `"fast_greedy"`: A method based on modularity optimization.
#' * `"label_prop"`: A method that uses label propagation.
#' * `"infomap"`: A method that uses information flow to detect communities.
#' * `"edge_betweenness"`: A method that uses edge betweenness to find
#'   communities.
#' * `"leading_eigen"`: A method using the leading eigenvector of the
#'   modularity matrix.
#' * `"spinglass"`: A method based on the spinglass model.
#'
#' @param colors A `character` vector of color values used for visualizing
#'   community assignments.
#' @family patterns
#' @examples
#' \dontrun{
#' plot_communities(tna_model, community_assignment, "walktrap")
#' }
#'
plot_communities <- function(x, y, cluster = 1L, community, colors) {
    # print((community_assignment[i][[1]]$community_assignments[,community]))
    # print(map_to_color(community_assignment[i][[1]]$community_assignments[,community], color_palette))
  # TODO for loop?, wont work with qplot
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  stopifnot_(
    is_communities(y),
    "Argument {.arg y} must be a {.cls communities} object."
  )
  colors <- ifelse_(
    is.missing(colors),
    default_colors,
    colors
  )
  plot(
    x,
    cluster = cluster,
    colors = map_to_color(y[[cluster]]$assignment[, community], colors)
  )
}

#' Plot the difference network between two models
#'
#' Plots the difference network between model `x` and model `y`. The edges are
#' computed from subtracting the two models. The pie chart is the difference in
#' initial probabilities between model `x` and model `y`. Green color indicates
#' that `x`is greater than `y`and red indicates otherwise.
#'
#' @export
#' @rdname plot_compare
#' @param x An object of class `tna`. It will be the principal model.
#' @param y An object of class `tna`. It will be the model subtracted from the
#'   principal model.
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` object displaying the difference network between the
#'   two models.
#' @family core
#' @examples
#' tna_model_1 <- build_tna(engagement[engagement[, 1] == "Active", ])
#' tna_model_2 <- build_tna(engagement[engagement[, 1] != "Active", ])
#' plot_compare(tna_model_1, tna_model_2)
#'
plot_compare <- function(x, y, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  stopifnot_(
    is_tna(y),
    "Argument {.arg y} must be a {.cls tna} object."
  )
  stopifnot_(
    all(x$labels == y$labels),
    "{.arg x} and {.arg y} must have the same labels."
  )
  pie <- abs(x$inits[[1]] - y$inits[[1]])
  piesign <- ifelse(x$inits[[1]] > y$inits[[1]], "#009900", "red")
  #pos_col <- c("#009900", "darkgreen")
  #neg_col <- c("#BF0000", "red")
  diff <- build_tna(x$weights[[1]] - y$weights[[1]], pie)
  plot.tna(
    diff,
    pie = pie,
    pieColor = piesign,
    color = x$colors,
    theme = NULL,
    palette = "colorblind",
    ...
  )
}

# Default Colors ------------------------------------------------------------------
default_colors <- c("#d1ea2c", "#fd5306", "#68b033", "#8601b0", "#fe2712", "#a7184d", "#3c02a6",  "#fd9a01", "#0392ce")

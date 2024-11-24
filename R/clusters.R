#' Build a grouped Transition Network Analysis Model
#'
#' @param x An `stslist` or `data.frame` object
#' @return A `group_tna` object
#' @export
#' @family clusters
#' @rdname group_tna
group_tna <- function(x, ...) {
  UseMethod("group_tna")
}

#' Build a grouped Transition Network Analysis Model providing group/cluster assignments
#'
#' @param x An `stslist` or `data.frame` object
#' @param group A vector indicating the cluster assignment of each
#' row of the data / sequence
#' @param ... Same as `tna`
#' @return A `group_tna` object
#' @export
#' @family clusters
#' @rdname group_tna
group_tna.default <- function(x, group, ...) {
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
    clusters[[i]] <- tna(x[group == i, ], ...)
  }
  structure(clusters, class = "group_tna")
}

#' Build a grouped Transition Network Analysis Model providing an MMM model from `seqHMM`
#'
#' @param x An `mhmm` object from `seqHMM`
#' @param ... Same as `tna`
#' @return A `group_tna` object
#' @export
#' @family clusters
#' @rdname group_tna
group_tna.mhmm <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )

  group <- (summary(x))$most_probable_cluster
  group_tna.default(x$observations, group = group, ...)
}



#' Check that argument is an object of class `group_tna`
#'
#' @param x An \R object.
#' @family clusters
#' @noRd
is_group_tna <- function(x) {
  inherits(x, "group_tna")
}

#' Check that argument is an object of class `group_tna_centralities`
#'
#' @param x An \R object.
#' @family clusters
#' @noRd
is_group_tna_centralities <- function(x) {
  inherits(x, "group_tna_centralities")
}

#' Check that argument is an object of class `group_tna_bootstrap`
#'
#' @param x An \R object.
#' @family clusters
#' @noRd
is_group_tna_bootstrap <- function(x) {
  inherits(x, "group_tna_bootstrap")
}

#' Check that argument is an object of class `group_tna_communities`
#'
#' @param x An \R object.
#' @family clusters
#' @noRd
is_group_tna_communities <- function(x) {
  inherits(x, "group_tna_communities")
}


#' Check that argument is an object of class `group_tna_cliques`
#'
#' @param x An \R object.
#' @family clusters
#' @noRd
is_group_tna_cliques <- function(x) {
  inherits(x, "group_tna_cliques")
}

#' Check that argument is an object of class `group_tna_stability`
#'
#' @param x An \R object.
#' @family clusters
#' @noRd
is_group_tna_stability <- function(x) {
  inherits(x, "group_tna_stability")
}

#' Plot a grouped Transition Network Analysis Model
#'
#' @param x A `group_tna` object.
#' @param title A title for each plot. It can be a single string (the same one
#'  will be used for all plots) or a list (one per group)
#' @param ... Same as `plot.tna`
#' @return NULL
#' @family clusters
#' @export
plot.group_tna <- function(x, title, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
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

#' Convert a specific group from a grouped Transition Network Analysis Model into an `igraph` object
#'
#' @param x A `group_tna` object.
#' @param which The number or name of group.
#' @return An `igraph` object
#' @family clusters
#' @export
as.igraph.group_tna <- function(x, which){
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
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

#' Summarize a grouped Transition Network Analysis Model
#'
#' @param x A `group_tna` object.
#' @param combined A logical indicating whether the summary results should be
#' combined into a single dataframe for all clusters (defaults to `TRUE`)
#' @return A `data.frame` object if combined is `FALSE` or a `list` if
#' combined is `TRUE`
#' @family clusters
#' @export
summary.group_tna <- function(x, combined = TRUE) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )
  if(combined == FALSE) {
    lapply(x, \(i) summary.tna(i))
  } else {
    dplyr::bind_rows(lapply(x, \(i) summary.tna(i)), .id = "Cluster") |>
      tidyr::pivot_wider(names_from = "Cluster", values_from = "value")
  }
}

#' Prune a grouped Transition Network Analysis Model
#'
#' @param x A `group_tna` object.
#' @param ... Same as `prune.tna`
#' @return A `group_tna` object
#' @family clusters
#' @export
prune.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )

  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )

  structure(
    lapply(x, \(i) prune.tna(i, ...)),
    class = "group_tna"
  )
}


#' @family clusters
#' @rdname pruning_details
#' @export
pruning_details.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )

  Map(function(y, i) {print(i); pruning_details.tna(y, ...)}, x, names(x))

}
#' @family clusters
#' @rdname deprune
#' @export
deprune.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )
  structure(
    lapply(x, \(i) deprune.tna(i, ...)),
    class = "group_tna"
  )
}

#' @family clusters
#' @rdname reprune
#' @export
reprune.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )
  structure(
    lapply(x, \(i) reprune.tna(i, ...)),
    class = "group_tna"
  )
}

#' @family clusters
#' @export
print.group_tna_bootstrap <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna_bootstrap(x),
    "Argument {.arg x} must be of type `group_tna_bootstrap`"
  )
  lapply(x, \(i) print.tna_bootstrap(i, ...))
}

#' Print the summary of a grouped Transition Network Analysis Model
#'
#' @param x A `group_tna` object.
#' @param ... Ignored
#' @family clusters
#' @export
print.summary.group_tna  <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )
  if (is.list(x)) {
    lapply(x, \(i) print.summary.tna(i, ...))
  } else {
    print.summary.tna(x, ...)
  }
}

#' @family clusters
#' @export
print.summary.group_tna_bootstrap  <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna_bootstrap(x),
    "Argument {.arg x} must be of type `group_tna_bootstrap`"
  )
  lapply(x, \(i) print.summary.tna_bootstrap(i, ...))
}

#' @family clusters
#' @export
print.group_tna_centralities  <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna_centralities(x),
    "Argument {.arg x} must be of type `group_tna_centralities`"
  )

  NextMethod(generic = "print", object = x, ...)
}
#' @family clusters
#' @export
print.group_tna_communities  <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna_communities(x),
    "Argument {.arg x} must be of type `group_tna_communities`"
  )
  Map(function(y, i) {print(i); print.tna_communities(y, ...)}, x, names(x))
}

#' @family clusters
#' @export
print.group_tna_cliques  <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna_cliques(x),
    "Argument {.arg x} must be of type `group_tna_cliques`"
  )
  Map(function(y, i) {print(i); print.tna_cliques(y, ...)}, x, names(x))

}

#' @family clusters
#' @export
hist.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )
  lapply(x, \(i) hist.tna(i, ...))
}

#' @family clusters
#' @export
plot.group_tna_centralities <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna_centralities(x),
    "Argument {.arg x} must be of type `group_tna_centralities`"
  )
  plot_centralities_multiple(x, ...)
}

#' @family clusters
#' @export
plot.group_tna_cliques <- function(x, title, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna_cliques(x),
    "Argument {.arg x} must be of type `group_tna_cliques`"
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
plot.group_tna_stability <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna_stability(x),
    "Argument {.arg x} must be of type `group_tna_stability`"
  )
  lapply(x, \(i) plot.tna_stability(i, ...))
}

#' @family clusters
#' @export
plot.group_tna_communities <- function(x, title = names(x), colors = lapply(x, \(x) NULL), ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna_communities(x),
    "Argument {.arg x} must be of type `group_tna_communities`"
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
communities.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )


  structure(
    lapply(x, \(i) communities.tna(i, ...)),
    class = "group_tna_communities"
  )
}

#' @family clusters
#' @export
cliques.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )
  structure(
    lapply(x, \(i) cliques.tna(i, ...)),
    class = "group_tna_cliques"
  )
}

#' @family clusters
#' @rdname centralities
#' @export
centralities.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )

  grc <- dplyr::bind_rows(lapply(x, \(i) data.frame(centralities.tna(i, ...))), .id = "Group")

  structure(
    grc,
    class = c("group_tna_centralities", "tbl_df", "tbl", "data.frame")
  )
}

#' @family clusters
#' @rdname estimate_cs
#' @export
estimate_centrality_stability.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )
  structure(
    lapply(x, \(i) estimate_centrality_stability.tna(i, ...)),
    class = "group_tna_stability"
  )

}


#' @family clusters
#' @export
bootstrap.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )
  structure(
    lapply(x, \(i) bootstrap.tna(i, ...)),
    class = "group_tna_bootstrap"
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
  coef <- coef[, -1]

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

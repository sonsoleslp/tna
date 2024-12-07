#' Build a grouped Transition Network Analysis Model
#'
#' This function constructs a transition network analysis (TNA) model for
#' each cluster from a given sequence, wide-formatted dataframe,
#' or a mixture Markov model.
#'
#' @export
#' @family clusters
#' @rdname group_model
#' @param x An `stslist` object describing a sequence of events or states to
#'   be used for building the Markov model. The argument `x` also accepts
#'   a `data.frame` object in wide format.
#'   (each column is a time point with no extra columns). Alternatively, the
#'   function accepts a mixture Markov model from the library `seqHMM`.
#' @param group A vector indicating the cluster assignment of each
#'  row of the data / sequence. Must have the same length as the number of
#'  rows/sequences of `x`.
#' @param ... Ignored.
#' @return An object of class `group_tna` which is a `list` containing one
#'   element per cluster. Each element is a `tna` object.
#'
#' @examples
#' group <- c(rep("High", 100), rep("Low", 100))
#' model <- group_model(engagement, group = group)
#'
group_model <- function(x, group, ...) {
  UseMethod("group_model")
}

#' @export
#' @family clusters
#' @rdname group_model
group_model.default <- function(x, group, ...) {
  check_missing(x)
  stopifnot_(
    !missing(group),
    "Argument {.arg group} is missing."
  )
  stopifnot_(
    inherits(x, "stslist") || inherits(x, "data.frame"),
    "Argument {.arg x} must be {.cls stslist} (sequence object)
     or a {.cls data.frame}."
  )
  group_len <- length(group)
  stopifnot_(
    group_len == nrow(x),
    "Argument {.arg group} must have the same length as number of
     rows/sequences in {.arg x}."
  )
  if (!is.factor(group)) {
    group <- factor(group)
  }
  levs <- levels(group)
  n_group <- length(levs)
  clusters <- stats::setNames(
    vector(mode = "list", length = n_group),
    levs
  )
  for (i in levs) {
    clusters[[i]] <- build_model(x[group == i, ], ...)
  }
  structure(clusters, class = "group_tna")
}

#' @export
#' @family clusters
#' @rdname group_model
group_model.mhmm <- function(x, ...) {
  stopifnot_(
    requireNamespace("seqHMM", quietly = TRUE),
    "Please install the {.pkg seqHMM} package."
  )
  check_missing(x)
  group <- summary(x)$most_probable_cluster
  group_model.default(x$observations, group = group, ...)
}

#' @export
#' @rdname build_model
#' @examples
#' model <- group_tna(engagement, group = gl(2, 100))
#'
group_tna <- function(x, scaling = character(0L), ...) {
  check_missing(x)
  group_model(x = x, type = "relative", scaling = scaling, ...)
}

#' @export
#' @rdname build_model
#' @examples
#' model <- group_ftna(engagement, group = gl(2, 100))
#'
group_ftna <- function(x, scaling = character(0L), ...) {
  group_model(x = x, type = "frequency", scaling = scaling, ...)
}

#' @export
#' @rdname build_model
#' @examples
#' model <- group_ctna(engagement, group = gl(2, 100))
#'
group_ctna <- function(x, scaling = character(0L), ...) {
  group_model(x = x, type = "co-occurrence", scaling = scaling, ...)
}

#' Retrieve statistics from a mixture Markov model (MMM)
#'
#' @export
#' @family clusters
#' @param x An `mhmm` object.
#' @param use_t_dist A `logical` value. If `TRUE` (the default), the
#' t-distribution is used to compute confidence intervals.
#' @param level A `numeric` value representing the significance level for
#' hypothesis testing and confidence intervals. Defaults to `0.05`.
#' @return A `data.frame` object.
#' @examples
#' mmm_stats(engagement_mmm)
#'
mmm_stats <- function(x, use_t_dist = TRUE, level = 0.05) {
  stopifnot_(
    requireNamespace("seqHMM", quietly = TRUE),
    "Please install the {.pkg seqHMM} package."
  )
  check_missing(x)
  check_flag(use_t_dist)
  check_probability(level)
  stopifnot_(
    inherits(x, "mhmm"),
    c(
      "Argument {.arg x} must be a {.cls mhmm} object.",
      `i` = "See the {.pkg seqHMM} package for more information."
    )
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
  stopifnot_(
    length(coef_flat) == length(se_flat),
    "The lengths of the coefficients and standard errors do not match."
  )

  # Calculate z-value or t-value
  statistic <- coef_flat / se_flat

  # Determine degrees of freedom if using t-distribution
  if (use_t_dist && !is.null(model_summary$df.residual)) {
    df <- model_summary$df.residual
    # Calculate p-values using t-distribution
    p_value <- 2 * (1 - stats::pt(abs(statistic), df))
    # Calculate confidence intervals
    ci_margin <- stats::qt(1 - level / 2.0, df) * se_flat
  } else {
    # Calculate p-values using normal distribution
    p_value <- 2 * (1 - stats::pnorm(abs(statistic)))
    # Calculate confidence intervals
    ci_margin <- stats::qnorm(1 - level / 2.0) * se_flat
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
  results
}

#' Rename clusters
#'
#' @export
#' @family clusters
#' @param x A `group_tna` object.
#' @param new_names A `vector` containing one name per cluster.
#' @return A renamed `group_tna` object.
#' @examples
#' model <- group_model(engagement_mmm)
#' model_renamed <- rename_groups(model, c("A", "B", "C"))
#'
rename_groups <- function(x, new_names) {
  check_missing(x)
  check_missing(new_names)
  check_class(x, "group_tna")
  stopifnot_(
    is.vector(new_names),
    "Argument {.arg new_names} must be a vector."
  )
  stopifnot_(
    length(new_names) == length(x),
    "Argument {.arg new_names} must be the same length as {.arg x}"
  )
  names(x) <- new_names
  x
}

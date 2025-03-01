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
#'   `data.frame` objects in wide format, and `tna_data` objects.
#'   Alternatively, the function accepts a mixture Markov model from `seqHMM`.
#' @param group A vector indicating the cluster assignment of each
#'   row of the data / sequence. Must have the same length as the number of
#'   rows/sequences of `x`. Alternatively, a single `character` string giving
#'   the column name of the data that defines the group when `x` is a wide
#'   format `data.frame` or a `tna_data` object.
#' @param cols An `integer`/`character` vector giving the indices/names of the
#'   columns that should be considered as sequence data.
#'   Defaults to all columns, i.e., `seq(1, ncol(x))`. The columns are
#'   automatically determined for `tna_data` objects.
#' @param na.rm A `logical` value that determines if observations with `NA`
#' value in `group` be removed. If `FALSE`, an additional category for `NA`
#' values will be added. The default is `FALSE` and a warning is issued
#' if `NA` values are detected.
#' @inheritParams build_model
#' @param ... Ignored.
#' @return An object of class `group_tna` which is a `list` containing one
#'   element per cluster. Each element is a `tna` object.
#'
#' @examples
#' group <- c(rep("High", 1000), rep("Low", 1000))
#' model <- group_model(group_regulation, group = group)
#'
#' model <- group_model(engagement_mmm)
#'
group_model <- function(x, ...) {
  UseMethod("group_model")
}

#' @export
#' @family clusters
#' @rdname group_model
group_model.default <- function(x, group, type = "relative",
                                scaling = character(0L), cols, params = list(),
                                na.rm = TRUE, ...) {
  check_missing(x)
  check_missing(group)
  stopifnot_(
    inherits(x, "stslist") ||
      inherits(x, "data.frame") || inherits(x, "tna_data"),
    "Argument {.arg x} must be {.cls stslist} (sequence object), a
    {.cls data.frame} or a {.cls tna_data} object."
  )
  if (inherits(x, "tna_data")) {
    wide <- cbind(x$sequence_data, x$meta_data)
    cols <- seq_len(ncol(x$sequence_data))
    x <- wide
  } else {
    cols <- ifelse_(missing(cols), seq_len(ncol(x)), cols)
    check_range(cols, type = "integer", scalar = FALSE, min = 1L, max = ncol(x))
  }
  type <- check_model_type(type)
  scaling <- check_model_scaling(scaling)
  group_len <- length(group)
  data <- NULL
  stopifnot_(
    group_len == nrow(x) || group_len == 1L,
    "Argument {.arg group} must be of length one or the same length as the
     number of rows/sequences in {.arg x}."
  )
  group_var <- ".group"
  prefix <- "Argument"
  if (group_len == 1L) {
    stopifnot_(
      group %in% names(x),
      "Argument {.arg group} must be a column name of {.arg x}
       when of length one."
    )
    group_var <- group
    group <- x[[group]]
    prefix <- "Column"
  }
  group_na <- any(is.na(group))
  if (group_na && na.rm) {
    warning_(
      c(
        "{prefix} {.arg group} contains missing values.",
        `i` = "The corresponding observations will be excluded. You can
               use {.code na.rm = FALSE} to keep missing values."
      )
    )
  }
  group <- ifelse_(
    is.factor(group),
    group,
    factor(group)
  )
  group <- ifelse_(
    group_na && !na.rm,
    addNA(group),
    group
  )
  levs <- levels(group)
  n_group <- length(levs)
  clusters <- stats::setNames(
    vector(mode = "list", length = n_group),
    levs
  )
  groups <- vector(mode = "list", length = n_group)
  group <- as.integer(group)
  vals <- sort(unique(unlist(x[, cols])))
  alphabet <- vals[!is.na(vals)]
  for (i in seq_along(levs)) {
    groups[[i]] <- rep(i, sum(group == i, na.rm = TRUE))
    rows <- which(group == i)
    d <- create_seqdata(
      x[rows, ],
      cols = cols,
      alphabet = alphabet
    )
    model <- initialize_model(d, type, scaling, params)
    clusters[[levs[i]]] <- build_model_(
      weights = model$weights,
      inits = model$inits,
      labels = model$labels,
      type = type,
      scaling = scaling,
      data = d,
      params = params
    )
  }
  structure(
    clusters,
    groups = groups,
    group_var = group_var,
    levels = levs,
    na.rm = na.rm,
    cols = cols,
    class = "group_tna"
  )
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
#' @rdname group_model
#' @return An object of class `group_tna` which is a `list` containing one
#'   element per cluster. Each element is a `tna` object.
#' @examples
#' model <- group_tna(group_regulation, group = gl(2, 1000))
#'
group_tna <- function(x, ...) {
  check_missing(x)
  group_model(x = x, type = "relative", ...)
}

#' @export
#' @rdname group_model
#' @return An object of class `group_tna` which is a `list` containing one
#'   element per cluster. Each element is a `tna` object.
#' @examples
#' model <- group_ftna(group_regulation, group = gl(2, 1000))
#'
group_ftna <- function(x, ...) {
  group_model(x = x, type = "frequency", ...)
}

#' @export
#' @rdname group_model
#' @return An object of class `group_tna` which is a `list` containing one
#'   element per cluster. Each element is a `tna` object.
#' @examples
#' model <- group_ctna(group_regulation, group = gl(2, 1000))
#'
group_ctna <- function(x, ...) {
  group_model(x = x, type = "co-occurrence", ...)
}

#' Retrieve statistics from a mixture Markov model (MMM)
#'
#' @export
#' @family clusters
#' @param x A `mhmm` object.
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
  check_range(level)
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
    cluster = cluster_list,
    variable = variable_list,
    estimate = coef_flat,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_Upper = ci_upper,
    std_rrror = se_flat,
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
#' @param new_names A `character` vector containing one name per cluster.
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
    is.character(new_names),
    "Argument {.arg new_names} must be a {.cls character} vector."
  )
  stopifnot_(
    length(new_names) == length(x),
    "Argument {.arg new_names} must be the same length as {.arg x}"
  )
  names(x) <- new_names
  x
}

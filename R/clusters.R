#' Build a Grouped Transition Network Analysis Model
#'
#' This function constructs a transition network analysis (TNA) model for
#' each cluster from a given sequence, wide-format dataframe,
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
#'   format `data.frame` or a `tna_data` object. If not provided, each row of
#'   the data forms a cluster.
#' @param cols An `integer`/`character` vector giving the indices/names of the
#'   columns that should be considered as sequence data.
#'   Defaults to all columns, i.e., `seq(1, ncol(x))`. The columns are
#'   automatically determined for `tna_data` objects.
#' @param na.rm A `logical` value that determines if observations with `NA`
#' value in `group` be removed. If `FALSE`, an additional category for `NA`
#' values will be added. The default is `FALSE` and a warning is issued
#' if `NA` values are detected.
#' @param groupwise A `logical` value that indicates whether scaling methods
#' should be applied by group (`TRUE`) or globally (`FALSE`, the default).
#' @inheritParams build_model
#' @param ... Ignored.
#' @return An object of class `group_tna` which is a `list` containing one
#'   element per cluster. Each element is a `tna` object.
#'
#' @examples
#' # Manually specified groups
#' group <- c(rep("High", 1000), rep("Low", 1000))
#' model <- group_model(group_regulation, group = group)
#'
#' # Groups defined by a mixed Markov model
#' model <- group_model(engagement_mmm)
#'
group_model <- function(x, ...) {
  UseMethod("group_model")
}

#' @export
#' @rdname group_model
group_model.default <- function(x, group, type = "relative",
                                scaling = character(0L), groupwise = FALSE,
                                cols, params = list(), na.rm = TRUE, ...) {
  check_missing(x)
  check_flag(groupwise)
  check_flag(na.rm)
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
  group <- ifelse_(
    missing(group),
    seq_len(nrow(x)),
    group
  )
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
  group_scaling <- ifelse_(groupwise, scaling, character(0L))
  groups <- vector(mode = "list", length = n_group)
  group <- as.integer(group)
  vals <- sort(unique(unlist(x[, cols])))
  alphabet <- vals[!is.na(vals)]
  a <- length(alphabet)
  seq_data <- create_seqdata(x, cols = cols, alphabet = alphabet)
  trans <- compute_transitions(seq_data, a, type, params)
  for (i in seq_along(levs)) {
    groups[[i]] <- rep(i, sum(group == i, na.rm = TRUE))
    rows <- which(group == i)
    d <- seq_data[rows, , drop = FALSE]
    attr(d, "alphabet") <- alphabet
    attr(d, "labels") <- alphabet
    attr(d, "colors") <- attr(seq_data, "colors")
    inits <- factor(d[, 1L], levels = seq_len(a), labels = alphabet)
    inits <- as.vector(table(inits))
    weights <- compute_weights(
      trans[rows, , , drop = FALSE],
      type = type,
      scaling = group_scaling,
      a = a
    )
    dimnames(weights) <- list(alphabet, alphabet)
    clusters[[i]] <- build_model_(
      weights = weights,
      inits = inits / sum(inits),
      labels = alphabet,
      type = type,
      scaling = group_scaling,
      data = d,
      params = params
    )
  }
  if (!groupwise && length(scaling > 0)) {
    weights <- scale_weights_global(
      weights = lapply(clusters, "[[", "weights"),
      type = type,
      scaling = scaling,
      a = a
    )
    for (i in seq_along(clusters)) {
      clusters[[i]]$weights <- weights[[i]]
    }
  }
  structure(
    clusters,
    groups = groups,
    group_var = group_var,
    levels = levs,
    na.rm = na.rm,
    cols = cols,
    groupwise = groupwise,
    type = type,
    scaling = scaling,
    class = "group_tna"
  )
}

#' @export
#' @rdname group_model
group_model.mhmm <- function(x, type = "relative",
                             scaling = character(0L), groupwise = FALSE,
                             cols, params = list(), na.rm = TRUE, ...) {
  stopifnot_(
    requireNamespace("seqHMM", quietly = TRUE),
    "Please install the {.pkg seqHMM} package."
  )
  check_missing(x)
  group <- summary(x)$most_probable_cluster
  group_model.default(
    x = x$observations,
    group = group,
    type = type,
    scaling = scaling,
    groupwise = groupwise,
    cols = cols,
    params = params,
    na.rm = na.rm,
    ...
  )
}

#' @export
#' @rdname group_model
#' @examples
#' model <- group_tna(group_regulation, group = gl(2, 1000))
#'
group_tna <- function(x, ...) {
  check_missing(x)
  group_model(x = x, type = "relative", ...)
}

#' @export
#' @rdname group_model
#' @examples
#' model <- group_ftna(group_regulation, group = gl(2, 1000))
#'
group_ftna <- function(x, ...) {
  group_model(x = x, type = "frequency", ...)
}

#' @export
#' @rdname group_model
#' @examples
#' model <- group_ctna(group_regulation, group = gl(2, 1000))
#'
group_ctna <- function(x, ...) {
  group_model(x = x, type = "co-occurrence", ...)
}

#' @export
#' @rdname group_model
#' @examples
#' model <- group_atna(group_regulation, group = gl(2, 1000))
#'
group_atna <- function(x, ...) {
  group_model(x = x, type = "attention", ...)
}

#' Retrieve Statistics from a Mixture Markov Model (MMM)
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

#' Rename Clusters
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

#' Scale Transition Network Weights
#'
#' @param weights A `lsit` of edge weights matrices
#' @param type Type of the transition network as a `character` string.
#' @param scaling Scaling methods to apply as a `character` vector.
#' @param a An `integer`, the number of states.
#' @noRd
scale_weights_global <- function(weights, type, scaling, a) {
  g <- length(weights)
  if (type == "relative") {
    for (i in seq_len(g)) {
      w <- weights[[i]]
      rs <- .rowSums(w, m = a, n = a)
      pos <- which(rs > 0)
      w[pos, ] <- w[pos, ] / rs[pos]
      w[!pos, ] <- NA
      weights[[i]] <- w
    }
  }
  weights_vec <- unlist(weights)
  for (i in seq_along(scaling)) {
    if (scaling[i] == "minmax") {
      weights_vec[] <- ranger(weights_vec)
    } else if (scaling[i] == "max") {
      weights_vec[] <- weights_vec / max(weights_vec, na.rm = TRUE)
    } else if (scaling[i] == "rank") {
      weights_vec[] <- rank(weights_vec, ties.method = "average")
    }
  }
  idx <- seq_len(a^2)
  for (i in seq_len(g)) {
    weights[[i]] <- matrix(weights_vec[idx], nrow = a, ncol = a)
    idx <- idx + a^2
  }
  weights
}

#' Combine data from clusters into a single dataset in long format
#'
#' @param x A `group_tna` object.
#' @param label optional group variable name as a `character` string
#' @noRd
combine_data <- function(x, label) {
  cols <- attr(x, "cols")
  groups <- attr(x, "groups")
  group_var <- attr(x, "group_var")
  data <- dplyr::bind_rows(
    #lapply(x, function(y) y$data[, cols])
    lapply(x, function(y) as.data.frame(y$data))
  )
  data[[group_var]] <- unlist(groups)
  label <- ifelse_(
    !missing(label),
    label,
    ifelse_(
      group_var == ".group",
      "Cluster",
      group_var
    )
  )
  check_string(label)
  names(data) <- c(names(data)[-ncol(data)], label)
  out <- data |>
    tidyr::pivot_longer(cols = !(!!rlang::sym(label))) |>
    dplyr::filter(!is.na(!!rlang::sym("value")))
  list(
    data = out,
    label = label
  )
}
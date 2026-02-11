#' Build a Grouped Transition Network Analysis Model
#'
#' This function constructs a transition network analysis (TNA) model for
#' each group from a given sequence, wide-format dataframe
#' or a mixture Markov model.
#'
#' @export
#' @family clusters
#' @rdname group_model
#' @param x An `stslist` object describing a sequence of events or states to
#'   be used for building the Markov model. The argument `x` also accepts
#'   `data.frame` objects in wide format, and `tna_data` objects.
#'   This can also be the output of clustering from
#'   [cluster_sequences()].
#' @param group A `vector` indicating the group assignment of each
#'   row of the data/sequence. Must have the same length as the number of
#'   rows/sequences of `x`. Alternatively, a single `character` string giving
#'   the column name of the data that defines the group when `x` is a wide
#'   format `data.frame` or a `tna_data` object. If not provided, each row of
#'   the data forms a cluster. Not used when `x` is a mixture Markov model
#'   or a clustering result.
#' @param cols An `expression` giving a tidy selection of the
#'   columns that should be considered as sequence data.
#'   The default is all columns. The columns are
#'   automatically determined for `tna_data` objects. The `group` column
#'   is automatically removed from these columns if provided.
#' @param na.rm A `logical` value that determines if observations with `NA`
#'   value in `group` be removed. If `FALSE`, an additional category for `NA`
#'   values will be added. The default is `FALSE` and a warning is issued
#'   if `NA` values are detected.
#' @param groupwise A `logical` value that indicates whether scaling methods
#'   should be applied by group (`TRUE`) or globally (`FALSE`, the default).
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
                                cols = tidyselect::everything(),
                                params = list(), concat = 1L,
                                na.rm = TRUE, ...) {
  check_missing(x)
  check_missing(group)
  check_flag(groupwise)
  check_flag(na.rm)
  stopifnot_(
    inherits(x, c("stslist", "data.frame", "tna_data", "tna_mmm")),
    "Argument {.arg x} must be {.cls stslist} (sequence object), a
    {.cls data.frame}, a {.cls tna_data} object."
  )
  if (inherits(x, "tna_data")) {
    wide <- cbind(x$sequence_data, x$meta_data)
    cols <- names(x$sequence_data)
    x <- wide
  } else {
    cols <- get_cols(rlang::enquo(cols), x)
  }
  type <- check_model_type(type)
  scaling <- check_model_scaling(scaling)
  n_group <- length(group)
  data <- NULL
  stopifnot_(
    n_group == nrow(x) || n_group == 1L,
    "Argument {.arg group} must be of length one or the same length as the
     number of rows/sequences in {.arg x}."
  )
  label <- "Cluster"
  prefix <- "Argument"
  if (n_group == 1L) {
    x_names <- names(x)
    stopifnot_(
      group %in% x_names,
      "Argument {.arg group} must be a column name of {.arg x}
       when of length one."
    )
    label <- group
    cols <- setdiff(cols, group)
    group <- as.factor(x[[group]])
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
  if (!is.factor(group)) {
    labels <- ifelse_(
      is.character(group),
      unique(group),
      paste0("Group ", seq_len(n_unique(group[!is.na(group)])))
    )
    group <- factor(group, labels = labels)
  }
  group <- ifelse_(group_na && !na.rm, addNA(group), group)
  levs <- levels(group)
  n_group <- length(levs)
  clusters <- stats::setNames(
    vector(mode = "list", length = n_group),
    levs
  )
  group_scaling <- ifelse_(groupwise, scaling, character(0L))
  groups <- vector(mode = "list", length = n_group)
  group <- as.integer(group)
  # TODO remove workaround if TraMineR is fixed
  vals <- ifelse_(
    inherits(x, "stslist"),
    sort(unique(unlist(x[, which(names(x) %in% cols)]))),
    vals <- sort(unique(unlist(x[, cols])))
  )
  alphabet <- ifelse_(
    inherits(x, "stslist"),
    attr(x, "alphabet"),
    vals[!is.na(vals)]
  )
  a <- length(alphabet)
  seq_data <- create_seqdata(
    x = x,
    cols = cols,
    alphabet = alphabet,
    concat = concat
  )
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
  names(groups) <- names(clusters)
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
    label = label,
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
group_model.mhmm <- function(x, type = "relative", scaling = character(0L),
                             groupwise = FALSE, params = list(),
                             na.rm = TRUE, ...) {
  stopifnot_(
    requireNamespace("seqHMM", quietly = TRUE),
    "Please install the {.pkg seqHMM} package."
  )
  check_missing(x)
  check_class(x, "mhmm")
  group_model.default(
    x = x$observations,
    group = summary(x)$most_probable_cluster,
    type = type,
    scaling = scaling,
    groupwise = groupwise,
    params = params,
    na.rm = na.rm,
    ...
  )
}

# #' @export
# #' @rdname group_model
# group_model.tna_mmm <- function(x, type = "relative", scaling = character(0L),
#                                 groupwise = FALSE, params = list(),
#                                 na.rm = TRUE, ...) {
#   check_missing(x)
#   check_class(x, "tna_mmm")
#   group_model.default(
#     x = x$data,
#     group = x$assignments,
#     type = type,
#     scaling = scaling,
#     groupwise = groupwise,
#     params = params,
#     na.rm = na.rm,
#     ...
#   )
# }

#' @export
#' @rdname group_model
group_model.tna_clustering <- function(x, type = "relative",
                                       scaling = character(0L),
                                       groupwise = FALSE, params = list(),
                                       na.rm = TRUE, ...) {
  check_missing(x)
  check_class(x, "tna_clustering")
  group_model.default(
    x = x$data,
    group = x$assignment,
    type = type,
    scaling = scaling,
    groupwise = groupwise,
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

#' Rename Groups
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
#' @param weights A `list` of edge weights matrices
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

#' Combine data from clusters into a single dataset
#'
#' @param x A `group_tna` object.
#' @noRd
combine_data <- function(x) {
  cols <- attr(x, "cols")
  groups <- attr(x, "groups")
  data <- dplyr::bind_rows(
    lapply(x, function(y) as.data.frame(y$data))
  )
  data$.group <- attr(x, "levels")[unlist(groups)]
  data
}

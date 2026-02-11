#' @importFrom stats simulate
#' @export
stats::simulate

#' Simulate Data from a Transition Network Analysis Model
#'
#' @export
#' @family data
#' @inheritParams stats::simulate
#' @param object A `tna` object. The edge weights must be transition
#'   probabilities or frequencies, i.e., the model must have 
#'   `type = "relative"` or `type = "frequency"`.
#' @param nsim An `integer` giving the number of sequences to simulate.
#'   The default is 1.
#' @param max_len An `integer` giving the maximum length of the simulated
#'   sequences. When no missing values are generated, this is the length of
#'   all simulated sequences.
#' @param na_range An `integer` vector of length 2 giving the minimum and
#'   maximum number of missing values to generate for each sequence. The number
#'   of missing values is drawn uniformly from this range. If both values are
#'   zero (the default), no missing values are generated.
#' @param format A `character` string indicating whether the data should be
#'   returned in `wide` or `long` format.
#' @param zero_row A `character` string describing how to process zero rows
#'   in the weight matrix. The option `"self"` (the default) assigns
#'   probability 1 to the corresponding state (self loop) and option
#'   `"uniform"` assigns a uniform distribution.
#' @param ... Ignored.
#' @return A `data.frame` of the simulated sequence data.
#' @examples
#' model <- tna(group_regulation)
#' sim <- simulate(model, nsim = 10, max_len = 10)
#'
simulate.tna <- function(object, nsim = 1, seed = NULL, max_len = 100L,
                         na_range = c(0L, 0L), zero_row = "self", 
                         format = "wide", ...) {
  check_missing(object)
  check_class(object, "tna")
  type <- attr(object, "type")
  stopifnot_(
    type %in% c("relative", "frequency"),
    "Simulation is only supported for models with transition probabilities 
    ('relative') or transition frequencies ('frequency')."
  )
  check_values(nsim, strict = TRUE)
  check_values(max_len, strict = TRUE)
  zero_row <- check_match(zero_row, c("self", "uniform"))
  zero_row <- check_match(format, c("wide", "long"))
  na_range <- as.integer(na_range)
  stopifnot_(
    na_range[1L] >= 0L &&
      na_range[2L] >= na_range[1L] &&
      na_range[2L] < max_len,
    c(
      "Invalid {.arg na_range} values.",
      `x` = "The maximum number of missing values must be greater or equal
      to the minimum number of missing values. The maximum must be less than
      {.arg max_len}."
    )
  )
  if (!is.null(seed)) set.seed(seed)
  labels <- object$labels
  n <- nodes(object)
  nu <- nsim * n
  init <- object$inits %||% rep(1.0 / n, n)
  prob <- object$weights
  stopifnot_(
    is.matrix(prob) && nrow(prob) == n && ncol(prob) == n,
    "Invalid transition matrix dimensions."
  )
  if (type == "frequency") {
    prob <- freq_to_prob(prob, zero_row = zero_row)
  }
  out <- matrix(0L, nrow = nsim, ncol = max_len)
  out[, 1L] <- sample(seq_len(n), size = nsim, replace = TRUE, prob = init)
  for (i in seq(2L, max_len)) {
    # Gumbel softmax trick
    log_prob <- log(prob[out[, i - 1L], , drop = FALSE])
    out[, i] <- max.col(log_prob - log(-log(stats::runif(nu))))
  }
  out <- apply(out, 2L, function(y) labels[y], simplify = FALSE)
  out <- as.data.frame(out, col.names = paste0("T", seq_len(max_len)))
  if (na_range[2L] > 0L) {
    max_seq <- seq(na_range[1L], na_range[2L])
    nas <- sample(max_seq, size = nsim, replace = TRUE)
    seqs <- vector(mode = "list", length = na_range[2])
    seqs[[na_range[2L]]] <- seq(max_len - na_range[2L] + 1L, max_len)
    for (i in seq(na_range[2L] - 1L, na_range[1L])) {
      seqs[[i]] <- seqs[[i + 1]][-1L]
    }
    for (i in max_seq) {
      idx <- which(nas == i)
      out[idx, seqs[[i]]] <- NA
    }
  }
  ifelse_(
    format == "long",
    as_long_sim(out),
    out
  )
}

#' Simulate Data from a Group Transition Network Analysis Model
#'
#' @export
#' @family data
#' @param object A `group_tna` object. The edge weights must be transition
#'   probabilities or frequencies, i.e., the model must have 
#'   `type = "relative"` or `type = "frequency"`.
#' @param nsim An `integer` vector giving the number of sequences to simulate 
#'   per group. If a single integer is provided, the same number of sequences
#'   is generated per each group. The default is 1.
#' @param max_len An `integer` vector giving the maximum length of the simulated
#'   sequences per group. When no missing values are generated, this is the 
#'   length of all simulated sequences. If a single integer is provided, the 
#'   maximum length is the same for each group.
#' @inheritParams simulate.tna
#' @return A `data.frame` of the simulated sequence data.
#' @examples
#' model <- group_tna(
#'   group_regulation,
#'   group = rep(c("High", "Low"), each = 1000)
#' )
#' sim <- simulate(model, nsim = 10, max_len = 10)
#'
simulate.group_tna <- function(object, nsim = 1, seed = NULL,
                               max_len = 100L, na_range = c(0L, 0L),
                               zero_row = "self", format = "wide", ...) {
  check_missing(object)
  check_class(object, "group_tna")
  zero_row <- check_match(zero_row, c("self", "uniform"))
  G <- length(object)
  if (length(nsim) == 1L) {
    nsim <- rep(as.integer(nsim), G)
  } else {
    nsim <- as.integer(nsim)
    stopifnot_(
      length(nsim) == G,
      "{.arg nsim} must be length 1 or the same length as {.arg object}."
    )
  }
  if (length(max_len) == 1L) {
    max_len <- rep(as.integer(max_len), G)
  } else {
    max_len <- as.integer(max_len)
    stopifnot_(
      length(max_len) == G,
      "{.arg max_len} must be length 1 or the same length as {.arg object}."
    )
  }
  stopifnot_(all(max_len >= 1L), "All {.arg max_len} values must be >= 1.")
  grp_names <- names(object)
  if (is.null(grp_names)) {
    grp_names <- as.character(seq_along(object))
  }
  out_list <- vector("list", G)
  for (g in seq_len(G)) {
    mod <- object[[g]]
    out_g <- simulate.tna(
      object = mod,
      nsim = nsim[g],
      seed = seed,
      max_len = max_len[g],
      na_range = na_range,
      zero_row = zero_row,
      format = format,
      ...
    )
    if (format == "wide") {
      out_g <- cbind(group = grp_names[g], out_g)
    } else {
      out_g$group <- grp_names[g]
      out_g <- out_g[, c("group", setdiff(names(out_g), "group")), drop = FALSE]
    }
    out_list[[g]] <- out_g
  }
  if (format == "wide") {
    do.call(dplyr::bind_rows, out_list)
  } else {
    out <- do.call(base::rbind, out_list)
    rownames(out) <- NULL
    out
  }
}

freq_to_prob <- function(prob, zero_row) {
  stopifnot_(is.matrix(prob), "Transition weights must be a matrix.")
  stopifnot_(all(is.finite(prob)), "Non-finite values in transition matrix.")
  stopifnot_(all(prob >= 0), "Negative values in transition frequencies.")
  n <- nrow(prob)
  rs <- rowSums(prob)
  ok <- rs > 0
  if (any(ok)) {
    prob[ok, ] <- prob[ok, , drop = FALSE] / rs[ok]
  }
  if (any(!ok)) {
    if (zero_row == "uniform") {
      prob[!ok, ] <- 1 / n
    } else {
      prob[!ok, ] <- 0
      idx <- which(!ok)
      prob[cbind(idx, idx)] <- 1
    }
  }
  prob
}

as_long_sim <- function(wide_df) {
  tt <- names(wide_df)
  n_actor <- nrow(wide_df)
  n_time <- ncol(wide_df)
  id <- rep(seq_len(n_actor), each = n_time)
  time <- rep(seq_len(n_time), times = n_actor)
  state <- as.vector(t(as.matrix(wide_df)))
  out <- data.frame(
    id = id,
    time = time,
    state = state,
    stringsAsFactors = FALSE
  )
  out
}

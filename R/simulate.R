#' @importFrom stats simulate
#' @export
stats::simulate

#' Simulate Data from a Transition Network Analysis Model
#'
#' @export
#' @family data
#' @param object A `tna` object. The edge weights must be transition
#' probabilities, i.e., the model must have `type = "relative"`.
#' @param nsim An `integer` giving the number of sequences to simulate.
#' The default is 1.
#' @param seed Ignored. Please use [set.seed()] manually.
#' @param max_len An `integer` giving the maximum length of the simulated
#' sequences. When no missing values are generated, this is the length of
#' all simulated sequences.
#' @param na_range An `integer` vector of length 2 giving the minimum and
#' maximum number of missing values to generate for each sequence. The number
#' of missing values is drawn uniformly from this range. If both values are
#' zero (the default), no missing values are generated.
#' @param ... Ignored.
#' @return A `data.frame` of the simulated sequence data with `nsim` rows and
#' `max_len` columns.
#' @examples
#' model <- tna(group_regulation)
#' sim <- simulate(model, nsim = 10, max_len = 10)
#'
simulate.tna <- function(object, nsim = 1, seed = NULL, max_len = 100L,
                         na_range = c(0L, 0L), ...) {
  check_missing(object)
  check_class(object, "tna")
  stopifnot_(
    attr(object, "type") == "relative",
    "Simulation is only supported for models with transition probabilities."
  )
  check_values(nsim, strict = TRUE)
  check_values(max_len, strict = TRUE)
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
  labels <- object$labels
  n <- nodes(object)
  nu <- nsim * n
  init <- object$inits %||% rep(1.0 / n, n)
  prob <- object$weights
  out <- matrix(0L, nrow = nsim, ncol = max_len)
  out[, 1L] <- sample(seq_len(n), size = nsim, replace = TRUE, prob = init)
  for (i in seq(2L, max_len)) {
    # Gumbell softmax "trick"
    log_prob <- log(prob[out[, i - 1L], , drop = FALSE])
    out[, i] <- max.col(log_prob - log(-log(stats::runif(nu))))
  }
  # simplify = FALSE in case of nsim = 1
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
  out
}


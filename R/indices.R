#' Compute Sequence Indices
#'
#' @export
#' @param x A `data.frame` or a `matrix`.
#' @return A `data.frame` containing the index values.
sequence_indices <- function(x, ...) {
  UseMethod("sequence_indices")
}

#' @export
#' @rdname sequence_indices
sequence_indices.default <- function(x, cols, favorable, min_length = 1L, ...) {
  stopifnot_(
    is.matrix(x) || is.data.frame(x),
    "Argument {.arg x} must be a matrix or a data.frame"
  )
  p <- ncol(x)
  cols <- cols %m% seq_len(p)
  data <- create_seqdata(x, cols = cols)
  model <- initialize_model(
    x = data,
    type = "relative",
    scaling = character(0L),
    params = list(),
    transitions = TRUE
  )
  trans <- model$trans
  lab <- model$labels
  s <- length(lab)
  if (!missing(favorable)) {
    fav <- which(lab %in% favorable)
    unfav <- setdiff(seq_len(lab), fav)
  }
  m <- as.matrix(data)
  nas <- is.na(m)
  last_obs <- max.col(!nas, ties.method = "last")
  n <- nrow(m)
  k <- ncol(m)
  valid <- integer(n)
  first <- character(n)
  last <- character(n)
  u_states <- integer(n)
  long_ent <- numeric(n)
  simpson <- numeric(n)
  loops <- numeric(n)
  rate <- numeric(n)
  mean_spells <- numeric(n)
  trans_comp <- numeric(n)
  init_per <- numeric(n)
  init_decay <- numeric(n)
  cyclic_str <- cyclic_strength(m, n, k, last_obs)
  attr_state <- character(n)
  attr_str <- numeric(n)
  comp <- numeric(n)
  for (i in seq_len(n)) {
    row <- m[i, ]
    p <- last_obs[i]
    valid[i] <- sum(!nas[i, ])
    first[i] <- lab[row[1L]]
    last[i] <- lab[row[last_obs[i]]]
    freq <- tabulate(row)
    prop <- freq / valid[i]
    pos <- freq > 0
    runs <- rle(row)
    spells <- runs$lengths[!is.na(runs$values)]
    mean_spells[i] <- mean(spells)
    u_states[i] <- length(freq)
    long_ent[i] <- -sum(prop[pos] * log(prop[pos]))
    simpson[i] <- 1.0 - sum(prop^2)
    self <- sum(diag(trans[i,,]))
    total <- sum(trans[i,,])
    loops[i] <- self / total
    rate[i] <- (total - self) / (valid[i] - 1)
    tmp <- trans[i,,]
    diag(tmp) <- 0
    # if (!missing(favorable)) {
    #   fav_to_unfav <- sum(tmp[i, fav, unfav])
    #   unfav_to_fav <- sum(tmp[i, unfav, fav])
    # }
    trans_comp[i] <- sum(tmp > 0) / (s * (s - 1))
    per <- which(is.na(row[-1L]) | (row[-1L] != row[1L]))[1L] / p
    per <- ifelse_(is.na(per), 1.0, per)
    init_per[i] <- per
    first_third <- row[1:ceiling(p / 3)]
    last_third <- row[ceiling(2 * p / 3):p]
    early <- sum(first_third == row[1L], na.rm = TRUE) / length(first_third)
    late <- sum(last_third == row[1L], na.rm = TRUE) / length(last_third)
    init_decay[i] <- early - late
    attr_vec <- attractor_state(row, p, freq, runs)
    attr_idx <- which.max(attr_vec)
    attr_state[i] <- lab[attr_idx]
    attr_str[i] <- prop[attr_idx]
    comp[i] <- 0.4 * (long_ent[i] / log(s)) +
      0.4 * (sum(tmp) / (p - 1)) +
      0.2 * min(sd(spells) / mean(spells), 1.0)
    # cyclic_str[i] <- max(
    #   vapply(
    #     seq(2L, p - 1L),
    #     function(y) {
    #       sum(diff(sequence, lag = y) == 0) / (p - y)
    #     },
    #     numeric(1L)
    #   )
    # )
  }
  data.frame(
    valid_n = valid,
    valid_prop = valid / last_obs,
    unique_states = u_states,
    mean_spell_duration = mean_spells,
    longitudinal_entropy = long_ent,
    simpson_diversity = simpson,
    self_loop_tendency = loops,
    transition_rate = rate,
    transition_complexity = trans_comp,
    initial_state_persistence = init_per,
    initial_state_influence_decay = init_decay,
    cyclic_feedback_strength = cyclic_str,
    first_state = first,
    last_state = last,
    attractor_state = attr_state,
    attractor_strength = attr_str,
    complexity_index = comp
  )
}

cyclic_strength <- function(m, n, k, last_obs) {
  max_strength <- rep(0, n)
  for (i in seq(2L, k - 1L)) {
    strength <- numeric(n)
    for (j in seq(1L, k - i)) {
      from <- m[, j]
      to <- m[, j + i]
      idx <- !is.na(from) & !is.na(to)
      strength[idx] <- strength[idx] + (from[idx] == to[idx])
    }
    idx <- last_obs > i
    strength[idx] <- strength[idx] / (last_obs[idx] - i)
    max_strength <- pmax(max_strength, strength)
  }
  max_strength
}

attractor_state <- function(x, p, freq, runs, weights = c(1.0, 0.5, 0.3)) {
  s <- length(freq)
  strength <- numeric(s)
  for (i in seq_len(s)) {
    if (freq[i] > 0) {
      spells <- runs$lengths[which(runs$values == 1)]
      per <- mean(spells) / p
      gaps <- diff(which(x == i)) - 1
      ret <- 1.0 / (1.0 + mean(gaps))
      strength[i] <- sum(weights * c(freq[i], per, ret))
    }
  }
  strength
}

integrative_potential <- function(x) {
  0
}

# mean_spell_length <- function(x, n, p) {
#   x_obs <- !is.na(x)
#   n_obs <- .rowSums(x_obs, m = n, n = p - 1L)
#   x_tail <- x[, -1L, drop = FALSE]
#   x_head <- x[, -p, drop = FALSE]
#   runs <- .rowSums(
#     x = (x_tail != x_head) & x_obs[, -1L] & x_obs[, -p],
#     m = n,
#     n = p - 1L,
#     na.rm = TRUE
#   ) + (n_obs > 0L)
#   out <- n_obs / runs
#   out[n_obs == 0L] <- NA
#   out
# }

# #' Compute Sequence Indices
# #'
# #' @export
# #' @param x A `data.frame` or a `matrix`.
# #' @param cols An `expression` giving a tidy selection of columns that should
# #'   be considered as sequence data. By default, all columns are used.
# #' @param favorable A `character` vector of state names that should be
# #'   considered as favorable states.
# #' @param omega A `numeric` value for the omega parameter used to compute
# #'   the integrative potential.
# #' @return A `data.frame` containing the index values.
# sequence_indices <- function(x, ...) {
#   UseMethod("sequence_indices")
# }

# #' @export
# #' @rdname sequence_indices
# sequence_indices.default <- function(x, cols, favorable, omega = 1) {
#   stopifnot_(
#     is.matrix(x) || is.data.frame(x),
#     "Argument {.arg x} must be a matrix or a data.frame"
#   )
#   p <- ncol(x)
#   cols <- cols %m% seq_len(p)
#   cols <- get_cols(rlang::enquo(cols), x)
#   data <- create_seqdata(x, cols = cols)
#   model <- initialize_model(
#     x = data,
#     type = "relative",
#     scaling = character(0L),
#     params = list(),
#     transitions = TRUE
#   )
#   trans <- model$trans
#   lab <- model$labels
#   s <- length(lab)
#   fav <- integer(0L)
#   if (!missing(favorable)) {
#     fav <- which(lab %in% favorable)
#     unfav <- setdiff(seq_along(lab), fav)
#   }
#   m <- as.matrix(data)
#   nas <- is.na(m)
#   last_obs <- max.col(!nas, ties.method = "last")
#   n <- nrow(m)
#   k <- ncol(m)
#   valid <- integer(n)
#   first <- character(n)
#   last <- character(n)
#   u_states <- integer(n)
#   long_ent <- numeric(n)
#   simpson <- numeric(n)
#   loops <- numeric(n)
#   rate <- numeric(n)
#   mean_spells <- numeric(n)
#   trans_comp <- numeric(n)
#   init_per <- numeric(n)
#   init_decay <- numeric(n)
#   cyclic_str <- cyclic_strength(m, n, k, last_obs)
#   dom_state <- character(n)
#   dom_prop <- numeric(n)
#   emergent_state <- rep(NA_character_, n)
#   emergent_prop <- rep(NA_real_, n)
#   emergent_per <- rep(NA_real_, n)
#   comp <- numeric(n)
#   int_pot <- numeric(n)
#   for (i in seq_len(n)) {
#     row <- m[i, ]
#     p <- last_obs[i]
#     valid[i] <- sum(!nas[i, ])
#     first[i] <- lab[row[1L]]
#     last[i] <- lab[row[last_obs[i]]]
#     freq <- tabulate(row)
#     prop <- freq / valid[i]
#     pos <- freq > 0
#     runs <- rle(row)
#     runs_obs <- !is.na(runs$values)
#     values <- runs$values[runs_obs]
#     spells <- runs$lengths[runs_obs]
#     mean_spells[i] <- mean(spells)
#     u_states[i] <- length(freq)
#     long_ent[i] <- -sum(prop[pos] * log(prop[pos]))
#     simpson[i] <- 1.0 - sum(prop^2)
#     self <- sum(diag(trans[i,,]))
#     total <- sum(trans[i,,])
#     loops[i] <- self / total
#     rate[i] <- (total - self) / (valid[i] - 1)
#     tmp <- trans[i,,]
#     diag(tmp) <- 0
#     # if (!missing(favorable)) {
#     #   fav_to_unfav <- sum(tmp[i, fav, unfav])
#     #   unfav_to_fav <- sum(tmp[i, unfav, fav])
#     # }
#     trans_comp[i] <- sum(tmp > 0) / (s * (s - 1))
#     per <- which(is.na(row[-1L]) | (row[-1L] != row[1L]))[1L] / p
#     per <- ifelse_(is.na(per), 1.0, per)
#     init_per[i] <- per
#     first_third <- row[1:ceiling(p / 3)]
#     last_third <- row[ceiling(2 * p / 3):p]
#     early <- sum(first_third == row[1L], na.rm = TRUE) / length(first_third)
#     late <- sum(last_third == row[1L], na.rm = TRUE) / length(last_third)
#     init_decay[i] <- early - late
#     dom_idx <- which.max(freq)
#     dom_state[i] <- lab[dom_idx]
#     dom_prop[i] <- prop[dom_idx]
#     dom_spell <- max(spells[values == dom_idx])
#     init_spell <- spells[1L]
#     persisting <- spells >= 3
#     if (any(persisting)) {
#       true_state <- 0
#       true_spell <- 0
#       true_emergent <- persisting & (spells > dom_spell)
#       if (any(true_emergent)) {
#         true_spell_idx <- which.max(spells[true_emergent])
#         true_state <- values[true_emergent][true_spell_idx]
#         true_spell <- spells[true_emergent][true_spell_idx]
#       }
#       dom_emergent <- dom_spell >= 3 && dom_idx != row[1L]
#       init_spells <- spells[-1][values[-1] == row[1]]
#       max_init <- ifelse_(
#         length(init_spells) > 0,
#         max(init_spells),
#         0
#       )
#       init_emergent <- max_init * 2 > init_spell && max_init >= 3
#       emergent_candidate <- c(true_state, dom_idx, row[1L])
#       spells_candidate <- c(
#         true_spell,
#         dom_spell * dom_emergent,
#         max_init * init_emergent
#       )
#       if (any(spells_candidate > 0)) {
#         emergent_idx <- which.max(spells_candidate)
#         emergent_state[i] <- lab[emergent_candidate[emergent_idx]]
#         emergent_per[i] <- spells_candidate[emergent_idx]
#         emergent_prop[i] <- prop[emergent_candidate[emergent_idx]]
#       }
#     }
#     comp[i] <- 0.4 * (long_ent[i] / log(s)) +
#       0.4 * (sum(tmp) / (p - 1)) +
#       0.2 * min(sd(spells) / mean(spells), 1.0)
#     if (length(fav) > 0L) {
#       idx <- 1:p
#       w <- (idx)^omega
#       pos <- row[idx] %in% fav
#       int_pot[i] <- sum(pos * w) / sum(w)
#     } else {
#       int_pot <- 1
#     }
#     # cyclic_str[i] <- max(
#     #   vapply(
#     #     seq(2L, p - 1L),
#     #     function(y) {
#     #       sum(diff(sequence, lag = y) == 0) / (p - y)
#     #     },
#     #     numeric(1L)
#     #   )
#     # )
#   }
#   data.frame(
#     valid_n = valid,
#     valid_prop = valid / last_obs,
#     unique_states = u_states,
#     mean_spell_duration = mean_spells,
#     longitudinal_entropy = long_ent,
#     simpson_diversity = simpson,
#     self_loop_tendency = loops,
#     transition_rate = rate,
#     transition_complexity = trans_comp,
#     initial_state_persistence = init_per,
#     initial_state_influence_decay = init_decay,
#     cyclic_feedback_strength = cyclic_str,
#     first_state = first,
#     last_state = last,
#     dominant_state = dom_state,
#     dominant_prop = dom_prop,
#     emergent_state = emergent_state,
#     emergent_state_persistence = emergent_per,
#     emergent_state_prop = emergent_prop,
#     integrative_potential = int_pot,
#     complexity_index = comp
#   )
# }

# cyclic_strength <- function(m, n, k, last_obs) {
#   max_strength <- rep(0, n)
#   for (i in seq(2L, k - 1L)) {
#     strength <- numeric(n)
#     for (j in seq(1L, k - i)) {
#       from <- m[, j]
#       to <- m[, j + i]
#       idx <- !is.na(from) & !is.na(to)
#       strength[idx] <- strength[idx] + (from[idx] == to[idx])
#     }
#     idx <- last_obs > i
#     strength[idx] <- strength[idx] / (last_obs[idx] - i)
#     max_strength <- pmax(max_strength, strength)
#   }
#   max_strength
# }

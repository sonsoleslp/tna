# #' Mixture Markov Model Clustering for Sequences
# #'
# #' Fits a mixture of first-order Markov models to sequence data using the
# #' Expectation-Maximization (EM) algorithm. Also performs model selection
# #' for the number of clusters using information criteria (AIC or BIC).
# #'
# #' @export
# #' @family clusters
# #' @param data A `data.frame` or `matrix` where rows represent sequences and
# #'   columns represent time points. Missing values (`NA`) are handled
# #'   appropriately. The columns should be either `integer`, `character` or
# #'   `factor` variables.
# #' @param cols An `expression` giving a tidy selection of column names
# #'   that should be considered as sequence data. The default is to use all
# #'   columns.
# #' @param formula An optional `formula` object that specifies the covariates
# #'   to use for the cluster membership probabilities.
# #' @param k An `integer` vector specifying the numbers of mixture components
# #'   (clusters) to fit. The values must be between 2 and the number of
# #'   sequences minus 1.
# #' @param cluster_names An optional `character` vector of cluster names.
# #'   The default naming scheme is Cluster 1, Cluster 2, etc. The length of
# #'   the vector must be the same as the largest value of `k`.
# #' @param criterion A `character` string specifying the information criterion
# #'   to use for model selection. Either `"bic"` (default) for Bayesian
# #'   information criterion or `"aic"` for Akaike information criterion.
# #' @param progressbar A `logical` value. If `TRUE`, a progress bar is displayed.
# #'   The default is `FALSE`. Disabled when using parallel processing.
# #' @param parallel A `logical` value.
# #' @param n_cores An `integer` specifying the number of cores to use for
# #'   parallel processing. The default is 1 for no parallel processing.
# #' @param cl An optional prespecified cluster object with a registered
# #'   parallel backend. Ignores `parallel` if provided.
# #' @param control An optional `list` of arguments for the EM algorithm.
# #'   The possible arguments are:
# #'
# #'   * `maxiter`: An `integer` for the maximum number of EM iterations per
# #'     restart. The default is `500`.
# #'   * `maxiter_m`: An `integer` for the maximum number of Newton-Rhapson
# #'     iterations in the M-step (only with covariates). The default is `500`.
# #'   * `reltol`: A `numeric` value specifying the relative convergence
# #'     tolerance for the log-likelihood change between iterations.
# #'     The default is `1e-10`.
# #'   * `reltol_m`: A `numeric` value specifying the relative tolerance for the
# #'     Newton-Rhapson optimization in the M-step (only with covariates).
# #'     The default is `1e-6`.
# #'   * `restarts`: An `integer` specifying the number of random restarts to
# #'     perform. Multiple restarts help avoid local optima. The default is `10`.
# #'   * `seed`: An `integer` specifying the base random seed.
# #'     The initial values are generated for each restart using `seed + i` as
# #'     the seed where `i` is the index of the restart.
# #'   * `step`: An `integer` defining the step size for the Newton-Rhapson
# #'     iterations. The default is `1.0`.
# #'
# #' @details
# #' The function implements a mixture of first-order Markov models where each
# #' component models sequences with initial state probabilities and transition
# #' probabilities between states, along with mixing weights for component
# #' membership. The estimation is carried out using the EM algorithm.
# #'
# #' Multiple random restarts are performed to avoid local optima, with the
# #' best solution (highest log-likelihood) selected as the final result.
# #'
# #' If `k` is a vector, then the model is fitted for each value in `k` and
# #' the optimal number of clusters is selected based on the specified
# #' information criterion.
# #'
# #' @examples
# #' \dontrun{
# #' # Fit a MMM with k = 3
# #' model1 <- cluster_mmm(engagement, k = 3)
# #'
# #' # Find optimal k for k = 2,3,4 using BIC
# #' model2 <- cluster_mmm(engagement, k = 2:4, criterion = "bic")
# #' }
# #'
# cluster_mmm <- function(data, cols = tidyselect::everything(), formula,
#                         k, cluster_names, criterion = "bic",
#                         progressbar = TRUE, parallel = FALSE,
#                         n_cores, cl, control) {
#   data_name <- deparse(substitute(data))
#   mm <- ifelse_(
#     missing(formula),
#     NULL,
#     stats::model.matrix(formula, data = data)
#   )
#   cols <- get_cols(rlang::enquo(cols), data)
#   d <- create_seqdata(x = data, cols = cols)
#   s <- length(attr(d, "labels"))
#   criterion <- check_match(criterion, c("bic", "aic"))
#   control <- check_em_control(control)
#   check_flag(progressbar)
#   check_flag(parallel)
#   check_range(
#     k, scalar = FALSE, type = "integer", lower = 2L, upper = nrow(data) - 1L
#   )
#   cluster_names <- cluster_names %m% paste("Cluster", seq_len(max(k)))
#   stopifnot_(
#     length(cluster_names) == max(k) && is.character(cluster_names),
#     "Argument {.arg cluster_names} must be a {.cls character}
#      vector with {.code max(k)} elements."
#   )
#   if (parallel && missing(cl)) {
#     stopifnot_(
#       requireNamespace("doParallel", quietly = TRUE),
#       "Please install the {.pkg doParallel} package for parallel computation."
#     )
#     n_cores <- n_cores %m% parallel::detectCores()
#     n_cores <- min(n_cores, parallel::detectCores())
#     cl <- parallel::makeCluster(n_cores)
#     doParallel::registerDoParallel(cl)
#     on.exit(parallel::stopCluster(cl), add = TRUE)
#   }
#   parallel <- parallel || !missing(cl)
#   if (parallel) {
#     stopifnot_(
#       inherits(cl, "cluster"),
#       "Argument {.arg cl} must be a {.cls cluster} object."
#     )
#     stopifnot_(
#       isTRUE(foreach::getDoParRegistered()),
#       "A parallel backend must be registered for parallel computation."
#     )
#     dopar <- foreach::getDoParName()
#     workers <- foreach::getDoParWorkers()
#     message_("Using {dopar} with {workers} workers.")
#   }
#   k_len <- length(k)
#   results <- vector(mode = "list", length = k_len)
#   if (progressbar && k_len > 1L) {
#     cli::cli_progress_bar(
#       name = "Estimating mixture Markov models",
#       total = k_len
#     )
#   }
#   for (i in seq_along(k)) {
#     results[[i]] <- fit_mmm(
#       data = d,
#       mm = mm,
#       k = k[i],
#       progressbar = progressbar && k_len == 1L,
#       parallel = parallel,
#       cl = cl,
#       control = control
#     )
#     if (progressbar && k_len > 1L) {
#       cli::cli_progress_update()
#     }
#   }
#   if (progressbar && k_len > 1L) {
#     cli::cli_progress_done()
#   }
#   if (k_len > 1L) {
#     criteria <- vapply(results, "[[", numeric(1L), criterion)
#     out <- results[[which.min(criteria)]]
#   } else {
#     out <- results[[1L]]
#   }
#   cluster_names <- cluster_names[seq_len(out$k)]
#   out$cluster_names <- cluster_names
#   out$data <- data
#   out$data_name <- data_name
#   out$assignments <- factor(
#     max.col(out$posterior),
#     levels = seq_len(ncol(out$posterior)),
#     labels = cluster_names
#   )
#   out$sizes <- table(out$assignments)
#   names(out$inits) <- cluster_names
#   names(out$trans) <- cluster_names
#   names(out$beta) <- cluster_names
#   if (!missing(formula)) {
#     out$formula <- formula
#   }
#   if (!out$converged) {
#     warning_("The algorithm did not converge.")
#   }
#   structure(
#     out,
#     class = "tna_mmm"
#   )
# }

# #' @param k An `integer` for the number of mixture components
# #' @inheritParams cluster_mmm
# #' @noRd
# fit_mmm <- function(data, mm, k, progressbar, parallel, cl, control) {
#   `%d%` <- ifelse_(parallel, foreach::`%dopar%`, foreach::`%do%`)
#   progressbar <- progressbar && !parallel
#   if (progressbar) {
#     cli::cli_progress_bar(
#       name = "Running EM Algorithm",
#       total = control$restarts
#     )
#   }
#   lab <- attr(data, "labels")
#   s <- length(lab)
#   em_fun <- ifelse_(is.null(mm), em, em_covariates)
#   # Avoid NSE Note with foreach index variable
#   i <- NULL
#   results <- foreach::foreach(i = seq_len(control$restarts)) %d% {
#     res <- tryCatch(
#       em_fun(i, data, mm, k, lab, control),
#       error = function(e) {
#         NULL
#       }
#     )
#     if (progressbar) {
#       cli::cli_progress_update()
#     }
#     res
#   }
#   # results <- vector(mode = "list", length = control$restarts)
#   # for (i in seq_len(control$restarts)) {
#   #   res <- em_fun(i, data, mm, k, lab, control)
#   #   results[[i]] <- res
#   # }
#   #   if (progressbar) {
#   #     cli::cli_progress_update()
#   #   }
#   #   results[[i]] <- res
#   # }
#   nulls <- vapply(results, is.null, logical(1L))
#   stopifnot_(
#     any(!nulls),
#     "All EM algorithm runs failed."
#   )
#   results <- results[!nulls]
#   logliks <- vapply(results, "[[", numeric(1L), "loglik")
#   best <- results[[which.max(logliks)]]
#   q <- ifelse_(is.null(mm), 1L, ncol(mm))
#   n <- nrow(data)
#   # Parameters: mixture + initial + transition
#   n_param <- (k - 1) * q + k * (s - 1) + k * s * (s - 1)
#   aic <- -2 * best$loglik + 2 * n_param
#   bic <- -2 * best$loglik + log(n) * n_param
#   if (progressbar) {
#     cli::cli_progress_done()
#   }
#   structure(
#     list(
#       prior = best$prior,
#       posterior = best$posterior,
#       hessian = best$hessian,
#       loglik = best$loglik,
#       k = k,
#       bic = bic,
#       aic = aic,
#       inits = best$inits,
#       trans = best$trans,
#       beta = best$beta,
#       n_parameters = n_param,
#       converged = best$converged,
#       iterations = best$iterations,
#       failed = sum(nulls),
#       sizes = best$sizes,
#       states = lab
#     ),
#     class = "tna_mmm"
#   )
# }

# The EM Algorithm for a mixture Markov Model
# em <- function(start, data, mm, k, labels, control) {
#   set.seed(control$seed + start)
#   reltol <- control$reltol
#   tol <- control$tol
#   maxiter <- control$maxiter
#   s <- length(labels)
#   state_names <- list(labels, labels)
#   # For some reason export does not work for this function
#   # defining it here instead as a workaround
#   log_sum_exp_rows <- function(x, m, n) {
#     maxs <- apply(x, 1L, max)
#     maxs + log(.rowSums(exp(x - maxs), m, n))
#   }
#   n <- nrow(data)
#   p <- ncol(data)
#   prior <- stats::runif(k)
#   prior <- prior / sum(prior)
#   inits <- vector("list", length = k)
#   trans <- vector("list", length = k)
#   beta <- vector("list", length = k)
#   for (i in seq_len(k)) {
#     inits[[i]] <- stats::runif(s)
#     inits[[i]] <- inits[[i]] / sum(inits[[i]])
#     trans[[i]] <- matrix(stats::runif(s^2), s, s)
#     trans[[i]] <- trans[[i]] / .rowSums(trans[[i]], s, s)
#     dimnames(trans[[i]]) <- state_names
#     names(inits[[i]]) <- labels
#   }
#   idx <- seq_len(n)
#   trans_count <- array(0L, dim = c(n, s, s))
#   from_na <- is.na(data[, 1L])
#   init_state <- data[!from_na, 1L]
#   init_state_idx <- lapply(1:s, function(x) which(init_state == x))
#   for (t in seq_len(p - 1L)) {
#     from <- data[, t]
#     to <- data[, t + 1L]
#     to_na <- is.na(to)
#     any_na <- from_na | to_na
#     new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
#     trans_count[new_trans] <- trans_count[new_trans] + 1L
#     from_na <- to_na
#   }
#   trans_count_mat <- matrix(trans_count, nrow = n, byrow = FALSE)
#   iter <- 0L
#   loglik <- 0
#   loglik_prev <- 0
#   loglik_reldiff <- Inf
#   loglik_mat <- matrix(-Inf, n, k)
#   log_sum_exp_vec <- numeric(n)
#   posterior <- matrix(NA, n, k)
#   post_clust_arr <- array(0, dim = c(n, s, s))
#   while (loglik_reldiff > reltol && iter < maxiter) {
#     iter <- iter + 1L
#     # E-step
#     for (j in 1:k) {
#       log_prob <- log(inits[[j]][init_state]) + log(prior[j])
#       log_trans_j <- c(log(trans[[j]] + 1e-10))
#       loglik_mat[, j] <- log_prob +
#         as.vector(trans_count_mat %*% log_trans_j)
#     }
#     log_sum_exp_vec <- log_sum_exp_rows(loglik_mat, m = n, n = k)
#     posterior[] <- exp(loglik_mat - log_sum_exp_vec)
#     # M-step
#     prior[] <- .colMeans(posterior, m = n, n = k)
#     for (j in 1:k) {
#       post_clust <- posterior[, j]
#       inits_new <- vapply(
#         seq_len(s),
#         function(x) {
#           sum(post_clust[init_state_idx[[x]]])
#         },
#         numeric(1L)
#       )
#       inits[[j]][] <- (inits_new + 1e-10) / sum(inits_new + s * 1e-10)
#       post_clust_arr[] <- rep(post_clust, s^2)
#       trans_new <- colSums(trans_count * post_clust_arr, dims = 1L)
#       rs <- .rowSums(trans_new, s, s)
#       pos <- which(rs > 0)
#       trans[[j]][pos, ] <- trans_new[pos, ] / rs[pos]
#     }
#     loglik <- sum(log_sum_exp_vec)
#     if (iter > 1L) {
#       loglik_reldiff <- (loglik - loglik_prev) / (abs(loglik_prev) + 0.1)
#     }
#     loglik_prev <- loglik
#   }
#   hessian <- matrix(0.0, k - 1L, k - 1L)
#   beta[[1L]] <- c(`(Intercept)` = 0.0)
#   for (j in 2:k) {
#     beta[[j]] <- c(`(Intercept)` = log(prior[j] / prior[1L]))
#     for (l in 2:j) {
#       hessian[j - 1L, l - 1L] <- -1.0 *
#         sum(posterior[, j] * ((j == l) - posterior[, l]))
#       if (j != l) {
#         hessian[l - 1L, j - 1L] <- hessian[j - 1L, l - 1L]
#       }
#     }
#   }
#   list(
#     prior = matrix(rep(prior, each = n), nrow = n, ncol = k),
#     posterior = posterior,
#     loglik = loglik,
#     inits = inits,
#     trans = trans,
#     beta = beta,
#     hessian = hessian,
#     converged = iter < maxiter,
#     iterations = iter
#   )
# }
# 
# # The EM Algorithm for a mixture Markov Model with Covariates
# em_covariates <- function(start, data, mm, k, labels, control) {
#   set.seed(control$seed + start)
#   maxiter <- control$maxiter
#   maxiter_m <- control$maxiter_m
#   reltol <- control$reltol
#   reltol_m <- control$reltol_m
#   step <- control$step
#   s <- length(labels)
#   state_names <- list(labels, labels)
#   # For some reason export does not work for this function
#   # defining it here instead as a workaround
#   log_sum_exp_rows <- function(x, m, n) {
#     maxs <- apply(x, 1L, max)
#     maxs + log(.rowSums(exp(x - maxs), m, n))
#   }
#   n <- nrow(data)
#   p <- ncol(data)
#   q <- ncol(mm)
#   inits <- vector("list", length = k)
#   trans <- vector("list", length = k)
#   beta <- vector("list", length = k)
#   for (i in seq_len(k)) {
#     inits[[i]] <- stats::runif(s)
#     inits[[i]] <- inits[[i]] / sum(inits[[i]])
#     trans[[i]] <- matrix(stats::runif(s^2), s, s)
#     trans[[i]] <- trans[[i]] / .rowSums(trans[[i]], s, s)
#     dimnames(trans[[i]]) <- state_names
#     names(inits[[i]]) <- labels
#     beta[[i]] <- rep(0, q) + (i > 1) * stats::runif(q, 0.5, 1.0)
#     names(beta[[i]]) <- colnames(mm)
#   }
#   idx <- seq_len(n)
#   trans_count <- array(0L, dim = c(n, s, s))
#   from_na <- is.na(data[, 1L])
#   init_state <- data[!from_na, 1L]
#   init_state_idx <- lapply(1:s, function(x) which(init_state == x))
#   for (t in seq_len(p - 1L)) {
#     from <- data[, t]
#     to <- data[, t + 1L]
#     to_na <- is.na(to)
#     any_na <- from_na | to_na
#     new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
#     trans_count[new_trans] <- trans_count[new_trans] + 1L
#     from_na <- to_na
#   }
#   trans_count_mat <- matrix(trans_count, nrow = n, byrow = FALSE)
#   iter <- 0L
#   loglik <- 0
#   loglik_prev <- 0
#   loglik_reldiff <- Inf
#   loglik_mat <- matrix(-Inf, n, k)
#   log_sum_exp_vec <- numeric(n)
#   prior <- matrix(0, n, k)
#   linpred <- matrix(0, n, k)
#   for (j in 2:k) {
#     linpred[, j] <- mm %*% beta[[j]]
#   }
#   prior[] <- exp(linpred - log_sum_exp_rows(linpred, m = n, n = k))
#   grad <- numeric((k - 1L) * q)
#   posterior <- matrix(NA, n, k)
#   post_clust_arr <- array(0, dim = c(n, s, s))
#   while (loglik_reldiff > reltol && iter < maxiter) {
#     iter <- iter + 1L
#     # E-step
#     for (j in 1:k) {
#       log_prob <- log(inits[[j]][init_state]) + log(prior[, j])
#       log_trans_j <- c(log(trans[[j]] + 1e-10))
#       loglik_mat[, j] <- log_prob +
#         as.vector(trans_count_mat %*% log_trans_j)
#     }
#     log_sum_exp_vec <- log_sum_exp_rows(loglik_mat, m = n, n = k)
#     posterior[] <- exp(loglik_mat - log_sum_exp_vec)
#     # M-step
#     beta_prev <- unlist(beta[-1L])
#     for (r in 1:maxiter_m) {
#       hessian <- matrix(0i, (k - 1L) * q, (k - 1L) * q)
#       for (j in 2:k) {
#         idx_row <- seq((j - 2) * q + 1, (j - 1) * q)
#         grad[idx_row] <- crossprod(mm, posterior[, j] - prior[, j])
#         for (l in 2:j) {
#           idx_col <- seq((l - 2) * q + 1, (l - 1) * q)
#           # No need to export ifelse_ for this
#           w <- if (j == l) {
#             sqrt(diag(prior[, j] * (1 - prior[, j])))
#           } else {
#             1i * sqrt(diag(prior[, j] * prior[, l]))
#           }
#           hessian[idx_row, idx_col] <- crossprod(w %*% mm)
#           if (j != l) {
#             hessian[idx_col, idx_row] <- t(hessian[idx_row, idx_col])
#           }
#         }
#       }
#       hessian <- -1.0 * Re(hessian)
#       # Regularization
#       f <- norm(hessian, type = "F")
#       lambda <- diag(rep(max(1e-10, 1e-8 * f), q * (k - 1L)))
#       beta_vec <- beta_prev - step * solve(hessian - lambda) %*% grad
#       grad_rel <- max(abs(grad)) / max(1.0, max(abs(beta_vec)))
#       beta_prev <- beta_vec
#       for (j in 2:k) {
#         idx <- seq((j - 2) * q + 1, (j - 1) * q)
#         beta[[j]][] <- beta_vec[idx]
#         linpred[, j] <- mm %*% beta[[j]]
#       }
#       prior[] <- exp(linpred - log_sum_exp_rows(linpred, m = n, n = k))
#       if (grad_rel < reltol_m) {
#         break
#       }
#     }
#     for (j in 1:k) {
#       post_clust <- posterior[, j]
#       inits_new <- vapply(
#         seq_len(s),
#         function(x) {
#           sum(post_clust[init_state_idx[[x]]])
#         },
#         numeric(1L)
#       )
#       inits[[j]][] <- (inits_new + 1e-10) / sum(inits_new + s * 1e-10)
#       post_clust_arr[] <- rep(post_clust, s^2)
#       trans_new <- colSums(trans_count * post_clust_arr, dims = 1L)
#       rs <- .rowSums(trans_new, s, s)
#       pos <- which(rs > 0)
#       trans[[j]][pos, ] <- trans_new[pos, ] / rs[pos]
#     }
#     loglik <- sum(log_sum_exp_vec)
#     if (iter > 1L) {
#       loglik_reldiff <- (loglik - loglik_prev) / (abs(loglik_prev) + 0.1)
#     }
#     loglik_prev <- loglik
#   }
#   list(
#     prior = prior,
#     posterior = posterior,
#     loglik = loglik,
#     trans = trans,
#     inits = inits,
#     beta = beta,
#     hessian = hessian,
#     converged = iter < maxiter,
#     iterations = iter
#   )
# }

#' Retrieve Statistics from a Mixture Markov Model (MMM)
#'
#' @export
#' @family clusters
#' @param x A `mhmm` object.
#' @param level A `numeric` value representing the significance level for
#' hypothesis testing and confidence intervals. Defaults to `0.05`.
#' @return A `data.frame` object.
#' @examples
#' mmm_stats(engagement_mmm)
#'
mmm_stats <- function(x, level = 0.05) {
  UseMethod("mmm_stats")
}

# #' @export
# #' @rdname mmm_stats
# mmm_stats.tna_mmm <- function(x, level = 0.05) {
#   check_missing(x)
#   check_class(x, "tna_mmm")
#   check_range(level, lower = 0.0, upper = 1.0)
#   sumr <- summary(x)
#   mmm_stats_(sumr$coefficients, sumr$vcov, level)
# }

#' @export
#' @rdname mmm_stats
mmm_stats.mhmm <- function(x, level = 0.05) {
  stopifnot_(
    requireNamespace("seqHMM", quietly = TRUE),
    "Please install the {.pkg seqHMM} package."
  )
  check_missing(x)
  check_range(level, lower = 0.0, upper = 1.0)
  stopifnot_(
    inherits(x, "mhmm"),
    c(
      "Argument {.arg x} must be a {.cls mhmm} object.",
      `i` = "See the {.pkg seqHMM} package for more information."
    )
  )
  sumr <- summary(x)
  mmm_stats_(sumr$coefficients, sumr$vcov, level)
}

#' Internal function for MMM statistics
#'
#' @param cf A `matrix` of regression coefficients from `coef`.
#' @param vc A variance-covariance matrix from `vcov`.
#' @param level The significance level
#' @noRd
mmm_stats_ <- function(cf, vc, level) {
  coef_flat <- c()
  se_flat <- c()
  cf <- as.matrix(cf)[, -1L, drop = FALSE]
  vcov_diag <- sqrt(diag(vc))
  num_vars <- nrow(cf)
  num_clusters <- ncol(cf)
  cluster_vec <- character(num_clusters)
  variable_vec <- character(num_vars)
  clusters <- colnames(cf)
  variables <- rownames(cf)
  idx <- 0L
  for (cluster in seq_len(num_clusters)) {
    for (var in seq_len(num_vars)) {
      idx <- idx + 1L
      coef_flat <- c(coef_flat, cf[var, cluster])
      se_flat <- c(se_flat, vcov_diag[(cluster - 1L) * num_vars + var])
      cluster_vec[idx] <- clusters[cluster]
      variable_vec[idx] <- variables[var]
    }
  }
  stopifnot_(
    length(coef_flat) == length(se_flat),
    "The lengths of the coefficients and standard errors do not match."
  )
  statistic <- coef_flat / se_flat
  p_value <- 2 * (1 - stats::pnorm(abs(statistic)))
  ci_margin <- stats::qnorm(1 - level / 2.0) * se_flat
  ci_lower <- coef_flat - ci_margin
  ci_upper <- coef_flat + ci_margin
  results <- data.frame(
    cluster = cluster_vec,
    variable = variable_vec,
    estimate = coef_flat,
    std_error = se_flat,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    z_value = statistic,
    p_value = p_value
  )
  rownames(results) <- NULL
  results
}

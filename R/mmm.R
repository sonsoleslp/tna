#' Mixture Markov Model Clustering for Sequences
#'
#' Fits a mixture of first-order Markov models to sequence data using the
#' Expectation-Maximization (EM) algorithm. Provides robust estimation through
#' multiple random restarts and comprehensive model diagnostics.
#' Also performs model selection across a range of cluster numbers using
#' information criteria (AIC or BIC) with comprehensive diagnostics and
#' performance metrics.
#'
#' @export
#' @param data A `data.frame` or `matrix` where rows represent sequences and
#'   columns represent time points. Missing values (`NA`) are handled
#'   appropriately. The columns should be either `integer`, `character` or
#'   `factor` variables.
#' @param cols An `integer`/`character` vector giving the indices/names of the
#'   columns that should be considered as sequence data.
#'   Defaults to all columns, i.e., `seq(1, ncol(data))`. Column names not
#'   found in `data` will be ignored without warning.
#' @param formula An optional `formula` object that specifies the covariates
#'   to use for the cluster membership probabilities.
#' @param k An `integer` vector specifying the numbers of mixture components
#'   (clusters) to fit. The values must be between 2 and the number of sequences
#'   minus 1.
#' @param criterion A `character` string specifying the information criterion
#'   to use for model selection. Either `"bic"` (default) for Bayesian
#'   information criterion or `"aic"` for Akaike information criterion.
#' @param n_starts An `integer` specifying the number of random restarts to
#'   perform. Multiple restarts help avoid local optima. Default is 10.
#' @param min_size An `integer` specifying the minimum number of sequences
#'   required per cluster for a solution to be considered valid. Default is 1.
#' @param progressbar #' @param progressbar A `logical` value. If `TRUE`, a
#'   progress bar is displayed. The default is `FALSE`. Disables when using
#'   parallel processing.
#' @param max_iter An `integer` specifying the maximum number of EM iterations
#'   per restart. Default is 300.
#' @param reltol A `numeric` value specifying the relative convergence
#'   tolerance for the log-likelihood change between iterations.
#'   Default is 1e-10.
#' @param seed An `integer` value specifying the base random seed.
#'   The initial values are generated for each restart using `seed + i` as
#'   the seed where `i` is the index of the restart.
#' @param parallel A `logical` value.
#' @param n_cores An `integer` specifying the number of cores to use for
#'   parallel processing. The default is 1 for no parallel processing.
#' @param cl An optional prespecified cluster object with a registered
#'   parallel backend. Ignores `parallel` if provided.
#'
#' @details
#' The function implements a mixture of first-order Markov models where each
#' component models sequences with initial state probabilities and transition
#' probabilities between states, along with mixing weights for component
#' membership.
#'
#' The EM algorithm alternates between E-step (computing posterior
#' probabilities of cluster membership) and M-step (updating model parameters).
#'
#' Multiple random restarts are performed to avoid local optima, with the
#' best solution (highest log-likelihood) selected as the final result.
#'
#' If `k` is a vector, then the model is fitted for each value in `k` and
#' the optimal number of clusters is selected based on the specified
#' information criterion.
#'
#' @examples
#' \dontrun{
#' # Fit a MMM with k = 3
#' model1 <- cluster_mmm(engagement, k = 3)
#'
#' # Find optimal k for k = 2,3,4 using BIC
#' model2 <- cluster_mmm(engagement, k = 2:4, criterion = "bic")
#' }
#'
cluster_mmm <- function(data, cols = seq(1L, ncol(data)), formula,
                        k, criterion = "bic", n_starts = 10L, min_size = 1L,
                        progressbar = TRUE, max_iter = 500L, reltol = 1e-10,
                        seed = 1L, parallel = FALSE, n_cores, cl) {
  data_name <- deparse(substitute(data))
  mm <- ifelse_(
    missing(formula),
    NULL,
    stats::model.matrix(formula, data = data)
  )
  data <- create_seqdata(x = data, cols = cols)
  criterion <- check_match(criterion, c("bic", "aic"))
  check_values(n_starts, strict = TRUE)
  check_values(min_size, strict = TRUE)
  check_values(max_iter, strict = TRUE)
  check_values(reltol, type = "numeric", strict = TRUE)
  check_flag(progressbar)
  check_flag(parallel)
  check_range(
    k, scalar = FALSE, type = "integer", lower = 2L, upper = nrow(data) - 1L
  )
  s <- length(attr(data, "labels"))
  if (parallel && missing(cl)) {
    stopifnot_(
      requireNamespace("parallel", quietly = TRUE) &&
        requireNamespace("doParallel", quietly = TRUE),
      "Please install the {.pkg parallel}, {.pkg doParallel}
       packages for parallel computation."
    )
    n_cores <- ifelse_(missing(n_cores), parallel::detectCores(), n_cores)
    n_cores <- min(n_cores, parallel::detectCores())
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }
  parallel <- parallel || !missing(cl)
  if (parallel) {
    stopifnot_(
      inherits(cl, "cluster"),
      "Argument {.arg cl} must be a {.cls cluster} object."
    )
    stopifnot_(
      isTRUE(foreach::getDoParRegistered()),
      "A parallel backend must be registered for parallel computation."
    )
    dopar <- foreach::getDoParName()
    workers <- foreach::getDoParWorkers()
    message_("Using {dopar} with {workers} workers.")
  }
  k_len <- length(k)
  results <- vector(mode = "list", length = k_len)
  if (progressbar && k_len > 1L) {
    cli::cli_progress_bar(
      name = "Estimating mixture Markov models",
      total = k_len
    )
  }
  for (i in seq_along(k)) {
    res <- tryCatch(
      fit_mmm(
        data = data,
        mm = mm,
        k = k[i],
        n_starts = n_starts,
        min_size = min_size,
        progressbar = progressbar && k_len == 1L,
        max_iter = max_iter,
        reltol = reltol,
        seed = seed,
        parallel = parallel,
        cl = cl
      ),
      error = function(e) {
        print(e)
        NULL
      }
    )
    if (progressbar && k_len > 1L) {
      cli::cli_progress_update()
    }
    if (!is.null(res)) {
      results[[i]] <- res
    }
  }
  if (progressbar && k_len > 1L) {
    cli::cli_progress_done()
  }
  if (k_len > 1L) {
    nulls <- vapply(results, is.null, logical(1L))
    stopifnot_(
      !all(nulls),
      "Fitting the model failed with all values of {.arg k}."
    )
    if (any(nulls)) {
      failed <- k[which(nulls)]
      k_failed <- cs(failed)
      warning_(
        "Fitting the model with k = {k_failed} failed."
      )
    }
    results <- results[!nulls]
    criteria <- vapply(results, "[[", numeric(1L), criterion)
    out <- results[[which.min(criteria)]]
  } else {
    stopifnot_(
      !is.null(results[[1L]]),
      "Fitting the model with k = {k} failed."
    )
    out <- results[[1L]]
  }
  out$data <- data
  out$data_name <- data_name
  if (!missing(formula)) {
    out$formula <- formula
  }
  structure(
    out,
    class = "tna_mmm"
  )
}

#' @param k An `integer` for the number of mixture components
#' @inheritParams cluster_mmm
#' @noRd
fit_mmm <- function(data, mm, k, n_starts, min_size, progressbar,
                    max_iter, reltol, seed, parallel, cl) {
  `%d%` <- ifelse_(parallel, foreach::`%dopar%`, foreach::`%do%`)
  progressbar <- progressbar && !parallel
  if (progressbar) {
    cli::cli_progress_bar(
      name = "Running EM Algorithm",
      total = n_starts
    )
  }
  s <- length(attr(data, "labels"))
  em_fun <- ifelse_(is.null(mm), em, em_covariates)
  # Avoid NSE Note with foreach index variable
  i <- NULL
  results <- foreach::foreach(i = seq_len(n_starts)) %d% {
    res <- em_fun(i, seed, data, mm, k, s, max_iter, reltol)
    if (progressbar) {
      cli::cli_progress_update()
    }
    res
  }
  valid <- vapply(
    results,
    function(x) {
      x$converged && min(x$sizes) >= min_size
    },
    logical(1L)
  )
  valid_results <- results[valid]
  stopifnot_(
    any(valid),
    "All EM algorithm runs failed."
  )
  logliks <- vapply(valid_results, "[[", numeric(1L), "loglik")
  best <- valid_results[[which.max(logliks)]]
  q <- ifelse_(is.null(mm), 1L, ncol(mm))
  n <- nrow(data)
  # Parameters: mixture + initial + transition
  n_param <- (k - 1) * q + k * (s - 1) + k * s * (s - 1)
  aic <- -2 * best$loglik + 2 * n_param
  bic <- -2 * best$loglik + log(n) * n_param
  if (progressbar) {
    cli::cli_progress_done()
  }
  structure(
    list(
      assignments = best$assignments,
      posterior = best$posterior,
      loglik = best$loglik,
      k = k,
      bic = bic,
      aic = aic,
      models = best$models,
      mixture = best$mixture,
      n_parameters = n_param,
      converged = best$converged,
      iterations = best$iterations,
      sizes = best$sizes,
      proportions = best$sizes / n,
      states = attr(data, "labels")
    ),
    class = "tna_mmm"
  )
}

# The EM Algorithm for a mixture Markov Model
em <- function(start, seed, data, mm, k, s, max_iter, reltol) {
  set.seed(seed + start)
  # For some reason export does not work for this function
  # defining it here instead as a workaround
  log_sum_exp_rows <- function(x, m, n) {
    maxs <- apply(x, 1L, max)
    maxs + log(.rowSums(exp(x - maxs), m, n))
  }
  n <- nrow(data)
  p <- ncol(data)
  mixture <- stats::runif(k)
  mixture <- mixture / sum(mixture)
  models <- vector("list", length = k)
  for (i in seq_len(k)) {
    inits <- stats::runif(s)
    inits <- inits / sum(inits)
    trans <- matrix(stats::runif(s^2), s, s)
    trans <- trans / .rowSums(trans, s, s)
    models[[i]] <- list(inits = inits, trans = trans)
  }
  idx <- seq_len(n)
  trans <- array(0L, dim = c(n, s, s))
  from_na <- is.na(data[, 1L])
  init_state <- data[!from_na, 1L]
  init_state_idx <- lapply(1:s, function(x) which(init_state == x))
  for (t in seq_len(p - 1L)) {
    from <- data[, t]
    to <- data[, t + 1L]
    to_na <- is.na(to)
    any_na <- from_na | to_na
    new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
    trans[new_trans] <- trans[new_trans] + 1L
    from_na <- to_na
  }
  iter <- 0L
  loglik <- 0
  loglik_prev <- 0
  loglik_reldiff <- Inf
  loglik_mat <- matrix(-Inf, n, k)
  log_sum_exp_vec <- numeric(n)
  posterior <- matrix(NA, n, k)
  post_clust_arr <- array(0, dim = c(n, s, s))
  while (loglik_reldiff > reltol && iter < max_iter) {
    iter <- iter + 1L
    # E-step
    for (j in 1:k) {
      trans_prob <- models[[j]]$trans
      log_prob <- log(models[[j]]$inits[init_state]) + log(mixture[j])
      loglik_mat[, j] <- log_prob + apply(trans, 1L, function(x) {
        sum(x * log(trans_prob + 1e-10))
      })
    }
    log_sum_exp_vec <- log_sum_exp_rows(loglik_mat, m = n, n = k)
    posterior[] <- exp(loglik_mat - log_sum_exp_vec)
    # M-step
    mixture <- .colMeans(posterior, m = n, n = k)
    for (j in 1:k) {
      post_clust <- posterior[, j]
      inits <- vapply(
        1:s,
        function(x) {
          sum(post_clust[init_state_idx[[x]]])
        },
        numeric(1L)
      )
      models[[j]]$inits <- (inits + 1e-10) / sum(inits + s * 1e-10)
      post_clust_arr[] <- rep(post_clust, s^2)
      trans_new <- apply(trans * post_clust_arr, c(2L, 3L), sum)
      models[[j]]$trans <- trans_new / .rowSums(trans_new, s, s)
    }
    loglik <- sum(log_sum_exp_vec)
    if (iter > 1L) {
      loglik_reldiff <- (loglik - loglik_prev) / (abs(loglik_prev) + 0.1)
    }
    loglik_prev <- loglik
  }
  assignments <- max.col(posterior)
  list(
    assignments = assignments,
    posterior = posterior,
    loglik = loglik,
    models = models,
    mixture = mixture,
    converged = iter < max_iter,
    iterations = iter,
    sizes = table(factor(assignments, levels = 1:k))
  )
}

# The EM Algorithm for a mixture Markov Model with Covariates
em_covariates <- function(start, seed, data, mm, k, s, max_iter, reltol) {
  set.seed(seed + start)
  # For some reason export does not work for this function
  # defining it here instead as a workaround
  log_sum_exp_rows <- function(x, m, n) {
    maxs <- apply(x, 1L, max)
    maxs + log(.rowSums(exp(x - maxs), m, n))
  }
  n <- nrow(data)
  p <- ncol(data)
  q <- ncol(mm)
  models <- vector("list", length = k)
  for (i in seq_len(k)) {
    inits <- stats::runif(s)
    inits <- inits / sum(inits)
    trans <- matrix(stats::runif(s^2), s, s)
    trans <- trans / .rowSums(trans, s, s)
    models[[i]] <- list(
      inits = inits,
      trans = trans,
      beta = rep(0, q) + (i > 1) * stats::runif(q, -0.01, 0.01)
    )
  }
  idx <- seq_len(n)
  trans <- array(0L, dim = c(n, s, s))
  from_na <- is.na(data[, 1L])
  init_state <- data[!from_na, 1L]
  init_state_idx <- lapply(1:s, function(x) which(init_state == x))
  for (t in seq_len(p - 1L)) {
    from <- data[, t]
    to <- data[, t + 1L]
    to_na <- is.na(to)
    any_na <- from_na | to_na
    new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
    trans[new_trans] <- trans[new_trans] + 1L
    from_na <- to_na
  }
  iter <- 0L
  loglik <- 0
  loglik_prev <- 0
  loglik_reldiff <- Inf
  loglik_mat <- matrix(-Inf, n, k)
  log_sum_exp_vec <- numeric(n)
  linpred <- matrix(0, n, k)
  mixture <- matrix(0, n, k)
  posterior <- matrix(NA, n, k)
  post_clust_arr <- array(0, dim = c(n, s, s))
  while (loglik_reldiff > reltol && iter < max_iter) {
    iter <- iter + 1L
    # E-step
    for (j in 1:k) {
      linpred[ ,j] <- mm %*% models[[j]]$beta
    }
    mixture[] <- exp(linpred - log_sum_exp_rows(linpred, m = n, n = k))
    for (j in 1:k) {
      trans_prob <- models[[j]]$trans
      log_prob <- log(models[[j]]$inits[init_state]) + log(mixture[, j])
      loglik_mat[, j] <- log_prob + apply(trans, 1L, function(x) {
        sum(x * log(trans_prob + 1e-10))
      })
    }
    log_sum_exp_vec <- log_sum_exp_rows(loglik_mat, m = n, n = k)
    posterior[] <- exp(loglik_mat - log_sum_exp_vec)
    # M-step
    for (j in 1:k) {
      # Iteratively Reweighted Least Squares
      if (j > 1) {
        beta_prev <- models[[j]]$beta
        beta_prev_resid <- sum(beta_prev^2)
        beta <- 0
        for (r in 1:max_iter) {
          w <- mixture[, j] * (1 - mixture[, j])
          z <- linpred[, j] + (posterior[, j] - mixture[, j]) / w
          Wdiag <- diag(w)
          WtWdiag <- crossprod(mm, Wdiag)
          beta <- solve(WtWdiag %*% mm) %*% WtWdiag %*% z
          beta_resid <- sum((beta - beta_prev)^2)
          rel <- beta_resid / (beta_prev_resid + 1e-9)
          if (rel < reltol) {
            break
          }
          beta_prev <- beta
          beta_prev_resid <- beta_resid
        }
        models[[j]]$beta <- beta
      }
      post_clust <- posterior[, j]
      inits <- vapply(
        1:s,
        function(x) {
          sum(post_clust[init_state_idx[[x]]])
        },
        numeric(1L)
      )
      models[[j]]$inits <- (inits + 1e-10) / sum(inits + s * 1e-10)
      post_clust_arr[] <- rep(post_clust, s^2)
      trans_new <- apply(trans * post_clust_arr, c(2L, 3L), sum)
      models[[j]]$trans <- trans_new / .rowSums(trans_new, s, s)
    }
    loglik <- sum(log_sum_exp_vec)
    if (iter > 1L) {
      loglik_reldiff <- (loglik - loglik_prev) / (abs(loglik_prev) + 0.1)
    }
    loglik_prev <- loglik
  }
  assignments <- max.col(posterior)
  list(
    assignments = assignments,
    posterior = posterior,
    loglik = loglik,
    models = models,
    mixture = mixture,
    converged = iter < max_iter,
    iterations = iter,
    sizes = table(factor(assignments, levels = 1:k))
  )
}

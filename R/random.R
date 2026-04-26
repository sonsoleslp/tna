#' Build a Random Transition Network Analysis Model
#'
#' @description
#' Construct a fully-functional `tna` object from synthetic parameters
#' without needing pre-existing sequence data. Random transition probabilities
#' are drawn from a Dirichlet distribution, sequences are simulated from the
#' resulting model, and a canonical `tna` object is fitted on those sequences.
#'
#' Calling `random_tna()` with no arguments returns a fresh, sticky network
#' with a coherent alphabet, drawn fresh on every call.
#'
#' @export
#' @family data
#' @param n_states An `integer` >= 2 giving the number of states. If `NULL`
#'   (the default), a value is drawn from `7:11` on each call.
#' @param states An optional `character` vector of state labels of length
#'   at least `n_states`. The first `n_states` are used. If `NULL` (the
#'   default), labels are taken from `category` or auto-picked from a
#'   built-in pool that fits `n_states`.
#' @param category An optional `character` string naming a built-in label
#'   pool. Available pools are returned by `list_random_state_pools()`.
#'   When `NULL` (the default), a pool whose size is at least `n_states`
#'   is sampled at random. Ignored when `states` is supplied.
#' @param alpha A positive `numeric` Dirichlet concentration parameter.
#'   Small values (e.g. `0.3`) produce sparse, peaked transitions; large
#'   values (e.g. `5`) produce near-uniform transitions. If `NULL` (the
#'   default), a value is drawn from `Uniform(0.5, 1.0)` on each call.
#' @param diag_boost A non-negative `numeric` added to the diagonal of the
#'   transition matrix before re-normalising rows. Larger values make
#'   states "stickier" (more self-transitions). If `NULL` (the default),
#'   a value is drawn from `Uniform(1.5, 3.0)` on each call.
#' @param trans_matrix An optional square `numeric` matrix of transition
#'   probabilities. When supplied, `alpha` and `diag_boost` are ignored
#'   for the transition matrix. Rows are renormalised to sum to one.
#' @param init_probs An optional `numeric` vector of initial state
#'   probabilities. Renormalised to sum to one. If `NULL`, drawn from a
#'   Dirichlet on the same alphabet.
#' @param n_sequences An `integer` giving the number of sequences to
#'   simulate from the true parameters. If `NULL` (the default), a value
#'   is drawn from `500:800` on each call.
#' @param seq_length An `integer` giving the length of each simulated
#'   sequence. If `NULL` (the default), a value is drawn from `6:20` on
#'   each call.
#' @param type A `character` string giving the model type. One of
#'   `"relative"` (the default), `"frequency"`, `"co-occurrence"`,
#'   `"attention"`.
#' @param return_params A `logical`. If `TRUE`, returns a `list` with the
#'   fitted model and the ground-truth parameters used to generate it.
#'   Default is `FALSE`.
#' @param seed An `integer` random seed for reproducibility, or `NULL`
#'   (the default) for fresh randomness on every call.
#' @return A `tna` object, or a `list` with elements `model`,
#'   `trans_matrix`, `init_probs`, `sequences`, `labels`, and `category`
#'   when `return_params = TRUE`.
#' @examples
#' # Fresh random model on every call
#' model <- random_tna()
#'
#' # Explicit small-state demo using the engagement pool
#' model <- random_tna(n_states = 3, category = "engagement")
#'
#' # Reproducible model
#' model <- random_tna(seed = 42)
#'
#' # Recover the ground-truth parameters
#' out <- random_tna(seed = 7, return_params = TRUE)
#' out$trans_matrix
#'
random_tna <- function(n_states = NULL, states = NULL, category = NULL,
                       alpha = NULL, diag_boost = NULL,
                       trans_matrix = NULL, init_probs = NULL,
                       n_sequences = NULL, seq_length = NULL,
                       type = "relative", return_params = FALSE,
                       seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  type <- check_match(type, c("relative", "frequency",
                              "co-occurrence", "attention"))
  n_states <- random_default_n_states(n_states)
  alpha <- random_default_alpha(alpha)
  diag_boost <- random_default_diag_boost(diag_boost)
  n_sequences <- random_default_n_sequences(n_sequences)
  seq_length <- random_default_seq_length(seq_length)
  picked <- random_resolve_states(n_states, states, category)
  labels <- picked$labels
  n <- length(labels)
  trans_matrix <- random_resolve_trans(trans_matrix, n, alpha, diag_boost)
  init_probs <- random_resolve_inits(init_probs, n, alpha)
  dimnames(trans_matrix) <- list(labels, labels)
  names(init_probs) <- labels

  true_model <- build_model_(
    weights = trans_matrix,
    inits = init_probs,
    labels = labels,
    type = "relative",
    scaling = character(0L),
    params = NULL
  )
  sim_data <- simulate.tna(
    true_model,
    nsim = n_sequences,
    max_len = seq_length,
    seed = NULL
  )
  fitter <- switch(type,
    "relative" = tna,
    "frequency" = ftna,
    "co-occurrence" = ctna,
    "attention" = atna
  )
  model <- fitter(sim_data)
  if (!return_params) {
    return(model)
  }
  list(
    model = model,
    trans_matrix = trans_matrix,
    init_probs = init_probs,
    sequences = sim_data,
    labels = labels,
    category = picked$category
  )
}

#' Build a Random Group Transition Network Analysis Model
#'
#' @description
#' Construct a fully-functional `group_tna` object from synthetic parameters.
#' Each group receives its own randomly drawn transition matrix and initial
#' probabilities over a shared alphabet, so groups have heterogeneous
#' dynamics by default. Per-group overrides allow custom group sizes,
#' sparsity, stickiness, or hand-supplied transition matrices.
#'
#' @export
#' @family data
#' @param n_groups An `integer` giving the number of groups. If `NULL`
#'   (the default), a value is drawn from `2:4` on each call.
#' @param group_names An optional `character` vector of group names of
#'   length `n_groups`. Defaults to `"Group 1"`, `"Group 2"`, ...
#' @param per_group An optional `list` of length `n_groups`. Each element is
#'   itself a `list` of overrides applied to that group only. Recognised
#'   override names: `alpha`, `diag_boost`, `trans_matrix`, `init_probs`,
#'   `n_sequences`, `seq_length`. Unset entries fall back to the top-level
#'   defaults (which may themselves be drawn at random when `NULL`).
#' @inheritParams random_tna
#' @return A `group_tna` object.
#' @examples
#' # Fresh random group model on every call
#' model <- random_group_tna()
#'
#' # Explicit two-group engagement demo with per-group differences
#' model <- random_group_tna(
#'   n_groups  = 2,
#'   n_states  = 3,
#'   category  = "engagement",
#'   per_group = list(
#'     list(n_sequences = 400, diag_boost = 3),
#'     list(n_sequences = 100, alpha = 0.3)
#'   ),
#'   seed = 42
#' )
#'
random_group_tna <- function(n_groups = NULL, group_names = NULL,
                             n_states = NULL, states = NULL, category = NULL,
                             alpha = NULL, diag_boost = NULL,
                             n_sequences = NULL, seq_length = NULL,
                             type = "relative", per_group = NULL,
                             seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_groups <- random_default_n_groups(n_groups)
  type <- check_match(type, c("relative", "frequency",
                              "co-occurrence", "attention"))
  if (is.null(group_names)) {
    group_names <- paste0("Group ", seq_len(n_groups))
  } else {
    stopifnot_(
      is.character(group_names) && length(group_names) == n_groups,
      "Argument {.arg group_names} must be a {.cls character} vector of length
      {.arg n_groups}."
    )
  }
  if (!is.null(per_group)) {
    stopifnot_(
      is.list(per_group) && length(per_group) == n_groups,
      "Argument {.arg per_group} must be a {.cls list} of length
      {.arg n_groups}."
    )
  }
  n_states <- random_default_n_states(n_states)
  picked <- random_resolve_states(n_states, states, category)
  labels <- picked$labels
  n <- length(labels)

  group_data <- vector("list", n_groups)
  for (g in seq_len(n_groups)) {
    ov <- if (is.null(per_group)) list() else per_group[[g]] %||% list()
    g_alpha <- random_default_alpha(ov$alpha %||% alpha)
    g_diag <- random_default_diag_boost(ov$diag_boost %||% diag_boost)
    g_nseq <- random_default_n_sequences(ov$n_sequences %||% n_sequences)
    g_slen <- random_default_seq_length(ov$seq_length %||% seq_length)
    g_trans <- random_resolve_trans(ov$trans_matrix, n, g_alpha, g_diag)
    g_inits <- random_resolve_inits(ov$init_probs, n, g_alpha)
    dimnames(g_trans) <- list(labels, labels)
    names(g_inits) <- labels
    true_model <- build_model_(
      weights = g_trans,
      inits = g_inits,
      labels = labels,
      type = "relative",
      scaling = character(0L),
      params = NULL
    )
    sim <- simulate.tna(
      true_model, nsim = g_nseq, max_len = g_slen, seed = NULL
    )
    sim[[".group"]] <- group_names[g]
    group_data[[g]] <- sim
  }
  combined <- do.call(dplyr::bind_rows, group_data)
  grp <- combined[[".group"]]
  combined[[".group"]] <- NULL
  group_fitter <- switch(type,
    "relative" = group_tna,
    "frequency" = group_ftna,
    "co-occurrence" = group_ctna,
    "attention" = group_atna
  )
  group_fitter(combined, group = grp)
}

#' Build a Random Mixture Markov Model Object
#'
#' @description
#' Construct a synthetic `tna_mmm` object that mirrors the *structure* a
#' fitted seqHMM mixed Markov model exposes to [tna] without depending on
#' seqHMM at runtime. The returned object can be passed to
#' [group_model()] (dispatching via `group_model.tna_mmm`) and to
#' [mmm_stats()] (dispatching via `mmm_stats.tna_mmm`).
#'
#' Real seqHMM `mhmm` objects continue to dispatch via the original
#' `*.mhmm` methods.
#'
#' @export
#' @family data
#' @param n_clusters An `integer` giving the number of mixture clusters.
#'   If `NULL` (the default), drawn from `2:4` on each call.
#' @param n_covariates An `integer` >= 1 giving the number of regression
#'   variables (including the intercept) used to predict cluster membership.
#'   Default is 1 (intercept only). When > 1, additional rows are added to
#'   the coefficient matrix.
#' @inheritParams random_tna
#' @return An object of class `tna_mmm` containing fields
#'   `observations`, `transition_probs`, `initial_probs`, `coefficients`,
#'   `vcov`, `most_probable_cluster`, `cluster_names`, `state_names`,
#'   `n_clusters`, `n_states`, `n_sequences`, `n_covariates`.
#' @examples
#' model <- random_tna_mmm(seed = 1)
#' mmm_stats(model)
#' grp <- group_model(model)
#'
random_tna_mmm <- function(n_clusters = NULL, n_states = NULL,
                           states = NULL, category = NULL,
                           alpha = NULL, diag_boost = NULL,
                           n_sequences = NULL, seq_length = NULL,
                           n_covariates = 1L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_clusters <- random_default_n_groups(n_clusters)
  n_states <- random_default_n_states(n_states)
  alpha <- random_default_alpha(alpha)
  diag_boost <- random_default_diag_boost(diag_boost)
  n_sequences <- random_default_n_sequences(n_sequences)
  seq_length <- random_default_seq_length(seq_length)
  n_covariates <- as.integer(n_covariates)
  stopifnot_(
    length(n_covariates) == 1L && !is.na(n_covariates) && n_covariates >= 1L,
    "Argument {.arg n_covariates} must be a single positive integer."
  )

  picked <- random_resolve_states(n_states, states, category)
  state_names <- picked$labels
  cluster_names <- paste0("Cluster ", seq_len(n_clusters))

  per_cluster_share <- as.numeric(random_rdirichlet(1L, rep(2, n_clusters)))
  cluster_size <- as.integer(round(per_cluster_share * n_sequences))
  cluster_size[n_clusters] <- n_sequences - sum(cluster_size[-n_clusters])
  cluster_size[cluster_size < 1L] <- 1L

  transition_probs <- vector("list", n_clusters)
  initial_probs <- vector("list", n_clusters)
  observations_parts <- vector("list", n_clusters)
  for (k in seq_len(n_clusters)) {
    tm <- random_resolve_trans(NULL, n_states, alpha, diag_boost)
    ip <- random_resolve_inits(NULL, n_states, alpha)
    dimnames(tm) <- list(state_names, state_names)
    names(ip) <- state_names
    transition_probs[[k]] <- tm
    initial_probs[[k]] <- ip
    true_model <- build_model_(
      weights = tm, inits = ip, labels = state_names,
      type = "relative", scaling = character(0L), params = NULL
    )
    observations_parts[[k]] <- simulate.tna(
      true_model,
      nsim = cluster_size[k],
      max_len = seq_length,
      seed = NULL
    )
  }
  names(transition_probs) <- cluster_names
  names(initial_probs) <- cluster_names

  observations <- do.call(rbind, observations_parts)
  rownames(observations) <- NULL
  most_probable_cluster <- factor(
    rep(cluster_names, times = cluster_size),
    levels = cluster_names
  )

  variable_names <- if (n_covariates == 1L) {
    "(Intercept)"
  } else {
    c("(Intercept)", paste0("X", seq_len(n_covariates - 1L)))
  }
  coefficients <- matrix(
    stats::rnorm(n_covariates * n_clusters, mean = 0, sd = 1),
    nrow = n_covariates, ncol = n_clusters,
    dimnames = list(variable_names, cluster_names)
  )
  coefficients[, 1L] <- 0  # reference cluster fixed at zero

  vcov_dim <- n_covariates * (n_clusters - 1L)
  vcov_raw <- matrix(stats::rnorm(vcov_dim * vcov_dim), vcov_dim, vcov_dim)
  vcov <- crossprod(vcov_raw) / vcov_dim + diag(0.05, vcov_dim)
  vcov_labels <- paste0(
    rep(variable_names, n_clusters - 1L),
    ":",
    rep(cluster_names[-1L], each = n_covariates)
  )
  dimnames(vcov) <- list(vcov_labels, vcov_labels)

  structure(
    list(
      observations = observations,
      transition_probs = transition_probs,
      initial_probs = initial_probs,
      coefficients = coefficients,
      vcov = vcov,
      most_probable_cluster = most_probable_cluster,
      cluster_names = cluster_names,
      state_names = state_names,
      n_clusters = n_clusters,
      n_states = n_states,
      n_sequences = n_sequences,
      n_covariates = n_covariates
    ),
    class = "tna_mmm"
  )
}

#' List Built-in Label Pools for [random_tna()]
#'
#' @description
#' Return the names of the curated state-label pools available to
#' `random_tna()` and `random_group_tna()`, with their sizes.
#'
#' @export
#' @family data
#' @return A named `integer` vector mapping pool names to pool sizes.
#' @examples
#' list_random_state_pools()
#'
list_random_state_pools <- function() {
  vapply(random_state_pools, length, integer(1L))
}

# Internal --------------------------------------------------------------------

#' Built-in label pools used by [random_tna()].
#'
#' Two tiers:
#' \itemize{
#'   \item State-descriptor pools (3-5 labels): semantically ordered
#'     learning-state descriptors (engagement, motivation, ...).
#'   \item Action-verb pools (12 labels each): student learning actions
#'     by category (cognitive, metacognitive, ...).
#' }
#' @noRd
random_state_pools <- list(
  # State-descriptor pools (small, ordered)
  engagement   = c("Engaged", "Moderate", "Disengaged"),
  engagement5  = c("Highly Engaged", "Engaged", "Neutral", "Passive",
                   "Disengaged"),
  motivation   = c("Motivated", "Steady", "Demotivated"),
  motivation4  = c("Driven", "Motivated", "Hesitant", "Demotivated"),
  attention    = c("Focused", "Distracted", "Off task"),
  affect       = c("Positive", "Neutral", "Negative"),
  performance  = c("High", "Medium", "Low"),
  effort       = c("High effort", "Moderate effort", "Low effort"),
  # Action-verb pools (12 each)
  cognitive = c(
    "Read", "Study", "Analyze", "Summarize",
    "Apply", "Compare", "Synthesize", "Memorize",
    "Infer", "Interpret", "Recall", "Classify"
  ),
  metacognitive = c(
    "Plan", "Monitor", "Evaluate", "Reflect",
    "Adjust", "Strategize", "Assess", "Reconsider",
    "Set goals", "Track", "Calibrate", "Anticipate"
  ),
  behavioral = c(
    "Practice", "Annotate", "Research", "Review",
    "Revise", "Test", "Write", "Note",
    "Highlight", "Outline", "Draft", "Cite"
  ),
  social = c(
    "Discuss", "Collaborate", "Explain", "Share",
    "Help", "Teach", "Ask", "Listen",
    "Negotiate", "Critique", "Mentor", "Support"
  )
)

#' @noRd
random_default_n_states <- function(x) {
  if (is.null(x)) {
    return(sample(7:11, 1L))
  }
  x <- as.integer(x)
  stopifnot_(
    length(x) == 1L && !is.na(x) && x >= 2L,
    "Argument {.arg n_states} must be a single integer >= 2."
  )
  x
}

#' @noRd
random_default_n_groups <- function(x) {
  if (is.null(x)) {
    return(sample(2:4, 1L))
  }
  x <- as.integer(x)
  stopifnot_(
    length(x) == 1L && !is.na(x) && x >= 1L,
    "Argument {.arg n_groups} must be a single positive integer."
  )
  x
}

#' @noRd
random_default_n_sequences <- function(x) {
  if (is.null(x)) {
    return(sample(500:800, 1L))
  }
  x <- as.integer(x)
  stopifnot_(
    length(x) == 1L && !is.na(x) && x >= 1L,
    "Argument {.arg n_sequences} must be a single positive integer."
  )
  x
}

#' @noRd
random_default_seq_length <- function(x) {
  if (is.null(x)) {
    return(sample(6:20, 1L))
  }
  x <- as.integer(x)
  stopifnot_(
    length(x) == 1L && !is.na(x) && x >= 2L,
    "Argument {.arg seq_length} must be a single integer >= 2."
  )
  x
}

#' @noRd
random_default_alpha <- function(x) {
  if (is.null(x)) {
    return(stats::runif(1L, 0.5, 1.0))
  }
  stopifnot_(
    length(x) == 1L && is.finite(x) && x > 0,
    "Argument {.arg alpha} must be a single positive {.cls numeric}."
  )
  x
}

#' @noRd
random_default_diag_boost <- function(x) {
  if (is.null(x)) {
    return(stats::runif(1L, 1.5, 3.0))
  }
  stopifnot_(
    length(x) == 1L && is.finite(x) && x >= 0,
    "Argument {.arg diag_boost} must be a single non-negative {.cls numeric}."
  )
  x
}

#' Resolve the alphabet for a random TNA model.
#'
#' Returns a list with two elements:
#' \itemize{
#'   \item `labels`: the chosen `character` vector of length `n_states`.
#'   \item `category`: the pool name actually used, or `NA` if not from a
#'     pool (user-supplied `states` or fallback to `LETTERS`).
#' }
#' @noRd
random_resolve_states <- function(n_states, states, category) {
  if (!is.null(states)) {
    stopifnot_(
      is.character(states) && length(states) >= n_states,
      "Argument {.arg states} must be a {.cls character} vector of length
      at least {.arg n_states}."
    )
    return(list(labels = states[seq_len(n_states)], category = NA_character_))
  }
  if (!is.null(category)) {
    pool <- random_state_pools[[category]]
    stopifnot_(
      !is.null(pool),
      "Unknown {.arg category}. Available: {.val {names(random_state_pools)}}."
    )
    stopifnot_(
      length(pool) >= n_states,
      "Pool {.val {category}} only has {length(pool)} labels; reduce
      {.arg n_states} or supply {.arg states}."
    )
    return(list(labels = pool[seq_len(n_states)], category = category))
  }
  pool_sizes <- vapply(random_state_pools, length, integer(1L))
  is_descriptor <- pool_sizes <= 5L
  preferred <- if (n_states <= 5L) is_descriptor else !is_descriptor
  candidates <- names(pool_sizes)[preferred & pool_sizes >= n_states]
  if (length(candidates) == 0L) {
    candidates <- names(pool_sizes)[pool_sizes >= n_states]
  }
  if (length(candidates) > 0L) {
    chosen <- sample(candidates, 1L)
    pool <- random_state_pools[[chosen]]
    return(list(
      labels = sample(pool, n_states),
      category = chosen
    ))
  }
  fallback <- if (n_states <= length(LETTERS)) {
    LETTERS[seq_len(n_states)]
  } else {
    paste0("S", seq_len(n_states))
  }
  list(labels = fallback, category = NA_character_)
}

#' @noRd
random_resolve_trans <- function(trans_matrix, n, alpha, diag_boost) {
  if (!is.null(trans_matrix)) {
    stopifnot_(
      is.matrix(trans_matrix) && nrow(trans_matrix) == n &&
        ncol(trans_matrix) == n,
      "Argument {.arg trans_matrix} must be a {.cls matrix} of dimension
      {n} x {n}."
    )
    stopifnot_(
      all(is.finite(trans_matrix)) && all(trans_matrix >= 0),
      "Argument {.arg trans_matrix} must contain non-negative finite values."
    )
    rs <- rowSums(trans_matrix)
    stopifnot_(
      all(rs > 0),
      "Every row of {.arg trans_matrix} must have at least one positive value."
    )
    return(trans_matrix / rs)
  }
  m <- random_rdirichlet(n, rep(alpha, n))
  if (diag_boost > 0) {
    diag(m) <- diag(m) + diag_boost
    m <- m / rowSums(m)
  }
  m
}

#' @noRd
random_resolve_inits <- function(init_probs, n, alpha) {
  if (!is.null(init_probs)) {
    stopifnot_(
      is.numeric(init_probs) && length(init_probs) == n,
      "Argument {.arg init_probs} must be a {.cls numeric} vector of length
      {n}."
    )
    stopifnot_(
      all(is.finite(init_probs)) && all(init_probs >= 0) &&
        sum(init_probs) > 0,
      "Argument {.arg init_probs} must contain non-negative finite values
      with positive sum."
    )
    return(init_probs / sum(init_probs))
  }
  as.numeric(random_rdirichlet(1L, rep(alpha, n)))
}

#' Sample from a Dirichlet distribution (base R, no external deps).
#' @noRd
random_rdirichlet <- function(n, alpha) {
  k <- length(alpha)
  x <- matrix(
    stats::rgamma(n * k, shape = rep(alpha, each = n), rate = 1),
    nrow = n, ncol = k
  )
  x / rowSums(x)
}

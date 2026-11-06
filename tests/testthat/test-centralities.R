test_that("centralities computes correctly for a tna object", {
  result <- centralities(mock_tna)
  expect_s3_class(result, "tna_centralities")
  expect_true(all(names(centrality_funs) %in% colnames(result)))
})

test_that("centralities handles loops correctly in a tna object", {
  tna_model <- mock_tna
  result_tna <- centralities(tna_model, loops = FALSE)
  diag(tna_model$weights) <- 0
  result_manual <- centralities(tna_model, loops = TRUE)
  expect_equal(result_tna$OutStrength, result_manual$OutStrength)
  expect_equal(result_tna$InStrength, result_manual$InStrength)
})

test_that("centralities normalizes correctly for a tna object", {
  result_tna <- centralities(mock_tna, normalize = TRUE)
  result_manual <- centralities(mock_tna, normalize = FALSE)
  result_manual[, -1] <- apply(result_manual[, -1], 2, ranger)
  expect_equal(result_tna, result_manual)
})

test_that("normalization does not amplify floating-point noise", {
  nearly_constant <- c(5 + 4 * .Machine$double.eps, rep(5, 4))
  expect_true(all(is.nan(ranger(nearly_constant))))

  model <- mock_tna
  normalized <- centralities(
    model,
    loops = TRUE,
    normalize = TRUE,
    measures = "OutStrength"
  )
  expect_true(all(is.nan(normalized$OutStrength)))
})

test_that("centralities can be computed for a matrix", {
  expect_error(
    result_mat <- centralities(mock_matrix),
    NA
  )
})

test_that("PageRank matches igraph on directed weighted networks", {
  set.seed(265)
  for (n in 2:10) {
    for (iteration in 1:5) {
      weights <- matrix(stats::runif(n * n), n, n)
      weights[stats::runif(n * n) < 0.45] <- 0
      dimnames(weights) <- list(paste0("s", seq_len(n)), paste0("s", seq_len(n)))

      for (loops in c(FALSE, TRUE)) {
        graph_weights <- weights
        if (!loops) {
          diag(graph_weights) <- 0
        }
        graph <- igraph::graph_from_adjacency_matrix(
          graph_weights,
          mode = "directed",
          weighted = TRUE
        )
        expected <- igraph::page_rank(
          graph,
          directed = TRUE,
          weights = igraph::E(graph)$weight
        )$vector
        observed <- centralities(
          weights,
          loops = loops,
          measures = "PageRank"
        )$PageRank

        expect_equal(unname(observed), unname(expected), tolerance = 1e-12)
        expect_equal(sum(observed), 1, tolerance = 1e-12)
      }
    }
  }
})

test_that("centrality stability can be estimated", {
  model <- tna(mock_sequence)
  expect_error(
    estimate_cs(model, drop_prop = seq(0.3, 0.9, by = 0.1), iter = 20),
    NA
  )
})

test_that("centralities can be computed for clusters", {
  expect_error(
    centralities(mmm_model),
    NA
  )
})

test_that("centrality stability can be estimated for clusters", {
  expect_error(
    estimate_cs(
      mmm_model,
      drop_prop = seq(0.3, 0.9, by = 0.1),
      iter = 10
    ),
    NA
  )
})

test_that("progressbar works", {
  expect_error(
    suppressMessages(
      estimate_cs(
        mmm_model,
        drop_prop = seq(0.3, 0.9, by = 0.1),
        iter = 10,
        progressbar = TRUE
      )
    ),
    NA
  )
})

test_that("no cases dropped warns", {
  expect_warning(
    estimate_cs(
      mmm_model[[1]],
      drop_prop = 0.001,
      iter = 10
    ),
    "No cases dropped for proportion 0\\.001\\. Skipping\\.\\.\\."
  )
})

test_that("RSP is NA with no transitions", {
  mat <- mock_matrix
  mat[1, ] <- 0
  expect_equal(rsp_bet(mat), rep(NA_real_, ncol(mat)))
})

test_that("BetweennessRSP alone gives one NA per state with a sink", {
  model <- tna(data.frame(V1 = rep("A", 4), V2 = rep("B", 4)))
  rsp <- centralities(model, measures = "BetweennessRSP")
  expect_equal(nrow(rsp), 2L)
  expect_equal(as.character(rsp$state), c("A", "B"))
  expect_true(all(is.na(rsp$BetweennessRSP)))
})

test_that("RSP ignores finite solver noise for zero entries", {
  mat <- matrix(
    0,
    7,
    7,
    dimnames = list(paste0("s", 1:7), paste0("s", 1:7))
  )
  mat[1, ] <- c(.133, .067, .556, .089, 0, .156, 0)
  mat[2, ] <- c(.308, .385, .077, 0, .231, 0, 0)
  mat[3, ] <- c(.289, .053, 0, .526, 0, .132, 0)
  mat[4, ] <- c(.188, 0, .250, .219, .344, 0, 0)
  mat[5, ] <- c(.293, .024, .024, 0, .634, .024, 0)
  mat[6, ] <- c(.125, .063, .250, .188, .375, 0, 0)
  mat[7, ] <- c(1, 0, 0, 0, 0, 0, 0)
  mat <- mat / rowSums(mat)

  # s7 has no incoming edges, so only s7 reaches it. Before 1.5.4 the
  # correction for target s7 was counted for all 7 sources, giving
  # c(59, 16, 48, 40, 60, 17, 1); the definition (rsp_oracle() below)
  # gives the values here.
  expect_equal(rsp_bet(mat), c(53, 10, 42, 34, 54, 11, 1))
})

# ---- Diffusion is always normalized ----

test_that("diffusion is min-max normalized whatever the value of normalize", {
  model <- tna(group_regulation)
  w <- model$weights
  diag(w) <- 0
  # Banerjee et al. (2014): row sums of the sum of the first n powers
  n <- ncol(w)
  powers <- Reduce(`%*%`, rep(list(w), n), accumulate = TRUE)
  raw <- rowSums(Reduce(`+`, powers))
  expected <- (raw - min(raw)) / (max(raw) - min(raw))
  plain <- centralities(model, measures = "Diffusion")
  normalized <- centralities(model, measures = "Diffusion", normalize = TRUE)
  expect_equal(plain$Diffusion, unname(expected))
  expect_equal(plain$Diffusion, normalized$Diffusion)
})

test_that("diffusion lies between 0 and 1 for every model type", {
  models <- list(
    tna(group_regulation),
    ftna(group_regulation),
    atna(group_regulation)
  )
  ranges <- vapply(
    models,
    function(m) range(centralities(m, measures = "Diffusion")$Diffusion),
    numeric(2L)
  )
  expect_equal(ranges[1L, ], c(0, 0, 0))
  expect_equal(ranges[2L, ], c(1, 1, 1))
})

test_that("normalizing diffusion leaves the other measures unchanged", {
  model <- tna(group_regulation)
  measures <- c("OutStrength", "InStrength", "Betweenness", "Diffusion")
  with_diffusion <- centralities(model, measures = measures)
  without <- centralities(model, measures = measures[-4L])
  expect_equal(with_diffusion[, 1:4], without)
})

test_that("diffusion is NaN when it does not vary across the states", {
  # Rows of a relative model sum to one, so with loops diffusion is constant
  model <- tna(group_regulation)
  cent <- centralities(model, loops = TRUE, measures = "Diffusion")
  expect_true(all(is.nan(cent$Diffusion)))
})

test_that("case-dropping handles a constant normalized diffusion", {
  model <- tna(group_regulation[1:200, ])
  set.seed(1)
  expect_no_error(
    stab <- estimate_cs(
      model,
      loops = TRUE,
      measures = c("InStrength", "Diffusion"),
      iter = 5,
      drop_prop = c(0.3, 0.6)
    )
  )
  expect_named(stab, "InStrength")
})

test_that("permutation tests compare normalized diffusion", {
  group <- rep(c("a", "b"), each = 1000)
  models <- group_tna(group_regulation, group = group)
  set.seed(2)
  perm <- permutation_test(
    models$a,
    models$b,
    iter = 10,
    measures = "Diffusion"
  )
  cent_a <- centralities(models$a, measures = "Diffusion")
  cent_b <- centralities(models$b, measures = "Diffusion")
  expect_equal(
    perm$centralities$stats$diff_true,
    cent_a$Diffusion - cent_b$Diffusion
  )
  expect_true(all(abs(perm$centralities$stats$diff_true) <= 1))
})

# ---- Edge stability in estimate_cs() ----

test_that("edge stability correlates the edges of the network", {
  model <- tna(group_regulation[1:300, ])
  w <- model$weights
  present <- w != 0
  n <- nrow(model$data)
  set.seed(3)
  stab <- estimate_cs(
    model,
    measures = "InStrength",
    iter = 4,
    drop_prop = 0.5,
    edges = TRUE
  )
  # Same random draws as estimate_cs(): one sample() per iteration
  set.seed(3)
  expected <- vapply(
    seq_len(4),
    function(i) {
      keep <- sample(seq_len(n), n - floor(n * 0.5), replace = FALSE)
      sub <- tna(group_regulation[1:300, ][keep, ])$weights
      stats::cor(sub[present], w[present])
    },
    numeric(1L)
  )
  expect_named(stab, c("InStrength", "Edges"))
  expect_equal(stab$Edges$correlations[, 1L], expected)
})

test_that("edge stability ignores the edges absent from the network", {
  # Sparse chain: every step moves one or two states ahead (mod 8), so each
  # state has two successors and most of the 64 cells are empty
  s <- paste0("s", 1:8)
  seqs <- t(vapply(
    seq_len(60),
    function(i) s[((i + cumsum(c(0, (i * 1:11) %% 2 + 1))) %% 8) + 1],
    character(12)
  ))
  model <- tna(as.data.frame(seqs))
  w <- model$weights
  present <- w != 0
  expect_gt(sum(!present), sum(present))
  set.seed(4)
  stab <- estimate_cs(
    model,
    measures = "InStrength",
    iter = 3,
    drop_prop = 0.7,
    edges = TRUE,
    method = "spearman"
  )
  n <- nrow(model$data)
  set.seed(4)
  keep <- sample(seq_len(n), n - floor(n * 0.7), replace = FALSE)
  sub <- tna(as.data.frame(seqs[keep, ]))$weights
  all_cells <- stats::cor(c(sub), c(w), method = "spearman")
  present_only <- stats::cor(sub[present], w[present], method = "spearman")
  expect_equal(stab$Edges$correlations[1L, 1L], present_only)
  expect_gt(all_cells, present_only)
})

test_that("edges = FALSE gives the same result as before", {
  model <- tna(group_regulation[1:200, ])
  set.seed(5)
  default <- estimate_cs(model, iter = 5, drop_prop = c(0.3, 0.6))
  set.seed(5)
  explicit <- estimate_cs(
    model,
    iter = 5,
    drop_prop = c(0.3, 0.6),
    edges = FALSE
  )
  expect_identical(default, explicit)
  expect_false("Edges" %in% names(default))
})

test_that("edge stability can be estimated alone, by actor, and by group", {
  model <- tna(cbind(group_regulation[1:100, ], id = 1:100), cols = T1:T26)
  set.seed(6)
  alone <- estimate_cs(
    model,
    measures = character(0),
    iter = 5,
    drop_prop = c(0.3, 0.6),
    edges = TRUE
  )
  expect_named(alone, "Edges")
  set.seed(6)
  nested <- estimate_cs(
    model,
    measures = character(0),
    iter = 5,
    drop_prop = c(0.3, 0.6),
    edges = TRUE,
    actor = "id"
  )
  expect_identical(nested, alone)
  groups <- group_tna(group_regulation, group = rep(1:2, each = 1000))
  set.seed(7)
  by_group <- estimate_cs(
    groups,
    measures = "InStrength",
    iter = 3,
    drop_prop = 0.5,
    edges = TRUE
  )
  expect_s3_class(by_group, "group_tna_stability")
  expect_true(all(vapply(by_group, \(y) "Edges" %in% names(y), logical(1L))))
})

test_that("edge stability warns when the edge weights do not vary", {
  model <- tna(data.frame(T1 = c("A", "B"), T2 = c("B", "A")))
  expect_warning(
    stab <- estimate_cs(model, measures = "InStrength", iter = 3,
                        drop_prop = 0.5, edges = TRUE),
    class = "tna_warning_edges"
  )
  expect_false("Edges" %in% names(stab))
})

test_that("stability print names the edges when they are included", {
  model <- tna(group_regulation[1:200, ])
  set.seed(8)
  stab <- estimate_cs(model, measures = "InStrength", iter = 3,
                      drop_prop = 0.5, edges = TRUE)
  expect_output(print(stab), "Centrality and Edge Stability Coefficients")
  set.seed(8)
  stab <- estimate_cs(model, measures = "InStrength", iter = 3,
                      drop_prop = 0.5)
  expect_output(print(stab), "^Centrality Stability Coefficients")
})

test_that("summary of case-dropping lists the CS-coefficients", {
  model <- tna(group_regulation[1:200, ])
  set.seed(9)
  stab <- estimate_cs(model, measures = c("InStrength", "Betweenness"),
                      iter = 5, drop_prop = c(0.3, 0.6), edges = TRUE)
  tab <- summary(stab)
  expect_equal(tab$measure, c("InStrength", "Betweenness", "Edges"))
  expect_equal(
    tab$cs_coefficient,
    unname(vapply(stab, "[[", numeric(1L), "cs_coefficient"))
  )
  groups <- group_tna(group_regulation, group = rep(c("a", "b"), each = 1000))
  set.seed(10)
  by_group <- summary(estimate_cs(groups, measures = "InStrength", iter = 3,
                                  drop_prop = 0.5))
  expect_equal(by_group$group, c("a", "b"))
})

test_that("summary of case-dropping keeps its columns when nothing is left", {
  # Constant edges are skipped and no centrality is requested; the expected
  # warnings (constant edges, tiny samples) are tested elsewhere
  sink <- data.frame(V1 = rep("A", 8), V2 = rep("B", 8))
  set.seed(11)
  stab <- suppressWarnings(
    estimate_cs(tna(sink), measures = character(0), edges = TRUE, iter = 3)
  )
  tab <- summary(stab)
  expect_named(tab, c("measure", "cs_coefficient"))
  expect_equal(nrow(tab), 0L)
  groups <- group_tna(sink, group = rep(c("x", "y"), each = 4))
  set.seed(12)
  by_group <- summary(suppressWarnings(
    estimate_cs(groups, measures = character(0), edges = TRUE, iter = 3)
  ))
  expect_named(by_group, c("group", "measure", "cs_coefficient"))
  expect_equal(nrow(by_group), 0L)
  # A group with results next to an empty one keeps its rows
  varied <- data.frame(
    V1 = group_regulation$T1[1:40],
    V2 = group_regulation$T2[1:40]
  )
  mixed <- group_tna(
    rbind(sink, varied),
    group = rep(c("x", "y"), c(8, 40))
  )
  set.seed(13)
  by_group <- summary(suppressWarnings(
    estimate_cs(mixed, measures = character(0), edges = TRUE, iter = 3,
                drop_prop = 0.5)
  ))
  expect_equal(by_group$group, "y")
  expect_equal(by_group$measure, "Edges")
})

test_that("states are named when the only measure returns no names", {
  model <- tna(group_regulation)
  measures <- c("Diffusion", "BetweennessRSP", "Clustering")
  states <- vapply(
    measures,
    function(m) {
      paste(as.character(centralities(model, measures = m)$state),
            collapse = ",")
    },
    character(1L)
  )
  expect_true(all(states == paste(model$labels, collapse = ",")))
})

test_that("an undefined stability correlation counts against the CS", {
  # 99 undefined draws and 1 above the threshold: 1% reach it, not 100%
  corr <- matrix(c(rep(NA_real_, 99), 0.9), ncol = 1L)
  expect_equal(calculate_cs(corr, 0.7, 0.95, 0.5), 0)
  # Fully defined draws are unaffected
  corr <- matrix(c(rep(0.8, 96), rep(0.5, 4)), ncol = 1L)
  expect_equal(calculate_cs(corr, 0.7, 0.95, 0.5), 0.5)
  corr <- matrix(c(rep(0.8, 94), rep(NA_real_, 6)), ncol = 1L)
  expect_equal(calculate_cs(corr, 0.7, 0.95, 0.5), 0)
})

# Randomized shortest path betweenness by its definition (Kivimaki et al.,
# 2016): make the target t absorbing and sum the expected visits to each
# node k != t over the walks from every source s that reaches t.
rsp_oracle <- function(mat, beta = 0.01) {
  n <- ncol(mat)
  p_ref <- mat / rowSums(mat)
  cost <- 1 / mat
  cost[is.infinite(cost)] <- 0
  w <- p_ref * exp(-beta * cost)
  pairs <- subset(expand.grid(s = seq_len(n), t = seq_len(n)), s != t)
  visits <- vapply(
    seq_len(nrow(pairs)),
    function(r) {
      s <- pairs$s[r]
      t <- pairs$t[r]
      w_t <- w
      w_t[t, ] <- 0
      z <- solve(diag(n) - w_t)
      if (z[s, t] < 1e-12) {
        return(numeric(n))
      }
      v <- z[s, ] * z[, t] / z[s, t]
      v[t] <- 0
      v
    },
    numeric(n)
  )
  out <- round(unname(rowSums(visits)))
  out - min(out) + 1
}

test_that("RSP betweenness matches its definition on connected networks", {
  set.seed(17)
  mats <- replicate(
    10,
    {
      m <- matrix(stats::runif(25), 5, dimnames = list(LETTERS[1:5],
                                                         LETTERS[1:5]))
      diag(m) <- 0
      m
    },
    simplify = FALSE
  )
  lapply(mats, function(m) expect_equal(rsp_bet(m), rsp_oracle(m)))
})

test_that("RSP betweenness counts only reachable pairs", {
  st <- LETTERS[1:5]
  w <- matrix(0, 5, 5, dimnames = list(st, st))
  w[1, 2] <- w[2, 1] <- 1
  w[3:5, 3:5] <- 1
  diag(w) <- 0
  rsp <- centralities(w, measures = "BetweennessRSP")$BetweennessRSP
  expect_equal(rsp, rsp_oracle(w))
  expect_equal(rsp, c(1, 1, 4, 4, 4))
  # Relabelling the states permutes the result
  perm <- c(4, 1, 5, 2, 3)
  rsp_perm <- centralities(w[perm, perm], measures = "BetweennessRSP")
  expect_equal(rsp_perm$BetweennessRSP, rsp[perm])
})

test_that("pruning does not enter the centrality stability estimate", {
  model <- tna(group_regulation[1:200, ])
  pruned <- prune(model, threshold = 0.1)
  set.seed(2)
  cs_pruned <- estimate_cs(
    pruned, iter = 20, drop_prop = c(0.1, 0.5), edges = TRUE
  )
  set.seed(2)
  cs_model <- estimate_cs(
    model, iter = 20, drop_prop = c(0.1, 0.5), edges = TRUE
  )
  expect_equal(cs_pruned, cs_model)
})

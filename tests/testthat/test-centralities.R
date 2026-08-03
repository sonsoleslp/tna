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
  expect_equal(rsp_bet(mat), NA)
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

  expect_equal(rsp_bet(mat), c(59, 16, 48, 40, 60, 17, 1))
})

test_that("betweenness network can be constructed", {
  expect_error(
    bet_model <- betweenness_network(mock_tna),
    NA
  )
})

# Edge betweenness computed directly with igraph, edge by edge
igraph_edge_betweenness <- function(w, directed = TRUE, invert = TRUE) {
  g <- igraph::graph_from_adjacency_matrix(w, mode = "directed",
                                           weighted = TRUE)
  weight <- igraph::E(g)$weight
  igraph::E(g)$weight <- if (invert) 1 / weight else weight
  ends <- igraph::as_edgelist(g, names = FALSE)
  out <- matrix(0, nrow(w), ncol(w), dimnames = dimnames(w))
  out[ends] <- igraph::edge_betweenness(g, directed = directed)
  out
}

test_that("edge betweenness matches igraph edge by edge", {
  model <- tna(group_regulation)
  expect_equal(
    betweenness_network(model)$weights,
    igraph_edge_betweenness(model$weights)
  )
  expect_equal(
    betweenness_network(model, directed = FALSE, invert = FALSE)$weights,
    igraph_edge_betweenness(model$weights, FALSE, FALSE)
  )
})

test_that("edge betweenness networks record the model they were built from", {
  model <- ftna(group_regulation, scaling = "minmax")
  bet <- betweenness_network(model, directed = FALSE, invert = FALSE)
  expect_equal(
    attr(bet, "params")$betweenness,
    list(type = "frequency", scaling = "minmax", directed = FALSE,
         invert = FALSE)
  )
})

test_that("rebuilding an edge betweenness network from its data is exact", {
  bet <- betweenness_network(tna(group_regulation))
  rebuilt <- initialize_model(
    bet$data,
    attr(bet, "type"),
    attr(bet, "scaling"),
    attr(bet, "params")
  )
  expect_equal(rebuilt$weights, bet$weights)
})

test_that("permutation tests recompute edge betweenness in every permutation", {
  group <- rep(c("a", "b"), each = 1000)
  models <- group_tna(group_regulation, group = group)
  bet_a <- betweenness_network(models$a)
  bet_b <- betweenness_network(models$b)
  set.seed(1)
  perm <- permutation_test(bet_a, bet_b, iter = 49)
  stats <- perm$edges$stats
  differ <- stats$diff_true != 0
  expect_equal(perm$edges$diffs_true, bet_a$weights - bet_b$weights)
  # Before the fix every permuted difference was zero, so every differing
  # edge had the smallest possible p-value
  expect_gt(sum(stats$p_value[differ] > 1 / 50), 0)
})

test_that("bootstrap and case-dropping resample edge betweenness networks", {
  bet <- betweenness_network(tna(group_regulation[1:300, ]))
  set.seed(2)
  boot <- bootstrap(bet, iter = 20)
  expect_gt(nrow(boot$summary), 0)
  expect_equal(unname(boot$weights_orig), unname(bet$weights))
  set.seed(3)
  stab <- estimate_cs(bet, measures = "InStrength", iter = 5,
                      drop_prop = c(0.2, 0.4))
  expect_true(all(stab$InStrength$correlations > 0, na.rm = TRUE))
})

test_that("edge betweenness networks without their source cannot be resampled", {
  bet <- betweenness_network(tna(group_regulation[1:100, ]))
  attr(bet, "params") <- NULL
  expect_error(bootstrap(bet, iter = 5), class = "tna_error_betweenness")
})

test_that("edge betweenness networks are compared only when built alike", {
  group <- rep(c("a", "b"), each = 1000)
  models <- group_tna(group_regulation, group = group)
  bet_a <- betweenness_network(models$a)
  expect_error(
    permutation_test(
      bet_a,
      betweenness_network(models$b, invert = FALSE),
      iter = 5
    ),
    class = "tna_error_betweenness"
  )
  expect_error(
    permutation_test(bet_a, models$b, iter = 5),
    class = "tna_error_betweenness"
  )
  expect_error(
    betweenness_network(bet_a),
    class = "tna_error_betweenness"
  )
})

test_that("edge betweenness networks of groups are compared in one call", {
  groups <- group_tna(group_regulation, group = rep(c("a", "b"), each = 1000))
  bet <- betweenness_network(groups)
  expect_s3_class(bet, "group_tna")
  expect_equal(attr(bet, "type"), "betweenness")
  expect_equal(bet$a$weights, betweenness_network(groups$a)$weights)
  set.seed(4)
  by_group <- permutation_test(bet, iter = 9)
  set.seed(4)
  by_pair <- permutation_test(
    betweenness_network(groups$a),
    betweenness_network(groups$b),
    iter = 9
  )
  expect_equal(by_group[[1L]]$edges$stats, by_pair$edges$stats)
})

test_that("edge betweenness of scaled groups is not resampled", {
  bet_of <- function(groupwise) {
    betweenness_network(
      group_model(
        group_regulation,
        group = rep(c("a", "b"), each = 1000),
        scaling = "rank",
        groupwise = groupwise
      )
    )
  }
  vapply(
    c(FALSE, TRUE),
    function(groupwise) {
      bet <- bet_of(groupwise)
      expect_error(bootstrap(bet, iter = 3), class = "tna_error_scaling")
      expect_error(bootstrap(bet$a, iter = 3), class = "tna_error_scaling")
      expect_error(
        permutation_test(bet$a, bet$b, iter = 3),
        class = "tna_error_scaling"
      )
      expect_error(
        estimate_cs(bet$a, measures = "InStrength", iter = 3),
        class = "tna_error_scaling"
      )
      TRUE
    },
    logical(1L)
  )
  # The networks themselves are built as before
  groups <- group_model(
    group_regulation,
    group = rep(c("a", "b"), each = 1000),
    scaling = "rank"
  )
  expect_equal(
    betweenness_network(groups)$a$weights,
    edge_betweenness_weights(groups$a$weights, TRUE, TRUE)
  )
})

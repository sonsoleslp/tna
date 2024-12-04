test_that("tna models can be plotted", {
  expect_error(
    plot.tna(mock_tna),
    NA
  )
})

test_that("histogram of edge weights can be plotted", {
  expect_error(
    hist.tna(mock_tna),
    NA
  )
})

test_that("centralities can be plotted", {
  cm <- centralities(mock_tna)
  pdf(NULL)
  expect_error(
    plot.tna_centralities(cm),
    NA
  )
})

test_that("cliques can be plotted", {
  cliq <- cliques(mock_tna, size = 2)
  expect_error(
    plot.tna_cliques(cliq),
    NA
  )
})

test_that("communities can be plotted", {
  comm <- communities(mock_tna)
  expect_error(
    plot.tna_communities(comm),
    NA
  )
})

test_that("centrality stability can be plotted", {
  model <- tna(mock_sequence)
  out <- estimate_cs(model, drop_prop = seq(0.3, 0.9, by = 0.2), iter = 10)
  pdf(NULL)
  expect_error(
    plot.tna_stability(out),
    NA
  )
})

test_that("permutation test significant edges can be plotted", {
  model_x <- tna(group_regulation[1:100, ])
  model_y <- tna(group_regulation[101:200, ])
  perm <- permutation_test(model_x, model_y, iter = 20)
  expect_error(
    plot.tna_permutation(perm),
    NA
  )
})

test_that("model comparison can be plotted", {
  model_x <- tna(engagement[engagement[, 1] == "Active", ])
  model_y <- tna(engagement[engagement[, 1] != "Active", ])
  expect_error(
    plot_compare(model_x, model_y),
    NA
  )
})

test_that("edge weight matrix can be plotted", {
  expect_error(
    plot_model(mock_matrix),
    NA
  )
})
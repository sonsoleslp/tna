test_that("tna models can be plotted", {
  pdf(NULL)
  expect_error(
    plot.tna(mock_tna),
    NA
  )
})

test_that("histogram of edge weights can be plotted", {
  pdf(NULL)
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
  expect_error(
    plot.tna_centralities(cm, colors = color_palette(4)),
    NA
  )
  expect_error(
    plot.tna_centralities(cm, colors = "red"),
    NA
  )
})

test_that("cliques can be plotted", {
  cliq <- cliques(mock_tna, size = 2)
  pdf(NULL)
  expect_error(
    plot.tna_cliques(cliq),
    NA
  )
})

test_that("communities can be plotted", {
  comm <- communities(mock_tna)
  pdf(NULL)
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
  pdf(NULL)
  expect_error(
    plot.tna_permutation(perm),
    NA
  )
})

test_that("model comparison can be plotted", {
  model_x <- tna(engagement[engagement[, 1] == "Active", ])
  model_y <- tna(engagement[engagement[, 1] != "Active", ])
  model_grouped <- group_tna(engagement_mmm)
  pdf(NULL)
  expect_error(
    plot_compare(model_x, model_y),
    NA
  )
  expect_error(
    plot_compare(model_grouped),
    NA
  )
})

test_that("edge weight matrix can be plotted", {
  pdf(NULL)
  expect_error(
    plot_model(mock_matrix),
    NA
  )
})

test_that("plotting with different layouts works", {
  expect_error(
    plot.tna(mock_tna, layout = "circle"),
    NA
  )
  expect_error(
    plot.tna(mock_tna, layout = matrix(rnorm(8), 4, 2)),
    NA
  )
  expect_error(
    plot.tna(mock_tna, layout = igraph::layout_nicely),
    NA
  )
  expect_error(
    plot.tna(
      mock_tna,
      layout = igraph::layout_as_tree,
      layout_args = list(flip.y = FALSE)
    ),
    NA
  )
  expect_error(
    plot_model(mock_matrix, layout = "circle"),
    NA
  )
  expect_error(
    plot_model(mock_matrix, layout = matrix(rnorm(8), 4, 2)),
    NA
  )
  expect_error(
    plot_model(mock_matrix, layout = igraph::layout_nicely),
    NA
  )
  expect_error(
    plot_model(
      mock_matrix,
      layout = igraph::layout_as_tree,
      layout_args = list(flip.y = FALSE)
    ),
    NA
  )
})

test_that("warning is issued by plot if no cliques are found", {
  cliq <- cliques(mock_tna, size = 2, threshold = 0.5)
  expect_warning(
    plot.tna_cliques(cliq),
    "No 2-cliques were found in the network\\."
  )
})

test_that("group model can be plotted", {
  expect_error(
    plot(mmm_model),
    NA
  )
  expect_error(
    plot(mmm_model, title = "Clusters"),
    NA
  )
})

test_that("centralities can be plotted for clusters", {
  cm <- centralities(mmm_model)
  expect_error(
    plot(cm),
    NA
  )
})

test_that("centrality stability results can be plotted for clusters", {
  stability <- estimate_cs(
    mmm_model,
    drop_prop = seq(0.3, 0.9, by = 0.1),
    iter = 10
  )
  expect_error(
    plot(stability),
    NA
  )
})

test_that("cliques can be plotted clusters", {
  cliq <- cliques(mmm_model, size = 2)
  expect_error(
    plot(cliq),
    NA
  )
  expect_error(
    plot(cliq, title = "Clusters"),
    NA
  )
})

test_that("communities can plotted for clusters", {
  comm <- communities(mmm_model)
  expect_error(
    plot(comm),
    NA
  )
  expect_error(
    plot(comm, title = "Community detection"),
    NA
  )
})

test_that("histogram of edge weights can be plotted", {
  pdf(NULL)
  expect_error(
    hist(mmm_model),
    NA
  )
})

test_that("comparison results can be plotted", {
  model_x <- tna(group_regulation[1:200, ])
  model_y <- tna(group_regulation[1001:1200, ])
  # Comparing models
  comp <- compare(model_x, model_y)
  expect_error(
    plot(comp, type = "heatmap"),
    NA
  )
  expect_error(
    plot(comp, type = "scatterplot"),
    NA
  )
  expect_error(
    plot(comp, type = "centrality_heatmap"),
    NA
  )
  expect_error(
    plot(comp, type = "weight_density"),
    NA
  )
})

test_that("pruned models can be plotted", {
  model_pruned <- prune(mock_tna)
  expect_error(
    plot(model_pruned),
    NA
  )
})

test_that("mosaic can be plotted", {
  ftna_model <- ftna(engagement)
  group_ftna_model <- group_ftna(engagement_mmm)
  expect_error(
    plot_mosaic(ftna_model),
    NA
  )
  expect_error(
    plot_mosaic(group_ftna_model),
    NA
  )
  expect_error(
    plot_mosaic(mock_tna_data, group = "group"),
    NA
  )
})
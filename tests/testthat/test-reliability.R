test_that("reliability can be analyzed", {
  expect_error(
    reliability(mock_tna_seq, iter = 50),
    NA
  )
  expect_error(
    reliability(mock_tna_seq, types = c("relative", "frequency"), iter = 50),
    NA
  )
})

test_that("summary of reliability lists every metric with its category", {
  model <- tna(group_regulation)
  set.seed(41)
  rel <- reliability(model, iter = 10)
  tab <- summary(rel)
  expect_named(tab, c("category", "metric", "mean", "sd", "median", "min",
                      "max", "q25", "q75"))
  expect_equal(nrow(tab), 22L)
  expect_equal(tab$metric, unique(rel$metrics$metric))
  pearson <- subset(rel$metrics, metric == "Pearson")
  expect_equal(subset(tab, metric == "Pearson")$median,
               stats::median(pearson$value))
  expect_equal(subset(tab, metric == "Pearson")$category, "Correlations")
  # Several model types: one block per type, in the order given
  set.seed(42)
  both <- summary(reliability(model, types = c("relative", "frequency"),
                              iter = 5))
  expect_equal(unique(both$model_type), c("relative", "frequency"))
  expect_equal(nrow(both), 44L)
  expect_error(summary.tna_reliability(model))
})

test_that("reliability estimates the requested types, whatever the model", {
  d <- head(group_regulation, 150)
  pearson <- function(model, types = NULL) {
    set.seed(3)  # the same splits for every call
    rel <- reliability(model, types = types, iter = 5)
    subset(rel$summary, metric == "Pearson")$mean
  }
  m_rel <- tna(d)
  m_freq <- ftna(d)
  m_cooc <- ctna(d)
  m_gap <- build_model(d, type = "gap")
  # Requesting a type gives the same result whichever model it starts from
  cooc <- pearson(m_cooc)
  expect_equal(pearson(m_rel, "co-occurrence"), cooc)
  expect_equal(pearson(m_freq, "co-occurrence"), cooc)
  expect_equal(pearson(m_rel, "gap"), pearson(m_gap))
  # ... and the types are genuinely different estimators
  expect_false(isTRUE(all.equal(cooc, pearson(m_rel, "frequency"))))
  # The default is the model's own type
  expect_equal(pearson(m_freq), pearson(m_freq, "frequency"))
  expect_equal(pearson(m_cooc), pearson(m_rel, "co-occurrence"))
  expect_error(reliability(m_rel, types = "no-such-type", iter = 2))
})

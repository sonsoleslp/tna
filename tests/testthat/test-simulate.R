test_that("transition networks can be simulated", {
  set.seed(0)
  # Special case with only one sequence
  expect_error(
    simulate(mock_tna, max_len = 10),
    NA
  )
  expect_error(
    simulate(mock_tna, nsim = 10, max_len = 10),
    NA
  )
})

test_that("missing values can be included", {
  set.seed(0)
  sim <- simulate(mock_tna, nsim = 100, max_len = 10, na_range = c(1, 3))
  nas <- apply(sim, 1L, function(y) sum(is.na(y)))
  expect_true(all(nas %in% c(1, 2, 3)))
})

test_that("transition matrix is recovered", {
  set.seed(0)
  sim <- simulate(mock_tna, nsim = 1e4, max_len = 100)
  tna_sim <- tna(sim)
  expect_equal(mock_tna$weights, tna_sim$weights, tolerance = 0.05)
})

test_that("transition networks can be simulated from `group_tna`", {
  set.seed(0)
  sim <- simulate(mock_group_tna, nsim = 100, max_len = 10, na_range = c(1, 3))
  nas <- apply(sim, 1L, function(y) sum(is.na(y)))
  expect_true(all(nas %in% c(1, 2, 3)))
})

test_that("simulate.tna returns expected shape for relative models", {
  set.seed(0)

  sim1 <- simulate(mock_tna, max_len = 10)
  expect_s3_class(sim1, "data.frame")
  expect_equal(dim(sim1), c(1L, 10L))
  expect_named(sim1, paste0("T", 1:10))

  sim10 <- simulate(mock_tna, nsim = 10, max_len = 10)
  expect_equal(dim(sim10), c(10L, 10L))
  expect_named(sim10, paste0("T", 1:10))

  # No missing values by default
  expect_true(all(!is.na(as.matrix(sim10))))
})

test_that("simulate.tna includes trailing missing values within na_range", {
  set.seed(0)

  sim <- simulate(mock_tna, nsim = 200, max_len = 10, na_range = c(1, 3))
  nas <- rowSums(is.na(sim))
  expect_true(all(nas %in% 1:3))

  # Missing values should be trailing only
  trailing_only <- apply(sim, 1L, function(y) {
    if (!any(is.na(y))) return(TRUE)
    first_na <- which(is.na(y))[1L]
    all(is.na(y[first_na:length(y)]))
  })
  expect_true(all(trailing_only))
})

test_that("simulate.tna approximately recovers the transition matrix (relative)", {
  set.seed(0)

  sim <- simulate(mock_tna, nsim = 2000, max_len = 200)
  tna_sim <- tna(sim)

  # A bit looser tolerance, but more stable across platforms/BLAS/R versions
  expect_equal(mock_tna$weights, tna_sim$weights, tolerance = 0.02)
})

test_that("simulate.tna works for frequency type by row-normalizing weights", {
  set.seed(0)

  mock_freq <- mock_tna
  attr(mock_freq, "type") <- "frequency"

  # Make sure weights are not already probabilities (simulate counts)
  mock_freq$weights <- round(mock_tna$weights * 100)

  sim <- simulate(mock_freq, nsim = 1000, max_len = 20)
  expect_equal(dim(sim), c(1000L, 20L))
  expect_named(sim, paste0("T", 1:20))

  # Recovered matrix should match the normalized version of the counts
  tna_sim <- tna(sim)
  target <- mock_freq$weights / rowSums(mock_freq$weights)
  expect_equal(target, tna_sim$weights, tolerance = 0.03)
})

test_that("simulate.group_tna supports scalar and vector nsim/max_len, wide default", {
  set.seed(0)

  # Scalar parameters (baseline)
  sim1 <- simulate(mock_group_tna, nsim = 10, max_len = 6, na_range = c(0, 0))
  expect_true("group" %in% names(sim1))
  expect_named(sim1, c("group", paste0("T", 1:6)))
  expect_equal(nrow(sim1), 10L * length(mock_group_tna))

  # Vector parameters imply different widths; wide output must fill with NA
  sim2 <- simulate(mock_group_tna, nsim = c(4, 8), max_len = c(4, 9), na_range = c(0, 0))
  expect_true("group" %in% names(sim2))
  expect_true(all(paste0("T", 1:9) %in% names(sim2)))
  expect_equal(nrow(sim2), 12L)  # 4 + 8

  # Rows from the shorter group should have NA beyond their max_len
  # Identify groups by name (or fallback to "1","2")
  grp_names <- names(mock_group_tna)
  if (is.null(grp_names)) grp_names <- as.character(seq_along(mock_group_tna))
  short_grp <- grp_names[1L]

  short_rows <- sim2[sim2$group == short_grp, , drop = FALSE]
  expect_true(all(is.na(short_rows[, paste0("T", 5:9), drop = FALSE])))
})

test_that("simulate.group_tna long format binds cleanly and has required columns", {
  set.seed(0)

  sim <- simulate(mock_group_tna, nsim = c(3, 5), max_len = c(4, 6), format = "long")
  expect_true(all(c("group", "id", "time", "state") %in% names(sim)))

  # t should be within [1, max_len] per group, actor should restart per group (simulate.tna default)
  expect_true(all(sim$time >= 1 & sim$time <= 6))
  #expect_true(all(grepl("^T\\d+$", sim$t_label)))
})

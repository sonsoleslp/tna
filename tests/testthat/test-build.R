test_that("tna works with matrix data with inits", {
  inits <- c(0.25, 0.25, 0.25, 0.25)
  tna_model <- tna(mock_matrix, inits = inits)
  expect_s3_class(tna_model, "tna")
  expect_true(is.matrix(tna_model$weights))
  expect_true(is.vector(tna_model$inits))
})

test_that("tna fails with non-square matrix", {
  trans_matrix <- matrix(c(0.1, 0.2, 0.0, 0.0, 0.2, 0.3), nrow = 2, ncol = 3)
  expect_error(
    tna(trans_matrix),
    "Argument `x` must be a square <matrix>"
  )
})

test_that("tna fails with too few inits", {
  expect_error(
    tna(mock_matrix, inits = c(0.1, 0.2, 0.3)),
    "Argument `inits` must provide initial probabilities for all states."
  )
})

test_that("single element matrix fails", {
  expect_error(
    build_model.matrix(x = 0L),
    "Argument `x` must have at least two columns"
  )
})

test_that("non-square matrix fails", {
  expect_error(
    build_model.matrix(x = matrix(0, 3, 2)),
    "Argument `x` must be a square <matrix>"
  )
})

test_that("non-coercible arguments fail", {
  expect_error(
    tna(x = identity),
    "Argument `x` must be coercible to a <matrix>"
  )
})

test_that("tna warns with too many inits", {
  expect_warning(
    tna(mock_matrix, inits = c(0.1, 0.2, 0.3, 0.4, 0.5)),
    paste0(
      "Argument `inits` contains more values than the number of states\\.\n",
      "i Only the first 4 values will be used\\."
    )
  )
})

test_that("tna handles missing x argument", {
  expect_error(tna(), "Argument `x` is missing.")
})

test_that("tna handles default case", {
  expect_error(build_model.default(mock_matrix), NA)
})

test_that("unnamed matrix gains dimnames", {
  mat <- mock_matrix
  dimnames(mat) <- NULL
  model <- tna(mat)
  expect_equal(
    dimnames(model$weights),
    list(as.character(1:4), as.character(1:4))
  )
})

test_that("tna aliases work", {
  expect_error(ftna(mock_freq_matrix), NA)
  expect_error(ctna(mock_sequence), NA)
  expect_error(atna(mock_sequence), NA)
})

test_that("scaling options work", {
  model_minmax <- tna(mock_freq_matrix, scaling = "minmax")
  expect_equal(
    range(model_minmax$weights),
    c(0, 1)
  )
  model_max <- tna(mock_freq_matrix, scaling = "max")
  expect_equal(
    model_max$weights,
    model_minmax$weights
  )
  model_rank <- tna(mock_matrix, scaling = "rank")
  expect_equal(
    sort(model_rank$weights),
    seq_len(prod(dim(mock_matrix)))
  )
})

test_that("model summary can be extracted", {
  model <- tna(mock_sequence)
  expect_error(
    summary(model),
    NA
  )
})

test_that("igraph conversion works", {
  model <- tna(mock_sequence)
  expect_error(
    as.igraph(model),
    NA
  )
})

test_that("igraph conversion works for clusters", {
  expect_error(
    as.igraph(mmm_model, which = 1),
    NA
  )
})

test_that("different model types work", {
  expect_error(
    build_model(mock_sequence, type = "relative"),
    NA
  )
  expect_error(
    build_model(mock_sequence, type = "frequency"),
    NA
  )
  expect_error(
    build_model(mock_sequence, type = "co-occurrence"),
    NA
  )
  expect_error(
    build_model(mock_sequence, type = "reverse"),
    NA
  )
  expect_error(
    build_model(mock_sequence, type = "n-gram"),
    NA
  )
  expect_error(
    build_model(mock_sequence, type = "window"),
    NA
  )
  expect_error(
    build_model(mock_sequence, type = "gap"),
    NA
  )
  expect_error(
    build_model(mock_sequence, type = "attention"),
    NA
  )
})

test_that("models can be constructed from tna_data objects", {
  expect_error(
    tna(mock_tna_data),
    NA
  )
})

test_that("log-sum-exp is correct", {
  x <- c(1.05, 1.27, 1.33, -1.7, -1.34, -sqrt(2), sqrt(3))
  expect_equal(
    log_sum_exp(x),
    log(sum(exp(x)))
  )
})

test_that("number of nodes is correct", {
  expect_equal(nodes(mock_tna), 4)
  expect_equal(nodes(mmm_model), 3)
  expect_equal(nodes(mock_matrix), 4)
})

test_that("sna works", {
  set.seed(123)
  d <- data.frame(
    from = sample(LETTERS[1:4], 10, replace = TRUE),
    to = sample(LETTERS[1:4], 10, replace = TRUE),
    weight = rexp(10)
  )
  expect_error(
    sna(d),
    NA
  )
  expect_error(
    sna(d, aggregate = mean),
    NA
  )
})

test_that("sna fails with incorrect aggregate", {
  set.seed(123)
  d <- data.frame(
    from = sample(LETTERS[1:4], 10, replace = TRUE),
    to = sample(LETTERS[1:4], 10, replace = TRUE),
    weight = rexp(10)
  )
  expect_error(
    sna(d, aggregate = "not function"),
    "Argument `aggregate` must be a function\\."
  )
  expect_error(
    sna(d, aggregate = matrix),
    "Argument `aggregate` must be a function that takes a <numeric> vector and returns a single <numeric> value\\."
  )
})

test_that("tna from tsn works", {
  expect_error(
    tsn(mock_tsn),
    NA
  )
  expect_error(
    model <- build_model(mock_tsn),
    NA
  )
  expect_true(
    all(model$weights > 0)
  )
  expect_true(
    all(model$inits > 0)
  )
})


test_that("begin and end states can be included", {
  expect_error(
    tna(mock_sequence, begin_state = "begin"),
    NA
  )
  expect_error(
    tna(mock_sequence, end_state = "end"),
    NA
  )
  expect_error(
    tna(mock_sequence, begin_state = "begin", end_state = "end"),
    NA
  )
})

test_that("forward attention works with time measurements and durations", {
  durations <- matrix(
    1 + abs(rnorm(prod(dim(mock_sequence)))),
    nrow = nrow(mock_sequence),
  )
  times <- cbind(0, t(apply(durations, 1, cumsum))[, -ncol(durations)])
  expect_error(
    model_t <- atna(mock_sequence, params = list(time = times)),
    NA
  )
  expect_error(
    model_d <- atna(mock_sequence, params = list(duration = durations)),
    NA
  )
  expect_equal(
    model_t$weights,
    model_d$weights
  )
})

test_that("directional attention works", {
  expect_error(
    model_b <- atna(mock_sequence, params = list(direction = "backward")),
    NA
  )
  expect_error(
    model_f <- atna(mock_sequence, params = list(direction = "forward")),
    NA
  )
  expect_error(
    model_bf <- atna(mock_sequence, params = list(direction = "both")),
    NA
  )
  expect_equal(
    model_b$weights + model_f$weights,
    model_bf$weights
  )
})

test_that("decay can be customized", {
  durations <- matrix(
    1 + abs(rnorm(prod(dim(mock_sequence)))),
    nrow = nrow(mock_sequence),
  )
  my_decay <- function(i, j, lambda) (i - j)^-lambda
  expect_error(
    atna(
      mock_sequence,
      params = list(time = durations, decay = my_decay, lambda = 2)
    ),
    NA
  )
})

test_that("time data from tna_data objects can be used for attention models", {
  data_ordered <- tibble::tibble(
     user = c("A", "A", "A", "A", "B", "B", "B", "B"),
     time = c(1, 2, 3, 6, 2, 7, 10, 14),
     action = c(
       "view", "click", "add_cart", "view",
       "checkout", "view", "click", "share"
     )
  )
  rlang::local_options(rlib_message_verbosity = "quiet")
  data_timed <- prepare_data(
    data_ordered, actor = "user", time = "time", action = "action"
  )
  expect_error(
    atna(data_timed, params = list(time = TRUE)),
    NA
  )
})

test_that("sequence data can be concatenated", {
  expect_error(
    model_concat <- tna(mock_sequence, concat = 2L),
    NA
  )
  expect_true(
    ncol(model_concat$data) == 2L * ncol(mock_sequence)
  )
  data_concat <- c(t(model_concat$data))
  data_concat <- data_concat[!is.na(data_concat)]
  data_orig <- c(t(mock_tna_seq$data))
  expect_true(
    all.equal(data_concat, data_orig)
  )
})

# Tests for windowed transitions
test_that("windowed transitions work for relative type", {
  d <- data.frame(
    actor = gl(10, 10),
    feature1 = rbinom(100, 1, prob = 0.33),
    feature2 = rbinom(100, 1, prob = 0.25),
    feature3 = rbinom(100, 1, prob = 0.50)
  )
  onehot <- import_onehot(d, feature1:feature3, window_size = 2)
  expect_error(
    model <- tna(onehot),
    NA
  )
  expect_s3_class(model, "tna")
})

test_that("windowed transitions work for frequency type", {
  d <- data.frame(
    actor = gl(10, 10),
    feature1 = rbinom(100, 1, prob = 0.33),
    feature2 = rbinom(100, 1, prob = 0.25),
    feature3 = rbinom(100, 1, prob = 0.50)
  )
  onehot <- import_onehot(d, feature1:feature3, window_size = 2)
  expect_error(
    model <- ftna(onehot),
    NA
  )
  expect_s3_class(model, "tna")
})

test_that("windowed transitions work for co-occurrence type", {
  d <- data.frame(
    actor = gl(10, 10),
    feature1 = rbinom(100, 1, prob = 0.33),
    feature2 = rbinom(100, 1, prob = 0.25),
    feature3 = rbinom(100, 1, prob = 0.50)
  )
  onehot <- import_onehot(d, feature1:feature3, window_size = 2)
  expect_error(
    model <- ctna(onehot),
    NA
  )
  expect_s3_class(model, "tna")
})

test_that("sna validates input correctly", {
  # Wrong number of columns
  expect_error(
    sna(data.frame(a = 1, b = 2)),
    "Argument `x` must have three columns"
  )

  # NA values
  expect_error(
    sna(data.frame(from = "A", to = NA, weight = 1)),
    "must not contain missing values"
  )
})

test_that("build_model.matrix handles negative inits", {
  expect_error(
    tna(mock_matrix, inits = c(-1, 0.5, 0.5, 0)),
    "non-negative"
  )
})

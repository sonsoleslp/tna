set.seed(0)

mock_matrix <- matrix(
  c(0, stats::runif(15)),
  nrow = 4,
  ncol = 4,
  dimnames = replicate(2, LETTERS[1:4], simplify = FALSE)
)

mock_freq_matrix <- matrix(
  c(0, rpois(24, lambda = 50)),
  nrow = 5,
  ncol = 5,
  dimnames = replicate(2, LETTERS[1:5], simplify = FALSE)
)

mock_sequence <- data.frame(
  T1 = c("A", "B", "C", "A", "A"),
  T2 = c("A", "C", "B", "B", "B"),
  T3 = c("B", "C", "A", "C", "A"),
  T4 = c("B", "A", "C", "A", "B"),
  T5 = c("C", "A", "B", "B", "A"),
  T6 = c("C", "C", "A", "A", "C")
)

mock_sequence_wide <- data.frame(
  ID = c("A", "A", "B", "B"),
  Time = c(1, 2, 1, 2),
  feature1 = c(10, 0, 15, 20),
  feature2 = c(5, 8, 0, 12),
  feature3 = c(2, 4, 6, 8),
  other_col = c("X", "Y", "Z", "W")
)

mock_tna <- tna(
  x = mock_matrix,
  inits = c(0.3, 0.2, 0.3, 0.2)
)

mock_tna_seq <- tna(mock_sequence)

mock_group_tna <- group_model(
  mock_sequence,
  group = c(1, 1, 2, 2, 2)
)

mmm_model <- group_tna(engagement_mmm)

mock_long <- data.frame(
  time = rep(1:6, 5),
  group = rep(1:5, each = 6),
  event =  as.vector(t(as.matrix(mock_sequence)))
)

mock_tsn <- structure(
  data.frame(
    id = gl(10, 100),
    value = c(
      replicate(
        10,
        stats::arima.sim(list(order = c(2, 1, 0), ar = c(0.5, 0.2)), n = 99)
      )
    ),
    state = factor(
      sample(3, 1000, replace = TRUE),
      labels = c("State 1", "State 2", "State 3")
    ),
    time = rep(seq_len(100), 10)
  ),
  class = c("tsn", "data.frame")
)

mock_cluster_data <- data.frame(
  T1 = c("A", "B", "A", "C", "A", "B"),
  T2 = c("B", "A", "B", "A", "C", "A"),
  T3 = c("C", "C", "A", "B", "B", "C")
)

{
  rlang::local_options(rlib_message_verbosity = "quiet")
  mock_tna_data <- prepare_data(
    mock_long,
    time = "time",
    actor = "group",
    action = "event"
  )
}


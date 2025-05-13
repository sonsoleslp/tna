# A mock transition matrix
set.seed(0)
mock_matrix <- matrix(
  c(0, runif(15)),
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
  T1 = c("A", "B", "C", "A"),
  T2 = c("A", "C", "B", "B"),
  T3 = c("B", "C", "A", "C"),
  T4 = c("B", "A", "C", "A"),
  T5 = c("C", "A", "B", "B")
)

mock_sequence_wide <- data.frame(
  ID = c("A", "A", "B", "B"),
  Time = c(1, 2, 1, 2),
  feature1 = c(10, 0, 15, 20),
  feature2 = c(5, 8, 0, 12),
  feature3 = c(2, 4, 6, 8),
  other_col = c("X", "Y", "Z", "W")
)

# A mock tna object
mock_tna <- tna(
  x = mock_matrix,
  inits = c(0.3, 0.2, 0.3, 0.2)
)

mock_tna_seq <- tna(mock_sequence)

mmm_model <- group_tna(engagement_mmm)

mock_long <- data.frame(
  time = rep(1:5, 4),
  group = rep(1:4, each = 5),
  event =  as.vector(t(as.matrix(mock_sequence)))
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


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

# A mock tna object
mock_tna <- tna(
  x = mock_matrix,
  inits = c(0.3, 0.2, 0.3, 0.2)
)

mmm_model <- group_tna(engagement_mmm)

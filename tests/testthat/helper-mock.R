# Helper function to create a mock tna object
create_mock_tna <- function() {
  # Create a simple mock transition matrix
  weights <- matrix(
    c(
      0.1, 0.2, 0.0, 0.1,
      0.0, 0.2, 0.3, 0.0,
      0.4, 0.0, 0.1, 0.2,
      0.1, 0.1, 0.0, 0.2
    ),
    nrow = 4,
    ncol = 4,
    byrow = TRUE
  )
  tna(weights)
}

# Helper function to create a sample transition matrix
create_sample_tna <- function() {
  mat <- matrix(
    c(
      0, 1, 2, 0,
      0, 0, 1, 1,
      1, 0, 0, 1,
      0, 1, 0, 0
    ),
    nrow = 4,
    ncol = 4,
    byrow = TRUE
  )
  tna(mat, inits = c(0.3, 0.2, 0.3, 0.2))
}

# Helper function to create a mock transition matrix
create_mock_matrix <- function() {
  matrix(
    c(
      0.8, 0.2, 0.0, 0.1,
      0.0, 0.2, 0.3, 0.0,
      0.4, 0.0, 0.1, 0.2,
      0.1, 0.1, 0.0, 0.2
    ),
    nrow = 4,
    ncol = 4,
    byrow = TRUE,
    dimnames = list(c("A", "B", "C", "D"), c("A", "B", "C", "D"))
  )
}

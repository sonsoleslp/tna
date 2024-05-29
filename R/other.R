#ranger <- function(xc) {
#  BBmisc::normalize(xc, method = "range")
#}

#' Normalize `x` to the unit interval from 0 to 1.
#'
#' @param x A `numeric` vector.
#' @param na.rm A `logical` value indicating whether missing values should be removed.
#' @noRd
ranger <- function(x, na.rm = FALSE) {
  mi <- min(x, na.rm)
  ma <- max(x, na.rm)
  (x + mi) / (ma - mi)
}

#' Compute diffusion centrality measure
#'
#' @param mat A transition probability matrix.
#' @noRd
diffusion <- function(mat) {
  s <- 0
  n <- ncol(mat)
  p <- diag(1, n, n)
  for (i in seq_len(n)) {
    p <- p %*% mat
    s <- s + p
  }
  .rowSums(s, n, n)
}

#' Compute randomized shortest path betweennes centrality measure
#'
#' @param mat A transition probability matrix.
#' @noRd
rsp_bet <- function(mat, beta = 0.01) {
  n <- ncol(mat)
  W <- mat * exp(-beta * mat^-1)
  Z <- solve(diag(1, n, n) - W)
  Zrecip <- Z^-1
  Zrecip_diag <- diag(Zrecip) * diag(1, n, n)
  out <- diag(tcrossprod(Z, Zrecip - n * Zrecip_diag) %*% Z)
  out <- round(out)
  out <- out - min(out) + 1
  out
}

#' Compute signed clustering coefficient
#'
#' @param mat A transition probability matrix.
#' @noRd
wcc <- function(mat) {
  diag(mat) <- 0
  n <- ncol(mat)
  num <- diag(mat %*% mat %*% mat)
  den <- .colSums(mat, n, n)^2 - .colSums(mat^2, n, n)
  num / den
}
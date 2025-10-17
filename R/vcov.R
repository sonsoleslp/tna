# #' Variance-Covariance Matrix of the Covariate Coefficients of a Mixture Markov Model
# #'
# #' @export
# #' @rdname vcov
# #' @param object A `tna_mmm` or a `summary.tna_mmm` object.
# #' @param ... Not used.
# #' @return A `matrix` containing the variance-covariance matrix.
#' vcov.tna_mmm <- function(object, ...) {
#'   out <- -1.0 * solve(object$hessian)
#'   cf <- names(object$beta[[1L]])
#'   clust <- rep(paste0(object$cluster_names[-1L], ": "), each = length(cf))
#'   nm <- paste0(clust, cf)
#'   dimnames(out) <- list(nm, nm)
#'   out
#' }
#'
#' #' @export
#' #' @rdname vcov
#' vcov.summary.tna_mmm <- function(object, ...) {
#'   object$vcov
#' }
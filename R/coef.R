# #' Extract Regression Coefficients of a Mixture Markov Model
# #'
# #' @export
# #' @rdname vcov
# #' @param object A `tna_mmm` or a `summary.tna_mmm` object.
# #' @param ... Not used.
# #' @return A `matrix` containing the regression coefficient estimates
# #' @examples
# #' # TODO
# coef.tna_mmm <- function(object, ...) {
#   d <- do.call("cbind", object$beta)
#   rownames(d) <- names(object$beta[[1L]])
#   colnames(d) <- object$cluster_names
#   d
# }

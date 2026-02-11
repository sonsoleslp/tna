# #' Extract Regression Coefficients of a Mixture Markov Model
# #'
# #' @export
# #' @param object A `tna_mmm` object.
# #' @param ... Not used.
# #' @return A `matrix` containing the regression coefficient estimates.
# #' @examples
# #' coef(engagement_tna_mmm)
# #'
# coef.tna_mmm <- function(object, ...) {
#   d <- do.call(base::cbind, object$beta)
#   rownames(d) <- names(object$beta[[1L]])
#   colnames(d) <- object$cluster_names
#   d
# }

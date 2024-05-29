#' Print Centrality Measures
#'
#' @param x A `centralities` object.
#' @param ... Ignored.
#' @export
#'
print.centralities <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

#' Print a `centralities` object
#'
#' @export
#' @param x A `centralities` object.
#' @param ... Ignored.
print.centralities <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

#' Print a `tna` object
#'
#' @export
#' @param x A `centralities` object.
#' @param ... Ignored.
print.tna <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

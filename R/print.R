#' Print Centrality Measures
#'
#' @param x A `centralities` object.
#' @param ... Ignored.
#' @export
#'
print.centralities <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

#' Print `tna` object
#'
#' @param x A `tna` object.
#' @param generic Use generic print. Defaults to `FALSE`
#' @param ... Ignored.
#' @export
#'
print.tna <- function(x, generic = FALSE, ...) {
  if (generic) {
    NextMethod(generic = "print", object = x, ...)
    return;
  }
  # Print State Labels
  cat("\n**State Labels:**\n")
  cat(paste(x$labels, collapse = ", "), "\n")

  # Print Transits
  cat("\n**Transition matrix:**\n")
  if (length(x$transits) == 1) {
    print(x$transits[[1]], digits = 3)
  } else {
    for (i in names(x$transits)) {
      cat(paste0( i, ":\n"))
      print(x$transits[[i]], digits = 3)
      cat("\n")
    }
  }

  # Print Initial State Probabilities (inits)
  cat("\n**Initial Probabilities:**\n")
  if (length(x$inits) == 1) {
    print(x$inits[[1]], digits = 3)
  } else {
    for (i in names(x$inits)) {
      cat(paste0( i, ":\n"))
      print(x$inits[[i]], digits = 3)
      cat("\n")
    }
  }



}


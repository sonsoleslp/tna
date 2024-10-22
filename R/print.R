#' Print Centrality Measures
#'
#' @param x A `centralities` object.
#' @param ... Ignored.
#' @export
#'
print.tna_centralities <- function(x, ...) {
  NextMethod(generic = "print", object = x, ...)
}

#' Print Detected Communities
#'
#' @param x A `tna_communities` object.
#' @param ... Ignored.
#' @export
#'
print.tna_communities <- function(x, ...) {
  stopifnot_(
    is_tna_communities(x),
    "Argument {.arg x} must be a {.cls tna_communities} object."
  )
  # TODO
  print(x)
}

#' Print Found Cliques
#'
#' @param x A `tna_cliques` object.
#' @param ... Ignored.
#' @export
#'
print.tna_cliques <- function(x, ...) {
  stopifnot_(
    is_tna_cliques(x),
    "Argument {.arg x} must be a {.cls cliques} object."
  )
  # TODO
  print(x)
}


#' Print a `tna` object
#'
#' @param x A `tna` object.
#' @param digits Number of decimal digits to print. Defaults to 2.
#' @param generic Use generic print. Defaults to `FALSE`
#' @param ... Ignored.
#' @export
#'
print.tna <- function(x, digits = 2, generic = FALSE, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  if (generic) {
    NextMethod(generic = "print", object = x, ...)
    return()
  }
  type <- attr(x, "type")
  mat_type <- ifelse_(type == "prop", "Probability", "Frequency")
  init_type <- ifelse_(type == "prop", "Probabilities", "Frequencies")
  cat("\n**State Labels:**\n")
  cat(paste(x$labels, collapse = ", "), "\n")
  cat("\n**", mat_type, " Matrix:**\n", sep = "")
  if (length(x$weights) == 1) {
    print(x$weights[[1]], digits = digits)
  } else {
    for (i in names(x$weights)) {
      cat(paste0(i, ":\n"))
      print(x$weights[[i]], digits = digits)
      cat("\n")
    }
  }
  cat("\n**Initial ", init_type, ":**\n", sep = "")
  if (length(x$inits) == 1) {
    print(x$inits[[1]], digits = digits)
  } else {
    for (i in names(x$inits)) {
      cat(paste0(i, ":\n"))
      print(x$inits[[i]], digits = digits)
      cat("\n")
    }
  }
}

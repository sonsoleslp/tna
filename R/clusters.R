#' @export
group_tna <- function(x, group, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )

  stopifnot_(
    !missing(group),
    "Argument {.arg group} is missing."
  )

  stopifnot_(
    !(typeof(group) %in% c("stslist","data.frame")),
    "Argument {.arg x} has to be of type `stslist` (sequence object) or `data.frame`."
  )

  if (typeof(group) != "factor") {
    group <- factor(group)
  }

  levs <- levels(group)
  clusters <- list()
  for (i in levs) {
    clusters[[i]] <- tna(x[group == i, ], ...)
  }
  structure(clusters, class = "group_tna")
}

#' Check that argument is an object of class `group_tna`
#'
#' @param x An \R object.
#' @noRd
is_group_tna <- function(x) {
  inherits(x, "group_tna")
}

#' @export
centralities.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )
 lapply(x, \(i) centralities.tna(i, ...))
}

#' @export
communities.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )
  lapply(x, \(i) communities.tna(i, ...))
}

#' @export
plot.group_tna <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    is_group_tna(x),
    "Argument {.arg x} must be of type `group_tna`"
  )
  Map(function(y, i) plot(y, title = i, ...), x, names(x))
}

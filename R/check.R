#' Check if argument is missing
#'
#' @param x An \R object.
#' @noRd
check_missing <- function(x) {
  arg <- deparse(substitute(x))
  stopifnot_(
    !missing(x),
    "Argument {.arg {arg}} is missing."
  )
}

#' Check if argument contains missing values
#'
#' @param x An \R object.
#' @noRd
check_na <- function(x) {
  arg <- deparse(substitute(x))
  stopifnot_(
    all(!is.na(x)),
    "Argument {.arg {arg}} must not contain missing values."
  )
}

#' Check Transition Network Type for Validity
#'
#' @param type Type of the transition network.
#' @noRd
check_model_type <- function(type) {
  check_match(type, c("relative", "frequency", "co-occurrence"))
}

#' Check Transition Network Weight Scaling for Validity
#'
#' @param scaling A `character` vector of scaling options to apply.
#' @noRd
check_model_scaling <- function(scaling) {
  if (length(scaling) == 0L) {
    return(character(0L))
  }
  check_match(scaling, c("minmax", "max", "rank"), several.ok = TRUE)
}

#' Check that `x` is of specific class
#'
#' @param x An \R object.
#' @inheritParams class
#' @noRd
check_class <- function(x, what) {
  arg <- deparse(substitute(x))
  stopifnot_(
    inherits(x, what),
    "Argument {.arg {arg}} must be a {.cls {what}} object."
  )
}

#' Check that `x` is a `tna` Object Created from Sequence Data
#'
#' @param x An \R object.
#' @noRd
check_tna_seq <- function(x) {
  arg <- deparse(substitute(x))
  stopifnot_(
    is_tna(x) && !is.null(x$data),
    "Argument {.arg {arg}} must be a {.cls tna}
     object created from sequence data."
  )
}

#' Check that `x` contains valid centrality measures
#'
#' @param x An \R object expected to be a `character` vector.
#' @noRd
check_measures <- function(x) {
  stopifnot_(
    checkmate::test_character(
      x = x,
      any.missing = FALSE,
      unique = TRUE,
    ),
    "Argument {.arg measures} must be a {.cls character} vector."
  )
  lower_measures <- tolower(x)
  lower_defaults <- tolower(available_centrality_measures)
  measures_match <- pmatch(lower_measures, lower_defaults)
  no_match <- is.na(measures_match)
  invalid_measures <- x[no_match]
  valid_measures <- x[!no_match]
  stopifnot_(
    length(invalid_measures) == 0L,
    c(
      "Argument {.arg measures} contains invalid centrality measures:",
      `x` = "Measure{?s} {.val {invalid_measures}} {?is/are} not recognized."
    )
  )
}

#' Check that `x` is Between 0 and 1.
#'
#' @param x An \R object expected to be a single  `numeric` or `integer` value.
#' @noRd
check_probability <- function(x) {
  arg <- deparse(substitute(x))
  stopifnot_(
    checkmate::test_number(x = x, lower = 0.0, upper = 1.0),
    "Argument {.arg {arg}} must be a single
    {.cls numeric} value between 0 and 1."
  )
}

#' Check that `x` is Non-Negative
#'
#' @param x An \R object expected to be a single  `numeric` or `integer` value.
#' @param type A `character` string corresponding to
#' the type that `x` should be.
#' @noRd
check_nonnegative <- function(x, type = "integer") {
  arg <- deparse(substitute(x))
  suffix <- ifelse_(type == "numeric", " value", "")
  test_fun <- ifelse_(
    type == "numeric",
    checkmate::test_number,
    checkmate::test_int
  )
  stopifnot_(
    test_fun(x = x, lower = 0),
    "Argument {.arg {arg}} must be a single
    non-negative {.cls {type}}{suffix}."
  )
}

#' Check that `x` is Positive
#'
#' @inheritParams check_nonnegative
#' @noRd
check_positive <- function(x, type = "integer") {
  arg <- deparse(substitute(x))
  suffix <- ifelse_(type == "numeric", " value", "")
  stopifnot_(
    checkmate::test_int(x = x, lower = 1),
    "Argument {.arg {arg}} must be a single
    positive {.cls {type}}{suffix}."
  )
}

#' Check That `x` is a Logical Value
#'
#' @param x An \R object expected to be a `logical` value.
#' @noRd
check_flag <- function(x) {
  arg <- deparse(substitute(x))
  stopifnot_(
    checkmate::test_flag(x = x),
    "Argument {.arg {arg}} must be a single {.cls logical} value."
  )
}

#' Check a `layout` Argument
#'
#' @param x A `tna` object
#' @param layout A `character` string, a `matrix`, or a `function`.
#' @param args A `list` of arguments to pass to the layout function.
#' @noRd
check_layout <- function(x, layout, args = list()) {
  if (is.character(layout)) {
    layout <- tolower(layout)
    layout <- try(
      match.arg(
        layout,
        c("circle", "groups", "spring")
      ),
      silent = TRUE
    )
    stopifnot_(
      !inherits(layout, "try-error"),
      "A {.cls character} layout must be either {.val circle}, {.val groups},
      or {.val spring}"
    )
    return(layout)
  }
  if (is.matrix(layout)) {
    stopifnot_(
      ncol(layout) == 2L,
      c(
        "A {.cls matrix} layout must have 2 columns.",
        `x` = "Found {ncol(layout)} columns instead."
      )
    )
    stopifnot_(
      nrow(layout) == nodes(x),
      c(
        "A {.cls matrix} layout must have a row for each node",
        `x` = "Expected {nodes(x)} rows but {nrow(layout)} were supplied."
      )
    )
    return(layout)
  }
  stopifnot_(
    is.function(layout),
    "Argument {.arg layout} must be a {.cls character} string,
     a {.cls matrix}, or a {.cls function}."
  )
  args$graph <- as.igraph(x)
  do.call(what = layout, args = args)
}

#' Check Edge Weight Matrix based on TNA Type
#'
#' @param x A `matrix` of edge weights.
#' @param type Type of the transition network as a `character` string.
#' @noRd
check_weights <- function(x, type) {
  d <- dim(x)[1L]
  if (type == "relative") {
    rs <- .rowSums(x = x, m = d, n = d)
    stopifnot_(
      all(rs > 0),
      "At least one element of each row of {.arg x} must be positive."
    )
    x[] <- x / rs
  } else if (type %in% c("frequency", "co-occurrence")) {
    x_int <- as.integer(x)
    stopifnot_(
      all(x == x_int) && all(x >= 0),
      "All elements of {.arg x} must be non-negative integers."
    )
    x[] <- x_int
  }
  x
}

#' Check if argument matches given choices ignoring case
#'
#' @param x A `character` string.
#' @inheritParams match.arg
#' @noRd
check_match <- function(x, choices, several.ok = FALSE) {
  arg <- deparse(substitute(x))
  x <- onlyif(is.character(x), tolower(x))
  x <- try(
    match.arg(
      arg = x,
      choices = choices,
      several.ok = several.ok
    ),
    silent = TRUE
  )
  n_choices <- length(choices)
  prefix <- ifelse_(
    several.ok,
    "Elements of",
    "Argument"
  )
  stopifnot_(
    !inherits(x, "try-error"),
    "{prefix} {.arg {arg}} must be either
    {qty(n_choices)} {.or {.val {choices}}}."
  )
  x
}

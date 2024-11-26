#' Check Transition Network Type for Validity
#'
#' @param type Type of the transition network.
#' @noRd
check_tna_type <- function(type) {
  type <- onlyif(is.character(type), tolower(type))
  type <- try(
    match.arg(
      type,
      c("relative", "scaled", "ranked", "absolute", "co", "co_scaled")
    ),
    silent = TRUE
  )
  stopifnot_(
    !inherits(type, "try-error"),
    "Argument {.arg type} must be either {.val relative}, {.val scaled},
     {.val ranked}, {.val absolute}, {.val co}, or {.val co_scaled}."
  )
  type
}

#' Check that `x` is a `tna` Object
#'
#' @param x An \R object.
#' @noRd
check_tna <- function(x) {
  arg <- deparse(substitute(x))
  stopifnot_(
    is_tna(x),
    "Argument {.arg {arg}} must be a {.cls tna} object."
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
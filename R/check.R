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
  check_match(
    type,
    c(
      "relative",
      "frequency",
      "co-occurrence",
      "n-gram",
      "gap",
      "window",
      "reverse",
      "attention"
    )
  )
}

#' Check Transition Network Weight Scaling for Validity
#'
#' @param scaling A `character` vector of scaling options to apply.
#' @noRd
check_model_scaling <- function(scaling) {
  if (length(scaling) == 0L) {
    return(character(0L))
  }
  check_match(
    scaling,
    c("minmax", "max", "rank"),
    several.ok = TRUE
  )
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

#' Check that `x` is a `tna` object created from sequence Data
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
  available_measures <- names(centrality_funs)
  lower_measures <- tolower(x)
  lower_defaults <- tolower(available_measures)
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
  available_measures[measures_match]
}

#' Check that `x` is numeric
#'
#' @param x An \R object expected to be a `numeric` value.
#' @noRd
check_numeric <- function(x) {
  arg <- deparse(substitute(x))
  stopifnot_(
    checkmate::test_number(x = x),
    "Argument {.arg {arg}} must be a single {.cls numeric} value."
  )
}

#' Check that `x` is a non-negative
#'
#' @param x An \R object expected to be a `numeric` or `integer`
#' value or a vector.
#' @param type A `character` string corresponding to
#' the type that `x` should be.
#' @param strict A `logical` value. If `FALSE` (the default), expects
#' non-negative values and positive otherwise.
#' @param scalar A `logical` value indicating if `x` should be expected
#' to be a single value.
#' @noRd
check_values <- function(x, type = "integer", strict = FALSE,
                         scalar = TRUE) {
  arg <- deparse(substitute(x))
  suffix <- ifelse_(
    scalar,
    ifelse_(type == "integer", "", " value"),
    " vector"
  )
  test_fun <- ifelse_(
    type == "numeric",
    ifelse_(scalar, checkmate::test_number, checkmate::test_numeric),
    ifelse_(scalar, checkmate::test_int, checkmate::test_integer)
  )
  strictness <- ifelse_(strict, "positive", "non-negative")
  stopifnot_(
    test_fun(x = x, lower = as.integer(strict && type == "integer")),
    "Argument {.arg {arg}} must be a {strictness} {.cls {type}}{suffix}."
  )
}

#' Check that `x` is between a minimum and a maximum value
#'
#' @param x An \R object expected to be within a specific range.
#' @noRd
check_range <- function(x, type = "numeric", scalar = TRUE,
                        lower = -Inf, upper = Inf) {
  arg <- deparse(substitute(x))
  prefix <- ifelse_(scalar, "be a single", "only contain")
  suffix <- ifelse_(
    scalar,
    ifelse_(type == "integer", "", " value"),
    " values"
  )
  test_fun <- ifelse_(
    type == "numeric",
    ifelse_(scalar, checkmate::test_number, checkmate::test_numeric),
    ifelse_(scalar, checkmate::test_int, checkmate::test_integerish)
  )
  bounds <- ""
  if (is.infinite(lower)) {
    bounds <- paste0("less than or equal to ", upper)
  } else if (is.infinite(upper)) {
    bounds <- paste0("greater than or equal to ", lower)
  } else {
    bounds <- paste0("between ", lower, " and ", upper)
  }
  stopifnot_(
    test_fun(x = x, lower = lower, upper = upper),
    "Argument {.arg {arg}} must {prefix}
    {.cls {type}} {suffix} {bounds}."
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
#' @param x A `tna` or a `tna_cliques` object.
#' @param layout A `character` string, a `matrix`, or a `function`.
#' @param args A `list` of arguments to pass to the layout function.
#' @param ... Additional arguments passed to `as.igraph`.
#' @noRd
check_layout <- function(x, layout, args = list(), ...) {
  if (is.character(layout)) {
    layout <- tolower(layout)
    layout_parsed <- try_(match.arg(layout, c("circle", "groups", "spring")))
    if (inherits(layout_parsed, "try-error")) {
      layout_fun <- str2lang(paste0("igraph::", layout))
      args$graph <- as.igraph(x, ...)
      layout_parsed <- try_(
        do.call(eval(layout_fun), args)
      )
    }
    stopifnot_(
      !inherits(layout_parsed, "try-error"),
      "A {.cls character} layout must be either {.val circle}, {.val groups},
      {.val spring}, or the name of an {.pkg igraph} layout."
    )
    return(layout_parsed)
  }
  if (is.matrix(layout)) {
    stopifnot_(
      ncol(layout) == 2L,
      c(
        "A {.cls matrix} layout must have two columns:",
        `x` = "Found {ncol(layout)} columns instead."
      )
    )
    stopifnot_(
      nrow(layout) == nodes(x),
      c(
        "A {.cls matrix} layout must have exactly one row for each node:",
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
  args$graph <- as.igraph(x, ...)
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
  } else if (type %in% c("frequency", "co-occurrence", "reverse", "window")) {
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
check_match <- function(x, choices, several.ok = FALSE, match_case = FALSE) {
  arg <- deparse(substitute(x))
  if (!match_case) {
    x <- onlyif(is.character(x), tolower(x))
    choices <- tolower(choices)
  }
  x <- try_(match.arg(arg = x, choices = choices, several.ok = several.ok))
  n_choices <- length(choices)
  prefix <- ifelse_(several.ok, "Elements of", "Argument")
  stopifnot_(
    !inherits(x, "try-error"),
    "{prefix} {.arg {arg}} must be either
    {cli::qty(n_choices)} {.or {.val {choices}}}."
  )
  x
}

#' Check if argument is a character string
#'
#' @param x An \R object.
#' @noRd
check_string <- function(x) {
  arg <- deparse(substitute(x))
  stopifnot_(
    checkmate::test_string(x = x),
    "Argument {.arg {arg}} must be a {.cls character} vector of length 1."
  )
}

#' #' Check if argument is a character vector
#' #'
#' #' @param x An \R object.
#' #' @noRd
#' check_character <- function(x) {
#'   if (missing(x)) {
#'     return()
#'   }
#'   arg <- deparse(substitute(x))
#'   stopifnot_(
#'     checkmate::test_character(
#'       x = x,
#'       min.len = 1L,
#'       any.missing = FALSE,
#'       unique = TRUE,
#'     ),
#'     "Argument {.arg {arg}} must be a {.cls character} vector."
#'   )
#' }

#' Check that argument is a valid cluster
#'
#' @param x A `group_tna` object.
#' @param i Index vector of clusters.
#' @noRd
check_cluster <- function(x, i) {
  i <- ifelse_(is.numeric(i), as.integer(i), i)
  arg <- deparse(substitute(i))
  n <- length(x)
  stopifnot_(
    is.integer(i) || all(i %in% names(x)),
    "Argument {.arg {arg}} must only contain names of {.arg x} when of type
     {.cls character}."
  )
  stopifnot_(
    is.character(i) || all(i >= 1 & i <= n),
    "Argument {.arg {arg}} must contain integers between 1 and {n} when of type
     {.cls numeric}."
  )
}

#' Check that indices/names are valid clusters
#'
#' @param x A `group_tna` object.
#' @param i Index of the first cluster.
#' @param j Index of the second cluster.
#' @noRd
check_clusters <- function(x, i, j) {
  i <- ifelse_(is.numeric(i), as.integer(i), i)
  j <- ifelse_(is.numeric(j), as.integer(j), j)
  stopifnot_(
    !identical(i, j),
    "Arguments {.arg i} and {.arg j} must be different."
  )
  n <- length(x)
  for (arg in c("i", "j")) {
    idx <- eval(rlang::sym(arg))
    stopifnot_(
      length(idx) == 1L && (is.integer(idx) || is.character(idx)),
      "Argument {.arg {arg}} must be a {.cls numeric} or a {.cls character}
      vector of length 1."
    )
    stopifnot_(
      is.integer(idx) || idx %in% names(x),
      "Argument {.arg {arg}} must be a name of {.arg x} when of type
      {.cls character}."
    )
    stopifnot_(
      is.character(idx) || (idx >= 1 && idx <= n),
      "Argument {.arg {arg}} must be between 1 and {n} when of type
      {.cls numeric}."
    )
  }
}

check_cols <- function(cols, single = TRUE, missing_ok = TRUE) {
  arg <- deparse(substitute(cols))
  if (missing(cols)) {
    stopifnot_(
      missing_ok,
      "Argument {.arg {arg}} is missing."
    )
    return()
  }
  if (single) {
    stopifnot_(
      length(cols) == 1L,
      "Argument {.arg {arg}} must provide a single column name."
    )
  }
}

check_em_control <- function(control) {
  em_control_defaults <- list(
    maxiter = 500L,
    maxiter_m = 500L,
    reltol = 1e-10,
    reltol_m = 1e-6,
    restarts = 10L,
    seed = 1L,
    step = 1.0
  )
  if (missing(control)) {
    return(em_control_defaults)
  }
  for (n in names(em_control_defaults)) {
    if (is.null(control[[n]])) {
      control[[n]] <- em_control_defaults[[n]]
    }
  }
  check_values(control$maxiter, strict = TRUE)
  check_values(control$maxiter_m, strict = TRUE)
  check_values(control$reltol, type = "numeric", strict = TRUE)
  check_values(control$reltol_m, type = "numeric", strict = TRUE)
  check_values(control$restarts, strict = TRUE)
  check_values(control$seed)
  check_range(control$step, lower = 0.0, upper = 1.0)
  control
}

#' Check `build_model` dots arguments
#'
#' @noRd
check_dots <- function(...) {
  dots <- list(...)
  dots_names <- names(dots)
  valid_args <- c(
    "cols",
    "inits",
    "begin_state",
    "end_state"
  )
  invalid_dots <- dots_names[!dots_names %in% valid_args]
  stopifnot_(
    length(invalid_dots) == 0L,
    "Argument{?/s} {.arg {invalid_dots}} {?is/are} not recognized."
  )
}

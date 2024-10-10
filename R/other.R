#' Normalize `x` to the unit interval from 0 to 1.
#'
#' @param x A `numeric` vector.
#' @param na.rm A `logical` value indicating whether missing values
#'   should be removed.
#' @noRd
ranger <- function(x, na.rm = FALSE) {
  mi <- min(x, na.rm)
  ma <- max(x, na.rm)
  (x + mi) / (ma - mi)
}


#' Check Weak Connectivity of an Adjacency Matrix
#'
#' This function checks if an adjacency matrix represents a weakly connected graph. A graph is considered weakly connected if there is a path between any two vertices when ignoring the direction of edges.
#'
#' @param mat A square adjacency matrix representing the graph.
#'
#' @return A logical value indicating whether the graph is weakly connected (`TRUE`) or not (`FALSE`).
#' @noRd
is_weakly_connected <- function(mat) {
  n <- nrow(mat)
  visited <- rep(FALSE, n)
  stack <- c(1)
  visited[1] <- TRUE

  while (length(stack) > 0) {
    v <- stack[length(stack)]
    stack <- stack[-length(stack)]

    neighbors <- which(mat[v, ] > 0 | mat[, v] > 0)
    for (u in neighbors) {
      if (!visited[u]) {
        visited[u] <- TRUE
        stack <- c(stack, u)
      }
    }
  }

  all(visited)
}



#' Shorthand for `try(., silent = TRUE)`
#'
#' @param expr An \R expression to try.
#' @noRd
try_ <- function(expr) {
  try(expr, silent = TRUE)
}

#' Check that argument is an object of class `"tna"`
#'
#' @param x An \R object.
#' @noRd
is_tna <- function(x) {
  inherits(x, "tna")
}

#' Check that argument is an object of class `"centralities"`
#'
#' @param x An \R object.
#' @noRd
is_centralities <- function(x) {
  inherits(x, "centralities")
}

# Functions borrowed from the `dynamite` package --------------------------
# https://github.com/ropensci/dynamite

#' Shorthand for `if (test) yes else no`
#'
#' @param test A `logical` value of the condition to evaluate.
#' @param yes An \R object to return when `test` evaluates to `TRUE`.
#' @param no An \R object to return when `test` evaluates to `FALSE`.
#' @noRd
ifelse_ <- function(test, yes, no) {
  if (test) {
    yes
  } else {
    no
  }
}

#' Return `yes` if `test` is `TRUE`, otherwise return `NULL`
#'
#' @param test \[`logical(1)`] Condition to evaluate.
#' @param yes An \R object to return when `test` evaluates to `TRUE`.
#' @noRd
onlyif <- function(test, yes) {
  if (test) {
    yes
  } else {
    NULL
  }
}

#' Generate a Warning Message
#'
#' @param message See [cli::cli_warn()].
#' @param ... See [cli::cli_warn()].
#' @noRd
warning_ <- function(message, ...) {
  cli::cli_warn(message, ..., .envir = parent.frame())
}


#' Generate an Info Message
#'
#' @param message See [cli::cli_inform()].
#' @param ... See [cli::cli_inform()].
#' @noRd
info_ <- function(message, ...) {
  cli::cli_text(message, ..., .envir = parent.frame())
}


#' Stop function execution unless a condition is true
#'
#' @param message See [cli::cli_abort()].
#' @param ... See [cli::cli_abort()].
#' @param call See [cli::cli_abort()].
#' @noRd
stopifnot_ <- function(cond, message, ..., call = rlang::caller_env()) {
  if (!cond) {
    cli::cli_abort(message, ..., .envir = parent.frame(), call = call)
  }
}


#' Map community assignments to a color palette.
#'
#' This function takes a vector of community assignments (numeric or categorical) and maps them to corresponding colors
#' from a provided palette. If all values in the input vector are the same, the function maps all of them to the first
#' color in the palette. Otherwise, it normalizes the values to ensure they span across the entire palette.
#'
#' @param x A numeric vector representing the community assignments.
#' @param palette A vector of colors to be used for mapping. The length of the palette defines the range of possible colors.
#'
#' @return A vector of colors corresponding to the input values, either a single color or a gradient of colors based on
#' the values in the input vector.
#'
#' @details
#' - If the input vector `x` contains only one unique value, all elements will be mapped to the first color in the palette.
#' - If the input vector `x` contains multiple unique values, these values are scaled linearly to cover the entire range of
#'   the provided palette, ensuring that higher values in the input correspond to later colors in the palette.
#' - The scaling formula used for normalization is:
#'   \deqn{scaled\_values = \left\lfloor \frac{(x - \text{min}(x))}{(\text{max}(x) - \text{min}(x))} \times (\text{length}(palette) - 1) \right\rfloor + 1}
#'
#' @examples
#' # Example usage with numeric input
#' x <- c(1, 2, 3, 4, 5)
#' palette <- c("red", "green", "blue", "yellow", "purple")
#' colors <- map_to_color(x, palette)
#'
#' # Example with a single unique value
#' x <- c(1, 1, 1)
#' colors <- map_to_color(x, palette)
#'
#' @noRd
map_to_color <- function(x, palette) {
  if (length(unique(x)) == 1) {
    # Handle case where all values are the same
    return(rep(palette[1], length(x)))  # Map to the first color in the palette
  } else {
    # Normalize the numeric values to a range from 1 to the length of the palette
    min_val <- min(x)
    max_val <- max(x)
    scaled_values <- as.integer((x - min_val) / (max_val - min_val) * (length(palette) - 1)) + 1
    return(palette[scaled_values])
  }
}


#' Get the Model from TNA Results
#'
#' This function extracts and returns the `tna` model object from a  results list
#' The `tna` model is typically stored within the results of an analysis
#' conducted with the `tna` package.
#'
#' @param results An object containing the results of a network analysis,
#'   including the model and other relevant information.
#' @return The  `tna`  model object extracted from the results.
#' @examples
#' \dontrun{
#' # Assuming `results` is a tna object
#' model <- get_model(results)
#' }
#' @export
get_model <- function(results) {
  results$model
}

#' Map community assignments to a color palette.
#'
#' This function takes a vector of community assignments
#' (numeric or categorical) and maps them to corresponding colors
#' from a provided palette. If all values in the input vector are the same,
#' the function maps all of them to the first
#' color in the palette. Otherwise, it normalizes the values to ensure they
#' span across the entire palette.
#'
#' @param x A `numeric` vector representing the community assignments.
#' @param palette A `character` vector of colors to be used for mapping.
#' The length of  the palette defines the range of possible colors.
#'
#' @return A vector of colors corresponding to the input values, either
#' a single color or a gradient of colors based on the values in
#' the input vector.
#'
#' @details
#' - If the input vector `x` contains only one unique value,
#'   all elements will be mapped to the first color in the palette.
#' - If the input vector `x` contains multiple unique values,
#'   these values are scaled linearly to cover the entire range of
#'   the provided palette, ensuring that higher values in
#'   the input correspond to later colors in the palette.
#' - The normalization is performed using min-max scaling.
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
    # Handle the case where all values are the same
    return(rep(palette[1], length(x))) # Map to the first color in the palette
  }
  # Normalize the numeric values to a range from 1 to the length of the palette
  scaled_values <- as.integer(ranger(x) * (length(palette) - 1L)) + 1L
  palette[scaled_values]
}

#' Colors for Sequence Data
#'
#' This functions creates colors analogous to those used by `TraMineR`
#'
#' @param n_states The number of states as an `integer`.
#' @noRd
color_palette <- function(n_states) {
  color_group <- 4L -
    1L * (n_states <= 2) -
    1L * (n_states <= 8) -
    1L * (n_states <= 12)
  switch(color_group,
    RColorBrewer::brewer.pal(n = 3, name = "Accent")[seq_len(n_states)],
    RColorBrewer::brewer.pal(n = n_states, name = "Accent"),
    RColorBrewer::brewer.pal(n = n_states, name = "Set3"),
    colorspace::qualitative_hcl(n = n_states, palette = "Set 3")
  )
}

# Default Community Colors ------------------------------------------------

default_colors <- c(
  "#d1ea2c",
  "#fd5306",
  "#68b033",
  "#8601b0",
  "#fe2712",
  "#a7184d",
  "#3c02a6",
  "#fd9a01",
  "#0392ce"
)

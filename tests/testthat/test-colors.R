test_that("color palette can be constructed", {
  for (n in 2:20) {
    expect_error(
      color_palette(n),
      NA
    )
  }
})

test_that("community assignments can be mapped to colors", {
  expect_equal(
    map_to_color(c(1), default_colors),
    default_colors[1]
  )
  expect_equal(
    map_to_color(c(1, 2, 3), default_colors),
    default_colors[c(1, 5, 9)]
  )
  expect_equal(
    map_to_color(c(10, 25, 100), default_colors),
    default_colors[c(1, 2, 9)]
  )
})

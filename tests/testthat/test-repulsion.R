library(testthat)
library(huerd)

context("Color Conversion")

test_that("convert_colors converts black RGB (0,0,0) to LAB correctly", {
  rgb_black <- matrix(c(0, 0, 0), ncol = 3)
  lab_black <- convert_colors(rgb_black, from = "rgb", to = "lab")

  expect_equal(lab_black[[1, 1]], 0, tolerance = 1e-6)
  expect_equal(lab_black[[1, 2]], 0, tolerance = 1e-6)
  expect_equal(lab_black[[1, 3]], 0, tolerance = 1e-6)
})

test_that(
  "convert_colors handles non-normalized RGB values when normalize is FALSE",
  {
    rgb_input <- matrix(c(255, 128, 0), ncol = 3)
    lab_output <- convert_colors(
      rgb_input,
      from = "rgb", to = "lab", normalized = FALSE
    )

    expected_lab <- farver::convert_colour(rgb_input, from = "rgb", to = "lab")

    expect_equal(lab_output, expected_lab, tolerance = 1e-6)
  }
)

test_that(
  "convert_colors returns same values for RGB to RGB conversion with
   normalization",
  {
    rgb_input <- matrix(c(128, 64, 32), ncol = 3)
    rgb_output <- convert_colors(
      rgb_input,
      from = "rgb", to = "rgb", normalized = FALSE
    )
    colnames(rgb_input) <- c("r", "g", "b")

    expect_equal(rgb_output, rgb_input, tolerance = 1e-6)
  }
)


test_that("color_distance calculates zero distance for identical colors", {
  color <- matrix(c(128, 64, 32), ncol = 3)
  distance <- color_distance(color, color)
  expect_equal(distance[[1, 1]], 0, tolerance = 1e-6)
})

test_that(
  "color_distance calculates consistent distances regardless of color order",
  {
    color1 <- matrix(c(255, 0, 0), ncol = 3) # Red
    color2 <- matrix(c(0, 255, 0), ncol = 3) # Green

    distance1 <- color_distance(color1, color2)
    distance2 <- color_distance(color2, color1)

    expect_equal(distance1, distance2)
  }
)

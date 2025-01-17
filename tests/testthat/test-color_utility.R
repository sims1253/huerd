library(testthat)

# Tests for color utility functions

test_that("validate_color_input handles basic input correctly", {
  # Test valid LAB input
  valid_lab <- matrix(c(
    50, 0, 0,
    60, 10, -10
  ), ncol = 3, byrow = TRUE)

  expect_no_error(validate_color_input(valid_lab, "oklab"))
  expect_equal(nrow(validate_color_input(valid_lab, "oklab")), 2)

  # Test valid RGB input
  valid_rgb <- matrix(c(
    128, 128, 128,
    255, 0, 0
  ), ncol = 3, byrow = TRUE)

  expect_no_error(validate_color_input(valid_rgb, "rgb"))

  # Test valid XYZ input
  valid_xyz <- matrix(c(
    95.047, 100, 108.883,
    50, 50, 50
  ), ncol = 3, byrow = TRUE)

  expect_no_error(validate_color_input(valid_xyz, "xyz"))
})

test_that("validate_color_input converts data frame to matrix", {
  # Test with data frame input
  df_input <- data.frame(
    L = c(50, 60),
    a = c(0, 10),
    b = c(0, -10)
  )

  result <- validate_color_input(df_input, "oklab")
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(2, 3))
})

test_that("validate_color_input validates LAB space constraints", {
  # Test L values outside 0-100 range
  invalid_L <- matrix(c(
    -10, 0, 0,
    110, 0, 0
  ), ncol = 3, byrow = TRUE)

  expect_error(
    validate_color_input(invalid_L, "oklab"),
    "L values must be between 0 and 100"
  )

  # Test extreme a/b values
  extreme_ab <- matrix(c(
    50, 150, 0,
    50, 0, -150
  ), ncol = 3, byrow = TRUE)

  expect_warning(
    validate_color_input(extreme_ab, "oklab"),
    "Extreme a\\* or b\\* values detected \\(>100 or <-100\\)"
  )
})

test_that("validate_color_input validates RGB space constraints", {
  # Test RGB values outside 0-255 range
  invalid_rgb <- matrix(c(
    -10, 128, 128,
    300, 0, 0
  ), ncol = 3, byrow = TRUE)

  expect_error(
    validate_color_input(invalid_rgb, "rgb"),
    "RGB values must be between 0 and 255"
  )
})

test_that("validate_color_input validates XYZ space constraints", {
  # Test negative XYZ values
  invalid_xyz <- matrix(c(
    -10, 100, 100,
    50, -50, 50
  ), ncol = 3, byrow = TRUE)

  expect_error(
    validate_color_input(invalid_xyz, "xyz"),
    "XYZ values cannot be negative"
  )

  # Test extremely large XYZ values
  large_xyz <- matrix(c(
    200, 100, 100,
    50, 50, 50
  ), ncol = 3, byrow = TRUE)

  expect_warning(
    validate_color_input(large_xyz, "xyz"),
    "XYZ values > 150 detected"
  )

  # Test very small non-zero XYZ values
  small_xyz <- matrix(c(
    0.1, 100, 100,
    50, 0.2, 50
  ), ncol = 3, byrow = TRUE)

  expect_warning(
    validate_color_input(small_xyz, "xyz"),
    "Very small non-zero XYZ values detected"
  )
})

test_that("validate_color_input handles missing values correctly", {
  # Test input with NA values
  na_input <- matrix(c(
    50, NA, 0,
    60, 10, -10
  ), ncol = 3, byrow = TRUE)

  expect_error(
    validate_color_input(na_input, "oklab"),
    "Input contains missing values"
  )
})

test_that("validate_color_input checks dimensions", {
  # Test input with wrong number of columns
  wrong_cols <- matrix(c(
    50, 0,
    60, 10
  ), ncol = 2, byrow = TRUE)

  expect_error(
    validate_color_input(wrong_cols, "oklab"),
    "Input must have exactly 3 columns"
  )

  # Test single row input with distance=TRUE
  single_row <- matrix(c(50, 0, 0), nrow = 1)
  expect_error(
    validate_color_input(single_row, "oklab", distance = TRUE),
    "Input must have at least 2 rows for distance calculation"
  )
})

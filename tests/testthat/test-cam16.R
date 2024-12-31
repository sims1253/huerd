library(testthat)

test_that("xyz_to_cam16ucs handles basic input correctly", {
  # Test with D65 white point
  xyz <- matrix(c(95.047, 100, 108.883), nrow = 1)
  result <- xyz_to_cam16ucs(xyz)

  # Check structure
  expect_type(result, "double")
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("J_p", "a_M", "b_M"))

  # White should have zero chroma (a_M and b_M near 0)
  expect_true(abs(result[1, 2]) < 1e-10)
  expect_true(abs(result[1, 3]) < 1e-10)

  # J_p should be positive for white
  expect_true(result[1, 1] > 0)
})

test_that("xyz_to_cam16ucs handles multiple input rows", {
  xyz <- matrix(c(
    95.047, 100, 108.883, # D65 white
    0, 0, 0 # Black
  ), nrow = 2, byrow = TRUE)

  result <- xyz_to_cam16ucs(xyz)

  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)

  # Black should have J_p near 0
  expect_true(abs(result[2, 1]) < 1e-10)
})

test_that("xyz_to_cam16ucs validates input parameters", {
  xyz <- matrix(c(95.047, 100, 108.883), nrow = 1)

  # Test invalid L_A
  expect_error(
    xyz_to_cam16ucs(xyz, L_A = -1),
    "L_A must be positive"
  )

  # Test invalid Y_b
  expect_error(
    xyz_to_cam16ucs(xyz, Y_b = -1),
    "Y_b must be between 0 and 100"
  )

  # Test invalid surround
  expect_error(
    xyz_to_cam16ucs(xyz, surround = "invalid"),
    "surround must be one of: average, dim, dark"
  )
})

test_that("xyz_to_cam16ucs handles different viewing conditions", {
  xyz <- matrix(c(95.047, 100, 108.883), nrow = 1)

  # Test different surrounds
  result1 <- xyz_to_cam16ucs(xyz, surround = "average")
  result2 <- xyz_to_cam16ucs(xyz, surround = "dim")
  result3 <- xyz_to_cam16ucs(xyz, surround = "dark")

  # Results should be different but reasonable
  expect_false(identical(result1, result2))
  expect_false(identical(result2, result3))
  expect_false(identical(result1, result3))
})

test_that("cam16ucs_distance calculates distances correctly", {
  # Test identical colors
  color <- c(50, 0, 0)
  expect_equal(cam16ucs_distance(color, color), 0)

  # Test axis-aligned differences
  color1 <- c(50, 0, 0)
  color2 <- c(60, 0, 0)
  dist <- cam16ucs_distance(color1, color2)
  expect_true(dist > 0)

  # Test symmetry
  dist1 <- cam16ucs_distance(color1, color2)
  dist2 <- cam16ucs_distance(color2, color1)
  expect_equal(dist1, dist2)
})

test_that("cam16ucs_distance_matrix calculates pairwise distances correctly", {
  colors <- matrix(c(
    50, 0, 0,
    60, 0, 0,
    50, 10, 0
  ), nrow = 3, byrow = TRUE)

  result <- cam16ucs_distance_matrix(colors)

  # Check matrix properties
  expect_equal(dim(result), c(3, 3))
  expect_true(isSymmetric(result))
  expect_equal(diag(result), c(0, 0, 0))

  # All off-diagonal elements should be positive
  expect_true(all(result[upper.tri(result)] > 0))
})

test_that("xyz_to_cam16ucs handles extreme values", {
  # Test very bright colors
  bright <- matrix(c(200, 200, 200), nrow = 1)
  expect_warning(xyz_to_cam16ucs(bright))

  # Test very dark colors
  dark <- matrix(c(0.1, 0.1, 0.1), nrow = 1)
  expect_warning(xyz_to_cam16ucs(dark))

  # Test numeric stability
  tiny <- matrix(c(1e-10, 1e-10, 1e-10), nrow = 1)
  expect_warning(result <- xyz_to_cam16ucs(tiny))
  expect_false(any(is.na(result)))
  expect_false(any(is.infinite(result)))
})

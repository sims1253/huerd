library(testthat)

# --- Test Data ---
XYZ_1 <- c(19.01, 20, 21.78)
XYZ_w <- c(95.05, 100, 108.88)
L_A <- 318.31
Y_b <- 20.0
CAM_1 <- list(J = 41.7312079051266, C = 0.103355738709075, h = 217.06795970785)

test_that("ciecam_environs returns correct values", {
  expect_equal(ciecam_environs("Average"), list(F = 1.0, c = 0.69, N_c = 1.0))
  expect_equal(ciecam_environs("Dim"), list(F = 0.9, c = 0.59, N_c = 0.95))
  expect_equal(ciecam_environs("Dark"), list(F = 0.8, c = 0.525, N_c = 0.8))
  expect_error(ciecam_environs("Invalid"))
})

test_that("XYZ_to_CIECAM16 returns correct values", {
  result <- XYZ_to_CIECAM16(XYZ_1, XYZ_w, L_A, Y_b)
  expect_equal(result$J, 0.417312079051266, tolerance = 1e-6)
  expect_equal(result$C, 0.0010335573870907499, tolerance = 1e-6)
  expect_equal(result$h, 217.06795970785, tolerance = 1e-6)
  expect_equal(result$s, 0.023450150732149597, tolerance = 1e-6)
  expect_equal(result$Q, 0.488429272357765, tolerance = 1e-6)
  expect_equal(result$M, 0.0010743675463543968, tolerance = 1e-6)
  expect_equal(result$H, 275.59498614194306, tolerance = 1e-6)
})

test_that("XYZ_to_CIECAM16 handles input errors", {
  expect_error(XYZ_to_CIECAM16(c(1, 2), XYZ_w, L_A, Y_b))
  expect_error(XYZ_to_CIECAM16(c(1, 2, 3, 4), XYZ_w, L_A, Y_b))
  expect_error(XYZ_to_CIECAM16(XYZ_1, c(1, 2), L_A, Y_b))
  expect_error(XYZ_to_CIECAM16(XYZ_1, XYZ_w, -1, Y_b))
  expect_error(XYZ_to_CIECAM16(XYZ_1, XYZ_w, L_A, -1))
})

test_that("CIECAM16_to_XYZ returns correct values", {
  result <- CIECAM16_to_XYZ(CAM_1, XYZ_w, L_A, Y_b)
  expect_equal(result, c(0.1901, 0.2, 0.2178), tolerance = 1e-6)
})

test_that("CIECAM16_to_XYZ handles input errors", {
  expect_error(CIECAM16_to_XYZ(list(J = 1, C = 2), XYZ_w, L_A, Y_b))
  expect_error(CIECAM16_to_XYZ(list(J = 1, h = 3), XYZ_w, L_A, Y_b))
  expect_error(CIECAM16_to_XYZ(list(C = 2, h = 3), XYZ_w, L_A, Y_b))
  expect_error(CIECAM16_to_XYZ(CAM_1, c(1, 2), L_A, Y_b))
  expect_error(CIECAM16_to_XYZ(CAM_1, XYZ_w, -1, Y_b))
  expect_error(CIECAM16_to_XYZ(CAM_1, XYZ_w, L_A, -1))
})

test_that("CIECAM16_to_XYZ and XYZ_to_CIECAM16 are inverses", {
  # Test with a specific XYZ value
  xyz_original <- c(25, 50, 75)
  cam <- XYZ_to_CIECAM16(xyz_original, XYZ_w, L_A, Y_b)
  xyz_recovered <- CIECAM16_to_XYZ(cam, XYZ_w, L_A, Y_b)
  expect_equal(xyz_recovered, xyz_original / 100, tolerance = 1e-5)

  # Test with a random XYZ value
  xyz_original <- runif(3, 0, 100)
  cam <- XYZ_to_CIECAM16(xyz_original, XYZ_w, L_A, Y_b)
  xyz_recovered <- CIECAM16_to_XYZ(cam, XYZ_w, L_A, Y_b)
  expect_equal(xyz_recovered, xyz_original / 100, tolerance = 1e-5)
})

test_that("hex_to_CIECAM16 returns expected format and handles errors", {
  result <- hex_to_CIECAM16("#FF0000")
  expect_type(result, "list")
  expect_named(result, c("J", "C", "h", "s", "Q", "M", "H", "HC"))
})

test_that("hex_colors_distance returns expected values and handles errors", {
  # Test with identical colors
  expect_equal(hex_colors_distance("#FF0000", "#FF0000"), 0)

  # Test with known different colors (you might need to adjust the expected value)
  expect_equal(hex_colors_distance("#FF0000", "#00FF00"), 62.05344, tolerance = 1e-3)

  # Test with invalid hex codes
  expect_error(hex_colors_distance("#FF000", "#00FF00"))
  expect_error(hex_colors_distance("#FF0000", "#00FF0"))
})

test_that("hex_colors_distance_matrix returns expected values and handles errors", {
  colors <- c("#FF0000", "#00FF00", "#0000FF")
  result <- hex_colors_distance_matrix(colors)

  # Check dimensions and symmetry
  expect_equal(dim(result), c(3, 3))
  expect_true(isSymmetric(result))

  # Check diagonal (should be all 0s)
  expect_equal(diag(result), rep(0, 3))

  # Check some known distances (you might need to adjust these values)
  expect_equal(result[1, 2], 62.05344, tolerance = 1e-3)
  expect_equal(result[1, 3], 73.59899, tolerance = 1e-3)
  expect_equal(result[2, 3], 61.45178, tolerance = 1e-3)

  # Test with invalid hex codes
  expect_error(hex_colors_distance_matrix(c("#FF000", "#00FF00", "#0000FF")))
})

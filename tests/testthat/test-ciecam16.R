library(testthat)

test_that("CAM16_Specification creation works", {
  # Test basic creation with all properties
  spec <- CAM16_Specification(
    J = 50,
    C = 30,
    h = 180,
    s = 2.5,
    Q = 75,
    M = 25,
    H = 200,
    e = 0.5
  )

  # expect_s7_class(spec, "CAM16_Specification")
  expect_equal(spec@J, 50)
  expect_equal(spec@C, 30)
  expect_equal(spec@h, 180)
  expect_equal(spec@s, 2.5)
  expect_equal(spec@Q, 75)
  expect_equal(spec@M, 25)
  expect_equal(spec@H, 200)
  expect_equal(spec@e, 0.5)

  # Test creation with partial properties
  spec_partial <- CAM16_Specification(J = 50, C = 30)
  expect_equal(spec_partial@J, 50)
  expect_equal(spec_partial@C, 30)
  expect_equal(spec_partial@h, numeric())

  # Test creation with no properties
  spec_empty <- CAM16_Specification()
  # expect_s7_class(spec_empty, "CAM16_Specification")
  expect_true(length(as.list(spec_empty)) == 0)
})

test_that("CAM16_Specification validates numeric ranges", {
  # Test J (Lightness) validation
  expect_error(CAM16_Specification(J = -1), "Invalid J: Value must be >= 0")
  expect_error(CAM16_Specification(J = 101), "Invalid J: Value must be <= 100")
  expect_error(CAM16_Specification(C = -1), "Invalid C: Value must be >= 0")
  expect_error(CAM16_Specification(C = 101), "Invalid C: Value must be <= 100")
  expect_error(CAM16_Specification(h = -1), "Invalid h: Value must be >= 0")
  expect_error(CAM16_Specification(h = 361), "Invalid h: Value must be <= 360")
  expect_error(CAM16_Specification(s = -1), "Invalid s: Value must be >= 0")
  expect_error(CAM16_Specification(s = 101), "Invalid s: Value must be <= 100")
  expect_error(CAM16_Specification(Q = -1), "Invalid Q: Value must be >= 0")
  expect_error(CAM16_Specification(Q = 101), "Invalid Q: Value must be <= 100")
  expect_error(CAM16_Specification(M = -1), "Invalid M: Value must be >= 0")
  expect_error(CAM16_Specification(M = 101), "Invalid M: Value must be <= 100")
  expect_error(CAM16_Specification(H = -1), "Invalid H: Value must be >= 0")
  expect_error(CAM16_Specification(H = 401), "Invalid H: Value must be <= 400")
})

test_that("CAM16_Specification validates types", {
  # Test numeric type validation
  expect_error(
    CAM16_Specification(J = "50"),
    "<huerd::CAM16_Specification> object properties are invalid:
- @J must be <integer> or <double>, not <character>"
  )
  expect_error(
    CAM16_Specification(C = "30"),
    "<huerd::CAM16_Specification> object properties are invalid:
- @C must be <integer> or <double>, not <character>"
  )
  expect_error(
    CAM16_Specification(h = "180"),
    "<huerd::CAM16_Specification> object properties are invalid:
- @h must be <integer> or <double>, not <character>"
  )
})

test_that("CAM16_Specification handles NA values", {
  # Test NA validation
  expect_error(
    CAM16_Specification(J = NA),
    "<huerd::CAM16_Specification> object properties are invalid:
- @J must be <integer> or <double>, not <logical>"
  )
  expect_error(
    CAM16_Specification(C = NA),
    "<huerd::CAM16_Specification> object properties are invalid:
- @C must be <integer> or <double>, not <logical>"
  )
  expect_error(
    CAM16_Specification(h = NA),
    "<huerd::CAM16_Specification> object properties are invalid:
- @h must be <integer> or <double>, not <logical>"
  )

  # Test NA in vectors
  expect_error(
    CAM16_Specification(J = c(50, NA)), "Invalid J: Value cannot be NA"
  )
})

# Test Constructor with Vectorized Inputs
test_that("CAM16_Specification constructor handles vectorized inputs", {
  multiple_colors <- CAM16_Specification(
    J = c(50, 60, 70),
    C = c(30, 40, 50),
    h = c(180, 200, 220)
  )

  # Check that a list is returned
  expect_true(is.list(multiple_colors))

  # Check the length of the list
  expect_equal(length(multiple_colors), 3)

  # Check the properties of individual objects in the list
  expect_equal(multiple_colors[[1]]@J, 50)
  expect_equal(multiple_colors[[2]]@C, 40)
  expect_equal(multiple_colors[[3]]@h, 220)

  # Check that uninitialized properties have length 0
  expect_equal(length(multiple_colors[[1]]@s), 0)
})

# Test Constructor with a Single Input
test_that("CAM16_Specification constructor handles single inputs", {
  single_color <- CAM16_Specification(J = 50, C = 30, h = 180)

  # Check that a single object is returned
  expect_false(is.list(single_color))

  # Check the properties of the object
  expect_equal(single_color@J, 50)
  expect_equal(single_color@C, 30)
  expect_equal(single_color@h, 180)

  # Check that uninitialized properties have length 0
  expect_equal(length(single_color@s), 0)
})

# Test as.list with Single Object
test_that("as.list works with a single CAM16_Specification object", {
  single_color <- CAM16_Specification(J = 50, C = 30, h = 180)
  color_list <- as.list(single_color)

  # Check the type of the output
  expect_true(is.list(color_list))

  # Check that initialized properties are present
  expect_equal(color_list$J, 50)
  expect_equal(color_list$C, 30)
  expect_equal(color_list$h, 180)

  # Check that uninitialized properties are not present
  expect_false(hasName(color_list, "s"))
  expect_false(hasName(color_list, "Q"))
  expect_false(hasName(color_list, "M"))
  expect_false(hasName(color_list, "H"))
  expect_false(hasName(color_list, "HC"))
})

# Test as.list with Multiple Objects
test_that("as.list works with multiple CAM16_Specification objects", {
  multiple_colors <- CAM16_Specification(
    J = c(50, 60, 70),
    C = c(30, 40, 50),
    h = c(180, 200, 220)
  )
  list_of_lists <- lapply(multiple_colors, as.list)

  # Check the type of the output
  expect_true(is.list(list_of_lists))

  # Check the length of the list
  expect_equal(length(list_of_lists), 3)

  # Check the properties of individual lists
  expect_equal(list_of_lists[[1]]$J, 50)
  expect_equal(list_of_lists[[2]]$C, 40)
  expect_equal(list_of_lists[[3]]$h, 220)

  # Check that uninitialized properties are not present
  expect_false(hasName(list_of_lists[[1]], "s"))
  expect_false(hasName(list_of_lists[[2]], "Q"))
  expect_false(hasName(list_of_lists[[3]], "M"))
})

# Test as.data.frame with Single Object
test_that("as.data.frame works with a single CAM16_Specification object", {
  single_color <- CAM16_Specification(J = 50, C = 30, h = 180)
  color_df <- as.data.frame(single_color)

  # Check the type of the output
  expect_true(is.data.frame(color_df))

  # Check the dimensions of the data frame
  expect_equal(nrow(color_df), 1)
  expect_equal(ncol(color_df), 3)

  # Check the values in the data frame
  expect_equal(color_df$J, 50)
  expect_equal(color_df$C, 30)
  expect_equal(color_df$h, 180)

  # Check that uninitialized properties are not present
  expect_false(hasName(color_df, "s"))
  expect_false(hasName(color_df, "Q"))
  expect_false(hasName(color_df, "M"))
  expect_false(hasName(color_df, "H"))
  expect_false(hasName(color_df, "HC"))
})

# Test as.data.frame with Multiple Objects
test_that("as.data.frame works with multiple CAM16_Specification objects", {
  multiple_colors <- CAM16_Specification(
    J = c(50, 60, 70),
    C = c(30, 40, 50),
    h = c(180, 200, 220)
  )
  color_df <- do.call(rbind, lapply(multiple_colors, as.data.frame))
  # Check the type of the output
  expect_true(is.data.frame(color_df))

  # Check the dimensions of the data frame
  expect_equal(nrow(color_df), 3)
  expect_equal(ncol(color_df), 3)

  # Check the values in the data frame
  expect_equal(color_df$J, c(50, 60, 70))
  expect_equal(color_df$C, c(30, 40, 50))
  expect_equal(color_df$h, c(180, 200, 220))

  # Check that uninitialized properties are not present
  expect_false(hasName(color_df, "s"))
  expect_false(hasName(color_df, "Q"))
  expect_false(hasName(color_df, "M"))
  expect_false(hasName(color_df, "H"))
  expect_false(hasName(color_df, "HC"))
})

# Test '==' Operator
test_that("'==' operator works correctly", {
  color1 <- CAM16_Specification(J = 50, C = 30, h = 180)
  color2 <- CAM16_Specification(J = 50, C = 30, h = 180)
  color3 <- CAM16_Specification(J = 60, C = 40, h = 200)
  color4 <- CAM16_Specification(C = 30, h = 180)

  # Test equality with initialized properties
  expect_true(color1 == color2)

  # Test inequality
  expect_false(color1 == color3)

  # Test inequality with uninitialized properties
  expect_false(color1 == color4) # Both have J, C, and h initialized, and they are equal
})

test_that("format.CAM16_Specification formats single object correctly", {
  color <- CAM16_Specification(J = 50, C = 30, h = 180, s = 85)

  expected_output <- paste0(
    "<CAM16_Specification:\n",
    "  Lightness (J) = 50,\n",
    "  Chroma (C) = 30,\n",
    "  Hue Angle (h) = 180,\n",
    "  Saturation (s) = 85\n",
    ">"
  )

  expect_equal(format(color), expected_output)
})

test_that("format.CAM16_Specification handles uninitialized properties", {
  color <- CAM16_Specification(J = 50, h = 180)

  expected_output <- paste0(
    "<CAM16_Specification:\n",
    "  Lightness (J) = 50,\n",
    "  Hue Angle (h) = 180\n",
    ">"
  )

  expect_equal(format(color), expected_output)
})

# Test '-' Operator
test_that("'-' operator calculates color difference correctly", {
  color1 <- CAM16_Specification(J = 50, C = 30, h = 180, H = 200, e = 0.9)
  color2 <- CAM16_Specification(J = 60, C = 40, h = 200, H = 220, e = 0.8)
  color3 <- CAM16_Specification(J = 50, C = 30, h = 180, H = 200, e = 0.9)

  # Test if symmetric
  expect_equal(color1 - color2, color2 - color1)

  # Test if distance from itself is zero
  expect_equal(color1 - color1, 0)

  # Test with objects missing properties
  color_missing <- CAM16_Specification(s = 90)
  expect_true(is.na(color1 - color_missing))
  expect_true(is.na(color_missing - color1))

  # Calculate expected difference using an alternative method (if available)
  # or manually calculate based on CAM16-UCS formulas
  expected_diff <- sqrt((60.93858 - 51.23460)^2 + (2.46998 - -15.57446)^2 + (-28.69250 - -14.27985)^2) # Replace with a reliable calculation

  # Test with single objects
  expect_equal(round(color1 - color2, 5), round(expected_diff, 5))
  
  # Test with a list
  expect_equal(round(color4 - color1, 5), round(c(expected_diff, 14.17776), 5))
  expect_equal(round(color1 - color4, 5), round(c(expected_diff, 14.17776), 5))

  # Test with objects missing properties
  color_missing <- CAM16_Specification(s = 90)
  expect_true(is.na(color1 - color_missing))
  expect_true(is.na(color_missing - color1))
})

# Test CAM16_UCS Function
test_that("CAM16_UCS calculates UCS coordinates correctly", {
  color <- CAM16_Specification(J = 50, C = 30, h = 180, H = 200, e = 0.9)

  # Calculate expected J_prime, a, and b using an alternative method or manual calculation
  expected_J_prime <- 51.23460
  expected_a <- -15.57446
  expected_b <- -14.27985

  ucs_coords <- CAM16_UCS(color)

  # Test the calculated values
  expect_equal(round(ucs_coords$J_prime, 5), round(expected_J_prime, 5))
  expect_equal(round(ucs_coords$a, 5), round(expected_a, 5))
  expect_equal(round(ucs_coords$b, 5), round(expected_b, 5))
  
  # Test with a list
  color_list <- CAM16_Specification(J = c(50, 60), C = c(30, 40), h = c(180, 200), H = c(200, 220), e = c(0.9, 0.8))
  expected_a_list <- c(-15.57446, 2.46998)
  expected_b_list <- c(-14.27985, -28.69250)
  
  ucs_coords <- CAM16_UCS(color_list)
  
  # Test the calculated values
  expect_equal(round(ucs_coords[[1]]$J_prime, 5), round(expected_J_prime, 5))
  expect_equal(round(ucs_coords[[1]]$a, 5), round(expected_a_list[[1]], 5))
  expect_equal(round(ucs_coords[[1]]$b, 5), round(expected_b_list[[1]], 5))
  expect_equal(round(ucs_coords[[2]]$J_prime, 5), round(60.93858, 5))
  expect_equal(round(ucs_coords[[2]]$a, 5), round(expected_a_list[[2]], 5))
  expect_equal(round(ucs_coords[[2]]$b, 5), round(expected_b_list[[2]], 5))
})

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

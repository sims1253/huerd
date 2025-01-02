library(testthat)

test_that("hex_to_cam16ucs handles basic input correctly", {
  # Test basic colors
  basic_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFFFF", "#000000")
  result <- hex_to_cam16ucs(basic_colors)

  # Check structure
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), length(basic_colors))
  expect_equal(colnames(result), c("J_p", "a_M", "b_M"))

  # White should have zero chroma (a_M and b_M near 0)
  white_idx <- which(basic_colors == "#FFFFFF")
  expect_true(abs(result[white_idx, "a_M"]) < 1e-10)
  expect_true(abs(result[white_idx, "b_M"]) < 1e-10)

  # Black should have near-zero lightness
  black_idx <- which(basic_colors == "#000000")
  expect_true(result[black_idx, "J_p"] < 1)

  # Pure colors should have significant chroma
  red_idx <- which(basic_colors == "#FF0000")
  expect_true(sqrt(result[red_idx, "a_M"]^2 + result[red_idx, "b_M"]^2) > 10)
})

test_that("hex_to_cam16ucs validates input correctly", {
  # Test invalid hex formats
  expect_error(hex_to_cam16ucs("not_hex"))
  expect_error(hex_to_cam16ucs("#FF")) # Too short
  expect_error(hex_to_cam16ucs("#FFFFFFF")) # Too long
  expect_error(hex_to_cam16ucs("#GGGGGG")) # Invalid characters

  # Test invalid viewing conditions
  expect_error(hex_to_cam16ucs("#FF0000", L_A = -1))
  expect_error(hex_to_cam16ucs("#FF0000", Y_b = -1))
  expect_error(hex_to_cam16ucs("#FF0000", Y_b = 101))
  expect_error(hex_to_cam16ucs("#FF0000", surround = "invalid"))
})

test_that("cam16ucs_to_hex handles basic input correctly", {
  # Create test coordinates
  test_coords <- list(
    J_p = c(50, 75, 25),
    a_M = c(20, -15, 0),
    b_M = c(10, 30, 0)
  )

  result <- cam16ucs_to_hex(
    test_coords$J_p,
    test_coords$a_M,
    test_coords$b_M
  )

  # Check structure
  expect_type(result, "character")
  expect_length(result, 3)
  expect_true(all(grepl("^#[0-9A-F]{6}$", result)))
})

test_that("cam16ucs_to_hex validates input correctly", {
  # Test mismatched vector lengths
  expect_error(
    cam16ucs_to_hex(
      J_p = c(50, 75),
      a_M = c(20),
      b_M = c(10, 30)
    )
  )

  # Test invalid viewing conditions
  expect_error(
    cam16ucs_to_hex(50, 20, 10, L_A = -1)
  )
  expect_error(
    cam16ucs_to_hex(50, 20, 10, Y_b = 101)
  )
  expect_error(
    cam16ucs_to_hex(50, 20, 10, surround = "invalid")
  )
})

test_that("roundtrip conversions are accurate", {
  # Test a range of colors
  test_colors <- c(
    "#FF0000", "#00FF00", "#0000FF", # Pure RGB
    "#FFFFFF", "#000000", # White and black
    "#808080", "#C0C0C0", # Grays
    "#FFA500", "#800080", "#008080" # Mixed colors
  )

  results <- check_cam16_roundtrip(test_colors)

  # Check structure
  expect_equal(names(results), c("original", "roundtrip", "matches"))
  expect_equal(nrow(results), length(test_colors))

  # Most colors should round-trip exactly
  # Note: Some minor variations are possible due to quantization
  expect_true(mean(results$matches) > 0.8)

  # Achromatic colors should definitely round-trip
  achromatic <- c("#FFFFFF", "#000000", "#808080", "#C0C0C0")
  achromatic_results <- results[results$original %in% achromatic, ]
  expect_true(all(achromatic_results$matches))
})

test_that("matrix conversion functions work correctly", {
  # Test hex_to_cam16ucs_matrix
  colors <- c("#FF0000", "#00FF00")
  result1 <- hex_to_cam16ucs_matrix(colors)
  expect_true(is.matrix(result1))
  expect_equal(colnames(result1), c("J_p", "a_M", "b_M"))

  # Test cam16ucs_matrix_to_hex
  ucs_matrix <- matrix(
    c(
      50, 20, 10,
      75, -15, 30
    ),
    ncol = 3,
    byrow = TRUE
  )
  result2 <- cam16ucs_matrix_to_hex(ucs_matrix)
  expect_type(result2, "character")
  expect_true(all(grepl("^#[0-9A-F]{6}$", result2)))

  # Test invalid input
  expect_error(cam16ucs_matrix_to_hex(matrix(1:4, ncol = 2)))
})

test_that("different viewing conditions produce different results", {
  colors <- c("#FF0000", "#00FF00")

  # Test different adaptation luminance
  result1 <- hex_to_cam16ucs(colors, L_A = 100)
  result2 <- hex_to_cam16ucs(colors, L_A = 50)
  expect_false(identical(result1, result2))

  # Test different backgrounds
  result3 <- hex_to_cam16ucs(colors, Y_b = 20)
  result4 <- hex_to_cam16ucs(colors, Y_b = 50)
  expect_false(identical(result3, result4))

  # Test different surrounds
  result5 <- hex_to_cam16ucs(colors, surround = "average")
  result6 <- hex_to_cam16ucs(colors, surround = "dim")
  expect_false(identical(result5, result6))
})

test_that("color relationships are preserved", {
  # Complementary colors should have opposite hue angles
  red <- hex_to_cam16ucs("#FF0000")
  cyan <- hex_to_cam16ucs("#00FFFF")
  red_angle <- atan2(red[, "b_M"], red[, "a_M"])
  cyan_angle <- atan2(cyan[, "b_M"], cyan[, "a_M"])
  angle_diff <- abs(abs(red_angle - cyan_angle) - pi)
  expect_true(angle_diff < 0.1) # Allow small numerical differences

  # Grayscale should have zero chroma
  grays <- hex_to_cam16ucs(c("#000000", "#808080", "#FFFFFF"))
  chromas <- sqrt(grays[, "a_M"]^2 + grays[, "b_M"]^2)
  expect_true(all(chromas < 1e-10))
})

test_that("edge cases and numerical stability", {
  # Test near-zero values
  near_black <- hex_to_cam16ucs("#010101")
  expect_true(all(near_black[1, ] >= 0))

  # Test near-white values
  near_white <- hex_to_cam16ucs("#FEFEFE")
  expect_true(all(near_white[1, ] <= 100))

  # Test near-singular hue angles (where tan approaches infinity)
  bright_yellow <- hex_to_cam16ucs("#FFFF00")
  roundtrip <- cam16ucs_matrix_to_hex(hex_to_cam16ucs("#FFFF00"))
  expect_equal(toupper(roundtrip), "#FFFF00")

  # Test colors near the sRGB gamut boundary
  saturated <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF")
  roundtrip_sat <- check_cam16_roundtrip(saturated)
  expect_true(all(roundtrip_sat$matches))
})

test_that("perceptual uniformity properties", {
  # Test that equal coordinate differences correspond to roughly equal perceptual differences
  gray_steps <- paste0("#", sprintf(
    "%02X%02X%02X",
    rep(seq(128, 192, 32), each = 3),
    rep(seq(128, 192, 32), each = 3),
    rep(seq(128, 192, 32), each = 3)
  ))

  diffs <- hex_cam16ucs_distance_matrix(gray_steps)

  # Check if differences between adjacent gray steps are similar
  step_diffs <- diff(diag(diffs[-1, -nrow(diffs)]))

  # Allow 20% variation in step sizes
  expect_true(max(step_diffs) / min(step_diffs) < 1.2)

  # Test complementary colors have large distances
  red_cyan <- hex_cam16ucs_distance("#FF0000", "#00FFFF")
  green_magenta <- hex_cam16ucs_distance("#00FF00", "#FF00FF")
  blue_yellow <- hex_cam16ucs_distance("#0000FF", "#FFFF00")

  # Complementary pairs should have similar large distances
  max_diff <- max(abs(c(
    red_cyan - green_magenta,
    green_magenta - blue_yellow,
    blue_yellow - red_cyan
  )))
  expect_true(max_diff / mean(c(red_cyan, green_magenta, blue_yellow)) < 0.1)
})

test_that("viewing condition parameter ranges", {
  test_color <- "#FF5500"

  # Test range of adaptation luminance
  l_as <- c(0.1, 1, 10, 100, 1000)
  results_la <- sapply(l_as, function(la) {
    hex_to_cam16ucs(test_color, L_A = la)[1, ]
  })
  # Lightness should increase with adaptation luminance
  expect_true(all(diff(results_la[1, ]) > 0))

  # Test range of background luminance
  y_bs <- c(1, 20, 50, 80)
  results_yb <- sapply(y_bs, function(yb) {
    hex_to_cam16ucs(test_color, Y_b = yb)[1, ]
  })
  # Contrast should decrease with increasing background luminance
  chroma_vals <- sqrt(results_yb[2, ]^2 + results_yb[3, ]^2)
  expect_true(all(diff(chroma_vals) < 0))

  # Test all surround conditions
  surrounds <- c("dark", "dim", "average")
  results_surr <- sapply(surrounds, function(surr) {
    hex_to_cam16ucs(test_color, surround = surr)[1, ]
  })
  # Lightness and chroma should increase from dark to average
  expect_true(all(diff(results_surr[1, ]) > 0))
})

test_that("color difference functions", {
  # Test distance symmetry
  col1 <- "#FF0000"
  col2 <- "#00FF00"
  dist_12 <- hex_cam16ucs_distance(col1, col2)
  dist_21 <- hex_cam16ucs_distance(col2, col1)
  expect_equal(dist_12, dist_21)

  # Test triangle inequality
  col3 <- "#0000FF"
  dist_13 <- hex_cam16ucs_distance(col1, col3)
  dist_23 <- hex_cam16ucs_distance(col2, col3)
  expect_true(dist_12 <= dist_13 + dist_23)

  # Test distance matrix properties
  colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFFFF")
  dist_mat <- hex_cam16ucs_distance_matrix(colors)

  # Test symmetry
  expect_equal(dist_mat, t(dist_mat))

  # Test diagonal is zero
  expect_true(all(diag(dist_mat) == 0))

  # Test all distances are positive
  expect_true(all(dist_mat >= 0))
})

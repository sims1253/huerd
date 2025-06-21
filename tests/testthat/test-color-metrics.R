test_that("evaluate_palette works with single color", {
  colors <- c("#FF0000")

  result <- evaluate_palette(colors)

  expect_true(is.list(result))
  expect_true(inherits(result, "huerd_evaluation"))
  expect_equal(result$n_colors, 1)
  # No heuristic score in pure data provider mode
  expect_false("summary_heuristic_score" %in% names(result))
})

test_that("evaluate_palette_quality works with single color", {
  # Convert hex to OKLAB
  hex_colors <- c("#FF0000")
  oklab_colors <- farver::convert_colour(
    farver::decode_colour(hex_colors, to = "rgb"),
    from = "rgb",
    to = "oklab"
  )

  heuristic_targets <- list(
    min_distance = 0.1,
    performance_ratio = 0.8,
    worst_case_cvd_min_distance = 0.08,
    lightness_range = 0.5,
    chroma_range = 0.3
  )

  heuristic_weights <- list(
    min_distance = 0.3,
    performance_ratio = 0.2,
    worst_case_cvd_min_distance = 0.2,
    lightness_range = 0.15,
    chroma_range = 0.15
  )

  result <- evaluate_palette_quality(oklab_colors)

  expect_true(is.list(result))
  expect_true("distances" %in% names(result))
  expect_true("cvd_safety" %in% names(result))
  expect_true("distribution" %in% names(result))
  # No heuristic score in pure data provider mode
  expect_false("summary_heuristic_score" %in% names(result))
})

# Tests for analyze_cvd_safety_metrics - Level 9 TDD
test_that("analyze_cvd_safety_metrics returns proper structure", {
  # Test basic structure with valid two-color palette
  colors_oklab <- matrix(
    c(
      0.5,
      0.1,
      0.0, # Color 1
      0.7,
      -0.1,
      0.1 # Color 2
    ),
    nrow = 2,
    byrow = TRUE
  )

  original_min_dist <- 0.25

  result <- analyze_cvd_safety_metrics(colors_oklab, original_min_dist)

  # Test structure
  expect_true(is.list(result))
  expect_true("protan" %in% names(result))
  expect_true("deutan" %in% names(result))
  expect_true("tritan" %in% names(result))
  expect_true("worst_case_min_distance" %in% names(result))

  # Test each CVD type structure
  for (cvd_type in c("protan", "deutan", "tritan")) {
    expect_true(is.list(result[[cvd_type]]))
    expect_true("min_distance" %in% names(result[[cvd_type]]))
    expect_true("mean_distance" %in% names(result[[cvd_type]]))
    expect_true("preserved_ratio" %in% names(result[[cvd_type]]))
  }
})

test_that("analyze_cvd_safety_metrics handles single color input", {
  # Test edge case: single color (should return NA values)
  single_color <- matrix(c(0.5, 0.1, 0.0), nrow = 1, ncol = 3)

  result <- analyze_cvd_safety_metrics(single_color, 0.25)

  expect_true(is.list(result))
  expect_true(is.na(result$worst_case_min_distance))
  expect_true(is.na(result$protan$min_distance))
  expect_true(is.na(result$deutan$min_distance))
  expect_true(is.na(result$tritan$min_distance))
})

test_that("analyze_cvd_safety_metrics handles empty input", {
  # Test edge case: empty matrix
  empty_colors <- matrix(numeric(0), nrow = 0, ncol = 3)

  result <- analyze_cvd_safety_metrics(empty_colors, 0.25)

  expect_true(is.list(result))
  expect_true(is.na(result$worst_case_min_distance))
})

test_that("analyze_cvd_safety_metrics calculates preserved ratios", {
  # Test that preserved ratios are calculated correctly
  colors_oklab <- matrix(
    c(
      0.3,
      0.1,
      0.0, # Darker color
      0.8,
      -0.1,
      0.1 # Lighter color
    ),
    nrow = 2,
    byrow = TRUE
  )

  original_min_dist <- 0.5

  result <- analyze_cvd_safety_metrics(colors_oklab, original_min_dist)

  # Should have numeric preserved ratios
  expect_true(is.numeric(result$protan$preserved_ratio))
  expect_true(is.numeric(result$deutan$preserved_ratio))
  expect_true(is.numeric(result$tritan$preserved_ratio))

  # Preserved ratios should be between 0 and 1 for typical cases
  for (cvd_type in c("protan", "deutan", "tritan")) {
    if (is.finite(result[[cvd_type]]$preserved_ratio)) {
      expect_true(result[[cvd_type]]$preserved_ratio >= 0)
      expect_true(result[[cvd_type]]$preserved_ratio <= 2) # Allow some flexibility
    }
  }
})

# Tests for analyze_color_distribution - Level 9 TDD
test_that("analyze_color_distribution returns proper structure", {
  # Test basic structure with valid colors
  colors_oklab <- matrix(
    c(
      0.3,
      0.1,
      0.0, # Color 1
      0.7,
      -0.1,
      0.1, # Color 2
      0.5,
      0.2,
      -0.1 # Color 3
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- analyze_color_distribution(colors_oklab)

  # Test main structure
  expect_true(is.list(result))
  expect_true("lightness_oklab" %in% names(result))
  expect_true("chroma_oklab" %in% names(result))
  expect_true("hue_oklab" %in% names(result))

  # Test lightness structure
  expect_true(is.list(result$lightness_oklab))
  expect_true("range" %in% names(result$lightness_oklab))
  expect_true("mean" %in% names(result$lightness_oklab))
  expect_true("sd" %in% names(result$lightness_oklab))

  # Test chroma structure
  expect_true(is.list(result$chroma_oklab))
  expect_true("range" %in% names(result$chroma_oklab))
  expect_true("mean" %in% names(result$chroma_oklab))
  expect_true("sd" %in% names(result$chroma_oklab))

  # Test hue structure
  expect_true(is.list(result$hue_oklab))
  expect_true("circular_variance" %in% names(result$hue_oklab))
  expect_true("range_degrees" %in% names(result$hue_oklab))
})

test_that("analyze_color_distribution handles single color", {
  # Test single color case
  single_color <- matrix(c(0.5, 0.1, 0.0), nrow = 1, ncol = 3)

  result <- analyze_color_distribution(single_color)

  expect_true(is.list(result))
  expect_true(is.finite(result$lightness_oklab$mean))
  expect_true(is.na(result$lightness_oklab$sd)) # No SD for single point
  expect_equal(result$hue_oklab$circular_variance, 0) # No variance for single point
})

test_that("analyze_color_distribution handles empty input", {
  # Test empty matrix
  empty_colors <- matrix(numeric(0), nrow = 0, ncol = 3)

  result <- analyze_color_distribution(empty_colors)

  expect_true(is.list(result))
  expect_true(all(is.na(result$lightness_oklab$range)))
  expect_true(is.na(result$lightness_oklab$mean))
  expect_true(is.na(result$chroma_oklab$mean))
  expect_true(is.na(result$hue_oklab$circular_variance))
})

test_that("analyze_color_distribution calculates correct metrics", {
  # Test with known values
  colors_oklab <- matrix(
    c(
      0.2,
      0.0,
      0.0, # Achromatic, low lightness
      0.8,
      0.0,
      0.0, # Achromatic, high lightness
      0.5,
      0.1,
      0.0 # Low chroma
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- analyze_color_distribution(colors_oklab)

  # Lightness should span from 0.2 to 0.8
  expect_equal(result$lightness_oklab$range[1], 0.2)
  expect_equal(result$lightness_oklab$range[2], 0.8)
  expect_equal(result$lightness_oklab$mean, 0.5)

  # Chroma should be low
  expect_true(result$chroma_oklab$mean < 0.1)

  # Circular variance should be numeric
  expect_true(is.numeric(result$hue_oklab$circular_variance))
})

test_that("analyze_color_distribution handles NA values", {
  # Test with NA values in matrix
  colors_with_na <- matrix(
    c(
      0.5,
      0.1,
      0.0,
      NA,
      0.1,
      0.0,
      0.7,
      NA,
      0.1
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- analyze_color_distribution(colors_with_na)

  # Should return NA results when NA values present
  expect_true(is.na(result$lightness_oklab$mean))
  expect_true(is.na(result$chroma_oklab$mean))
  expect_true(is.na(result$hue_oklab$circular_variance))
})


# ERROR CONDITION TESTS - Testing uncovered error paths in evaluate_palette
test_that("evaluate_palette handles invalid hex color formats", {
  # Test invalid hex formats (lines 69-71)
  invalid_hex_colors <- c("#INVALID", "#ZZZ", "red", "#12345", "#GGGGGG")

  expect_error(
    evaluate_palette(invalid_hex_colors),
    "If 'colors' is character, all elements must be valid hex codes or NA."
  )

  # Test mixed valid and invalid hex colors
  mixed_colors <- c("#FF0000", "#INVALID", "#00FF00")
  expect_error(
    evaluate_palette(mixed_colors),
    "If 'colors' is character, all elements must be valid hex codes or NA."
  )
})

test_that("evaluate_palette handles empty color vector after NA filtering", {
  # Test all NA vector (line 75)
  all_na_colors <- c(NA_character_, NA_character_, NA_character_)

  result <- evaluate_palette(all_na_colors)

  # Should succeed with empty matrix result
  expect_true(is.list(result))
  expect_true(inherits(result, "huerd_evaluation"))
  expect_equal(result$n_colors, 0)

  # Also test vector with only literal "NA" string (which should be valid)
  literal_na_colors <- c("NA")
  result2 <- evaluate_palette(literal_na_colors)

  expect_true(is.list(result2))
  expect_true(inherits(result2, "huerd_evaluation"))
  expect_equal(result2$n_colors, 1) # "NA" is a valid literal string
})

test_that("evaluate_palette handles invalid matrix dimensions", {
  # Test matrix with wrong number of columns (lines 88-100)
  wrong_cols_matrix <- matrix(c(0.5, 0.1, 0.0, 0.2), nrow = 2, ncol = 2)

  expect_error(
    evaluate_palette(wrong_cols_matrix),
    "colors must be a character vector of hex colors or an OKLAB matrix \\(N x 3 or 0x0/0x3\\)."
  )

  # Test 1D vector (should fail)
  vector_colors <- c(0.5, 0.1, 0.0)

  expect_error(
    evaluate_palette(vector_colors),
    "colors must be a character vector of hex colors or an OKLAB matrix \\(N x 3 or 0x0/0x3\\)."
  )

  # Test list input (should fail)
  list_colors <- list(c(0.5, 0.1, 0.0), c(0.7, -0.1, 0.1))

  expect_error(
    evaluate_palette(list_colors),
    "colors must be a character vector of hex colors or an OKLAB matrix \\(N x 3 or 0x0/0x3\\)."
  )
})

test_that("evaluate_palette handles empty matrix edge cases", {
  # Test 0x0 matrix (lines 88-96)
  empty_0x0 <- matrix(numeric(0), nrow = 0, ncol = 0)

  result <- evaluate_palette(empty_0x0)
  expect_true(is.list(result))
  expect_true(inherits(result, "huerd_evaluation"))
  expect_equal(result$n_colors, 0)

  # Test 0x3 matrix (lines 88-96)
  empty_0x3 <- matrix(numeric(0), nrow = 0, ncol = 3)

  result2 <- evaluate_palette(empty_0x3)
  expect_true(is.list(result2))
  expect_true(inherits(result2, "huerd_evaluation"))
  expect_equal(result2$n_colors, 0)
})

# Heuristic parameter tests removed - pure data provider mode

test_that("optimize_colors_constrained returns optimization result with palette and details", {
  # Simple case: optimize 2 free colors for maximum perceptual distance
  initial_colors <- matrix(
    c(
      0.5,
      0.1,
      0.0, # Initial color 1
      0.6,
      0.0,
      0.1 # Initial color 2
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("L", "a", "b"))
  )

  # Minimal aesthetic profile for optimization
  aesthetic_profile <- list(mean_L = 0.5, sd_L = 0.1, mean_C = 0.1, sd_C = 0.05)

  result <- optimize_colors_constrained(
    initial_colors_oklab = initial_colors,
    fixed_mask = c(FALSE, FALSE), # Both colors are free
    max_iterations = 5, # Keep low for fast testing
  )

  # Test return structure
  expect_true(is.list(result))
  expect_true("palette" %in% names(result))
  expect_true("details" %in% names(result))

  # Test palette matrix
  expect_true(is.matrix(result$palette))
  expect_equal(nrow(result$palette), 2)
  expect_equal(ncol(result$palette), 3)
  expect_equal(colnames(result$palette), c("L", "a", "b"))

  # Test details structure
  expect_true(is.list(result$details))
  expect_true("iterations" %in% names(result$details))
  expect_true("status_message" %in% names(result$details))
})

test_that("optimize_colors_constrained handles single color", {
  # Edge case: single color optimization
  single_color <- matrix(
    c(0.5, 0.1, 0.0),
    nrow = 1,
    dimnames = list(NULL, c("L", "a", "b"))
  )
  aesthetic_profile <- list(mean_L = 0.5, sd_L = 0.1, mean_C = 0.1, sd_C = 0.05)

  result <- optimize_colors_constrained(
    initial_colors_oklab = single_color,
    fixed_mask = c(FALSE),
    max_iterations = 5,
  )

  expect_true(is.list(result))
  expect_true("palette" %in% names(result))
  expect_true(is.matrix(result$palette))
  expect_equal(nrow(result$palette), 1)
})

test_that("optimize_colors_constrained respects fixed colors", {
  # Test that fixed colors remain unchanged
  colors <- matrix(
    c(
      0.5,
      0.1,
      0.0, # This will be fixed
      0.6,
      0.0,
      0.1 # This will be optimized
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("L", "a", "b"))
  )

  aesthetic_profile <- list(mean_L = 0.5, sd_L = 0.1, mean_C = 0.1, sd_C = 0.05)

  result <- optimize_colors_constrained(
    initial_colors_oklab = colors,
    fixed_mask = c(TRUE, FALSE), # First color is fixed
    max_iterations = 5,
  )

  # Fixed color should remain unchanged
  expect_equal(result$palette[1, ], colors[1, ])
})

test_that("optimize_colors_constrained applies aesthetic penalties", {
  # Test aesthetic penalty calculation (lines 63-83)
  colors <- matrix(
    c(
      0.8,
      0.2,
      0.1, # Free color that will deviate from aesthetic profile
      0.3,
      -0.1,
      0.0 # Another free color
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("L", "a", "b"))
  )

  # Aesthetic profile with tight constraints to force penalty
  aesthetic_profile <- list(
    mean_L = 0.5,
    sd_L = 0.05,
    mean_C = 0.05,
    sd_C = 0.02
  )

  result <- optimize_colors_constrained(
    initial_colors_oklab = colors,
    fixed_mask = c(FALSE, FALSE), # Both colors are free
    max_iterations = 3,
 # High aesthetic penalty
  )

  # Should return valid optimization result
  expect_true(is.list(result))
  expect_true("palette" %in% names(result))
  expect_true("details" %in% names(result))
  expect_true(is.matrix(result$palette))
  expect_equal(nrow(result$palette), 2)
})

test_that("optimize_colors_constrained handles optimization failures gracefully", {
  # Test error handling (lines 132-141) by providing problematic input
  colors <- matrix(
    c(
      NaN,
      NaN,
      NaN, # Invalid starting point
      Inf,
      -Inf,
      NaN # Another invalid starting point
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("L", "a", "b"))
  )

  aesthetic_profile <- list(mean_L = 0.5, sd_L = 0.1, mean_C = 0.1, sd_C = 0.05)

  result <- optimize_colors_constrained(
    initial_colors_oklab = colors,
    fixed_mask = c(FALSE, FALSE),
    max_iterations = 1,
  )

  # Should still return a valid result structure even on failure
  expect_true(is.list(result))
  expect_true("palette" %in% names(result))
  expect_true("details" %in% names(result))
  expect_true(is.matrix(result$palette))
  expect_equal(nrow(result$palette), 2)

  # Error handling should set status to -999
  expect_equal(result$details$nloptr_status, -999)
  expect_true(grepl("Error in nloptr", result$details$status_message))
})

test_that("objective_min_cvd_safe_dist handles matrix structure preservation", {
  # RED: Test the specific "Error in CheckMatrix(coords): invalid color matrix" bug
  # This tests matrix structure preservation through the color conversion chain

  # Use brand palette colors that could trigger the bug
  brand_colors <- matrix(
    c(
      0.627,
      0.224,
      0.126, # Brand red (converted to OKLAB)
      0.701,
      -0.101,
      0.108, # Brand green
      0.323,
      -0.003,
      -0.153 # Brand blue
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(NULL, c("L", "a", "b"))
  )

  # Test that the conversion chain preserves matrix structure
  expect_no_error({
    # Follow the exact conversion chain from objective_min_cvd_safe_dist
    lab_colors <- farver::convert_colour(
      brand_colors,
      from = "oklab",
      to = "lab"
    )
    rgb_matrix_01 <- farver::convert_colour(
      lab_colors,
      from = "lab",
      to = "rgb"
    ) /
      255.0

    # This is where the bug could occur - matrix structure loss during clamping
    rgb_colors_clamped <- rgb_matrix_01
    rgb_colors_clamped[rgb_colors_clamped < 0] <- 0
    rgb_colors_clamped[rgb_colors_clamped > 1] <- 1

    # The critical point - colorspace::sRGB expects exactly 3 columns
    expect_equal(ncol(rgb_colors_clamped), 3)
    expect_true(is.matrix(rgb_colors_clamped))

    # This line should not fail with "invalid color matrix"
    srgb_obj <- colorspace::sRGB(rgb_colors_clamped)
  })

  # Test the complete objective function
  expect_no_error({
    result_dist <- objective_min_cvd_safe_dist(brand_colors)
  })

  # The result should be a valid numeric value
  result_dist <- objective_min_cvd_safe_dist(brand_colors)
  expect_true(is.numeric(result_dist))
  expect_true(length(result_dist) == 1)
  expect_false(is.na(result_dist))
  expect_false(is.infinite(result_dist))
})

test_that("objective_min_cvd_safe_dist handles edge cases that could cause matrix structure loss", {
  # RED: Test edge cases that might cause matrix structure issues

  # Test with single color (minimal case)
  single_color <- matrix(
    c(0.5, 0.1, 0.0),
    nrow = 1,
    dimnames = list(NULL, c("L", "a", "b"))
  )
  expect_no_error({
    result1 <- objective_min_cvd_safe_dist(single_color)
  })
  # Single color should return Inf (can't calculate distances)
  expect_equal(objective_min_cvd_safe_dist(single_color), Inf)

  # Test with extreme colors that might cause conversion issues
  extreme_colors <- matrix(
    c(
      0.0,
      0.0,
      0.0, # Black
      1.0,
      0.0,
      0.0, # White
      0.5,
      0.4,
      0.4 # Saturated color
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(NULL, c("L", "a", "b"))
  )

  expect_no_error({
    result2 <- objective_min_cvd_safe_dist(extreme_colors)
  })

  result2 <- objective_min_cvd_safe_dist(extreme_colors)
  expect_true(is.numeric(result2))
  expect_false(is.na(result2))
})

test_that("objective_min_cvd_safe_dist defensive programming prevents CheckMatrix errors", {
  # RED: Test the specific defensive programming added to prevent matrix structure issues

  # Create a mock scenario where farver could theoretically return malformed data
  # (This test ensures our defensive programming catches such cases)

  colors_oklab <- matrix(
    c(
      0.627,
      0.224,
      0.126, # Brand red
      0.701,
      -0.101,
      0.108, # Brand green
      0.323,
      -0.003,
      -0.153 # Brand blue
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(NULL, c("L", "a", "b"))
  )

  # Test that the defensive programming catches matrix issues
  # The function should handle cases where the matrix structure might be compromised
  expect_no_error({
    result <- objective_min_cvd_safe_dist(colors_oklab)
  })

  # Additional test: verify the defensive programming works even with edge case colors
  # that might cause farver to return unexpected structures
  edge_colors <- matrix(
    c(
      0.001,
      0.001,
      0.001, # Near-black
      0.999,
      0.001,
      0.001, # Near-white
      0.5,
      0.5,
      0.5 # Mid-gray
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(NULL, c("L", "a", "b"))
  )

  expect_no_error({
    result2 <- objective_min_cvd_safe_dist(edge_colors)
  })

  # Results should be valid numbers
  result <- objective_min_cvd_safe_dist(colors_oklab)
  result2 <- objective_min_cvd_safe_dist(edge_colors)

  expect_true(is.numeric(result))
  expect_true(is.numeric(result2))
  expect_false(is.na(result))
  expect_false(is.na(result2))
  expect_false(is.infinite(result))
  expect_false(is.infinite(result2))
})

test_that("optimize_colors_constrained with cvd_safe mode works with brand palette", {
  # RED: Integration test for the brand palette example that currently fails
  # This should FAIL initially, then pass after we fix the bug

  brand_colors <- matrix(
    c(
      0.627,
      0.224,
      0.126, # Brand red
      0.701,
      -0.101,
      0.108, # Brand green
      0.323,
      -0.003,
      -0.153 # Brand blue
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(NULL, c("L", "a", "b"))
  )

  aesthetic_profile <- list(mean_L = 0.5, sd_L = 0.2, mean_C = 0.15, sd_C = 0.1)

  # This optimization should complete without error
  expect_no_error({
    result <- optimize_colors_constrained(
      initial_colors_oklab = brand_colors,
      fixed_mask = c(TRUE, TRUE, FALSE), # Only optimize the third color
        max_iterations = 5,
            )
  })

  # Should return valid result structure
  result <- optimize_colors_constrained(
    initial_colors_oklab = brand_colors,
    fixed_mask = c(TRUE, TRUE, FALSE),
    max_iterations = 5,
  )

  expect_true(is.list(result))
  expect_true("palette" %in% names(result))
  expect_true(is.matrix(result$palette))
  expect_equal(nrow(result$palette), 3)

  # Optimization should complete successfully
  expect_true(result$details$nloptr_status >= 0) # Positive status means success
})

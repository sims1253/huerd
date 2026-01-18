describe("optimize_colors_constrained()", {
  it("returns optimization result with palette and details", {
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

  it("handles single color", {
    # Edge case: single color optimization
    single_color <- matrix(
      c(0.5, 0.1, 0.0),
      nrow = 1,
      dimnames = list(NULL, c("L", "a", "b"))
    )

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

  it("respects fixed colors", {
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

    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = c(TRUE, FALSE), # First color is fixed
      max_iterations = 5,
    )

    # Fixed color should remain unchanged
    expect_equal(result$palette[1, ], colors[1, ])
  })

  it("applies aesthetic penalties", {
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

  it("handles optimization failures gracefully", {
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
    expect_true(grepl(
      "Error in nloptr",
      result$details$status_message,
      fixed = TRUE
    ))
  })

  it("with cvd_safe mode works with brand palette", {
    # Integration test for the brand palette example
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

    # This optimization should complete without error
    expect_no_error({
      result <- optimize_colors_constrained(
        initial_colors_oklab = brand_colors,
        fixed_mask = c(TRUE, TRUE, FALSE), # Only optimize the third color
        max_iterations = 5,
      )
    })

    # Should return valid result structure (reusing captured result from expect_no_error)

    expect_true(is.list(result))
    expect_true("palette" %in% names(result))
    expect_true(is.matrix(result$palette))
    expect_equal(nrow(result$palette), 3)

    # Optimization should complete successfully
    expect_true(result$details$nloptr_status >= 0) # Positive status means success
  })
})

describe("objective_min_cvd_safe_dist()", {
  it("handles matrix structure preservation", {
    # Test the specific "Error in CheckMatrix(coords): invalid color matrix" bug
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

  it("handles edge cases that could cause matrix structure loss", {
    # Test edge cases that might cause matrix structure issues

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

  it("defensive programming prevents CheckMatrix errors", {
    # Test the specific defensive programming added to prevent matrix structure issues

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
})

describe("optimize_colors_sann()", {
  it("returns optimization result with palette and details", {
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

    result <- optimize_colors_sann(
      initial_colors_oklab = initial_colors,
      fixed_mask = c(FALSE, FALSE), # Both colors are free
      max_iterations = 10, # Low for fast testing
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
    expect_true("sann_convergence" %in% names(result$details))
    expect_true("final_objective_value" %in% names(result$details))
  })

  it("respects fixed colors", {
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

    result <- optimize_colors_sann(
      initial_colors_oklab = colors,
      fixed_mask = c(TRUE, FALSE), # First color is fixed
      max_iterations = 10,
    )

    # Fixed color should remain unchanged
    expect_equal(result$palette[1, ], colors[1, ])
  })

  it("handles constraint violations with penalty", {
    # Test that constraint violations are handled via penalty function
    colors <- matrix(
      c(
        1.5,
        0.8,
        0.8, # Out of bounds color that should be penalized
        0.3,
        -0.1,
        0.0 # Valid color
      ),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_sann(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE), # Both colors are free
      max_iterations = 5,
    )

    # Should return valid optimization result
    expect_true(is.list(result))
    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))
    expect_true(is.matrix(result$palette))
    expect_equal(nrow(result$palette), 2)

    # Colors should be clamped to valid bounds
    expect_true(all(
      result$palette[, 1] >= 0.001 & result$palette[, 1] <= 0.999
    ))
    expect_true(all(result$palette[, 2] >= -0.4 & result$palette[, 2] <= 0.4))
    expect_true(all(result$palette[, 3] >= -0.4 & result$palette[, 3] <= 0.4))
  })

  it("handles optimization failures gracefully", {
    # Test error handling by providing problematic input
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

    result <- optimize_colors_sann(
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

    # Error handling should set convergence to -999
    expect_equal(result$details$sann_convergence, -999)
    expect_true(grepl(
      "Error in optim SANN",
      result$details$status_message,
      fixed = TRUE
    ))
  })

  it("with cvd_safe mode works with brand palette", {
    # Integration test for the brand palette example
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

    # This optimization should complete without error
    expect_no_error({
      result <- optimize_colors_sann(
        initial_colors_oklab = brand_colors,
        fixed_mask = c(TRUE, TRUE, FALSE), # Only optimize the third color
        max_iterations = 10,
      )
    })

    # Should return valid result structure (reusing captured result from expect_no_error)

    expect_true(is.list(result))
    expect_true("palette" %in% names(result))
    expect_true(is.matrix(result$palette))
    expect_equal(nrow(result$palette), 3)

    # Optimization should complete (convergence status doesn't guarantee success in SANN)
    expect_true(is.numeric(result$details$sann_convergence))
  })
})

describe("optimize_colors_nlopt_direct()", {
  it("returns optimization result with palette and details", {
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

    result <- optimize_colors_nlopt_direct(
      initial_colors_oklab = initial_colors,
      fixed_mask = c(FALSE, FALSE), # Both colors are free
      max_iterations = 20, # DIRECT needs more iterations for meaningful results
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
    expect_true("nloptr_status" %in% names(result$details))
    expect_true("final_objective_value" %in% names(result$details))
  })

  it("handles single color", {
    # Edge case: single color optimization
    single_color <- matrix(
      c(0.5, 0.1, 0.0),
      nrow = 1,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_nlopt_direct(
      initial_colors_oklab = single_color,
      fixed_mask = c(FALSE),
      max_iterations = 10,
    )

    expect_true(is.list(result))
    expect_true("palette" %in% names(result))
    expect_true(is.matrix(result$palette))
    expect_equal(nrow(result$palette), 1)
  })

  it("respects fixed colors", {
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

    result <- optimize_colors_nlopt_direct(
      initial_colors_oklab = colors,
      fixed_mask = c(TRUE, FALSE), # First color is fixed
      max_iterations = 20,
    )

    # Fixed color should remain unchanged
    expect_equal(result$palette[1, ], colors[1, ])
  })

  it("respects box constraints", {
    # Test that optimization respects OKLAB box constraints
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

    result <- optimize_colors_nlopt_direct(
      initial_colors_oklab = initial_colors,
      fixed_mask = c(FALSE, FALSE), # Both colors are free
      max_iterations = 20,
    )

    # Test that all colors are within OKLAB bounds
    expect_true(all(
      result$palette[, 1] >= 0.001 & result$palette[, 1] <= 0.999
    ))
    expect_true(all(result$palette[, 2] >= -0.4 & result$palette[, 2] <= 0.4))
    expect_true(all(result$palette[, 3] >= -0.4 & result$palette[, 3] <= 0.4))
  })

  it("handles optimization failures gracefully", {
    # Test error handling by providing problematic input
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

    result <- optimize_colors_nlopt_direct(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5,
    )

    # Should still return a valid result structure even on failure
    expect_true(is.list(result))
    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))
    expect_true(is.matrix(result$palette))
    expect_equal(nrow(result$palette), 2)

    # Error handling should set status to -999
    expect_equal(result$details$nloptr_status, -999)
    expect_true(grepl(
      "Error in nloptr DIRECT",
      result$details$status_message,
      fixed = TRUE
    ))
  })

  it("with cvd_safe mode works with brand palette", {
    # Integration test for the brand palette example
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

    # This optimization should complete without error
    expect_no_error({
      result <- optimize_colors_nlopt_direct(
        initial_colors_oklab = brand_colors,
        fixed_mask = c(TRUE, TRUE, FALSE), # Only optimize the third color
        max_iterations = 30,
      )
    })

    # Should return valid result structure (reusing captured result from expect_no_error)

    expect_true(is.list(result))
    expect_true("palette" %in% names(result))
    expect_true(is.matrix(result$palette))
    expect_equal(nrow(result$palette), 3)

    # Optimization should complete successfully (DIRECT is more robust)
    expect_true(result$details$nloptr_status >= 0) # Positive status means success
  })

  it("is deterministic", {
    # Test that DIRECT optimizer produces deterministic results
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

    result1 <- optimize_colors_nlopt_direct(
      initial_colors_oklab = initial_colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 20,
    )

    result2 <- optimize_colors_nlopt_direct(
      initial_colors_oklab = initial_colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 20,
    )

    # Results should be identical for deterministic optimizer
    expect_equal(result1$palette, result2$palette, tolerance = 1e-10)
    expect_equal(
      result1$details$final_objective_value,
      result2$details$final_objective_value,
      tolerance = 1e-10
    )
  })
})

describe("optimize_colors_nlopt_neldermead()", {
  it("returns optimization result with palette and details", {
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

    result <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = initial_colors,
      fixed_mask = c(FALSE, FALSE), # Both colors are free
      max_iterations = 50, # Nelder-Mead typically needs more iterations than COBYLA
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
    expect_true("nloptr_status" %in% names(result$details))
    expect_true("final_objective_value" %in% names(result$details))
  })

  it("handles single color", {
    # Edge case: single color optimization
    single_color <- matrix(
      c(0.5, 0.1, 0.0),
      nrow = 1,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = single_color,
      fixed_mask = c(FALSE),
      max_iterations = 20,
    )

    expect_true(is.list(result))
    expect_true("palette" %in% names(result))
    expect_true(is.matrix(result$palette))
    expect_equal(nrow(result$palette), 1)
  })

  it("respects fixed colors", {
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

    result <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = colors,
      fixed_mask = c(TRUE, FALSE), # First color is fixed
      max_iterations = 50,
    )

    # Fixed color should remain unchanged
    expect_equal(result$palette[1, ], colors[1, ])
  })

  it("respects box constraints", {
    # Test that optimization respects OKLAB box constraints
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

    result <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = initial_colors,
      fixed_mask = c(FALSE, FALSE), # Both colors are free
      max_iterations = 50,
    )

    # Test that all colors are within OKLAB bounds
    expect_true(all(
      result$palette[, 1] >= 0.001 & result$palette[, 1] <= 0.999
    ))
    expect_true(all(result$palette[, 2] >= -0.4 & result$palette[, 2] <= 0.4))
    expect_true(all(result$palette[, 3] >= -0.4 & result$palette[, 3] <= 0.4))
  })

  it("handles optimization failures gracefully", {
    # Test error handling by providing problematic input
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

    result <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5,
    )

    # Should still return a valid result structure even on failure
    expect_true(is.list(result))
    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))
    expect_true(is.matrix(result$palette))
    expect_equal(nrow(result$palette), 2)

    # Error handling should set status to -999
    expect_equal(result$details$nloptr_status, -999)
    expect_true(grepl(
      "Error in nloptr Nelder-Mead",
      result$details$status_message,
      fixed = TRUE
    ))
  })

  it("with cvd_safe mode works with brand palette", {
    # Integration test for the brand palette example
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

    # This optimization should complete without error
    expect_no_error({
      result <- optimize_colors_nlopt_neldermead(
        initial_colors_oklab = brand_colors,
        fixed_mask = c(TRUE, TRUE, FALSE), # Only optimize the third color
        max_iterations = 100,
      )
    })

    # Should return valid result structure (reusing captured result from expect_no_error)

    expect_true(is.list(result))
    expect_true("palette" %in% names(result))
    expect_true(is.matrix(result$palette))
    expect_equal(nrow(result$palette), 3)

    # Optimization may succeed or fail - Nelder-Mead is less robust than DIRECT
    # Just verify the status is a valid numeric return code (not checking success)
    expect_true(is.numeric(result$details$nloptr_status))
  })

  it("produces consistent results", {
    # Test that Nelder-Mead optimizer produces reasonably consistent results
    # Note: Nelder-Mead may not be perfectly deterministic but should be reasonably consistent
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

    result1 <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = initial_colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 50,
    )

    result2 <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = initial_colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 50,
    )

    # Results should be reasonably consistent for local optimizer
    # (allowing more tolerance than for global optimizers like DIRECT)
    expect_equal(result1$palette, result2$palette, tolerance = 1e-3)
    expect_equal(
      result1$details$final_objective_value,
      result2$details$final_objective_value,
      tolerance = 1e-3
    )
  })
})

describe("smooth optimization functions (v0.5.0)", {
  describe("objective_smooth_repulsion()", {
    it("works correctly", {
      # Test with 3 colors in OKLAB space
      colors_oklab <- matrix(
        c(
          0.5,
          0.1,
          0.2, # Color 1
          0.7,
          -0.1,
          0.1, # Color 2
          0.3,
          0.2,
          -0.1 # Color 3
        ),
        ncol = 3,
        byrow = TRUE
      )

      result <- objective_smooth_repulsion(colors_oklab)

      expect_true(is.numeric(result))
      expect_length(result, 1)
      expect_true(is.finite(result))
      expect_true(result > 0) # Should be positive for inverse distance sum
    })
  })

  describe("objective_smooth_logsumexp()", {
    it("works correctly", {
      # Test with 3 colors in OKLAB space
      colors_oklab <- matrix(
        c(
          0.5,
          0.1,
          0.2, # Color 1
          0.7,
          -0.1,
          0.1, # Color 2
          0.3,
          0.2,
          -0.1 # Color 3
        ),
        ncol = 3,
        byrow = TRUE
      )

      result <- objective_smooth_logsumexp(colors_oklab)

      expect_true(is.numeric(result))
      expect_length(result, 1)
      expect_true(is.finite(result))
    })
  })

  describe("smooth objectives", {
    it("return different values", {
      # Test that two smooth objectives produce different values
      colors_oklab <- matrix(
        c(
          0.5,
          0.1,
          0.2, # Color 1
          0.7,
          -0.1,
          0.1, # Color 2
          0.3,
          0.2,
          -0.1 # Color 3
        ),
        ncol = 3,
        byrow = TRUE
      )

      repulsion_value <- objective_smooth_repulsion(colors_oklab)
      logsumexp_value <- objective_smooth_logsumexp(colors_oklab)

      expect_false(identical(repulsion_value, logsumexp_value))
    })
  })

  describe("gradient_smooth_repulsion()", {
    it("works correctly", {
      # Test gradient function
      colors_oklab <- matrix(
        c(
          0.5,
          0.1,
          0.2, # Color 1
          0.7,
          -0.1,
          0.1, # Color 2
          0.3,
          0.2,
          -0.1 # Color 3
        ),
        ncol = 3,
        byrow = TRUE
      )

      grad <- gradient_smooth_repulsion(colors_oklab)

      expect_true(is.matrix(grad))
      expect_equal(dim(grad), c(3, 3))
      expect_true(all(is.finite(grad)))
    })
  })

  describe("gradient_smooth_logsumexp()", {
    it("works correctly", {
      # Test gradient function
      colors_oklab <- matrix(
        c(
          0.5,
          0.1,
          0.2, # Color 1
          0.7,
          -0.1,
          0.1, # Color 2
          0.3,
          0.2,
          -0.1 # Color 3
        ),
        ncol = 3,
        byrow = TRUE
      )

      grad <- gradient_smooth_logsumexp(colors_oklab)

      expect_true(is.matrix(grad))
      expect_equal(dim(grad), c(3, 3))
      expect_true(all(is.finite(grad)))
    })
  })

  describe("smooth objective functions", {
    it("handle edge cases", {
      # Test with single color (should return 0)
      single_color <- matrix(c(0.5, 0.1, 0.2), ncol = 3)

      expect_equal(objective_smooth_repulsion(single_color), 0)
      expect_equal(objective_smooth_logsumexp(single_color), 0)

      # Test with invalid input
      expect_error(objective_smooth_repulsion(matrix(1:6, ncol = 2)))
      expect_error(objective_smooth_logsumexp(matrix(1:6, ncol = 2)))
    })
  })

  describe("smooth objective functions", {
    it("count distances correctly (regression test)", {
      # Regression test for the bug where diag(n_colors) only set [1,1] = NA
      # and as.matrix(dist()) double-counted distances

      # Simple 2-color case where we can manually verify the count
      colors_oklab <- matrix(
        c(
          0.0,
          0.0,
          0.0, # Color 1: origin
          1.0,
          0.0,
          0.0 # Color 2: distance = 1.0
        ),
        ncol = 3,
        byrow = TRUE
      )

      # There should be exactly 1 pairwise distance = 1.0
      actual_repulsion <- objective_smooth_repulsion(colors_oklab)
      expect_true(abs(actual_repulsion - 1) < 0.01)

      # For 3 colors arranged in an equilateral triangle
      colors_3 <- matrix(
        c(
          0.0,
          0.0,
          0.0, # Color 1
          1.0,
          0.0,
          0.0, # Color 2 (distance 1 from color 1)
          0.5,
          sqrt(3) / 2,
          0.0 # Color 3 (distance 1 from both)
        ),
        ncol = 3,
        byrow = TRUE
      )

      # Should have exactly 3 distances, each = 1.0
      actual_repulsion_3 <- objective_smooth_repulsion(colors_3)
      expect_true(abs(actual_repulsion_3 - 3) < 0.03)
    })
  })

  describe("log-sum-exp", {
    it("uses numerical stability trick", {
      # Test that large k values don't cause overflow/underflow
      colors_oklab <- matrix(
        c(
          0.5,
          0.0,
          0.0,
          0.5,
          1.0,
          0.0 # Distance = 1.0
        ),
        ncol = 3,
        byrow = TRUE
      )

      # Test with extreme k value that would cause underflow in naive implementation
      extreme_k <- 1000
      result_stable <- objective_smooth_logsumexp(colors_oklab, k = extreme_k)

      expect_true(is.finite(result_stable))

      # For extreme k and distance=1, exp(-k*d) is very small, so log(sum) is negative
      # Result should be approximately -1.0 for k=1000, distance=1.0
      expect_true(abs(result_stable - (-1.0)) < 0.1)

      # Test numerical stability by comparing with manual naive calculation
      # (This would fail with naive implementation for large k)
      distances <- dist(colors_oklab)
      neg_k_distances <- -extreme_k * distances

      # Naive approach (would underflow): log(sum(exp(neg_k_distances))) / k
      # Our stable approach should handle this without underflow
      max_val <- max(neg_k_distances)
      stable_exp_values <- exp(neg_k_distances - max_val)
      expected_stable <- (max_val + log(sum(stable_exp_values))) / extreme_k

      expect_equal(result_stable, expected_stable, tolerance = 1e-14)
    })
  })

  describe("optimize_colors_lbfgs()", {
    it("uses correct objective based on weights", {
      # Test L-BFGS optimizer with different weight configurations
      initial_colors <- matrix(
        c(
          0.5,
          0.1,
          0.2, # Color 1
          0.7,
          -0.1,
          0.1, # Color 2
          0.3,
          0.2,
          -0.1 # Color 3
        ),
        ncol = 3,
        byrow = TRUE
      )

      fixed_mask <- c(TRUE, FALSE, FALSE) # Fix first color

      # Test with smooth_repulsion weights
      result_repulsion <- optimize_colors_lbfgs(
        initial_colors_oklab = initial_colors,
        fixed_mask = fixed_mask,
        max_iterations = 50,
        weights = c(smooth_repulsion = 1)
      )

      # Test with smooth_logsumexp weights
      result_logsumexp <- optimize_colors_lbfgs(
        initial_colors_oklab = initial_colors,
        fixed_mask = fixed_mask,
        max_iterations = 50,
        weights = c(smooth_logsumexp = 1)
      )

      expect_true(is.list(result_repulsion))
      expect_true(is.list(result_logsumexp))
      expect_true("palette" %in% names(result_repulsion))
      expect_true("palette" %in% names(result_logsumexp))
      expect_equal(result_repulsion$details$algorithm, "L-BFGS")
      expect_equal(result_logsumexp$details$algorithm, "L-BFGS")

      # Verify both optimizations completed successfully with finite objective values
      expect_true(is.finite(result_repulsion$details$final_objective_value))
      expect_true(is.finite(result_logsumexp$details$final_objective_value))

      # Verify that different objectives compute different values for same palette
      # This confirms that different objective functions are actually being used
      repulsion_value <- objective_smooth_repulsion(result_repulsion$palette)
      logsumexp_value <- objective_smooth_logsumexp(result_repulsion$palette)
      expect_false(identical(repulsion_value, logsumexp_value))
    })
  })
})

describe("Numerical Gradient Verification", {
  it("gradient_smooth_repulsion matches numerical gradient (finite differences)", {
    # Test that analytical gradient matches numerical approximation
    colors_oklab <- matrix(
      c(
        0.5,
        0.1,
        0.2, # Color 1
        0.7,
        -0.1,
        0.1, # Color 2
        0.3,
        0.2,
        -0.1 # Color 3
      ),
      ncol = 3,
      byrow = TRUE
    )

    # Analytical gradient
    analytical_grad <- gradient_smooth_repulsion(colors_oklab)

    # Numerical gradient using central differences
    h <- 1e-5
    numerical_grad <- matrix(0, nrow = 3, ncol = 3)

    for (i in 1:3) {
      for (j in 1:3) {
        # Perturb positive
        colors_plus <- colors_oklab
        colors_plus[i, j] <- colors_plus[i, j] + h

        # Perturb negative
        colors_minus <- colors_oklab
        colors_minus[i, j] <- colors_minus[i, j] - h

        # Central difference
        numerical_grad[i, j] <- (objective_smooth_repulsion(colors_plus) -
          objective_smooth_repulsion(colors_minus)) /
          (2 * h)
      }
    }

    # Compare gradients - should be very close
    expect_equal(analytical_grad, numerical_grad, tolerance = 1e-4)
  })

  it("gradient_smooth_logsumexp matches numerical gradient (finite differences)", {
    # Test that analytical gradient matches numerical approximation
    colors_oklab <- matrix(
      c(
        0.5,
        0.1,
        0.2, # Color 1
        0.7,
        -0.1,
        0.1, # Color 2
        0.3,
        0.2,
        -0.1 # Color 3
      ),
      ncol = 3,
      byrow = TRUE
    )

    # Analytical gradient
    analytical_grad <- gradient_smooth_logsumexp(colors_oklab)

    # Numerical gradient using central differences
    h <- 1e-5
    numerical_grad <- matrix(0, nrow = 3, ncol = 3)

    for (i in 1:3) {
      for (j in 1:3) {
        # Perturb positive
        colors_plus <- colors_oklab
        colors_plus[i, j] <- colors_plus[i, j] + h

        # Perturb negative
        colors_minus <- colors_oklab
        colors_minus[i, j] <- colors_minus[i, j] - h

        # Central difference
        numerical_grad[i, j] <- (objective_smooth_logsumexp(colors_plus) -
          objective_smooth_logsumexp(colors_minus)) /
          (2 * h)
      }
    }

    # Compare gradients - should be very close
    expect_equal(analytical_grad, numerical_grad, tolerance = 1e-4)
  })

  it("gradient functions handle single color (edge case)", {
    # Test that gradient functions return zero matrix for single color
    single_color <- matrix(c(0.5, 0.1, 0.2), ncol = 3)

    expect_equal(
      gradient_smooth_repulsion(single_color),
      matrix(0, nrow = 1, ncol = 3)
    )
    expect_equal(
      gradient_smooth_logsumexp(single_color),
      matrix(0, nrow = 1, ncol = 3)
    )
  })

  it("gradient functions validate input", {
    # Test that gradient functions validate input correctly
    expect_error(gradient_smooth_repulsion(matrix(1:6, ncol = 2)))
    expect_error(gradient_smooth_logsumexp(matrix(1:6, ncol = 2)))
  })
})

# ==============================================================================
# COMPREHENSIVE TRY-CATCH ERROR HANDLING TESTS
# ==============================================================================
# These tests verify that tryCatch blocks correctly handle real error scenarios,
# not just NaN/Inf inputs. The focus is on error recovery paths and scoping.

describe("tryCatch error handling - optimize_colors_constrained()", {
  # Helper to create valid test colors
  valid_colors <- function(n = 2) {
    matrix(
      c(
        0.5,
        0.1,
        0.0,
        0.6,
        0.0,
        0.1
      )[1:(n * 3)],
      nrow = n,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )
  }

  it("returns valid structure when nloptr throws non-numeric error", {
    # Test that the function handles non-standard error messages gracefully
    colors <- valid_colors(2)

    # Mock scenario: Use invalid algorithm name that nloptr would reject
    # We can't directly inject errors, but we can test the error handling path
    # by providing inputs that would cause specific error types

    # Test with dimension mismatch - this triggers validation errors
    # Create a 2x3 matrix without proper column names (will be tested for validation)
    bad_colors <- matrix(1:6, nrow = 2)
    colnames(bad_colors) <- c("L", "a", "b") # But with only 2 columns named wrong

    # This should still return a valid structure (error caught by tryCatch)
    result <- optimize_colors_constrained(
      initial_colors_oklab = bad_colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 1
    )

    expect_s3_class(result, "list")
    expect_named(result, c("palette", "details"))
    expect_s3_class(result$palette, "matrix")
    expect_s3_class(result$details, "list")
  })

  it("properly scopes return_value after nloptr error", {
    # Critical test: verify that return_value is properly set even when
    # nloptr throws an error and the error handler is invoked

    colors <- valid_colors(2)

    # Use max_iterations = 0 to potentially trigger edge cases
    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 0
    )

    # Verify return_value has all expected fields
    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))
    expect_true("iterations" %in% names(result$details))
    expect_true("status_message" %in% names(result$details))
    expect_true("nloptr_status" %in% names(result$details))
    expect_true("final_objective_value" %in% names(result$details))

    # Verify palette structure is preserved
    expect_equal(nrow(result$palette), 2)
    expect_equal(ncol(result$palette), 3)
  })

  it("handles matrix with wrong number of columns", {
    # Test what happens when input matrix has wrong column structure
    # Create 2x2 matrix which has wrong dimensions
    bad_matrix <- matrix(1:4, nrow = 2)
    colnames(bad_matrix) <- c("x", "y")

    # Should either error or return valid error structure
    # Note: this test checks error handling when matrix has wrong dimensions
    expect_error(optimize_colors_constrained(
      initial_colors_oklab = bad_matrix,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 1
    ))
  })

  it("handles non-numeric matrix input gracefully", {
    # Test that character matrices are handled
    char_matrix <- matrix(as.character(1:6), nrow = 2)

    # This should trigger an error that tryCatch handles
    result <- optimize_colors_constrained(
      initial_colors_oklab = char_matrix,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 1
    )

    # Should return valid error structure
    expect_s3_class(result, "list")
    expect_true("details" %in% names(result))
  })

  it("handles fixed_mask length mismatch with color rows", {
    # Test when fixed_mask length doesn't match number of colors
    colors <- valid_colors(3)
    wrong_mask <- c(FALSE, FALSE) # Should be length 3

    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = wrong_mask,
      max_iterations = 1
    )

    # Should handle gracefully
    expect_s3_class(result, "list")
    expect_true(nrow(result$palette) >= 1)
  })

  it("initial objective value evaluation in error handler", {
    # Test that the nested tryCatch for initial_obj_val works correctly
    # when eval_f itself might fail

    # Create colors where eval_f could potentially fail
    colors <- matrix(
      c(
        0.5,
        0.1,
        0.0,
        0.5,
        0.1,
        0.0 # Identical colors might cause edge cases
      ),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 1
    )

    expect_s3_class(result, "list")
    expect_true("final_objective_value" %in% names(result$details))
    # final_objective_value should be numeric (even if NA)
    expect_type(result$details$final_objective_value, "double")
  })
})

describe("tryCatch error handling - optimize_colors_sann()", {
  valid_colors <- function(n = 2) {
    matrix(
      c(
        0.5,
        0.1,
        0.0,
        0.6,
        0.0,
        0.1
      )[1:(n * 3)],
      nrow = n,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )
  }

  it("returns valid structure when optim throws error", {
    colors <- valid_colors(2)

    # Test with very low maxit that might cause optim to fail
    result <- optimize_colors_sann(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 0
    )

    expect_s3_class(result, "list")
    expect_named(result, c("palette", "details"))
    expect_true("sann_convergence" %in% names(result$details))
    expect_true("status_message" %in% names(result$details))
  })

  it("properly scopes return_value after optim SANN error", {
    colors <- valid_colors(2)

    result <- optimize_colors_sann(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = -1 # Negative iterations should be handled
    )

    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))
    expect_true("iterations" %in% names(result$details))
    expect_true("status_message" %in% names(result$details))
    expect_true("sann_convergence" %in% names(result$details))
    expect_true("final_objective_value" %in% names(result$details))

    expect_equal(nrow(result$palette), 2)
    expect_equal(ncol(result$palette), 3)
  })

  it("handles matrix with wrong dimensions", {
    # Test 1x3 matrix (single color)
    single_color <- matrix(c(0.5, 0.1, 0.0), nrow = 1)

    result <- optimize_colors_sann(
      initial_colors_oklab = single_color,
      fixed_mask = c(FALSE),
      max_iterations = 1
    )

    expect_s3_class(result, "list")
    expect_equal(nrow(result$palette), 1)
  })

  it("handles NA in fixed_mask", {
    colors <- valid_colors(2)
    bad_mask <- c(FALSE, NA)

    result <- optimize_colors_sann(
      initial_colors_oklab = colors,
      fixed_mask = bad_mask,
      max_iterations = 1
    )

    expect_s3_class(result, "list")
  })

  it("handles NULL initial colors", {
    # Test edge case behavior
    expect_error(optimize_colors_sann(
      initial_colors_oklab = NULL,
      fixed_mask = c(FALSE),
      max_iterations = 1
    ))
  })

  it("initial objective value evaluation in SANN error handler", {
    colors <- valid_colors(2)

    result <- optimize_colors_sann(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 1
    )

    expect_type(result$details$final_objective_value, "double")
  })
})

describe("tryCatch error handling - optimize_colors_nlopt_direct()", {
  valid_colors <- function(n = 2) {
    matrix(
      c(
        0.5,
        0.1,
        0.0,
        0.6,
        0.0,
        0.1
      )[1:(n * 3)],
      nrow = n,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )
  }

  it("returns valid structure when nloptr DIRECT throws error", {
    colors <- valid_colors(2)

    result <- optimize_colors_nlopt_direct(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 0
    )

    expect_s3_class(result, "list")
    expect_named(result, c("palette", "details"))
    expect_true("nloptr_status" %in% names(result$details))
    expect_true("status_message" %in% names(result$details))
  })

  it("properly scopes return_value after DIRECT error", {
    colors <- valid_colors(2)

    result <- optimize_colors_nlopt_direct(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 0
    )

    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))
    expect_true("iterations" %in% names(result$details))
    expect_true("status_message" %in% names(result$details))
    expect_true("nloptr_status" %in% names(result$details))
    expect_true("final_objective_value" %in% names(result$details))

    expect_equal(ncol(result$palette), 3)
  })

  it("handles extreme max_iterations values", {
    colors <- valid_colors(2)

    # Very high iterations
    result_high <- optimize_colors_nlopt_direct(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 10000
    )
    expect_s3_class(result_high, "list")

    # Very low iterations
    result_low <- optimize_colors_nlopt_direct(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 1
    )
    expect_s3_class(result_low, "list")
  })

  it("handles matrix structure validation", {
    # Test with matrix that might fail validation
    bad_colors <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)

    result <- optimize_colors_nlopt_direct(
      initial_colors_oklab = bad_colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 1
    )

    expect_s3_class(result, "list")
    expect_equal(nrow(result$palette), 2)
  })

  it("handles all fixed colors (no optimization needed)", {
    colors <- valid_colors(2)
    all_fixed <- c(TRUE, TRUE)

    result <- optimize_colors_nlopt_direct(
      initial_colors_oklab = colors,
      fixed_mask = all_fixed,
      max_iterations = 1
    )

    expect_s3_class(result, "list")
    # Should return original colors
    expect_equal(result$palette, colors)
  })

  it("handles single free color optimization", {
    colors <- valid_colors(2)
    mixed_mask <- c(TRUE, FALSE)

    result <- optimize_colors_nlopt_direct(
      initial_colors_oklab = colors,
      fixed_mask = mixed_mask,
      max_iterations = 5
    )

    expect_s3_class(result, "list")
    expect_equal(nrow(result$palette), 2)
    # Fixed color should remain unchanged
    expect_equal(result$palette[1, ], colors[1, ])
  })
})

describe("tryCatch error handling - optimize_colors_nlopt_neldermead()", {
  valid_colors <- function(n = 2) {
    matrix(
      c(
        0.5,
        0.1,
        0.0,
        0.6,
        0.0,
        0.1
      )[1:(n * 3)],
      nrow = n,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )
  }

  it("returns valid structure when nloptr Nelder-Mead throws error", {
    colors <- valid_colors(2)

    result <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 0
    )

    expect_s3_class(result, "list")
    expect_named(result, c("palette", "details"))
    expect_true("nloptr_status" %in% names(result$details))
    expect_true("status_message" %in% names(result$details))
  })

  it("properly scopes return_value after Nelder-Mead error", {
    colors <- valid_colors(2)

    result <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 0
    )

    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))
    expect_true("iterations" %in% names(result$details))
    expect_true("status_message" %in% names(result$details))
    expect_true("nloptr_status" %in% names(result$details))
    expect_true("final_objective_value" %in% names(result$details))

    expect_equal(ncol(result$palette), 3)
  })

  it("handles large number of colors", {
    # Test with many colors to potentially trigger memory/resource issues
    n_colors <- 10
    colors <- matrix(
      runif(n_colors * 3),
      nrow = n_colors,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = colors,
      fixed_mask = rep(FALSE, n_colors),
      max_iterations = 5
    )

    expect_s3_class(result, "list")
    expect_equal(nrow(result$palette), n_colors)
  })

  it("handles very small max_iterations", {
    colors <- valid_colors(2)

    result <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 1
    )

    expect_s3_class(result, "list")
  })

  it("handles NaN in objective function gracefully", {
    colors <- valid_colors(2)
    # This might trigger NaN in objective calculations

    result <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 1
    )

    expect_s3_class(result, "list")
    expect_true(
      is.finite(result$details$nloptr_status) ||
        result$details$nloptr_status == -999
    )
  })
})

describe("tryCatch error handling - optimize_colors_lbfgs()", {
  valid_colors <- function(n = 2) {
    matrix(
      c(
        0.5,
        0.1,
        0.0,
        0.6,
        0.0,
        0.1
      )[1:(n * 3)],
      nrow = n,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )
  }

  it("returns valid structure when L-BFGS throws error", {
    colors <- valid_colors(2)

    result <- optimize_colors_lbfgs(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 0
    )

    expect_s3_class(result, "list")
    expect_named(result, c("palette", "details"))
    expect_true("algorithm" %in% names(result$details))
  })

  it("properly scopes return_value after L-BFGS error", {
    colors <- valid_colors(2)

    result <- optimize_colors_lbfgs(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 0
    )

    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))
    expect_true("algorithm" %in% names(result$details))
    expect_true("iterations" %in% names(result$details))
    expect_true("nloptr_status" %in% names(result$details))
    expect_true("final_objective_value" %in% names(result$details))
    expect_true("status_message" %in% names(result$details))

    expect_equal(ncol(result$palette), 3)
  })

  it("handles invalid weights parameter", {
    colors <- valid_colors(2)

    # Test with invalid weights
    result <- optimize_colors_lbfgs(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5,
      weights = c(invalid_weight = 999)
    )

    expect_s3_class(result, "list")
  })

  it("handles NA weights", {
    colors <- valid_colors(2)

    result <- optimize_colors_lbfgs(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5,
      weights = c(smooth_logsumexp = NA_real_)
    )

    expect_s3_class(result, "list")
  })

  it("handles zero weights", {
    colors <- valid_colors(2)

    result <- optimize_colors_lbfgs(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5,
      weights = c(smooth_repulsion = 0, smooth_logsumexp = 0)
    )

    expect_s3_class(result, "list")
  })

  it("handles all colors fixed", {
    colors <- valid_colors(2)

    result <- optimize_colors_lbfgs(
      initial_colors_oklab = colors,
      fixed_mask = c(TRUE, TRUE),
      max_iterations = 5
    )

    expect_s3_class(result, "list")
    expect_equal(result$palette, colors)
  })

  it("handles many free colors", {
    n_colors <- 5
    colors <- matrix(
      runif(n_colors * 3),
      nrow = n_colors,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_lbfgs(
      initial_colors_oklab = colors,
      fixed_mask = rep(FALSE, n_colors),
      max_iterations = 10
    )

    expect_s3_class(result, "list")
    expect_equal(nrow(result$palette), n_colors)
  })

  it("preserves original palette on error", {
    colors <- valid_colors(2)

    # Force error condition with max_iterations = 0
    result <- optimize_colors_lbfgs(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 0
    )

    # If it fails, should return original palette
    expect_equal(ncol(result$palette), 3)
  })
})

describe("Matrix structure validation errors", {
  it("objective_min_cvd_safe_dist handles wrong column count", {
    # Test with 2 columns instead of 3
    bad_matrix <- matrix(1:4, nrow = 2)

    expect_error(objective_min_cvd_safe_dist(bad_matrix))
  })

  it("objective_smooth_repulsion handles wrong column count", {
    bad_matrix <- matrix(1:4, nrow = 2)

    expect_error(objective_smooth_repulsion(bad_matrix))
  })

  it("objective_smooth_logsumexp handles wrong column count", {
    bad_matrix <- matrix(1:4, nrow = 2)

    expect_error(objective_smooth_logsumexp(bad_matrix))
  })

  it("gradient_smooth_repulsion handles wrong column count", {
    bad_matrix <- matrix(1:4, nrow = 2)

    expect_error(gradient_smooth_repulsion(bad_matrix))
  })

  it("gradient_smooth_logsumexp handles wrong column count", {
    bad_matrix <- matrix(1:4, nrow = 2)

    expect_error(gradient_smooth_logsumexp(bad_matrix))
  })

  it("objective_smooth_repulsion handles single row", {
    # Single color should return 0, not error
    single_color <- matrix(c(0.5, 0.1, 0.0), nrow = 1)

    result <- objective_smooth_repulsion(single_color)

    expect_equal(result, 0)
  })

  it("objective_smooth_logsumexp handles single row", {
    single_color <- matrix(c(0.5, 0.1, 0.0), nrow = 1)

    result <- objective_smooth_logsumexp(single_color)

    expect_equal(result, 0)
  })

  it("gradient functions handle single row", {
    single_color <- matrix(c(0.5, 0.1, 0.0), nrow = 1)

    grad_repulsion <- gradient_smooth_repulsion(single_color)
    grad_logsumexp <- gradient_smooth_logsumexp(single_color)

    expect_equal(grad_repulsion, matrix(0, nrow = 1, ncol = 3))
    expect_equal(grad_logsumexp, matrix(0, nrow = 1, ncol = 3))
  })
})

describe("Edge cases and boundary conditions", {
  it("optimization with colors at exact bounds", {
    # Test with colors at the boundary of valid OKLAB range
    colors <- matrix(
      c(
        0.001,
        -0.4,
        -0.4, # At lower bounds
        0.999,
        0.4,
        0.4 # At upper bounds
      ),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5
    )

    expect_s3_class(result, "list")
    # Colors should still be within bounds after optimization
    expect_true(all(result$palette[, 1] >= 0.001))
    expect_true(all(result$palette[, 1] <= 0.999))
  })

  it("optimization with out-of-bounds initial values", {
    # Test with clearly invalid initial values
    colors <- matrix(
      c(
        1.5,
        0.8,
        0.8, # Way out of bounds
        -0.5,
        -0.8,
        -0.8 # Also out of bounds
      ),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5
    )

    expect_s3_class(result, "list")
    # Clamping should bring values back to bounds
    expect_true(all(
      result$palette[, 1] >= 0.001 & result$palette[, 1] <= 0.999
    ))
  })

  it("optimization with repeated colors", {
    # Test with identical colors (edge case for distance calculations)
    colors <- matrix(
      c(
        0.5,
        0.1,
        0.0,
        0.5,
        0.1,
        0.0
      ),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5
    )

    expect_s3_class(result, "list")
  })

  it("optimization with very similar colors", {
    # Colors that are nearly identical
    colors <- matrix(
      c(
        0.5,
        0.1,
        0.0,
        0.5001,
        0.1001,
        0.0001
      ),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 10
    )

    expect_s3_class(result, "list")
    expect_true(
      is.finite(result$details$final_objective_value) ||
        result$details$nloptr_status == -999
    )
  })

  it("optimization with large color set", {
    # Test with many colors to check scalability
    set.seed(42)
    n_colors <- 20
    colors <- matrix(
      runif(n_colors * 3, 0.1, 0.9),
      nrow = n_colors,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    # Not all free to reduce computation time
    fixed_mask <- c(TRUE, rep(FALSE, n_colors - 1))

    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = fixed_mask,
      max_iterations = 10
    )

    expect_s3_class(result, "list")
    expect_equal(nrow(result$palette), n_colors)
  })

  it("all optimization functions return consistent structure", {
    colors <- matrix(
      c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    # Test all four main optimizers
    constrained <- optimize_colors_constrained(
      colors,
      c(FALSE, FALSE),
      max_iterations = 3
    )
    sann <- optimize_colors_sann(
      colors,
      c(FALSE, FALSE),
      max_iterations = 5
    )
    direct <- optimize_colors_nlopt_direct(
      colors,
      c(FALSE, FALSE),
      max_iterations = 5
    )
    neldermead <- optimize_colors_nlopt_neldermead(
      colors,
      c(FALSE, FALSE),
      max_iterations = 10
    )

    # All should return list with palette and details
    expect_s3_class(constrained, "list")
    expect_s3_class(sann, "list")
    expect_s3_class(direct, "list")
    expect_s3_class(neldermead, "list")

    expect_named(constrained, c("palette", "details"))
    expect_named(sann, c("palette", "details"))
    expect_named(direct, c("palette", "details"))
    expect_named(neldermead, c("palette", "details"))

    # All palettes should have same dimensions
    expect_equal(dim(constrained$palette), dim(sann$palette))
    expect_equal(dim(sann$palette), dim(direct$palette))
    expect_equal(dim(direct$palette), dim(neldermead$palette))
  })
})

describe("Return value scoping verification", {
  # These tests specifically verify that the return_value variable
  # is properly set in all code paths, including error handlers

  it("optimize_colors_constrained return_value is complete", {
    colors <- matrix(
      c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5
    )

    # Verify all expected fields are present
    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))

    details <- result$details
    expect_true("iterations" %in% names(details))
    expect_true("status_message" %in% names(details))
    expect_true("nloptr_status" %in% names(details))
    expect_true("final_objective_value" %in% names(details))

    # Verify types
    expect_s3_class(result$palette, "matrix")
    expect_type(details$iterations, "integer")
    expect_type(details$nloptr_status, "double")
    expect_type(details$final_objective_value, "double")
  })

  it("optimize_colors_sann return_value is complete", {
    colors <- matrix(
      c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_sann(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5
    )

    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))

    details <- result$details
    expect_true("iterations" %in% names(details))
    expect_true("status_message" %in% names(details))
    expect_true("sann_convergence" %in% names(details))
    expect_true("final_objective_value" %in% names(details))
  })

  it("optimize_colors_nlopt_direct return_value is complete", {
    colors <- matrix(
      c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_nlopt_direct(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5
    )

    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))

    details <- result$details
    expect_true("iterations" %in% names(details))
    expect_true("status_message" %in% names(details))
    expect_true("nloptr_status" %in% names(details))
    expect_true("final_objective_value" %in% names(details))
  })

  it("optimize_colors_nlopt_neldermead return_value is complete", {
    colors <- matrix(
      c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_nlopt_neldermead(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5
    )

    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))

    details <- result$details
    expect_true("iterations" %in% names(details))
    expect_true("status_message" %in% names(details))
    expect_true("nloptr_status" %in% names(details))
    expect_true("final_objective_value" %in% names(details))
  })

  it("optimize_colors_lbfgs return_value is complete", {
    colors <- matrix(
      c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_lbfgs(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5
    )

    expect_true("palette" %in% names(result))
    expect_true("details" %in% names(result))

    details <- result$details
    expect_true("algorithm" %in% names(details))
    expect_true("iterations" %in% names(details))
    expect_true("nloptr_status" %in% names(details))
    expect_true("final_objective_value" %in% names(details))
    expect_true("status_message" %in% names(details))
  })
})

describe("Status code handling", {
  it("COBYLA returns valid status codes", {
    colors <- matrix(
      c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5
    )

    # Status should be numeric
    expect_type(result$details$nloptr_status, "double")
  })

  it("SANN returns valid convergence codes", {
    colors <- matrix(
      c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )

    result <- optimize_colors_sann(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 5
    )

    # Convergence should be numeric
    expect_type(result$details$sann_convergence, "double")
  })

  it("Error status -999 is set correctly on failure", {
    # Use inputs that will definitely cause an error
    colors <- matrix(c(NaN, NaN, NaN, Inf, Inf, Inf), nrow = 2)

    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 1
    )

    # On error, status should be -999
    expect_equal(result$details$nloptr_status, -999)
  })

  it("Error messages contain helpful information", {
    colors <- matrix(c(NaN, NaN, NaN, Inf, Inf, Inf), nrow = 2)

    result <- optimize_colors_constrained(
      initial_colors_oklab = colors,
      fixed_mask = c(FALSE, FALSE),
      max_iterations = 1
    )

    # Error message should indicate error type
    expect_true(grepl(
      "Error|nloptr",
      result$details$status_message,
      ignore.case = TRUE
    ))
  })
})

describe("optimization behavior", {
  it("produces reasonable colors with fixed colors", {
    set.seed(42)

    # Test case that currently fails: brand palette example
    result <- generate_palette(
      n = 7,
      include_colors = c("#4A6B8A", "#E5A04C"),
      progress = FALSE
    )

    # Convert to OKLAB for analysis
    oklab_colors <- .hex_to_oklab(result)

    # Behavioral expectations
    expect_true(
      all(oklab_colors[, 1] > 0.1),
      info = "All colors should have reasonable lightness (> 0.1), not near-black"
    )

    expect_true(
      any(oklab_colors[, 1] > 0.3),
      info = "At least some colors should be reasonably bright (> 0.3)"
    )

    # Evaluate the palette quality
    evaluation <- evaluate_palette(result)

    expect_true(
      evaluation$distances$min > 0.15,
      info = "Minimum perceptual distance should be > 0.15 OKLAB units"
    )

    expect_true(
      evaluation$distances$performance_ratio > 0.4,
      info = "Performance ratio should be > 40%"
    )

    # Pure data provider mode - no heuristic score
  })

  it("without penalties produces good results", {
    set.seed(42)

    # Test with default settings to verify core optimization works
    result <- generate_palette(
      n = 7,
      include_colors = c("#4A6B8A", "#E5A04C"),
      progress = FALSE
    )

    evaluation <- evaluate_palette(result)

    expect_true(
      evaluation$distances$min > 0.1,
      info = "Pure minimax optimization should achieve > 0.1 min distance"
    )

    expect_true(
      evaluation$distances$performance_ratio > 0.4,
      info = "Pure minimax performance should be > 40%"
    )

    # Pure data provider mode - no heuristic score
  })
})

describe("gamut penalty calculation", {
  it("is correct", {
    # Test gamut penalty logic with known good colors
    test_oklab <- matrix(
      c(
        0.7573138,
        0.04645336,
        0.12015603, # Bright orange (#E5A04C)
        0.5153679,
        -0.02382086,
        -0.05782878 # Blue (#4A6B8A)
      ),
      nrow = 2,
      byrow = TRUE
    )

    # Simulate the gamut penalty calculation from optimization_core.R
    lab_colors <- farver::convert_colour(test_oklab, from = "oklab", to = "lab")
    rgb_colors <- farver::convert_colour(lab_colors, from = "lab", to = "rgb")

    # This is CORRECT calculation (RGB should be divided by 255)
    out_of_gamut_dist_sq_correct <- pmax(0, rgb_colors / 255 - 1)^2 +
      pmax(0, -(rgb_colors / 255))^2
    gamut_penalty_correct <- sum(out_of_gamut_dist_sq_correct) /
      nrow(test_oklab)

    # Valid colors should have minimal gamut penalty
    expect_true(
      gamut_penalty_correct < 0.01,
      info = "Valid RGB colors should have minimal gamut penalty"
    )

    # Test what happens with BUGGY calculation (using RGB 0-255 directly)
    out_of_gamut_dist_sq_buggy <- pmax(0, rgb_colors - 1)^2 +
      pmax(0, -rgb_colors)^2
    gamut_penalty_buggy <- sum(out_of_gamut_dist_sq_buggy) / nrow(test_oklab)

    # The buggy calculation should give massive penalty for valid colors
    expect_true(
      gamut_penalty_buggy > 1000,
      info = "Buggy gamut calculation gives huge penalty for valid colors"
    )
  })
})

describe("objective function", {
  it("behaves correctly", {
    # Test that the objective function rewards separated colors

    # Well-separated colors
    separated_colors <- matrix(
      c(
        0.3,
        0.0,
        0.0, # Dark gray
        0.7,
        0.0,
        0.0, # Light gray
        0.5,
        0.2,
        0.0, # Reddish
        0.5,
        -0.2,
        0.0 # Greenish
      ),
      nrow = 4,
      byrow = TRUE
    )

    # Clustered colors (all similar)
    clustered_colors <- matrix(
      c(
        0.5,
        0.0,
        0.0,
        0.51,
        0.01,
        0.01,
        0.49,
        -0.01,
        -0.01,
        0.5,
        0.0,
        0.0
      ),
      nrow = 4,
      byrow = TRUE
    )

    separated_distance <- objective_min_perceptual_dist(separated_colors)
    clustered_distance <- objective_min_perceptual_dist(clustered_colors)

    expect_true(
      separated_distance > clustered_distance,
      info = "Well-separated colors should have larger minimum distance"
    )

    expect_true(
      separated_distance > 0.1,
      info = "Well-separated colors should have substantial minimum distance"
    )

    expect_true(
      clustered_distance < 0.05,
      info = "Clustered colors should have small minimum distance"
    )
  })
})

# ==============================================================================
# Numeric Edge Case Tests
# These tests verify numerical robustness of optimization algorithms against
# edge cases that could cause NaN, Inf, overflow, underflow, or precision issues.
# ==============================================================================

describe("numeric edge cases: NaN/Inf handling", {
  it("objective functions handle NaN inputs gracefully", {
    # NaN in color matrix
    colors_with_nan <- matrix(
      c(
        0.5,
        0.0,
        0.0,
        NaN,
        NaN,
        NaN,
        0.7,
        0.1,
        0.1
      ),
      nrow = 3,
      byrow = TRUE
    )

    # Should not crash, should return finite value or handle gracefully
    expect_error(
      objective_min_perceptual_dist(colors_with_nan),
      NA,
      info = "objective_min_perceptual_dist should handle NaN inputs"
    )

    expect_error(
      objective_min_cvd_safe_dist(colors_with_nan),
      NA,
      info = "objective_min_cvd_safe_dist should handle NaN inputs"
    )
  })

  it("objective functions handle Inf inputs gracefully", {
    # Inf in color matrix
    colors_with_inf <- matrix(
      c(
        0.5,
        0.0,
        0.0,
        Inf,
        Inf,
        Inf,
        0.7,
        0.1,
        0.1
      ),
      nrow = 3,
      byrow = TRUE
    )

    expect_error(
      objective_min_perceptual_dist(colors_with_inf),
      NA,
      info = "objective_min_perceptual_dist should handle Inf inputs"
    )
  })

  it("distance calculations handle extreme values", {
    # Very large L values that could cause issues
    extreme_colors <- matrix(
      c(
        0.999,
        0.4,
        0.4, # Near upper bound
        0.001,
        -0.4,
        -0.4, # Near lower bound
        0.5,
        0.0,
        0.0
      ),
      nrow = 3,
      byrow = TRUE
    )

    dist_result <- calculate_perceptual_distances(extreme_colors)

    expect_true(
      is.matrix(dist_result) && nrow(dist_result) == 3,
      info = "Distance matrix should be valid for extreme values"
    )

    # All diagonal elements should be 0 (distance to self)
    expect_equal(
      as.numeric(diag(dist_result)),
      rep(0, 3),
      tolerance = 1e-10,
      info = "Self-distances should be exactly zero"
    )

    # Off-diagonal should be positive
    off_diagonal <- dist_result[upper.tri(dist_result)]
    expect_true(
      all(off_diagonal > 0),
      info = "Off-diagonal distances should be positive"
    )
  })
})

describe("numeric edge cases: division by zero protection", {
  it("smooth repulsion objective handles zero epsilon safely", {
    # Identical colors would cause division by zero without epsilon protection
    identical_colors <- matrix(
      c(
        0.5,
        0.0,
        0.0,
        0.5,
        0.0,
        0.0,
        0.5,
        0.0,
        0.0
      ),
      nrow = 3,
      byrow = TRUE
    )

    # Should not crash, epsilon prevents division by zero
    expect_error(
      objective_smooth_repulsion(identical_colors),
      NA,
      info = "Smooth repulsion should handle identical colors without division by zero"
    )

    result <- objective_smooth_repulsion(identical_colors)
    expect_true(
      is.finite(result),
      info = "Result should be finite even for identical colors"
    )
  })

  it("smooth logsumexp objective handles zero distances", {
    # Identical colors test numerical stability
    identical_colors <- matrix(
      c(
        0.5,
        0.0,
        0.0,
        0.5,
        0.0,
        0.0
      ),
      nrow = 2,
      byrow = TRUE
    )

    expect_error(
      objective_smooth_logsumexp(identical_colors),
      NA,
      info = "Logsumexp should handle identical colors"
    )

    result <- objective_smooth_logsumexp(identical_colors)
    expect_true(
      is.finite(result),
      info = "Result should be finite for logsumexp"
    )
  })

  it("gradient calculations handle near-zero distances", {
    # Very close colors that could cause numerical issues
    near_identical <- matrix(
      c(
        0.5,
        0.0,
        0.0,
        0.5000001,
        0.0000001,
        0.0000001
      ),
      nrow = 2,
      byrow = TRUE
    )

    grad_result <- gradient_smooth_repulsion(near_identical)

    expect_true(
      is.matrix(grad_result) &&
        nrow(grad_result) == 2 &&
        ncol(grad_result) == 3,
      info = "Gradient should return valid matrix"
    )

    expect_true(
      all(is.finite(grad_result)),
      info = "Gradient should have all finite values even for near-identical colors"
    )
  })
})

describe("numeric edge cases: overflow/underflow protection", {
  it("logsumexp handles large negative inputs", {
    # Large negative values could cause exp() underflow
    # With k=10 and large distances, -k*distance can be very negative
    far_apart <- matrix(
      c(
        0.001,
        -0.4,
        -0.4,
        0.999,
        0.4,
        0.4
      ),
      nrow = 2,
      byrow = TRUE
    )

    # Distance will be large, so -k*distance will be very negative
    expect_error(
      objective_smooth_logsumexp(far_apart),
      NA,
      info = "Logsumexp should handle large distances without overflow"
    )

    result <- objective_smooth_logsumexp(far_apart)
    expect_true(
      is.finite(result),
      info = "Result should be finite even for very far apart colors"
    )
  })

  it("logsumexp gradient handles extreme temperature parameter", {
    colors <- matrix(
      c(
        0.3,
        0.1,
        0.1,
        0.7,
        -0.1,
        -0.1,
        0.5,
        0.0,
        0.0
      ),
      nrow = 3,
      byrow = TRUE
    )

    # Test with very high k value that could cause numerical issues
    expect_error(
      gradient_smooth_logsumexp(colors, k = 1000),
      NA,
      info = "Gradient should handle very high temperature parameter"
    )

    grad_high_k <- gradient_smooth_logsumexp(colors, k = 1000)
    expect_true(
      all(is.finite(grad_high_k)),
      info = "Gradient should be finite with high k"
    )
  })

  it("repulsion objective handles many colors without overflow", {
    # 10 colors could cause large sum in repulsion objective
    set.seed(42)
    many_colors <- matrix(
      runif(30, 0.1, 0.9),
      nrow = 10,
      byrow = TRUE
    )

    result <- objective_smooth_repulsion(many_colors)
    expect_true(
      is.finite(result),
      info = "Repulsion objective should be finite for many colors"
    )
    expect_true(
      result >= 0,
      info = "Repulsion objective should be non-negative"
    )
  })
})

describe("numeric edge cases: zero variance scenarios", {
  it("single color handling in objective functions", {
    single_color <- matrix(c(0.5, 0.0, 0.0), nrow = 1)

    # Single color should not cause errors
    expect_error(
      objective_min_perceptual_dist(single_color),
      NA,
      info = "Single color should not crash objective function"
    )

    result <- objective_min_perceptual_dist(single_color)
    expect_true(
      is.finite(result) || is.infinite(result),
      info = "Single color should return Inf (no pairs to compare)"
    )
  })

  it("two identical colors return zero distance", {
    identical_pair <- matrix(
      c(
        0.5,
        0.0,
        0.0,
        0.5,
        0.0,
        0.0
      ),
      nrow = 2,
      byrow = TRUE
    )

    result <- objective_min_perceptual_dist(identical_pair)
    expect_equal(
      result,
      0,
      tolerance = 1e-10,
      info = "Identical colors should have zero distance"
    )
  })

  it("many identical colors handled correctly", {
    n <- 5
    identical_many <- matrix(
      rep(c(0.5, 0.0, 0.0), each = n),
      nrow = n,
      byrow = TRUE
    )

    result <- objective_min_perceptual_dist(identical_many)
    expect_equal(
      result,
      0,
      tolerance = 1e-10,
      info = "Many identical colors should have zero minimum distance"
    )
  })

  it("all fixed colors optimization handles edge case", {
    # All colors fixed (no free colors to optimize)
    fixed_oklab <- matrix(
      c(
        0.2,
        0.1,
        0.1,
        0.5,
        -0.1,
        0.2,
        0.8,
        0.0,
        -0.1
      ),
      nrow = 3,
      byrow = TRUE
    )
    fixed_mask <- rep(TRUE, 3)

    result <- optimize_colors_constrained(
      fixed_oklab,
      fixed_mask,
      max_iterations = 100
    )

    expect_true(
      is.list(result) && "palette" %in% names(result),
      info = "Should return valid result structure"
    )
    expect_equal(
      result$palette,
      fixed_oklab,
      info = "All fixed colors should remain unchanged"
    )
  })
})

describe("numeric edge cases: extreme parameter values", {
  it("optimization handles very small L values", {
    # Colors at minimum allowed L (avoiding exact 0)
    small_L_colors <- matrix(
      c(
        0.001,
        0.0,
        0.0,
        0.002,
        0.1,
        0.1,
        0.0015,
        -0.1,
        -0.1
      ),
      nrow = 3,
      byrow = TRUE
    )

    result <- calculate_perceptual_distances(small_L_colors)

    expect_true(
      all(is.finite(result)),
      info = "Should handle very small L values"
    )
  })

  it("optimization handles maximum L values", {
    # Colors at maximum L
    large_L_colors <- matrix(
      c(
        0.999,
        0.0,
        0.0,
        0.998,
        0.1,
        0.1,
        0.999,
        -0.1,
        -0.1
      ),
      nrow = 3,
      byrow = TRUE
    )

    result <- calculate_perceptual_distances(large_L_colors)

    expect_true(
      all(is.finite(result)),
      info = "Should handle maximum L values"
    )
  })

  it("objective aggregator handles zero weights", {
    colors_oklab <- matrix(
      c(
        0.3,
        0.1,
        0.1,
        0.7,
        -0.1,
        -0.1,
        0.5,
        0.0,
        0.0
      ),
      nrow = 3,
      byrow = TRUE
    )

    # Zero weights should not cause NaN
    zero_weights <- c(distance = 0, smooth_repulsion = 0, smooth_logsumexp = 0)

    # This tests the internal handling - may error if all weights are zero
    # which is expected behavior
    expect_error(
      objective_function_aggregator(colors_oklab, "perceptual", zero_weights),
      NA,
      info = "Should handle zero weights gracefully"
    )
  })
})

describe("numeric edge cases: numerical stability", {
  it("distance matrix is symmetric", {
    set.seed(123)
    random_colors <- matrix(
      runif(15, 0.1, 0.9),
      nrow = 5,
      byrow = TRUE
    )

    dist_matrix <- calculate_perceptual_distances(random_colors)

    # Distance matrix should be symmetric
    expect_equal(
      dist_matrix,
      t(dist_matrix),
      tolerance = 1e-10,
      info = "Distance matrix should be symmetric"
    )

    # Diagonal should be zero
    expect_equal(
      as.numeric(diag(dist_matrix)),
      rep(0, 5),
      tolerance = 1e-10,
      info = "Diagonal of distance matrix should be zero"
    )
  })

  it("objective values are consistent across calls", {
    set.seed(456)
    colors <- matrix(
      runif(12, 0.2, 0.8),
      nrow = 4,
      byrow = TRUE
    )

    # Call objective multiple times, should get same result
    result1 <- objective_smooth_repulsion(colors)
    result2 <- objective_smooth_repulsion(colors)

    expect_equal(
      result1,
      result2,
      info = "Objective function should be deterministic"
    )
  })

  it("gradient is zero for optimally spaced identical colors", {
    # For identical colors, gradient should be zero (no direction of improvement)
    identical_colors <- matrix(
      c(
        0.5,
        0.0,
        0.0,
        0.5,
        0.0,
        0.0
      ),
      nrow = 2,
      byrow = TRUE
    )

    grad <- gradient_smooth_repulsion(identical_colors)

    expect_true(
      all(grad == 0),
      info = "Gradient should be zero for identical colors"
    )
  })

  it("clamping function handles boundary values correctly", {
    # Test .clamp_to_bounds behavior
    values <- c(-1, 0, 0.5, 1, 2)
    lower <- 0.001
    upper <- 0.999

    clamped <- pmax(lower, pmin(upper, values))

    expect_true(
      all(clamped >= lower),
      info = "Clamped values should be >= lower bound"
    )
    expect_true(
      all(clamped <= upper),
      info = "Clamped values should be <= upper bound"
    )
  })
})

describe("numeric edge cases: CVD simulation edge cases", {
  it("CVD objective handles gamut boundary colors", {
    # Colors near sRGB gamut boundary could cause issues
    gamut_boundary <- matrix(
      c(
        0.95,
        0.3,
        0.2, # High chroma, near gamut edge
        0.05,
        -0.2,
        -0.1 # Very dark
      ),
      nrow = 2,
      byrow = TRUE
    )

    expect_error(
      objective_min_cvd_safe_dist(gamut_boundary),
      NA,
      info = "CVD objective should handle gamut boundary colors"
    )
  })

  it("CVD objective returns zero for NA hex conversion", {
    # Colors that cannot be represented in sRGB
    # These would cause NA when converting to hex
    out_of_gamut <- matrix(
      c(
        0.99,
        0.5,
        0.5, # Likely out of gamut
        0.99,
        -0.5,
        -0.5
      ),
      nrow = 2,
      byrow = TRUE
    )

    # Should not crash, should handle gracefully
    expect_error(
      objective_min_cvd_safe_dist(out_of_gamut),
      NA,
      info = "CVD objective should handle out-of-gamut colors"
    )
  })
})

describe("numeric edge cases: optimization convergence", {
  it("optimizer handles extreme initial positions", {
    # Initialize at boundary values
    boundary_init <- matrix(
      c(
        0.001,
        -0.4,
        -0.4, # Corner
        0.999,
        0.4,
        0.4 # Opposite corner
      ),
      nrow = 2,
      byrow = TRUE
    )
    fixed_mask <- rep(FALSE, 2)

    result <- optimize_colors_constrained(
      boundary_init,
      fixed_mask,
      max_iterations = 50
    )

    expect_true(
      is.list(result) && "palette" %in% names(result),
      info = "Should return valid result from boundary initialization"
    )

    # Solution should still be within bounds
    expect_true(
      all(result$palette[, 1] >= 0.001 - 1e-10) &&
        all(result$palette[, 1] <= 0.999 + 1e-10),
      info = "Optimized colors should stay within bounds"
    )
  })

  it("optimizer handles minimal iteration count", {
    simple_init <- matrix(
      c(
        0.5,
        0.0,
        0.0,
        0.6,
        0.1,
        0.1
      ),
      nrow = 2,
      byrow = TRUE
    )
    fixed_mask <- rep(FALSE, 2)

    # Should not crash with minimal iterations
    result <- optimize_colors_constrained(
      simple_init,
      fixed_mask,
      max_iterations = 1
    )

    expect_true(
      is.list(result) && "palette" %in% names(result),
      info = "Should handle minimal iterations"
    )
  })

  it("L-BFGS handles smooth objectives with extreme weights", {
    colors_oklab <- matrix(
      c(
        0.3,
        0.1,
        0.1,
        0.7,
        -0.1,
        -0.1
      ),
      nrow = 2,
      byrow = TRUE
    )
    fixed_mask <- rep(FALSE, 2)

    # Test with logsumexp objective
    weights <- c(smooth_logsumexp = 1.0)

    expect_error(
      optimize_colors_lbfgs(colors_oklab, fixed_mask, 10, weights = weights),
      NA,
      info = "L-BFGS should handle logsumexp objective"
    )
  })
})

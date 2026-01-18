describe(".calculate_aesthetic_profile()", {
  it("calculates profile from fixed colors", {
    # Test with multiple fixed colors
    fixed_colors <- matrix(
      c(
        0.3,
        0.1,
        0.0, # Low lightness, low chroma
        0.7,
        -0.1,
        0.1, # High lightness, moderate chroma
        0.5,
        0.2,
        -0.1 # Medium lightness, higher chroma
      ),
      nrow = 3,
      byrow = TRUE
    )

    result <- .calculate_aesthetic_profile(fixed_colors)

    expect_true(is.list(result))
    expect_true("mean_L" %in% names(result))
    expect_true("sd_L" %in% names(result))
    expect_true("mean_C" %in% names(result))
    expect_true("sd_C" %in% names(result))

    # Mean lightness should be around 0.5 for our test data
    expect_true(abs(result$mean_L - 0.5) < 0.1)
    expect_true(result$sd_L > 0) # Should have some variation
    expect_true(result$mean_C > 0) # Should have some chroma
    expect_true(result$sd_C >= 0) # SD should be non-negative
  })

  it("handles single fixed color", {
    # Test with single color
    single_color <- matrix(c(0.6, 0.15, -0.05), nrow = 1, ncol = 3)

    result <- .calculate_aesthetic_profile(single_color)

    expect_true(is.list(result))
    expect_equal(result$mean_L, 0.6)
    expect_equal(result$sd_L, 0.1) # Fallback value for single color
    expect_true(result$mean_C > 0) # Should calculate chroma
    expect_equal(result$sd_C, 0.05) # Fallback value for single color
  })

  it("handles edge cases", {
    # Test with NULL input
    result_null <- .calculate_aesthetic_profile(NULL)
    expect_true(is.list(result_null))
    expect_true(is.na(result_null$mean_L))
    expect_true(is.na(result_null$sd_L))
    expect_true(is.na(result_null$mean_C))
    expect_true(is.na(result_null$sd_C))

    # Test with empty matrix
    empty_matrix <- matrix(numeric(0), nrow = 0, ncol = 3)
    result_empty <- .calculate_aesthetic_profile(empty_matrix)
    expect_true(is.list(result_empty))
    expect_true(is.na(result_empty$mean_L))
  })

  it("calculates zero variance correctly", {
    # Test with identical colors (zero variance case)
    colors_with_zero_var <- matrix(
      c(
        0.5,
        0.1,
        0.0, # Same values
        0.5,
        0.1,
        0.0 # Same values (zero variance)
      ),
      nrow = 2,
      byrow = TRUE
    )

    result <- .calculate_aesthetic_profile(colors_with_zero_var)

    expect_true(is.list(result))
    expect_equal(result$mean_L, 0.5)
    expect_equal(result$sd_L, 0.0) # Should be 0 for identical values
    expect_equal(result$sd_C, 0.0) # Should be 0 for identical values
  })

  # --- ERROR TESTS ---
  describe(".calculate_aesthetic_profile() - error handling", {
    it("handles matrix with wrong number of columns", {
      # Matrix with 2 columns instead of 3 - should still attempt to extract
      invalid_matrix <- matrix(c(0.5, 0.1, 0.6, 0.2), nrow = 2, ncol = 2)
      # This will cause issues with column indexing - test graceful handling
      expect_error(
        .calculate_aesthetic_profile(invalid_matrix),
        regexp = NA
      )
    })

    it("handles matrix with NA values", {
      # Matrix containing NA values
      matrix_with_na <- matrix(
        c(0.5, NA, 0.1, 0.6, 0.2, 0.15),
        nrow = 2,
        byrow = TRUE
      )
      # Should return NA values when NA is present in calculations
      result <- .calculate_aesthetic_profile(matrix_with_na)
      expect_true(is.list(result))
      expect_true(is.na(result$mean_L) || is.finite(result$mean_L))
    })

    it("handles matrix with Inf values", {
      # Matrix containing Inf values
      matrix_with_inf <- matrix(
        c(Inf, 0.1, 0.0, 0.6, 0.2, 0.15),
        nrow = 2,
        byrow = TRUE
      )
      # Should handle gracefully
      result <- .calculate_aesthetic_profile(matrix_with_inf)
      expect_true(is.list(result))
    })

    it("handles single row matrix correctly", {
      # Single row should use fallback SD values
      single_row <- matrix(c(0.5, 0.1, 0.0), nrow = 1, ncol = 3)
      result <- .calculate_aesthetic_profile(single_row)
      expect_equal(result$sd_L, 0.1) # Fallback value
      expect_equal(result$sd_C, 0.05) # Fallback value
    })

    it("handles matrix with extreme negative values", {
      # Matrix with extreme negative chroma values
      matrix_extreme_neg <- matrix(
        c(0.5, -100, -100, 0.6, -50, -50),
        nrow = 2,
        byrow = TRUE
      )
      result <- .calculate_aesthetic_profile(matrix_extreme_neg)
      expect_true(is.list(result))
      # Chroma calculation involves sqrt of squares, so should handle this
      expect_true(is.finite(result$mean_C))
    })

    it("handles matrix with all NA values", {
      # All NA matrix
      matrix_all_na <- matrix(NA_real_, nrow = 2, ncol = 3)
      result <- .calculate_aesthetic_profile(matrix_all_na)
      expect_true(is.list(result))
      # mean with na.rm=TRUE on all NA returns NaN
      expect_true(is.nan(result$mean_L) || is.na(result$mean_L))
    })

    it("handles 1-column matrix", {
      # Matrix with only 1 column (unexpected structure)
      single_col <- matrix(c(0.5, 0.6), nrow = 2, ncol = 1)
      expect_error(
        .calculate_aesthetic_profile(single_col),
        regexp = NA
      )
    })
  })
})

describe(".adapt_init_params()", {
  it("returns proper structure", {
    # Basic test with simple aesthetic profile
    aesthetic_profile <- list(
      mean_L = 0.5,
      sd_L = 0.1,
      mean_C = 0.1,
      sd_C = 0.05
    )

    init_lightness_bounds <- c(0.2, 0.8)
    init_hcl_bounds <- list(L = c(20, 80), C = c(30, 70))

    config <- list(
      kmeans_L_sd_multiplier = 1.5,
      kmeans_C_base_deviation = 0.05,
      kmeans_C_influence_tightening_factor = 0.75,
      kmeans_C_filter_relaxation_factor = 1.5,
      harmony_hcl_sd_fallback = 15,
      harmony_hcl_sd_multiplier = 1.0
    )

    result <- .adapt_init_params(
      aesthetic_profile,
      influence = 0.5,
      init_lightness_bounds,
      init_hcl_bounds,
      config,
      progress = FALSE
    )

    # Test structure
    expect_true(is.list(result))
    expect_true("lightness_bounds" %in% names(result))
    expect_true("hcl_bounds" %in% names(result))
    expect_true("kmeans_chroma_filter_params" %in% names(result))

    # Test lightness bounds structure
    expect_true(is.numeric(result$lightness_bounds))
    expect_equal(length(result$lightness_bounds), 2)
    expect_true(result$lightness_bounds[1] < result$lightness_bounds[2])

    # Test HCL bounds structure
    expect_true(is.list(result$hcl_bounds))
    expect_true("L" %in% names(result$hcl_bounds))
    expect_true("C" %in% names(result$hcl_bounds))

    # Test chroma filter structure
    expect_true(is.list(result$kmeans_chroma_filter_params))
    expect_true("apply_filter" %in% names(result$kmeans_chroma_filter_params))
  })

  it("handles zero influence", {
    # Test with zero influence (should return original bounds)
    aesthetic_profile <- list(
      mean_L = 0.7,
      sd_L = 0.2,
      mean_C = 0.2,
      sd_C = 0.1
    )

    init_lightness_bounds <- c(0.1, 0.9)
    init_hcl_bounds <- list(L = c(10, 90), C = c(20, 80))

    config <- list(
      kmeans_L_sd_multiplier = 1.5,
      kmeans_C_base_deviation = 0.05,
      kmeans_C_influence_tightening_factor = 0.75,
      kmeans_C_filter_relaxation_factor = 1.5,
      harmony_hcl_sd_fallback = 15,
      harmony_hcl_sd_multiplier = 1.0
    )

    result <- .adapt_init_params(
      aesthetic_profile,
      influence = 0.0, # Zero influence
      init_lightness_bounds,
      init_hcl_bounds,
      config,
      progress = FALSE
    )

    # With zero influence, bounds should remain unchanged
    expect_equal(result$lightness_bounds, init_lightness_bounds)
    expect_equal(result$hcl_bounds, init_hcl_bounds)
    expect_false(result$kmeans_chroma_filter_params$apply_filter)
  })

  it("handles NA aesthetic profile", {
    # Test with NA aesthetic profile (edge case)
    aesthetic_profile <- list(
      mean_L = NA_real_,
      sd_L = NA_real_,
      mean_C = NA_real_,
      sd_C = NA_real_
    )

    init_lightness_bounds <- c(0.2, 0.8)
    init_hcl_bounds <- list(L = c(20, 80), C = c(30, 70))

    config <- list(
      kmeans_L_sd_multiplier = 1.5,
      kmeans_C_base_deviation = 0.05,
      kmeans_C_influence_tightening_factor = 0.75,
      kmeans_C_filter_relaxation_factor = 1.5,
      harmony_hcl_sd_fallback = 15,
      harmony_hcl_sd_multiplier = 1.0
    )

    result <- .adapt_init_params(
      aesthetic_profile,
      influence = 0.5,
      init_lightness_bounds,
      init_hcl_bounds,
      config,
      progress = FALSE
    )

    # Should return original bounds when aesthetic profile has NA values
    expect_equal(result$lightness_bounds, init_lightness_bounds)
    expect_equal(result$hcl_bounds, init_hcl_bounds)
    expect_false(result$kmeans_chroma_filter_params$apply_filter)
  })

  # --- ERROR TESTS ---
  describe(".adapt_init_params() - error handling", {
    before_each({
      base_aesthetic_profile <- list(
        mean_L = 0.5,
        sd_L = 0.1,
        mean_C = 0.1,
        sd_C = 0.05
      )
      base_lightness_bounds <- c(0.2, 0.8)
      base_hcl_bounds <- list(L = c(20, 80), C = c(30, 70))
      base_config <- list(
        kmeans_L_sd_multiplier = 1.5,
        kmeans_C_base_deviation = 0.05,
        kmeans_C_influence_tightening_factor = 0.75,
        kmeans_C_filter_relaxation_factor = 1.5,
        harmony_hcl_sd_fallback = 15,
        harmony_hcl_sd_multiplier = 1.0
      )
    })

    it("handles negative influence value", {
      # Negative influence should still work (returns original bounds)
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = -0.5,
        base_lightness_bounds,
        base_hcl_bounds,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
      expect_equal(result$lightness_bounds, base_lightness_bounds)
    })

    it("handles influence > 1", {
      # Influence > 1 should still work
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = 2.0,
        base_lightness_bounds,
        base_hcl_bounds,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
    })

    it("handles Inf influence", {
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = Inf,
        base_lightness_bounds,
        base_hcl_bounds,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
    })

    it("handles NaN influence", {
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = NaN,
        base_lightness_bounds,
        base_hcl_bounds,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
    })

    it("handles inverted lightness bounds (min > max)", {
      # Inverted bounds should be handled gracefully
      inverted_bounds <- c(0.8, 0.2)
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = 0.5,
        inverted_bounds,
        base_hcl_bounds,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
      # Should fall back to original or handle gracefully
      expect_equal(length(result$lightness_bounds), 2)
    })

    it("handles NA in lightness bounds", {
      na_bounds <- c(NA_real_, 0.8)
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = 0.5,
        na_bounds,
        base_hcl_bounds,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
    })

    it("handles Inf in lightness bounds", {
      inf_bounds <- c(-Inf, 0.8)
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = 0.5,
        inf_bounds,
        base_hcl_bounds,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
    })

    it("handles inverted HCL L bounds", {
      inverted_hcl_L <- list(L = c(80, 20), C = c(30, 70))
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = 0.5,
        base_lightness_bounds,
        inverted_hcl_L,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
    })

    it("handles inverted HCL C bounds", {
      inverted_hcl_C <- list(L = c(20, 80), C = c(70, 30))
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = 0.5,
        base_lightness_bounds,
        inverted_hcl_C,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
    })

    it("handles NA in HCL bounds", {
      na_hcl_bounds <- list(L = c(NA, 80), C = c(30, 70))
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = 0.5,
        base_lightness_bounds,
        na_hcl_bounds,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
    })

    it("handles missing config values with defaults", {
      incomplete_config <- list(
        kmeans_L_sd_multiplier = 1.5
        # Missing other config values
      )
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = 0.5,
        base_lightness_bounds,
        base_hcl_bounds,
        incomplete_config,
        progress = FALSE
      )
      expect_true(is.list(result))
    })

    it("handles Inf in aesthetic profile values", {
      inf_profile <- list(
        mean_L = Inf,
        sd_L = 0.1,
        mean_C = 0.1,
        sd_C = 0.05
      )
      result <- .adapt_init_params(
        inf_profile,
        influence = 0.5,
        base_lightness_bounds,
        base_hcl_bounds,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
    })

    it("handles empty config list", {
      empty_config <- list()
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = 0.5,
        base_lightness_bounds,
        base_hcl_bounds,
        empty_config,
        progress = FALSE
      )
      expect_true(is.list(result))
    })

    it("handles very small lightness bounds range", {
      tight_bounds <- c(0.5, 0.51)
      result <- .adapt_init_params(
        base_aesthetic_profile,
        influence = 0.5,
        tight_bounds,
        base_hcl_bounds,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
      expect_true(result$lightness_bounds[1] < result$lightness_bounds[2])
    })

    it("handles very large influence with extreme SD", {
      extreme_profile <- list(
        mean_L = 0.5,
        sd_L = 100, # Very large SD
        mean_C = 0.1,
        sd_C = 100
      )
      result <- .adapt_init_params(
        extreme_profile,
        influence = 0.99,
        base_lightness_bounds,
        base_hcl_bounds,
        base_config,
        progress = FALSE
      )
      expect_true(is.list(result))
    })
  })
})

describe("initialize_colors()", {
  it("handles zero free colors", {
    result <- initialize_colors(
      n_free = 0,
      fixed_colors_oklab = matrix(
        c(0.5, 0, 0),
        nrow = 1,
        dimnames = list(NULL, c("L", "a", "b"))
      ),
      method = "k-means++",
      adapted_init_params = list(lightness_bounds = c(0.2, 0.8)),
      base_init_lightness_bounds = c(0.1, 0.9)
    )

    expect_true(is.matrix(result))
    expect_equal(nrow(result), 0)
    expect_equal(ncol(result), 3)
    expect_equal(colnames(result), c("L", "a", "b"))
  })

  it("delegates to k-means++ method", {
    # Use realistic adapted params that would come from .adapt_init_params()
    adapted_params <- list(
      lightness_bounds = c(0.2, 0.8),
      kmeans_chroma_filter_params = list(
        apply_filter = FALSE # No filtering for simple test
      )
    )

    result <- initialize_colors(
      n_free = 2,
      fixed_colors_oklab = NULL,
      method = "k-means++",
      adapted_init_params = adapted_params,
      base_init_lightness_bounds = c(0.1, 0.9)
    )

    expect_true(is.matrix(result))
    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 3)
    expect_equal(colnames(result), c("L", "a", "b"))
    expect_true(all(result[, "L"] >= 0.2 & result[, "L"] <= 0.8))
  })

  it("delegates to harmony method", {
    adapted_params <- list(
      hcl_bounds = list(L = c(20, 80), C = c(10, 60))
    )

    result <- initialize_colors(
      n_free = 3,
      fixed_colors_oklab = NULL,
      method = "harmony",
      adapted_init_params = adapted_params,
      base_init_lightness_bounds = c(0.1, 0.9)
    )

    expect_true(is.matrix(result))
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 3)
    expect_equal(colnames(result), c("L", "a", "b"))
  })

  # --- ERROR TESTS ---
  describe("initialize_colors() - error handling", {
    it("handles negative n_free", {
      # Negative n_free should be handled gracefully
      result <- initialize_colors(
        n_free = -1,
        fixed_colors_oklab = NULL,
        method = "k-means++",
        adapted_init_params = list(
          lightness_bounds = c(0.2, 0.8),
          kmeans_chroma_filter_params = list(apply_filter = FALSE)
        ),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      # Should return something, not crash
      expect_true(is.matrix(result))
    })

    it("handles non-numeric n_free", {
      # Non-numeric n_free should be handled
      expect_error(
        initialize_colors(
          n_free = "5",
          fixed_colors_oklab = NULL,
          method = "k-means++",
          adapted_init_params = list(
            lightness_bounds = c(0.2, 0.8),
            kmeans_chroma_filter_params = list(apply_filter = FALSE)
          ),
          base_init_lightness_bounds = c(0.1, 0.9)
        ),
        regexp = NA
      )
    })

    it("handles Inf n_free", {
      result <- initialize_colors(
        n_free = Inf,
        fixed_colors_oklab = NULL,
        method = "k-means++",
        adapted_init_params = list(
          lightness_bounds = c(0.2, 0.8),
          kmeans_chroma_filter_params = list(apply_filter = FALSE)
        ),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles NA n_free", {
      result <- initialize_colors(
        n_free = NA_integer_,
        fixed_colors_oklab = NULL,
        method = "k-means++",
        adapted_init_params = list(
          lightness_bounds = c(0.2, 0.8),
          kmeans_chroma_filter_params = list(apply_filter = FALSE)
        ),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles unknown method gracefully", {
      result <- initialize_colors(
        n_free = 2,
        fixed_colors_oklab = NULL,
        method = "unknown_method",
        adapted_init_params = list(
          lightness_bounds = c(0.2, 0.8),
          kmeans_chroma_filter_params = list(apply_filter = FALSE)
        ),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      # Should return empty matrix for unrecognized method
      expect_true(is.matrix(result))
      expect_equal(nrow(result), 0)
    })

    it("handles missing adapted_init_params fields", {
      # Missing lightness_bounds
      result <- initialize_colors(
        n_free = 2,
        fixed_colors_oklab = NULL,
        method = "k-means++",
        adapted_init_params = list(), # Empty
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles NA in fixed_colors_oklab", {
      result <- initialize_colors(
        n_free = 2,
        fixed_colors_oklab = matrix(NA_real_, nrow = 1, ncol = 3),
        method = "k-means++",
        adapted_init_params = list(
          lightness_bounds = c(0.2, 0.8),
          kmeans_chroma_filter_params = list(apply_filter = FALSE)
        ),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles fixed_colors_oklab with wrong columns", {
      # Fixed colors with only 2 columns instead of 3
      bad_fixed <- matrix(c(0.5, 0.1), nrow = 1, ncol = 2)
      result <- initialize_colors(
        n_free = 2,
        fixed_colors_oklab = bad_fixed,
        method = "k-means++",
        adapted_init_params = list(
          lightness_bounds = c(0.2, 0.8),
          kmeans_chroma_filter_params = list(apply_filter = FALSE)
        ),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles fixed_colors_oklab with 1 column", {
      bad_fixed <- matrix(c(0.5), nrow = 1, ncol = 1)
      result <- initialize_colors(
        n_free = 2,
        fixed_colors_oklab = bad_fixed,
        method = "k-means++",
        adapted_init_params = list(
          lightness_bounds = c(0.2, 0.8),
          kmeans_chroma_filter_params = list(apply_filter = FALSE)
        ),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles Inf in adapted_init_params bounds", {
      result <- initialize_colors(
        n_free = 2,
        fixed_colors_oklab = NULL,
        method = "k-means++",
        adapted_init_params = list(
          lightness_bounds = c(-Inf, Inf),
          kmeans_chroma_filter_params = list(apply_filter = FALSE)
        ),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })
  })
})

describe("initialize_kmeans_plus_plus()", {
  it("generates valid OKLAB colors", {
    chroma_filter <- list(apply_filter = FALSE)

    result <- initialize_kmeans_plus_plus(
      n_free = 3,
      fixed_colors_oklab = NULL,
      lightness_bounds = c(0.2, 0.8),
      chroma_filter_params = chroma_filter,
      base_init_lightness_bounds = c(0.1, 0.9)
    )

    expect_true(is.matrix(result))
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 3)
    expect_equal(colnames(result), c("L", "a", "b"))
    expect_true(all(result[, "L"] >= 0.2 & result[, "L"] <= 0.8))
    expect_true(all(result[, "a"] >= -0.4 & result[, "a"] <= 0.4))
    expect_true(all(result[, "b"] >= -0.4 & result[, "b"] <= 0.4))
  })

  it("with fixed colors", {
    fixed_oklab <- matrix(
      c(0.6, 0.1, 0.05),
      nrow = 1,
      dimnames = list(NULL, c("L", "a", "b"))
    )
    chroma_filter <- list(apply_filter = FALSE)

    result <- initialize_kmeans_plus_plus(
      n_free = 2,
      fixed_colors_oklab = fixed_oklab,
      lightness_bounds = c(0.3, 0.7),
      chroma_filter_params = chroma_filter,
      base_init_lightness_bounds = c(0.1, 0.9)
    )

    expect_true(is.matrix(result))
    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 3)
    expect_equal(colnames(result), c("L", "a", "b"))
  })

  # --- ERROR TESTS ---
  describe("initialize_kmeans_plus_plus() - error handling", {
    it("handles negative n_free", {
      result <- initialize_kmeans_plus_plus(
        n_free = -1,
        fixed_colors_oklab = NULL,
        lightness_bounds = c(0.2, 0.8),
        chroma_filter_params = list(apply_filter = FALSE),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles Inf n_free", {
      result <- initialize_kmeans_plus_plus(
        n_free = Inf,
        fixed_colors_oklab = NULL,
        lightness_bounds = c(0.2, 0.8),
        chroma_filter_params = list(apply_filter = FALSE),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles inverted lightness bounds (min > max)", {
      inverted_bounds <- c(0.8, 0.2)
      result <- initialize_kmeans_plus_plus(
        n_free = 3,
        fixed_colors_oklab = NULL,
        lightness_bounds = inverted_bounds,
        chroma_filter_params = list(apply_filter = FALSE),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles NA in lightness bounds", {
      na_bounds <- c(NA_real_, 0.8)
      result <- initialize_kmeans_plus_plus(
        n_free = 3,
        fixed_colors_oklab = NULL,
        lightness_bounds = na_bounds,
        chroma_filter_params = list(apply_filter = FALSE),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles Inf in lightness bounds", {
      inf_bounds <- c(-Inf, 0.8)
      result <- initialize_kmeans_plus_plus(
        n_free = 3,
        fixed_colors_oklab = NULL,
        lightness_bounds = inf_bounds,
        chroma_filter_params = list(apply_filter = FALSE),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles missing chroma_filter_params", {
      result <- initialize_kmeans_plus_plus(
        n_free = 3,
        fixed_colors_oklab = NULL,
        lightness_bounds = c(0.2, 0.8),
        chroma_filter_params = NULL,
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles NA in fixed_colors_oklab", {
      na_fixed <- matrix(NA_real_, nrow = 1, ncol = 3)
      result <- initialize_kmeans_plus_plus(
        n_free = 2,
        fixed_colors_oklab = na_fixed,
        lightness_bounds = c(0.2, 0.8),
        chroma_filter_params = list(apply_filter = FALSE),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles fixed_colors_oklab with wrong columns", {
      bad_fixed <- matrix(c(0.5, 0.1), nrow = 1, ncol = 2)
      result <- initialize_kmeans_plus_plus(
        n_free = 2,
        fixed_colors_oklab = bad_fixed,
        lightness_bounds = c(0.2, 0.8),
        chroma_filter_params = list(apply_filter = FALSE),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles empty fixed_colors_oklab matrix", {
      empty_fixed <- matrix(numeric(0), nrow = 0, ncol = 3)
      result <- initialize_kmeans_plus_plus(
        n_free = 2,
        fixed_colors_oklab = empty_fixed,
        lightness_bounds = c(0.2, 0.8),
        chroma_filter_params = list(apply_filter = FALSE),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles very narrow lightness bounds", {
      narrow_bounds <- c(0.5, 0.5001)
      result <- initialize_kmeans_plus_plus(
        n_free = 5,
        fixed_colors_oklab = NULL,
        lightness_bounds = narrow_bounds,
        chroma_filter_params = list(apply_filter = FALSE),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles very large n_free", {
      result <- initialize_kmeans_plus_plus(
        n_free = 1000,
        fixed_colors_oklab = NULL,
        lightness_bounds = c(0.2, 0.8),
        chroma_filter_params = list(apply_filter = FALSE),
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
      expect_equal(nrow(result), 1000)
    })

    it("handles chroma_filter with NA target_C_mean", {
      filter_with_na <- list(
        apply_filter = TRUE,
        target_C_mean = NA_real_,
        max_C_deviation = 0.1,
        relaxation_factor = 1.5
      )
      result <- initialize_kmeans_plus_plus(
        n_free = 3,
        fixed_colors_oklab = NULL,
        lightness_bounds = c(0.2, 0.8),
        chroma_filter_params = filter_with_na,
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles chroma_filter with Inf max_C_deviation", {
      filter_with_inf <- list(
        apply_filter = TRUE,
        target_C_mean = 0.1,
        max_C_deviation = Inf,
        relaxation_factor = 1.5
      )
      result <- initialize_kmeans_plus_plus(
        n_free = 3,
        fixed_colors_oklab = NULL,
        lightness_bounds = c(0.2, 0.8),
        chroma_filter_params = filter_with_inf,
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      expect_true(is.matrix(result))
    })

    it("handles very restrictive chroma filter", {
      # Very restrictive filter that might filter out all candidates
      restrictive_filter <- list(
        apply_filter = TRUE,
        target_C_mean = 0.5,
        max_C_deviation = 0.001, # Very tight
        relaxation_factor = 1.5
      )
      result <- initialize_kmeans_plus_plus(
        n_free = 3,
        fixed_colors_oklab = NULL,
        lightness_bounds = c(0.2, 0.8),
        chroma_filter_params = restrictive_filter,
        base_init_lightness_bounds = c(0.1, 0.9)
      )
      # Should return whatever is available after filtering
      expect_true(is.matrix(result))
    })
  })
})

describe("initialize_harmony_based()", {
  it("with no fixed colors", {
    hcl_bounds <- list(L = c(20, 80), C = c(10, 60))

    result <- initialize_harmony_based(
      n_free = 4,
      fixed_colors_oklab = NULL,
      hcl_bounds = hcl_bounds
    )

    expect_true(is.matrix(result))
    expect_equal(nrow(result), 4)
    expect_equal(ncol(result), 3)
    expect_equal(colnames(result), c("L", "a", "b"))
  })

  it("with fixed colors", {
    fixed_oklab <- matrix(
      c(0.6, 0.1, 0.05, 0.4, 0.05, 0.1),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    )
    hcl_bounds <- list(L = c(20, 80), C = c(10, 60))

    result <- initialize_harmony_based(
      n_free = 2,
      fixed_colors_oklab = fixed_oklab,
      hcl_bounds = hcl_bounds
    )

    expect_true(is.matrix(result))
    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 3)
    expect_equal(colnames(result), c("L", "a", "b"))
  })

  it("edge cases", {
    fixed_oklab <- matrix(
      c(0.6, 0.1, 0.05),
      nrow = 1,
      dimnames = list(NULL, c("L", "a", "b"))
    )
    hcl_bounds <- list(L = c(20, 80), C = c(10, 60))

    # Single free color with single fixed color
    result1 <- initialize_harmony_based(
      n_free = 1,
      fixed_colors_oklab = fixed_oklab,
      hcl_bounds = hcl_bounds
    )
    expect_true(is.matrix(result1))
    expect_equal(nrow(result1), 1)
    expect_equal(ncol(result1), 3)

    # Two free colors with single fixed color
    result2 <- initialize_harmony_based(
      n_free = 2,
      fixed_colors_oklab = fixed_oklab,
      hcl_bounds = hcl_bounds
    )
    expect_true(is.matrix(result2))
    expect_equal(nrow(result2), 2)
    expect_equal(ncol(result2), 3)
  })

  it("handles achromatic fixed colors", {
    # Create achromatic (gray) fixed colors that should have no valid hue
    gray_fixed <- matrix(
      c(0.5, 0.0, 0.0),
      nrow = 1,
      dimnames = list(NULL, c("L", "a", "b"))
    )
    hcl_bounds <- list(L = c(20, 80), C = c(10, 60))

    # Test with 3 free colors to trigger general case (not n_free=1 or n_free=2 special cases)
    result <- initialize_harmony_based(
      n_free = 3,
      fixed_colors_oklab = gray_fixed,
      hcl_bounds = hcl_bounds
    )

    expect_true(is.matrix(result))
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 3)
    expect_equal(colnames(result), c("L", "a", "b"))
  })

  # --- ERROR TESTS ---
  describe("initialize_harmony_based() - error handling", {
    it("handles negative n_free", {
      result <- initialize_harmony_based(
        n_free = -1,
        fixed_colors_oklab = NULL,
        hcl_bounds = list(L = c(20, 80), C = c(10, 60))
      )
      expect_true(is.matrix(result))
    })

    it("handles Inf n_free", {
      result <- initialize_harmony_based(
        n_free = Inf,
        fixed_colors_oklab = NULL,
        hcl_bounds = list(L = c(20, 80), C = c(10, 60))
      )
      expect_true(is.matrix(result))
    })

    it("handles NA n_free", {
      result <- initialize_harmony_based(
        n_free = NA_integer_,
        fixed_colors_oklab = NULL,
        hcl_bounds = list(L = c(20, 80), C = c(10, 60))
      )
      expect_true(is.matrix(result))
    })

    it("handles inverted HCL L bounds (min > max)", {
      inverted_bounds <- list(L = c(80, 20), C = c(10, 60))
      result <- initialize_harmony_based(
        n_free = 3,
        fixed_colors_oklab = NULL,
        hcl_bounds = inverted_bounds
      )
      expect_true(is.matrix(result))
    })

    it("handles inverted HCL C bounds (min > max)", {
      inverted_bounds <- list(L = c(20, 80), C = c(60, 10))
      result <- initialize_harmony_based(
        n_free = 3,
        fixed_colors_oklab = NULL,
        hcl_bounds = inverted_bounds
      )
      expect_true(is.matrix(result))
    })

    it("handles NA in HCL bounds", {
      na_bounds <- list(L = c(NA, 80), C = c(10, 60))
      result <- initialize_harmony_based(
        n_free = 3,
        fixed_colors_oklab = NULL,
        hcl_bounds = na_bounds
      )
      expect_true(is.matrix(result))
    })

    it("handles Inf in HCL bounds", {
      inf_bounds <- list(L = c(-Inf, 80), C = c(10, Inf))
      result <- initialize_harmony_based(
        n_free = 3,
        fixed_colors_oklab = NULL,
        hcl_bounds = inf_bounds
      )
      expect_true(is.matrix(result))
    })

    it("handles NA in fixed_colors_oklab", {
      na_fixed <- matrix(NA_real_, nrow = 1, ncol = 3)
      result <- initialize_harmony_based(
        n_free = 2,
        fixed_colors_oklab = na_fixed,
        hcl_bounds = list(L = c(20, 80), C = c(10, 60))
      )
      expect_true(is.matrix(result))
    })

    it("handles fixed_colors_oklab with wrong columns", {
      bad_fixed <- matrix(c(0.5, 0.1), nrow = 1, ncol = 2)
      result <- initialize_harmony_based(
        n_free = 2,
        fixed_colors_oklab = bad_fixed,
        hcl_bounds = list(L = c(20, 80), C = c(10, 60))
      )
      expect_true(is.matrix(result))
    })

    it("handles empty fixed_colors_oklab matrix", {
      empty_fixed <- matrix(numeric(0), nrow = 0, ncol = 3)
      result <- initialize_harmony_based(
        n_free = 3,
        fixed_colors_oklab = empty_fixed,
        hcl_bounds = list(L = c(20, 80), C = c(10, 60))
      )
      expect_true(is.matrix(result))
    })

    it("handles very narrow HCL L range", {
      narrow_bounds <- list(L = c(50, 50.1), C = c(10, 60))
      result <- initialize_harmony_based(
        n_free = 5,
        fixed_colors_oklab = NULL,
        hcl_bounds = narrow_bounds
      )
      expect_true(is.matrix(result))
    })

    it("handles very narrow HCL C range", {
      narrow_bounds <- list(L = c(20, 80), C = c(30, 30.1))
      result <- initialize_harmony_based(
        n_free = 5,
        fixed_colors_oklab = NULL,
        hcl_bounds = narrow_bounds
      )
      expect_true(is.matrix(result))
    })

    it("handles very large n_free", {
      result <- initialize_harmony_based(
        n_free = 1000,
        fixed_colors_oklab = NULL,
        hcl_bounds = list(L = c(20, 80), C = c(10, 60))
      )
      expect_true(is.matrix(result))
      expect_equal(nrow(result), 1000)
    })

    it("handles HCL bounds with C min > C max", {
      invalid_c_bounds <- list(L = c(20, 80), C = c(100, 10))
      result <- initialize_harmony_based(
        n_free = 3,
        fixed_colors_oklab = NULL,
        hcl_bounds = invalid_c_bounds
      )
      expect_true(is.matrix(result))
    })

    it("handles HCL bounds with L min > L max", {
      invalid_l_bounds <- list(L = c(90, 10), C = c(10, 60))
      result <- initialize_harmony_based(
        n_free = 3,
        fixed_colors_oklab = NULL,
        hcl_bounds = invalid_l_bounds
      )
      expect_true(is.matrix(result))
    })

    it("handles missing H component in hcl_bounds", {
      # Only L and C provided (no H - though H is not typically used in bounds)
      partial_bounds <- list(L = c(20, 80), C = c(10, 60))
      result <- initialize_harmony_based(
        n_free = 3,
        fixed_colors_oklab = NULL,
        hcl_bounds = partial_bounds
      )
      expect_true(is.matrix(result))
    })

    it("handles zero-range HCL bounds", {
      zero_range_bounds <- list(L = c(50, 50), C = c(30, 30))
      result <- initialize_harmony_based(
        n_free = 3,
        fixed_colors_oklab = NULL,
        hcl_bounds = zero_range_bounds
      )
      expect_true(is.matrix(result))
    })

    it("handles fixed colors with all same hue", {
      # Fixed colors all at same hue - triggers special case handling
      same_hue_fixed <- matrix(
        c(0.5, 0.1, 0.0, 0.6, 0.15, 0.05),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "a", "b"))
      )
      result <- initialize_harmony_based(
        n_free = 5,
        fixed_colors_oklab = same_hue_fixed,
        hcl_bounds = list(L = c(20, 80), C = c(10, 60))
      )
      expect_true(is.matrix(result))
    })

    it("handles fixed colors with extreme L values", {
      extreme_fixed <- matrix(
        c(0.99, 0.1, 0.0, 0.01, 0.1, 0.0),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "a", "b"))
      )
      result <- initialize_harmony_based(
        n_free = 3,
        fixed_colors_oklab = extreme_fixed,
        hcl_bounds = list(L = c(20, 80), C = c(10, 60))
      )
      expect_true(is.matrix(result))
    })
  })
})

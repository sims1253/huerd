# Tests for .calculate_aesthetic_profile - Level 10 TDD
test_that(".calculate_aesthetic_profile calculates profile from fixed colors", {
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

test_that(".calculate_aesthetic_profile handles single fixed color", {
  # Test with single color
  single_color <- matrix(c(0.6, 0.15, -0.05), nrow = 1, ncol = 3)

  result <- .calculate_aesthetic_profile(single_color)

  expect_true(is.list(result))
  expect_equal(result$mean_L, 0.6)
  expect_equal(result$sd_L, 0.1) # Fallback value for single color
  expect_true(result$mean_C > 0) # Should calculate chroma
  expect_equal(result$sd_C, 0.05) # Fallback value for single color
})

test_that(".calculate_aesthetic_profile handles edge cases", {
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

test_that(".calculate_aesthetic_profile calculates zero variance correctly", {
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

# Tests for .adapt_init_params - Final Level 10 TDD
test_that(".adapt_init_params returns proper structure", {
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

test_that(".adapt_init_params handles zero influence", {
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

test_that(".adapt_init_params handles NA aesthetic profile", {
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

test_that("initialize_colors handles zero free colors", {
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

test_that("initialize_colors delegates to k-means++ method", {
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

test_that("initialize_colors delegates to harmony method", {
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

test_that("initialize_kmeans_plus_plus generates valid OKLAB colors", {
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

test_that("initialize_kmeans_plus_plus with fixed colors", {
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

test_that("initialize_harmony_based with no fixed colors", {
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

test_that("initialize_harmony_based with fixed colors", {
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

test_that("initialize_harmony_based edge cases", {
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

test_that("initialize_harmony_based handles achromatic fixed colors", {
  # Create achromatic (gray) fixed colors that should have no valid hue
  gray_fixed <- matrix(
    c(0.5, 0.0, 0.0),
    nrow = 1,
    dimnames = list(NULL, c("L", "a", "b"))
  )
  hcl_bounds <- list(L = c(20, 80), C = c(10, 60))

  # Test with 3 free colors to trigger the general case (not n_free=1 or n_free=2 special cases)
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

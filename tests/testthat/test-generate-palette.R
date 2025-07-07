# Tests for generate_palette() - Main user-facing function

test_that("generate_palette creates palette with specified number of colors", {
  # Basic functionality: generates correct number of colors
  palette <- generate_palette(n = 5, progress = FALSE)

  expect_true(is.character(palette))
  expect_equal(length(palette), 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palette)))
  expect_true(inherits(palette, "huerd_palette"))
})

test_that("generate_palette handles edge cases", {
  # Single color palette
  single_color <- generate_palette(n = 1, progress = FALSE)
  expect_equal(length(single_color), 1)
  expect_true(inherits(single_color, "huerd_palette"))

  # Zero colors (should be valid)
  zero_colors <- generate_palette(n = 0, progress = FALSE)
  expect_equal(length(zero_colors), 0)
  expect_true(inherits(zero_colors, "huerd_palette"))
})

test_that("generate_palette includes fixed colors unchanged", {
  # Test that include_colors are preserved in output
  fixed_colors <- c("#FF0000", "#00FF00")
  palette <- generate_palette(
    n = 4,
    include_colors = fixed_colors,
    progress = FALSE
  )

  expect_equal(length(palette), 4)
  expect_true(all(fixed_colors %in% palette))
  expect_true(inherits(palette, "huerd_palette"))
})

test_that("generate_palette works with all fixed colors", {
  # Edge case: all colors are fixed
  all_fixed <- c("#FF0000", "#00FF00", "#0000FF")
  palette <- generate_palette(
    n = 3,
    include_colors = all_fixed,
    progress = FALSE
  )

  expect_equal(length(palette), 3)
  expect_true(all(all_fixed %in% palette))
  expect_true(inherits(palette, "huerd_palette"))
})

test_that("generate_palette uses pure minimax optimization", {
  # Test pure minimax optimization (no targets needed)
  pal <- generate_palette(n = 3, progress = FALSE)
  expect_equal(length(pal), 3)
  expect_true(inherits(pal, "huerd_palette"))
})

test_that("generate_palette accepts different initialization methods", {
  # Test both initialization methods
  kmeans_pal <- generate_palette(
    n = 3,
    initialization = "k-means++",
    progress = FALSE
  )
  expect_equal(length(kmeans_pal), 3)
  expect_true(inherits(kmeans_pal, "huerd_palette"))

  harmony_pal <- generate_palette(
    n = 3,
    initialization = "harmony",
    progress = FALSE
  )
  expect_equal(length(harmony_pal), 3)
  expect_true(inherits(harmony_pal, "huerd_palette"))
})

test_that("generate_palette respects return_metrics parameter", {
  # With metrics
  with_metrics <- generate_palette(
    n = 3,
    return_metrics = TRUE,
    progress = FALSE
  )
  expect_true("metrics" %in% names(attributes(with_metrics)))

  # Without metrics
  without_metrics <- generate_palette(
    n = 3,
    return_metrics = FALSE,
    progress = FALSE
  )
  expect_false("metrics" %in% names(attributes(without_metrics)))
})

test_that("generate_palette has proper attributes", {
  # Check that optimization details are included
  palette <- generate_palette(n = 3, progress = FALSE)

  expect_true(inherits(palette, "huerd_palette"))
  expect_true("optimization_details" %in% names(attributes(palette)))

  details <- attr(palette, "optimization_details")
  expect_true(is.list(details))
  expect_true("iterations" %in% names(details))
  expect_true("status_message" %in% names(details))
  expect_true("nloptr_status" %in% names(details))

  # Check that generation metadata is included (v0.4.0+ feature)
  expect_true("generation_metadata" %in% names(attributes(palette)))
  metadata <- attr(palette, "generation_metadata")
  expect_true(is.list(metadata))
  expect_true("n_colors" %in% names(metadata))
  expect_true("optimizer" %in% names(metadata))
  expect_true("package_version" %in% names(metadata))
})

test_that("generate_palette handles custom parameters", {
  # Test with custom max_iterations only (pure minimax API)
  custom_pal <- generate_palette(
    n = 3,
    max_iterations = 100,
    progress = FALSE
  )

  expect_equal(length(custom_pal), 3)
  expect_true(inherits(custom_pal, "huerd_palette"))
})

test_that("generate_palette validates input parameters", {
  # Test that invalid inputs are rejected
  expect_error(
    generate_palette(n = -1),
    "'n' must be a single non-negative integer"
  )

  expect_error(
    generate_palette(
      n = 2,
      include_colors = c("#FF0000", "#00FF00", "#0000FF")
    ),
    "Number of fixed colors.*cannot exceed total requested colors"
  )

  expect_error(
    generate_palette(n = 3, include_colors = c("#INVALID")),
    "All elements in 'include_colors' must be valid hex colors"
  )
})

test_that("generate_palette returns colors sorted by brightness (lightness)", {
  # Generate a palette and check that colors are sorted by brightness
  palette <- generate_palette(n = 5, progress = FALSE)

  # Convert to OKLAB to get lightness values
  oklab_matrix <- .hex_to_oklab(palette)
  lightness_values <- oklab_matrix[, 1] # L channel is first column

  # Check that lightness values are monotonically increasing (sorted)
  expect_true(all(diff(lightness_values) >= 0))

  # Test with fixed colors too
  fixed_colors <- c("#1a1a1a", "#ffffff") # Very dark and very light
  palette_with_fixed <- generate_palette(
    n = 5,
    include_colors = fixed_colors,
    progress = FALSE
  )

  oklab_matrix_fixed <- .hex_to_oklab(palette_with_fixed)
  lightness_values_fixed <- oklab_matrix_fixed[, 1]

  # Should still be sorted despite having fixed colors
  expect_true(all(diff(lightness_values_fixed) >= 0))
})

test_that("generate_palette accepts weights parameter", {
  # Test with NULL weights (default behavior)
  palette_default <- generate_palette(n = 3, progress = FALSE)
  expect_equal(length(palette_default), 3)
  expect_true(inherits(palette_default, "huerd_palette"))

  # Test with explicit NULL weights
  palette_null <- generate_palette(n = 3, weights = NULL, progress = FALSE)
  expect_equal(length(palette_null), 3)
  expect_true(inherits(palette_null, "huerd_palette"))

  # Test with valid distance weights
  palette_distance <- generate_palette(
    n = 3,
    weights = c(distance = 1),
    progress = FALSE
  )
  expect_equal(length(palette_distance), 3)
  expect_true(inherits(palette_distance, "huerd_palette"))

  # Test with different distance weight values
  palette_distance_2 <- generate_palette(
    n = 3,
    weights = c(distance = 2.5),
    progress = FALSE
  )
  expect_equal(length(palette_distance_2), 3)
  expect_true(inherits(palette_distance_2, "huerd_palette"))
})

test_that("generate_palette validates weights parameter", {
  # Test invalid weights - non-numeric
  expect_error(
    generate_palette(n = 3, weights = "invalid"),
    "'weights' must be a named numeric vector or NULL"
  )

  # Test invalid weights - unnamed
  expect_error(
    generate_palette(n = 3, weights = c(1, 2)),
    "'weights' must be a named numeric vector with all elements named"
  )

  # Test invalid weights - negative values
  expect_error(
    generate_palette(n = 3, weights = c(distance = -1)),
    "All 'weights' values must be non-negative"
  )

  # Test invalid weights - invalid objective names
  expect_error(
    generate_palette(n = 3, weights = c(invalid_objective = 1)),
    "Invalid objective names in 'weights': invalid_objective"
  )

  # Test invalid weights - all zero
  expect_error(
    generate_palette(n = 3, weights = c(distance = 0)),
    "At least one 'weights' value must be positive"
  )

  # Test invalid weights - empty vector
  expect_error(
    generate_palette(n = 3, weights = numeric(0)),
    "'weights' must be a named numeric vector or NULL"
  )
})

test_that("generate_palette backward compatibility with weights", {
  # Generate palettes with and without weights - should produce same results
  # for distance-based optimization
  set.seed(123)
  palette_old <- generate_palette(n = 3, progress = FALSE)

  set.seed(123)
  palette_new <- generate_palette(
    n = 3,
    weights = c(distance = 1),
    progress = FALSE
  )

  # Should produce the same color results (both are distance-based optimization)
  expect_identical(as.character(palette_old), as.character(palette_new))

  # Metadata will differ due to weights parameter, but optimization results should be same
  expect_identical(
    attr(palette_old, "optimization_details"),
    attr(palette_new, "optimization_details")
  )

  # Test with fixed colors
  fixed_colors <- c("#FF0000", "#00FF00")

  set.seed(456)
  palette_fixed_old <- generate_palette(
    n = 4,
    include_colors = fixed_colors,
    progress = FALSE
  )

  set.seed(456)
  palette_fixed_new <- generate_palette(
    n = 4,
    include_colors = fixed_colors,
    weights = c(distance = 1),
    progress = FALSE
  )

  # Should produce the same color results
  expect_identical(
    as.character(palette_fixed_old),
    as.character(palette_fixed_new)
  )

  # Optimization details should be same
  expect_identical(
    attr(palette_fixed_old, "optimization_details"),
    attr(palette_fixed_new, "optimization_details")
  )
})

test_that("generate_palette accepts optimizer parameter", {
  # Test with default optimizer
  palette_default <- generate_palette(n = 3, progress = FALSE)
  expect_equal(length(palette_default), 3)
  expect_true(inherits(palette_default, "huerd_palette"))

  # Test with explicit nloptr_cobyla optimizer
  palette_nloptr <- generate_palette(
    n = 3,
    optimizer = "nloptr_cobyla",
    progress = FALSE
  )
  expect_equal(length(palette_nloptr), 3)
  expect_true(inherits(palette_nloptr, "huerd_palette"))

  # Test with fixed colors
  fixed_colors <- c("#FF0000", "#00FF00")
  palette_fixed <- generate_palette(
    n = 4,
    include_colors = fixed_colors,
    optimizer = "nloptr_cobyla",
    progress = FALSE
  )
  expect_equal(length(palette_fixed), 4)
  expect_true(all(fixed_colors %in% palette_fixed))
  expect_true(inherits(palette_fixed, "huerd_palette"))
})

test_that("generate_palette validates optimizer parameter", {
  # Test invalid optimizer - non-character
  expect_error(
    generate_palette(n = 3, optimizer = 123),
    "'optimizer' must be a single character string"
  )

  # Test invalid optimizer - character vector with length > 1
  expect_error(
    generate_palette(n = 3, optimizer = c("nloptr_cobyla", "invalid")),
    "'optimizer' must be a single character string"
  )

  # Test invalid optimizer - unsupported optimizer
  expect_error(
    generate_palette(n = 3, optimizer = "invalid_optimizer"),
    "Invalid optimizer 'invalid_optimizer'"
  )

  # Test invalid optimizer - empty string
  expect_error(
    generate_palette(n = 3, optimizer = ""),
    "Invalid optimizer ''"
  )
})

test_that("generate_palette optimizer parameter is backward compatible", {
  # Default behavior should be identical to explicit nloptr_cobyla
  set.seed(789)
  palette_default <- generate_palette(n = 3, progress = FALSE)

  set.seed(789)
  palette_explicit <- generate_palette(
    n = 3,
    optimizer = "nloptr_cobyla",
    progress = FALSE
  )

  # Should produce identical results
  expect_equal(palette_default, palette_explicit)

  # Test with different parameters
  set.seed(101)
  palette_complex_default <- generate_palette(
    n = 5,
    include_colors = c("#FF0000"),
    initialization = "harmony",
    max_iterations = 500,
    progress = FALSE
  )

  set.seed(101)
  palette_complex_explicit <- generate_palette(
    n = 5,
    include_colors = c("#FF0000"),
    initialization = "harmony",
    max_iterations = 500,
    optimizer = "nloptr_cobyla",
    progress = FALSE
  )

  # Should produce identical results
  expect_equal(palette_complex_default, palette_complex_explicit)
})

test_that("generate_palette accepts sann optimizer", {
  # Test with sann optimizer
  palette_sann <- generate_palette(n = 3, optimizer = "sann", progress = FALSE)
  expect_equal(length(palette_sann), 3)
  expect_true(inherits(palette_sann, "huerd_palette"))

  # Test with fixed colors
  fixed_colors <- c("#FF0000", "#00FF00")
  palette_sann_fixed <- generate_palette(
    n = 4,
    include_colors = fixed_colors,
    optimizer = "sann",
    progress = FALSE
  )
  expect_equal(length(palette_sann_fixed), 4)
  expect_true(all(fixed_colors %in% palette_sann_fixed))
  expect_true(inherits(palette_sann_fixed, "huerd_palette"))
})

test_that("generate_palette sann optimizer has proper attributes", {
  # Check that optimization details are included for sann optimizer
  palette <- generate_palette(n = 3, optimizer = "sann", progress = FALSE)

  expect_true(inherits(palette, "huerd_palette"))
  expect_true("optimization_details" %in% names(attributes(palette)))

  details <- attr(palette, "optimization_details")
  expect_true(is.list(details))
  expect_true("iterations" %in% names(details))
  expect_true("status_message" %in% names(details))
  expect_true("sann_convergence" %in% names(details))
  expect_true("final_objective_value" %in% names(details))
})

test_that("generate_palette sann optimizer returns colors sorted by brightness", {
  # Generate a palette with sann optimizer and check that colors are sorted by brightness
  set.seed(42) # For reproducibility with stochastic optimizer
  palette <- generate_palette(n = 5, optimizer = "sann", progress = FALSE)

  # Convert to OKLAB to get lightness values
  oklab_matrix <- .hex_to_oklab(palette)
  lightness_values <- oklab_matrix[, 1] # L channel is first column

  # Check that lightness values are monotonically increasing (sorted)
  expect_true(all(diff(lightness_values) >= 0))

  # Test with fixed colors too
  fixed_colors <- c("#1a1a1a", "#ffffff") # Very dark and very light
  set.seed(42)
  palette_with_fixed <- generate_palette(
    n = 5,
    include_colors = fixed_colors,
    optimizer = "sann",
    progress = FALSE
  )

  oklab_matrix_fixed <- .hex_to_oklab(palette_with_fixed)
  lightness_values_fixed <- oklab_matrix_fixed[, 1]

  # Should still be sorted despite having fixed colors
  expect_true(all(diff(lightness_values_fixed) >= 0))
})

test_that("generate_palette sann optimizer is stochastic", {
  # Test that sann optimizer produces different results with different seeds
  set.seed(42)
  palette1 <- generate_palette(n = 3, optimizer = "sann", progress = FALSE)

  set.seed(123)
  palette2 <- generate_palette(n = 3, optimizer = "sann", progress = FALSE)

  # Different seeds should produce different results (stochastic nature)
  expect_false(identical(palette1, palette2))

  # But with same seed, should produce same results
  set.seed(42)
  palette3 <- generate_palette(n = 3, optimizer = "sann", progress = FALSE)

  set.seed(42)
  palette4 <- generate_palette(n = 3, optimizer = "sann", progress = FALSE)

  expect_equal(palette3, palette4)
})

test_that("generate_palette validates sann optimizer", {
  # Test that sann is now a valid optimizer
  expect_no_error(
    generate_palette(n = 3, optimizer = "sann", progress = FALSE)
  )
})

test_that("generate_palette sann optimizer works with all parameters", {
  # Test sann optimizer with full parameter set
  set.seed(42)
  palette_full <- generate_palette(
    n = 5,
    include_colors = c("#FF0000"),
    initialization = "harmony",
    init_lightness_bounds = c(0.3, 0.8),
    init_hcl_bounds = list(C = c(20, 60), L = c(40, 80)),
    fixed_aesthetic_influence = 0.5,
    max_iterations = 100,
    return_metrics = TRUE,
    optimizer = "sann",
    progress = FALSE
  )

  expect_equal(length(palette_full), 5)
  expect_true(inherits(palette_full, "huerd_palette"))
  expect_true("metrics" %in% names(attributes(palette_full)))
  expect_true("optimization_details" %in% names(attributes(palette_full)))

  # Check that fixed color is included
  expect_true("#FF0000" %in% palette_full)
})

test_that("generate_palette accepts nlopt_direct optimizer", {
  # Test with nlopt_direct optimizer
  palette_direct <- generate_palette(
    n = 3,
    optimizer = "nlopt_direct",
    progress = FALSE
  )

  expect_equal(length(palette_direct), 3)
  expect_true(inherits(palette_direct, "huerd_palette"))
  expect_true("metrics" %in% names(attributes(palette_direct)))
  expect_true("optimization_details" %in% names(attributes(palette_direct)))

  # Test with fixed colors
  palette_fixed <- generate_palette(
    n = 4,
    include_colors = c("#FF0000"),
    optimizer = "nlopt_direct",
    progress = FALSE
  )

  expect_equal(length(palette_fixed), 4)
  expect_true("#FF0000" %in% palette_fixed)
})

test_that("generate_palette nlopt_direct optimizer has proper attributes", {
  # Check that optimization details are included for nlopt_direct optimizer
  palette <- generate_palette(
    n = 3,
    optimizer = "nlopt_direct",
    progress = FALSE
  )

  expect_true("optimization_details" %in% names(attributes(palette)))
  details <- attr(palette, "optimization_details")

  # Check details structure
  expect_true("iterations" %in% names(details))
  expect_true("status_message" %in% names(details))
  expect_true("nloptr_status" %in% names(details))
  expect_true("final_objective_value" %in% names(details))

  # Check that values are reasonable
  expect_true(is.numeric(details$iterations))
  expect_true(is.character(details$status_message))
  expect_true(is.numeric(details$nloptr_status))
  expect_true(is.numeric(details$final_objective_value))
})

test_that("generate_palette nlopt_direct optimizer returns colors sorted by brightness", {
  # Generate a palette with nlopt_direct optimizer and check that colors are sorted by brightness
  palette <- generate_palette(
    n = 5,
    optimizer = "nlopt_direct",
    progress = FALSE
  )

  # Convert to OKLAB and check that L values are sorted
  oklab_matrix <- .hex_to_oklab(palette)

  # Should be sorted by L (lightness) values
  expect_true(all(diff(oklab_matrix[, 1]) >= 0))

  # Test with fixed colors
  palette_fixed <- generate_palette(
    n = 4,
    include_colors = c("#FF0000"),
    optimizer = "nlopt_direct",
    progress = FALSE
  )

  # Should still be sorted by brightness
  oklab_matrix_fixed <- .hex_to_oklab(palette_fixed)
  expect_true(all(diff(oklab_matrix_fixed[, 1]) >= 0))
})

test_that("generate_palette nlopt_direct optimizer is deterministic", {
  # Test that nlopt_direct optimizer produces consistent results
  palette1 <- generate_palette(
    n = 3,
    optimizer = "nlopt_direct",
    progress = FALSE
  )
  palette2 <- generate_palette(
    n = 3,
    optimizer = "nlopt_direct",
    progress = FALSE
  )

  # Colors should be identical for deterministic optimizer
  expect_identical(as.character(palette1), as.character(palette2))

  # Optimization details should be identical
  expect_identical(
    attr(palette1, "optimization_details"),
    attr(palette2, "optimization_details")
  )

  # Test with the same seed (should be redundant for deterministic optimizer)
  palette3 <- generate_palette(
    n = 3,
    optimizer = "nlopt_direct",
    progress = FALSE
  )
  palette4 <- generate_palette(
    n = 3,
    optimizer = "nlopt_direct",
    progress = FALSE
  )

  expect_identical(as.character(palette3), as.character(palette4))
  expect_identical(
    attr(palette3, "optimization_details"),
    attr(palette4, "optimization_details")
  )
})

test_that("generate_palette validates nlopt_direct optimizer", {
  # Test that nlopt_direct is now a valid optimizer
  expect_no_error({
    generate_palette(n = 3, optimizer = "nlopt_direct", progress = FALSE)
  })
})

test_that("generate_palette nlopt_direct optimizer works with all parameters", {
  # Test nlopt_direct optimizer with full parameter set
  palette_full <- generate_palette(
    n = 5,
    include_colors = c("#FF0000"),
    initialization = "k-means++",
    init_lightness_bounds = c(0.3, 0.8),
    init_hcl_bounds = list(C = c(30, 70), L = c(40, 70)),
    fixed_aesthetic_influence = 0.8,
    max_iterations = 50,
    return_metrics = TRUE,
    progress = FALSE,
    optimizer = "nlopt_direct"
  )

  expect_equal(length(palette_full), 5)
  expect_true(inherits(palette_full, "huerd_palette"))
  expect_true("metrics" %in% names(attributes(palette_full)))
  expect_true("optimization_details" %in% names(attributes(palette_full)))

  # Check that fixed color is included
  expect_true("#FF0000" %in% palette_full)
})

test_that("generate_palette nlopt_direct optimizer provides scientific reproducibility", {
  # Test scientific reproducibility claim by running same optimization multiple times

  # Use a fixed set of parameters
  params <- list(
    n = 4,
    include_colors = c("#FF0000"),
    initialization = "k-means++",
    init_lightness_bounds = c(0.2, 0.8),
    max_iterations = 100,
    optimizer = "nlopt_direct",
    progress = FALSE
  )

  # Run optimization 3 times
  palette1 <- do.call(generate_palette, params)
  palette2 <- do.call(generate_palette, params)
  palette3 <- do.call(generate_palette, params)

  # All colors should be identical for scientific reproducibility
  expect_identical(as.character(palette1), as.character(palette2))
  expect_identical(as.character(palette2), as.character(palette3))

  # Optimization details should also be identical
  details1 <- attr(palette1, "optimization_details")
  details2 <- attr(palette2, "optimization_details")
  details3 <- attr(palette3, "optimization_details")

  expect_equal(details1$final_objective_value, details2$final_objective_value)
  expect_equal(details2$final_objective_value, details3$final_objective_value)
})

# Tests for nlopt_neldermead optimizer
# ====================================

test_that("generate_palette accepts nlopt_neldermead optimizer", {
  # Test with nlopt_neldermead optimizer
  palette_neldermead <- generate_palette(
    n = 3,
    optimizer = "nlopt_neldermead",
    progress = FALSE
  )

  expect_equal(length(palette_neldermead), 3)
  expect_true(inherits(palette_neldermead, "huerd_palette"))
  expect_true("metrics" %in% names(attributes(palette_neldermead)))
  expect_true("optimization_details" %in% names(attributes(palette_neldermead)))

  # Test with fixed colors
  palette_fixed <- generate_palette(
    n = 4,
    include_colors = c("#FF0000"),
    optimizer = "nlopt_neldermead",
    progress = FALSE
  )

  expect_equal(length(palette_fixed), 4)
  expect_true("#FF0000" %in% palette_fixed)
})

test_that("generate_palette nlopt_neldermead optimizer has proper attributes", {
  # Check that optimization details are included for nlopt_neldermead optimizer
  palette <- generate_palette(
    n = 3,
    optimizer = "nlopt_neldermead",
    progress = FALSE
  )

  expect_true("optimization_details" %in% names(attributes(palette)))
  details <- attr(palette, "optimization_details")

  # Check details structure
  expect_true("iterations" %in% names(details))
  expect_true("status_message" %in% names(details))
  expect_true("nloptr_status" %in% names(details))
  expect_true("final_objective_value" %in% names(details))

  # Check that values are reasonable
  expect_true(is.numeric(details$iterations))
  expect_true(is.character(details$status_message))
  expect_true(is.numeric(details$nloptr_status))
  expect_true(is.numeric(details$final_objective_value))
})

test_that("generate_palette nlopt_neldermead optimizer returns colors sorted by brightness", {
  # Generate a palette with nlopt_neldermead optimizer and check that colors are sorted by brightness
  palette <- generate_palette(
    n = 5,
    optimizer = "nlopt_neldermead",
    progress = FALSE
  )

  # Convert to OKLAB and check that L values are sorted
  oklab_matrix <- .hex_to_oklab(palette)

  # Should be sorted by L (lightness) values
  lightness_values <- oklab_matrix[, 1]
  expect_equal(lightness_values, sort(lightness_values))

  # Test with fixed colors to ensure sorting works with mixed optimization
  palette_fixed <- generate_palette(
    n = 4,
    include_colors = c("#808080"),
    optimizer = "nlopt_neldermead",
    progress = FALSE
  )

  oklab_matrix_fixed <- .hex_to_oklab(palette_fixed)
  lightness_values_fixed <- oklab_matrix_fixed[, 1]
  expect_equal(lightness_values_fixed, sort(lightness_values_fixed))
})

test_that("generate_palette nlopt_neldermead optimizer produces reasonable consistency", {
  # Test that nlopt_neldermead optimizer produces reasonably consistent results
  # Note: Nelder-Mead is a local optimizer so results may vary slightly
  palette1 <- generate_palette(
    n = 3,
    optimizer = "nlopt_neldermead",
    progress = FALSE
  )
  palette2 <- generate_palette(
    n = 3,
    optimizer = "nlopt_neldermead",
    progress = FALSE
  )

  # Results should exist and be valid palettes
  expect_equal(length(palette1), 3)
  expect_equal(length(palette2), 3)

  # While not perfectly deterministic, should produce similar quality results
  metrics1 <- attr(palette1, "metrics")
  metrics2 <- attr(palette2, "metrics")

  # Both should have reasonable performance (within expected local optimizer variation)
  expect_true(metrics1$distances$performance_ratio > 0.5)
  expect_true(metrics2$distances$performance_ratio > 0.5)
})

test_that("generate_palette validates nlopt_neldermead optimizer", {
  # Test that nlopt_neldermead is now a valid optimizer
  expect_no_error(
    generate_palette(n = 3, optimizer = "nlopt_neldermead", progress = FALSE)
  )
})

test_that("generate_palette nlopt_neldermead optimizer works with all parameters", {
  # Test nlopt_neldermead optimizer with full parameter set
  palette <- generate_palette(
    n = 5,
    include_colors = c("#FF5733"),
    initialization = "k-means++",
    init_lightness_bounds = c(0.3, 0.8),
    fixed_aesthetic_influence = 0.5,
    max_iterations = 200,
    return_metrics = TRUE,
    progress = FALSE,
    optimizer = "nlopt_neldermead"
  )

  expect_equal(length(palette), 5)
  expect_true("#FF5733" %in% palette)
  expect_true("metrics" %in% names(attributes(palette)))
  expect_true("optimization_details" %in% names(attributes(palette)))
})

test_that("generate_palette nlopt_neldermead optimizer provides robust local optimization", {
  # Generate multiple palettes with nlopt_neldermead and verify they complete successfully
  for (i in 1:3) {
    palette <- generate_palette(
      n = 4,
      max_iterations = 100,
      optimizer = "nlopt_neldermead",
      progress = FALSE
    )

    expect_equal(length(palette), 4)
    expect_true(inherits(palette, "huerd_palette"))

    # Should have optimization details
    details <- attr(palette, "optimization_details")
    expect_true(is.list(details))
    expect_true(is.numeric(details$iterations))
    expect_true(is.numeric(details$final_objective_value))

    # Should have valid metrics
    metrics <- attr(palette, "metrics")
    expect_true(is.list(metrics))
    expect_true(metrics$distances$min > 0)
  }
})

test_that("generate_palette stores generation metadata", {
  palette <- generate_palette(n = 3, progress = FALSE)

  expect_true("generation_metadata" %in% names(attributes(palette)))

  metadata <- attr(palette, "generation_metadata")
  expect_true(is.list(metadata))

  required_fields <- c(
    "n_colors",
    "include_colors",
    "initialization",
    "init_lightness_bounds",
    "init_hcl_bounds",
    "fixed_aesthetic_influence",
    "aesthetic_init_config",
    "max_iterations",
    "return_metrics",
    "weights",
    "optimizer",
    "seed",
    "package_version",
    "target_space",
    "timestamp"
  )

  for (field in required_fields) {
    expect_true(field %in% names(metadata))
  }

  expect_equal(metadata$n_colors, 3)
  expect_equal(metadata$target_space, "oklab")
  expect_equal(metadata$optimizer, "nloptr_cobyla")
  expect_true(inherits(metadata$package_version, "package_version"))
  expect_true(inherits(metadata$timestamp, "POSIXct"))
})

test_that("generate_palette metadata includes custom parameters", {
  palette <- generate_palette(
    n = 3,
    initialization = "harmony",
    init_lightness_bounds = c(0.3, 0.8),
    optimizer = "nlopt_direct",
    progress = FALSE
  )

  metadata <- attr(palette, "generation_metadata")
  expect_equal(metadata$initialization, "harmony")
  expect_equal(metadata$init_lightness_bounds, c(0.3, 0.8))
  expect_equal(metadata$optimizer, "nlopt_direct")
})

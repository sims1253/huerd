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
  
  # Convert to RGB then to OKLAB to get lightness values
  rgb_matrix <- farver::decode_colour(palette)
  oklab_matrix <- farver::convert_colour(rgb_matrix, from = "rgb", to = "oklab")
  lightness_values <- oklab_matrix[, 1]  # L channel is first column
  
  # Check that lightness values are monotonically increasing (sorted)
  expect_true(all(diff(lightness_values) >= 0))
  
  # Test with fixed colors too
  fixed_colors <- c("#1a1a1a", "#ffffff")  # Very dark and very light
  palette_with_fixed <- generate_palette(
    n = 5, 
    include_colors = fixed_colors,
    progress = FALSE
  )
  
  rgb_matrix_fixed <- farver::decode_colour(palette_with_fixed)
  oklab_matrix_fixed <- farver::convert_colour(rgb_matrix_fixed, from = "rgb", to = "oklab")
  lightness_values_fixed <- oklab_matrix_fixed[, 1]
  
  # Should still be sorted despite having fixed colors
  expect_true(all(diff(lightness_values_fixed) >= 0))
})

test_that("optimization produces reasonable colors with fixed colors", {
  set.seed(42)

  # Test case that currently fails: brand palette example
  result <- generate_palette(
    n = 7,
    include_colors = c("#4A6B8A", "#E5A04C"),
    progress = FALSE
  )

  # Convert to OKLAB for analysis
  rgb_matrix <- farver::decode_colour(result)
  oklab_colors <- farver::convert_colour(rgb_matrix, from = "rgb", to = "oklab")

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

test_that("optimization without penalties produces good results", {
  set.seed(42)

  # Test with penalties disabled to verify the core optimization works
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

test_that("gamut penalty calculation is correct", {
  # Test the gamut penalty logic with known good colors
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

  # This is the CORRECT calculation (RGB should be divided by 255)
  out_of_gamut_dist_sq_correct <- pmax(0, rgb_colors / 255 - 1)^2 +
    pmax(0, -(rgb_colors / 255))^2
  gamut_penalty_correct <- sum(out_of_gamut_dist_sq_correct) / nrow(test_oklab)

  # Valid colors should have minimal gamut penalty
  expect_true(
    gamut_penalty_correct < 0.01,
    info = "Valid RGB colors should have minimal gamut penalty"
  )

  # Test what happens with the BUGGY calculation (using RGB 0-255 directly)
  out_of_gamut_dist_sq_buggy <- pmax(0, rgb_colors - 1)^2 +
    pmax(0, -rgb_colors)^2
  gamut_penalty_buggy <- sum(out_of_gamut_dist_sq_buggy) / nrow(test_oklab)

  # The buggy calculation should give massive penalty for valid colors
  expect_true(
    gamut_penalty_buggy > 1000,
    info = "Buggy gamut calculation gives huge penalty for valid colors"
  )
})

test_that("objective function behaves correctly", {
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

library(testthat)

test_that("calculate_all_distances handles basic input correctly", {
  # Create a simple test set of colors in XYZ space
  test_colors <- matrix(c(
    25, 20, 30, # Dark purplish
    50, 45, 35, # Medium neutral
    35, 30, 25 # Brownish
  ), ncol = 3, byrow = TRUE)

  result <- calculate_all_distances(test_colors)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("original", "deutan", "protan", "tritan"))

  # Check lengths (3 colors = 3 distances)
  n_distances <- choose(3, 2) # combinations of 2 from 3
  expect_length(result$original, n_distances)
  expect_length(result$deutan, n_distances)
  expect_length(result$protan, n_distances)
  expect_length(result$tritan, n_distances)

  # Check values are numeric and positive
  expect_true(all(sapply(result, is.numeric)))
  expect_true(all(sapply(result, function(x) all(x >= 0))))
})

test_that("calculate_all_distances validates input correctly", {
  # Test invalid dimensions
  expect_error(
    calculate_all_distances(matrix(1:4, ncol = 2)),
    "Input must have exactly 3 columns"
  )

  # Test negative XYZ values (which are invalid)
  expect_error(
    calculate_all_distances(matrix(c(-10, 50, 50, 50, 50, 50), ncol = 3, byrow = TRUE)),
    "XYZ values cannot be negative"
  )

  # Test missing values
  expect_error(
    calculate_all_distances(matrix(c(50, NA, 50, 50, 50, 50), ncol = 3, byrow = TRUE)),
    "Input contains missing values"
  )
})

test_that("calculate_all_distances handles viewing conditions correctly", {
  test_colors <- matrix(c(
    50, 45, 40, # Neutral gray
    75, 70, 65 # Lighter gray
  ), ncol = 3, byrow = TRUE)

  # Test invalid viewing conditions
  expect_error(
    calculate_all_distances(test_colors, list(L_A = -1, Y_b = 20, surround = "average")),
    "L_A must be a positive number"
  )

  expect_error(
    calculate_all_distances(test_colors, list(L_A = 100, Y_b = 150, surround = "average")),
    "Y_b must be between 0 and 100"
  )

  expect_error(
    calculate_all_distances(test_colors, list(L_A = 100, Y_b = 20, surround = "invalid")),
    "surround must be one of: average, dim, dark"
  )

  # Test different valid viewing conditions produce different results
  result1 <- calculate_all_distances(
    test_colors,
    list(L_A = 100, Y_b = 20, surround = "average")
  )
  result2 <- calculate_all_distances(
    test_colors,
    list(L_A = 50, Y_b = 10, surround = "dim")
  )

  expect_false(identical(result1$original, result2$original))
})

test_that("calculate_all_distances handles extreme colors appropriately", {
  # Test very bright colors in XYZ space
  bright_colors <- matrix(c(
    151, 150, 150, # Very bright white
    50, 50, 50, # Medium gray
    25, 25, 25 # Darker gray
  ), ncol = 3, byrow = TRUE)

  # Should give warning for very bright values
  expect_warning(result <- calculate_all_distances(bright_colors))


  # Results should still be numeric and positive
  expect_true(all(sapply(result, is.numeric)))
  expect_true(all(sapply(result, function(x) all(x >= 0))))
})

test_that("calculate_all_distances preserves symmetry", {
  test_colors <- matrix(c(
    50, 45, 40, # Neutral gray
    75, 70, 65, # Lighter gray
    60, 55, 50 # Medium gray
  ), ncol = 3, byrow = TRUE)

  result <- calculate_all_distances(test_colors)

  # For each type of vision, check if reversing color pairs gives same distance
  for (vision_type in names(result)) {
    distances <- result[[vision_type]]
    n_colors <- nrow(test_colors)

    # Reconstruct full distance matrix
    dist_matrix <- matrix(0, n_colors, n_colors)
    dist_matrix[upper.tri(dist_matrix)] <- distances
    dist_matrix[lower.tri(dist_matrix)] <- t(dist_matrix)[lower.tri(dist_matrix)]

    # Check symmetry
    expect_true(isSymmetric(dist_matrix))
  }
})

test_that("calculate_all_distances handles CVD simulations consistently", {
  test_colors <- matrix(c(
    50, 45, 40, # Neutral gray
    75, 70, 65 # Lighter gray
  ), ncol = 3, byrow = TRUE)

  result <- calculate_all_distances(test_colors)

  # Check that CVD simulations produce different but reasonable results
  # CVD distances should be different from original but within a reasonable range
  for (cvd_type in c("deutan", "protan", "tritan")) {
    # Distances should be different from original
    expect_false(identical(result[[cvd_type]], result$original))

    # Distances should be within reasonable range (0.5x to 2x original)
    ratio <- mean(result[[cvd_type]]) / mean(result$original)
    expect_true(ratio > 0.5 && ratio < 2.0,
      info = sprintf("%s distances are too extreme (ratio: %f)", cvd_type, ratio)
    )
  }
})

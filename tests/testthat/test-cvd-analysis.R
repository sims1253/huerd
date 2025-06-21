test_that("is_cvd_safe handles single color", {
  color <- "#FF0000"

  result <- is_cvd_safe(color)

  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("is_cvd_safe handles empty input", {
  result <- is_cvd_safe(character(0))

  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("is_cvd_safe returns logical value for multiple colors", {
  colors <- c("#FF0000", "#00FF00", "#0000FF")

  result <- is_cvd_safe(colors)

  expect_type(result, "logical")
  expect_length(result, 1)
})

# TODO: Investigate if threshold parameter should be supported
# test_that("is_cvd_safe accepts threshold parameter", {
#   color <- "#FF0000"
#
#   result1 <- is_cvd_safe(color, threshold = 0.1)
#   result2 <- is_cvd_safe(color, threshold = 0.01)
#
#   expect_type(result1, "logical")
#   expect_type(result2, "logical")
#   expect_length(result1, 1)
#   expect_length(result2, 1)
# })

# Tests for simulate_palette_cvd expected behavior
test_that("simulate_palette_cvd returns colors for single CVD type", {
  colors <- c("#FF0000", "#00FF00", "#0000FF")

  result <- simulate_palette_cvd(colors, cvd_type = "deutan")

  expect_true(is.character(result))
  expect_equal(length(result), 3)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))
  expect_true(inherits(result, "huerd_simulation_result"))
})

test_that("simulate_palette_cvd handles different CVD types", {
  colors <- c("#FF0000", "#00FF00")

  deutan_result <- simulate_palette_cvd(colors, cvd_type = "deutan")
  protan_result <- simulate_palette_cvd(colors, cvd_type = "protan")
  tritan_result <- simulate_palette_cvd(colors, cvd_type = "tritan")

  expect_true(is.character(deutan_result))
  expect_true(is.character(protan_result))
  expect_true(is.character(tritan_result))
  expect_equal(length(deutan_result), 2)
  expect_equal(length(protan_result), 2)
  expect_equal(length(tritan_result), 2)
})

test_that("simulate_palette_cvd returns list for all CVD types", {
  colors <- c("#FF0000", "#00FF00")

  result <- simulate_palette_cvd(colors, cvd_type = "all")

  expect_true(is.list(result))
  expect_true(inherits(result, "huerd_simulation_result"))
  expect_true("original" %in% names(result))
  expect_true("deutan" %in% names(result))
  expect_true("protan" %in% names(result))
  expect_true("tritan" %in% names(result))
})

test_that("simulate_palette_cvd handles severity parameter", {
  colors <- c("#FF0000")

  mild_result <- simulate_palette_cvd(
    colors,
    cvd_type = "deutan",
    severity = 0.5
  )
  complete_result <- simulate_palette_cvd(
    colors,
    cvd_type = "deutan",
    severity = 1.0
  )

  expect_true(is.character(mild_result))
  expect_true(is.character(complete_result))
  expect_equal(length(mild_result), 1)
  expect_equal(length(complete_result), 1)
  expect_true(inherits(mild_result, "huerd_simulation_result"))
  expect_true(inherits(complete_result, "huerd_simulation_result"))
})

test_that("simulate_palette_cvd handles empty input", {
  expect_warning(
    {
      result <- simulate_palette_cvd(character(0), cvd_type = "deutan")
    },
    "Input 'colors' contains no valid colors"
  )

  expect_true(is.character(result))
  expect_equal(length(result), 0)
  expect_true(inherits(result, "huerd_simulation_result"))
})

# Tests for plot_cvd_comparison expected behavior
test_that("plot_cvd_comparison accepts CVD simulation results", {
  colors <- c("#FF0000", "#00FF00")
  sim_results <- simulate_palette_cvd(colors, cvd_type = "all")

  # Should not error when given proper simulation results
  expect_silent(plot_cvd_comparison(sim_results))
})

# ERROR CONDITION TESTS - Testing uncovered error paths in CVD functions

test_that("simulate_palette_cvd handles invalid hex color formats", {
  # Test invalid hex formats (line 47)
  invalid_hex_colors <- c("#INVALID", "#ZZZ", "red", "#12345", "#GGGGGG")

  expect_error(
    simulate_palette_cvd(invalid_hex_colors),
    "colors must be a character vector of valid hex codes or NA."
  )

  # Test mixed valid and invalid hex colors
  mixed_colors <- c("#FF0000", "#INVALID", "#00FF00")
  expect_error(
    simulate_palette_cvd(mixed_colors),
    "colors must be a character vector of valid hex codes or NA."
  )
})

test_that("simulate_palette_cvd handles empty colors after filtering", {
  # Test all NA vector (lines 54-59 for single CVD type)
  all_na_colors <- c(NA_character_, NA_character_, NA_character_)

  # Test single CVD type - should warn about no valid colors
  expect_warning(
    {
      result1 <- simulate_palette_cvd(all_na_colors, cvd_type = "protan")
    },
    "Input 'colors' contains no valid colors"
  )

  expect_true(is.character(result1))
  expect_equal(length(result1), 0)
  expect_true(inherits(result1, "huerd_simulation_result"))
  expect_equal(attr(result1, "cvd_type"), "protan")

  # Test "all" CVD types - should also warn about no valid colors
  expect_warning(
    {
      result2 <- simulate_palette_cvd(all_na_colors, cvd_type = "all")
    },
    "Input 'colors' contains no valid colors"
  )

  expect_true(is.list(result2))
  expect_true(inherits(result2, "huerd_simulation_result"))
  expect_equal(attr(result2, "cvd_type"), "all")
  expect_true("original" %in% names(result2))
  expect_equal(length(result2$original), 0)
})

test_that("simulate_palette_cvd handles invalid severity values", {
  colors <- c("#FF0000", "#00FF00", "#0000FF")

  # Test severity < 0 (line 70)
  expect_error(
    simulate_palette_cvd(colors, severity = -0.1),
    "severity must be a number between 0 and 1."
  )

  # Test severity > 1 (line 70)
  expect_error(
    simulate_palette_cvd(colors, severity = 1.5),
    "severity must be a number between 0 and 1."
  )

  # Test non-numeric severity (line 70)
  expect_error(
    simulate_palette_cvd(colors, severity = "invalid"),
    "severity must be a number between 0 and 1."
  )

  # Test NA severity (line 70)
  expect_error(
    simulate_palette_cvd(colors, severity = NA),
    "severity must be a number between 0 and 1."
  )
})

test_that("plot_cvd_comparison handles single CVD type results", {
  colors <- c("#FF0000", "#00FF00")
  deutan_result <- simulate_palette_cvd(colors, cvd_type = "deutan")

  # Should handle single CVD type results by creating comparison
  expect_silent(plot_cvd_comparison(list(
    original = colors,
    simulated = deutan_result
  )))
})

test_that("plot_cvd_comparison handles empty input gracefully", {
  # Should handle empty results without error but may produce informational message
  expect_message(
    {
      expect_no_error(plot_cvd_comparison(list(original = character(0))))
    },
    "Cannot plot CVD comparison"
  )
})

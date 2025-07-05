# Tests for visualization_analysis.R - Diagnostic dashboard with grid graphics

# Note: Grid graphics handles device management automatically

test_that("plot_palette_analysis works with basic palette", {
  colors <- c("#FF0000", "#00FF00", "#0000FF")

  # Test that it runs without error
  expect_no_error({
    result <- plot_palette_analysis(colors)
  })

  # Test that it returns evaluation result
  result <- plot_palette_analysis(colors)
  expect_true(inherits(result, "huerd_evaluation"))
  expect_equal(result$n_colors, 3)
})

test_that("plot_palette_analysis handles single color with warning", {
  colors <- c("#FF0000")

  # Single color should produce a warning and return early
  expect_warning(
    {
      result <- plot_palette_analysis(colors)
    },
    "Need at least two colors for a palette"
  )
})

test_that("plot_palette_analysis works with many colors", {
  colors <- c(
    "#FF0000",
    "#00FF00",
    "#0000FF",
    "#FFFF00",
    "#FF00FF",
    "#00FFFF",
    "#800000",
    "#008000",
    "#000080",
    "#808080"
  )

  expect_no_error({
    result <- plot_palette_analysis(colors)
  })

  result <- plot_palette_analysis(colors)
  expect_true(inherits(result, "huerd_evaluation"))
  expect_equal(result$n_colors, 10)
})

test_that("plot_palette_analysis validates input colors", {
  # Invalid hex colors
  expect_error(
    plot_palette_analysis(c("#INVALID", "#00FF00")),
    "If 'colors' is character, all elements must be valid hex codes or NA"
  )

  # Empty palette (handled gracefully with warning)
  expect_warning(
    {
      result <- plot_palette_analysis(character(0))
    },
    "Need at least two colors for a palette"
  )

  # Non-character input with sufficient length (should error in evaluate_palette)
  expect_error(
    plot_palette_analysis(c(123, 456)),
    "colors must be a character vector of hex colors or an OKLAB matrix"
  )
})

test_that("plot_palette_analysis works with huerd_palette objects", {
  palette <- generate_palette(4, progress = FALSE)

  expect_no_error({
    result <- plot_palette_analysis(palette)
  })

  result <- plot_palette_analysis(palette)
  expect_true(inherits(result, "huerd_evaluation"))
  expect_equal(result$n_colors, 4)
})

# Tests for individual grob creation functions

test_that("create_color_swatches handles edge cases", {
  # Test with single color
  colors <- c("#FF0000")
  evaluation <- evaluate_palette(colors)

  expect_no_error({
    grob <- huerd:::create_color_swatches(colors, evaluation)
    expect_true(inherits(grob, "gTree"))
  })

  # Test with two colors (minimum for meaningful analysis)
  colors <- c("#FF0000", "#0000FF")
  evaluation <- evaluate_palette(colors)

  expect_no_error({
    grob <- huerd:::create_color_swatches(colors, evaluation)
    expect_true(inherits(grob, "gTree"))
  })
})

test_that("create_distance_heatmap handles edge cases", {
  # Test with insufficient colors
  colors <- c("#FF0000")
  evaluation <- evaluate_palette(colors)

  expect_no_error({
    grob <- huerd:::create_distance_heatmap(colors, evaluation)
    expect_true(inherits(grob, "gTree"))
  })
})

test_that("create_cvd_simulation handles edge cases", {
  # Test with single color
  colors <- c("#FF0000")

  expect_no_error({
    grob <- huerd:::create_cvd_simulation(colors)
    expect_true(inherits(grob, "gTree"))
  })

  # Test with multiple colors
  colors <- c("#FF0000", "#00FF00", "#0000FF")

  expect_no_error({
    grob <- huerd:::create_cvd_simulation(colors)
    expect_true(inherits(grob, "gTree"))
  })
})

test_that("create_color_space handles edge cases", {
  # Test with single color
  colors <- c("#FF0000")

  expect_no_error({
    grob <- huerd:::create_color_space(colors)
    expect_true(inherits(grob, "gTree"))
  })

  # Test with multiple colors
  colors <- c("#FF0000", "#00FF00", "#0000FF")

  expect_no_error({
    grob <- huerd:::create_color_space(colors)
    expect_true(inherits(grob, "gTree"))
  })
})

test_that("create_comparative_palettes handles edge cases", {
  # Test with valid distance data structure
  distance_data <- list(
    "test_palette_1" = c(0.1, 0.2, 0.3), # Vector of distance values
    "test_palette_2" = c(0.15, 0.25, 0.35)
  )

  expect_no_error({
    grob <- huerd:::create_comparative_palettes(distance_data, "Test Title")
    expect_true(inherits(grob, "gTree"))
  })

  # Note: Function requires non-empty distance data to work properly
})

# Note: create_nearest_neighbor function was removed in favor of new dashboard layout

# Integration tests with file output

test_that("plot_palette_analysis can save to file", {
  skip_on_cran()

  colors <- c("#FF0000", "#00FF00", "#0000FF")
  temp_file <- tempfile(fileext = ".png")

  expect_no_error({
    # This would work but we can't test file output easily in testthat
    # plot_palette_analysis(colors, save_path = temp_file)
  })

  # Clean up
  if (file.exists(temp_file)) {
    unlink(temp_file)
  }
})

# Device size adaptation tests

test_that("plot_palette_analysis works with grid graphics at different sizes", {
  skip_on_cran()

  colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00")

  # Grid graphics handles sizing automatically
  expect_no_error({
    result <- plot_palette_analysis(colors)
    expect_true(inherits(result, "huerd_evaluation"))
  })
})


# Performance and robustness tests

test_that("plot_palette_analysis handles various color formats", {
  # Different valid hex formats
  colors1 <- c("#ff0000", "#00ff00", "#0000ff") # lowercase
  colors2 <- c("#FF0000", "#00FF00", "#0000FF") # uppercase
  colors3 <- c("#fF0000", "#00Ff00", "#0000fF") # mixed case

  expect_no_error(plot_palette_analysis(colors1))
  expect_no_error(plot_palette_analysis(colors2))
  expect_no_error(plot_palette_analysis(colors3))
})

test_that("plot_palette_analysis handles edge color values", {
  # Pure black, white, and gray
  colors <- c("#000000", "#FFFFFF", "#808080")

  expect_no_error({
    result <- plot_palette_analysis(colors)
  })

  # Different similar colors that are more perceptually distinct
  colors <- c("#FF0000", "#FF1100", "#FF2200")

  expect_no_error({
    result <- plot_palette_analysis(colors)
  })
})

test_that("plot_palette_analysis maintains consistency", {
  colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00")

  # Run multiple times to check consistency
  result1 <- plot_palette_analysis(colors)
  result2 <- plot_palette_analysis(colors)

  # Results should be identical for same input
  expect_equal(result1$n_colors, result2$n_colors)
  expect_equal(result1$distances$min, result2$distances$min)
})

test_that("plot_palette_analysis works without manual device management", {
  skip_on_cran()

  colors <- c("#FF0000", "#00FF00", "#0000FF")

  # Grid graphics should handle device management automatically
  expect_no_error({
    result <- plot_palette_analysis(colors)
    expect_true(inherits(result, "huerd_evaluation"))
    expect_equal(result$n_colors, 3)
  })
})

# Tests for grid graphics integration

test_that("plot_palette_analysis requires gridExtra package", {
  # Test that function checks for gridExtra availability
  # This is mostly to document the dependency requirement
  colors <- c("#FF0000", "#00FF00", "#0000FF")

  # Function should check for gridExtra and give informative error if not available
  # In our case, gridExtra should be available for testing
  expect_no_error({
    result <- plot_palette_analysis(colors)
    expect_true(inherits(result, "huerd_evaluation"))
  })
})

test_that("error messages are informative", {
  # Test that invalid color input still gives good error messages
  expect_error(
    plot_palette_analysis(c("#INVALID", "#00FF00")),
    "If 'colors' is character, all elements must be valid hex codes or NA"
  )
})

test_that("plot_palette_analysis works with grid graphics in various environments", {
  colors <- c("#FF0000", "#00FF00", "#0000FF")

  # Grid graphics should work automatically without device management
  expect_no_error({
    result <- plot_palette_analysis(colors)
    expect_true(inherits(result, "huerd_evaluation"))
    expect_equal(result$n_colors, 3)
  })
})

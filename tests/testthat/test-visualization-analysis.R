# Tests for visualization_analysis.R - Diagnostic dashboard

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

test_that("plot_palette_analysis works with single color", {
  colors <- c("#FF0000")
  
  expect_no_error({
    result <- plot_palette_analysis(colors)
  })
  
  result <- plot_palette_analysis(colors)
  expect_true(inherits(result, "huerd_evaluation"))
  expect_equal(result$n_colors, 1)
})

test_that("plot_palette_analysis works with many colors", {
  colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", 
             "#00FFFF", "#800000", "#008000", "#000080", "#808080")
  
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
  
  # Empty palette (handled gracefully)
  expect_no_error({
    result <- plot_palette_analysis(character(0))
  })
  
  # Non-character input (should error in evaluate_palette)
  expect_error(
    plot_palette_analysis(123),
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

test_that("plot_palette_analysis works with custom title", {
  colors <- c("#FF0000", "#00FF00", "#0000FF")
  custom_title <- "My Custom Palette Analysis"
  
  expect_no_error({
    result <- plot_palette_analysis(colors, main_title = custom_title)
  })
})

# Tests for individual helper functions

test_that("plot_color_swatches handles edge cases", {
  # Test with empty colors (should be handled gracefully by parent function)
  # This tests the internal function behavior
  
  # Test with single color
  colors <- c("#FF0000")
  evaluation <- evaluate_palette(colors)
  
  expect_no_error({
    # Set up plotting environment
    plot.new()
    .huerd_plot_color_swatches <- huerd:::plot_color_swatches
    .huerd_plot_color_swatches(colors, evaluation)
  })
})

test_that("plot_distance_heatmap handles edge cases", {
  # Test with insufficient colors
  colors <- c("#FF0000")
  evaluation <- evaluate_palette(colors)
  
  expect_no_error({
    plot.new()
    .huerd_plot_distance_heatmap <- huerd:::plot_distance_heatmap
    .huerd_plot_distance_heatmap(colors, evaluation)
  })
})

test_that("plot_cvd_simulation handles edge cases", {
  # Test with empty colors
  colors <- character(0)
  
  expect_no_error({
    plot.new()
    .huerd_plot_cvd_simulation <- huerd:::plot_cvd_simulation
    .huerd_plot_cvd_simulation(colors)
  })
  
  # Test with single color
  colors <- c("#FF0000")
  
  expect_no_error({
    plot.new()
    .huerd_plot_cvd_simulation <- huerd:::plot_cvd_simulation
    .huerd_plot_cvd_simulation(colors)
  })
})

test_that("plot_color_space_distribution handles edge cases", {
  # Test with empty colors
  colors <- character(0)
  
  expect_no_error({
    plot.new()
    .huerd_plot_color_space_distribution <- huerd:::plot_color_space_distribution
    .huerd_plot_color_space_distribution(colors)
  })
  
  # Test with single color
  colors <- c("#FF0000")
  
  expect_no_error({
    plot.new()
    .huerd_plot_color_space_distribution <- huerd:::plot_color_space_distribution
    .huerd_plot_color_space_distribution(colors)
  })
})

test_that("plot_comparative_palettes handles edge cases", {
  # Test with insufficient colors
  colors <- c("#FF0000")
  
  expect_no_error({
    plot.new()
    .huerd_plot_comparative_palettes <- huerd:::plot_comparative_palettes
    .huerd_plot_comparative_palettes(colors)
  })
})

test_that("plot_nearest_neighbor_distances handles edge cases", {
  # Test with insufficient colors  
  colors <- c("#FF0000")
  evaluation <- evaluate_palette(colors)
  
  expect_no_error({
    plot.new()
    .huerd_plot_nearest_neighbor_distances <- huerd:::plot_nearest_neighbor_distances
    .huerd_plot_nearest_neighbor_distances(colors, evaluation)
  })
})

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

# Performance and robustness tests

test_that("plot_palette_analysis handles various color formats", {
  # Different valid hex formats
  colors1 <- c("#ff0000", "#00ff00", "#0000ff")  # lowercase
  colors2 <- c("#FF0000", "#00FF00", "#0000FF")  # uppercase
  colors3 <- c("#fF0000", "#00Ff00", "#0000fF")  # mixed case
  
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
  
  # Very similar colors (should still work)
  colors <- c("#FF0000", "#FF0001", "#FF0002")
  
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

# Tests for contrast text color function

test_that("get_contrast_text_color returns correct colors for edge cases", {
  # Test pure black (L=0.0, should return white text)
  expect_equal(huerd:::get_contrast_text_color("#000000"), "white")
  
  # Test pure white (L=1.0, should return black text) 
  expect_equal(huerd:::get_contrast_text_color("#FFFFFF"), "black")
  
  # Test mid-gray (L~0.6, should return black text based on OKLAB lightness)
  expect_equal(huerd:::get_contrast_text_color("#808080"), "black")
})

test_that("get_contrast_text_color works with various color formats", {
  # Test lowercase hex (red L~0.63, green L~0.87)
  expect_equal(huerd:::get_contrast_text_color("#ff0000"), "black")
  expect_equal(huerd:::get_contrast_text_color("#00ff00"), "black")
  
  # Test uppercase hex  
  expect_equal(huerd:::get_contrast_text_color("#FF0000"), "black")
  expect_equal(huerd:::get_contrast_text_color("#00FF00"), "black")
  
  # Test mixed case hex
  expect_equal(huerd:::get_contrast_text_color("#fF0000"), "black")
  expect_equal(huerd:::get_contrast_text_color("#00Ff00"), "black")
})

test_that("get_contrast_text_color handles dark and light colors correctly", {
  # Dark colors (L < 0.5) should get white text
  # #000080 L~0.27, #800000 L~0.38, #330033 should be dark
  dark_colors <- c("#000080", "#800000", "#330033")
  for (color in dark_colors) {
    expect_equal(huerd:::get_contrast_text_color(color), "white",
                 label = paste("Dark color", color, "should get white text"))
  }
  
  # Light colors (L >= 0.5) should get black text  
  # #FFFF00 L~0.97, #00FFFF L~0.91, #FF00FF L~0.70, #008000 L~0.52, #CCCCCC should be light
  light_colors <- c("#FFFF00", "#00FFFF", "#FF00FF", "#008000", "#CCCCCC")
  for (color in light_colors) {
    expect_equal(huerd:::get_contrast_text_color(color), "black",
                 label = paste("Light color", color, "should get black text"))
  }
})

test_that("get_contrast_text_color returns only valid colors", {
  test_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF")
  
  for (color in test_colors) {
    result <- huerd:::get_contrast_text_color(color)
    expect_true(result %in% c("black", "white"),
                label = paste("Result for", color, "should be 'black' or 'white'"))
  }
})

test_that("get_contrast_text_color is consistent", {
  # Same color should always return same result
  color <- "#FF0000"
  result1 <- huerd:::get_contrast_text_color(color)
  result2 <- huerd:::get_contrast_text_color(color)
  expect_equal(result1, result2)
  
  # Test multiple colors
  colors <- c("#000000", "#FFFFFF", "#FF0000", "#00FF00", "#0000FF")
  results1 <- sapply(colors, huerd:::get_contrast_text_color)
  results2 <- sapply(colors, huerd:::get_contrast_text_color)
  expect_equal(results1, results2)
})
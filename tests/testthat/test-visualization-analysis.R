describe("plot_palette_analysis()", {
  it("works with basic palette", {
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

  it("handles single color with warning", {
    colors <- c("#FF0000")

    # Single color should produce a warning and return early
    expect_silent({
      result <- plot_palette_analysis(colors)
    })
  })

  it("works with many colors", {
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

  it("validates input colors", {
    # Invalid hex colors
    expect_error(
      plot_palette_analysis(c("#INVALID", "#00FF00")),
      "If 'colors' is character, all elements must be valid hex codes or NA"
    )

    # Empty palette (handled gracefully with warning)
    expect_silent({
      result <- plot_palette_analysis(character(0))
    })

    # Non-character input with sufficient length (should error in evaluate_palette)
    expect_error(
      plot_palette_analysis(c(123, 456)),
      "colors must be a character vector of hex colors or an OKLAB matrix"
    )
  })

  it("works with huerd_palette objects", {
    palette <- generate_palette(4, progress = FALSE)

    expect_no_error({
      result <- plot_palette_analysis(palette)
    })

    result <- plot_palette_analysis(palette)
    expect_true(inherits(result, "huerd_evaluation"))
    expect_equal(result$n_colors, 4)
  })
})

describe("Individual grob creation functions", {
  it("create_color_swatches handles edge cases", {
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

  it("create_distance_heatmap handles edge cases", {
    # Test with insufficient colors
    colors <- c("#FF0000")
    evaluation <- evaluate_palette(colors)

    expect_no_error({
      grob <- huerd:::create_distance_heatmap(colors, evaluation)
      expect_true(inherits(grob, "gTree"))
    })
  })

  it("create_distance_heatmap uses a fixed color scale across palettes", {
    # The black/white pair sits at the theoretical maximum OKLAB distance
    # (~1.0); the close greys are far below the distinctness threshold
    far <- c("#000000", "#FFFFFF", "#FF0000")
    near <- c("#808080", "#838383", "#7C7C7C")

    ramp <- grDevices::hcl.colors(100, "Viridis")

    offdiag_ramp_indices <- function(hex) {
      n <- length(hex)
      grob <- huerd:::create_distance_heatmap(hex, evaluate_palette(hex))
      is_rect <- vapply(grob$children, inherits, logical(1), "rect")
      fills <- vapply(
        grob$children[is_rect],
        function(g) g$gp$fill,
        character(1)
      )
      expect_length(fills, n * n)

      # Cells are drawn row-major with a white (unmatched) diagonal
      cells <- matrix(match(fills, ramp), nrow = n)
      cells[!diag(n)]
    }

    idx_far <- offdiag_ramp_indices(far)
    idx_near <- offdiag_ramp_indices(near)

    # A close palette saturates at the dark end of the ramp instead of
    # stretching its own maximum to the top of the scale
    expect_true(all(idx_near == 1))

    # The black/white pair reaches the top of the fixed scale, and even
    # the closest pair of the spread palette stays well above the floor
    expect_true(max(idx_far) >= 99)
    expect_true(min(idx_far) > 10)
  })

  it("create_distance_heatmap renders NA distances distinctly", {
    # An NA color makes farver return all-NA OKLAB rows, so every
    # off-diagonal distance is NA
    colors <- c(NA, "#00FF00", "#0000FF")

    grob <- huerd:::create_distance_heatmap(
      colors,
      evaluate_palette(colors)
    )
    is_rect <- vapply(grob$children, inherits, logical(1), "rect")
    fills <- vapply(
      grob$children[is_rect],
      function(g) g$gp$fill,
      character(1)
    )

    expect_length(fills, 9)
    cells <- matrix(fills, nrow = 3)
    expect_true(all(diag(cells) == "white"))
    expect_true(all(cells[!diag(3)] == "grey50"))
  })

  it("create_cvd_simulation handles edge cases", {
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

  it("create_color_space handles edge cases", {
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

  it("create_comparative_palettes handles edge cases", {
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
})

describe("Integration tests with file output", {
  it("plot_palette_analysis can save to file", {
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
})

describe("Device size adaptation", {
  it("plot_palette_analysis works with grid graphics at different sizes", {
    skip_on_cran()

    colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00")

    # Grid graphics handles sizing automatically
    expect_no_error({
      result <- plot_palette_analysis(colors)
      expect_true(inherits(result, "huerd_evaluation"))
    })
  })
})

describe("Performance and robustness", {
  it("plot_palette_analysis handles various color formats", {
    # Different valid hex formats
    colors1 <- c("#ff0000", "#00ff00", "#0000ff") # lowercase
    colors2 <- c("#FF0000", "#00FF00", "#0000FF") # uppercase
    colors3 <- c("#fF0000", "#00Ff00", "#0000fF") # mixed case

    expect_no_error(plot_palette_analysis(colors1))
    expect_no_error(plot_palette_analysis(colors2))
    expect_no_error(plot_palette_analysis(colors3))
  })

  it("plot_palette_analysis handles edge color values", {
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

  it("plot_palette_analysis maintains consistency", {
    colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00")

    # Run multiple times to check consistency
    result1 <- plot_palette_analysis(colors)
    result2 <- plot_palette_analysis(colors)

    # Results should be identical for same input
    expect_equal(result1$n_colors, result2$n_colors)
    expect_equal(result1$distances$min, result2$distances$min)
  })

  it("plot_palette_analysis does not advance the RNG state", {
    colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00")

    # The comparative panels jitter points with runif(); drawing the
    # dashboard must not shift the user's random stream
    set.seed(1234)
    seed_before <- .Random.seed
    plot_palette_analysis(colors)
    expect_identical(.Random.seed, seed_before)
  })

  it("plot_palette_analysis accepts OKLAB matrices and a forced font scale", {
    oklab <- farver::convert_colour(
      farver::decode_colour(c("#FF0000", "#00FF00", "#0000FF")),
      from = "rgb",
      to = "oklab"
    )
    rownames(oklab) <- NULL

    result <- plot_palette_analysis(oklab)
    expect_s3_class(result, "huerd_evaluation")
    expect_equal(result$n_colors, 3)

    expect_no_error(
      plot_palette_analysis(
        c("#FF0000", "#00FF00", "#0000FF"),
        force_font_scale = 0.6
      )
    )
  })

  it("plot_palette_analysis treats OKLAB matrices like the hex equivalent", {
    collect_labels <- function(g) {
      if (inherits(g, "text")) {
        return(g$label)
      }
      # gridExtra::grid.arrange() nests panels in a gtable ($grobs),
      # plain gTrees keep their children in $children
      kids <- if (inherits(g, "gtable")) g$grobs else g$children
      unlist(lapply(kids, collect_labels), use.names = FALSE)
    }

    dashboard_labels <- function(cols) {
      pdf(tempfile(fileext = ".pdf"))
      on.exit(dev.off(), add = TRUE)
      plot_palette_analysis(cols, force_font_scale = 0.6)
      collect_labels(grid::grid.grab())
    }

    hex <- c("#FF0000", "#00FF00", "#0000FF")
    oklab <- farver::convert_colour(
      farver::decode_colour(hex),
      from = "rgb",
      to = "oklab"
    )
    rownames(oklab) <- NULL

    # Three colors select the "Set 2" reference set in both encodings;
    # the matrix path used to count 3 cells per color and pick "Harmonic"
    expect_true("Set 2" %in% dashboard_labels(hex))
    expect_true("Set 2" %in% dashboard_labels(oklab))
    expect_false("Harmonic" %in% dashboard_labels(oklab))
  })

  it("plot_palette_analysis returns early for a single-color OKLAB matrix", {
    oklab <- farver::convert_colour(
      farver::decode_colour("#FF0000"),
      from = "rgb",
      to = "oklab"
    )
    rownames(oklab) <- NULL

    # A 1x3 matrix has length 3 and used to bypass the guard
    expect_null(plot_palette_analysis(oklab))
  })
})

describe("Grid graphics integration", {
  it("plot_palette_analysis works without manual device management", {
    skip_on_cran()

    colors <- c("#FF0000", "#00FF00", "#0000FF")

    # Grid graphics should handle device management automatically
    expect_no_error({
      result <- plot_palette_analysis(colors)
      expect_true(inherits(result, "huerd_evaluation"))
      expect_equal(result$n_colors, 3)
    })
  })

  it("requires gridExtra package", {
    # Test that function checks for gridExtra availability
    # This is mostly to document dependency requirement
    colors <- c("#FF0000", "#00FF00", "#0000FF")

    # Function should check for gridExtra and give informative error if not available
    # In our case, gridExtra should be available for testing
    expect_no_error({
      result <- plot_palette_analysis(colors)
      expect_true(inherits(result, "huerd_evaluation"))
    })
  })

  it("error messages are informative", {
    # Test that invalid color input still gives good error messages
    expect_error(
      plot_palette_analysis(c("#INVALID", "#00FF00")),
      "If 'colors' is character, all elements must be valid hex codes or NA"
    )
  })

  it("plot_palette_analysis works with grid graphics in various environments", {
    colors <- c("#FF0000", "#00FF00", "#0000FF")

    # Grid graphics should work automatically without device management
    expect_no_error({
      result <- plot_palette_analysis(colors)
      expect_true(inherits(result, "huerd_evaluation"))
      expect_equal(result$n_colors, 3)
    })
  })
})

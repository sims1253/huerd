# Tests for print_color_vector

describe("print_color_vector()", {
  it("handles empty input", {
    expect_no_error({
      output <- capture.output(print_color_vector(character(0)))
    })
  })

  it("handles valid colors", {
    colors <- c("#FF0000", "#00FF00", "#0000FF")
    expect_no_error({
      output <- capture.output(print_color_vector(colors))
    })
  })
})

# Tests for .get_estimated_max_dist

describe(".get_estimated_max_dist()", {
  it("handles invalid inputs", {
    # n < 2
    expect_true(is.na(.get_estimated_max_dist(1)))
    expect_true(is.na(.get_estimated_max_dist(0)))
    expect_true(is.na(.get_estimated_max_dist(-1)))

    # Wrong length
    expect_true(is.na(.get_estimated_max_dist(c(2, 3))))

    # Not numeric
    expect_true(is.na(.get_estimated_max_dist("2")))
  })

  it("works with valid data", {
    result <- .get_estimated_max_dist(5)
    expect_true(is.numeric(result))
    expect_equal(length(result), 1)
    expect_false(is.na(result))
  })
})

# Tests for new utility functions

describe(".hex_to_oklab()", {
  it("works correctly with single color", {
    hex_color <- "#FF0000"
    result <- .hex_to_oklab(hex_color)
    expect_true(is.matrix(result))
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 1)
    expect_true(all(is.finite(result)))
  })

  it("works correctly with multiple colors", {
    hex_colors <- c("#FF0000", "#00FF00", "#0000FF")
    result <- .hex_to_oklab(hex_colors)
    expect_true(is.matrix(result))
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 3)
    expect_true(all(is.finite(result)))
  })

  it("handles empty vector", {
    result <- .hex_to_oklab(character(0))
    expect_true(is.matrix(result))
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 0)
  })
})

describe(".oklab_to_hex()", {
  it("works correctly with single color", {
    oklab_color <- matrix(c(0.6, 0.2, -0.1), nrow = 1)
    result <- .oklab_to_hex(oklab_color)
    expect_true(is.character(result))
    expect_equal(length(result), 1)
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", result))
  })

  it("works correctly with multiple colors", {
    oklab_colors <- matrix(
      c(0.6, 0.2, -0.1, 0.4, -0.1, 0.3, 0.8, 0.0, 0.0),
      nrow = 3,
      byrow = TRUE
    )
    result <- .oklab_to_hex(oklab_colors)
    expect_true(is.character(result))
    expect_equal(length(result), 3)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))
  })

  it("handles empty matrix", {
    empty_matrix <- matrix(numeric(0), nrow = 0, ncol = 3)
    result <- .oklab_to_hex(empty_matrix)
    expect_true(is.character(result))
    expect_equal(length(result), 0)
  })
})

describe("utility functions", {
  it("are inverses (round-trip)", {
    original_hex <- c("#FF0000", "#00FF00", "#0000FF", "#FFFFFF", "#000000")
    oklab_result <- .hex_to_oklab(original_hex)
    hex_result <- .oklab_to_hex(oklab_result)

    final_oklab <- .hex_to_oklab(hex_result)

    expect_true(all(abs(oklab_result - final_oklab) < 1e-6))
  })

  it("handle edge cases gracefully", {
    boundary_colors <- c("#000000", "#FFFFFF", "#FF0000", "#00FF00", "#0000FF")

    expect_no_error({
      oklab_result <- .hex_to_oklab(boundary_colors)
      hex_result <- .oklab_to_hex(oklab_result)
    })

    hex_colors <- c("#FF0000", "#00FF00", "#0000FF")

    util_result <- .hex_to_oklab(hex_colors)

    farver_result <- farver::convert_colour(
      farver::decode_colour(hex_colors),
      from = "rgb",
      to = "oklab"
    )

    expect_equal(util_result, farver_result)
  })
})

# Tests for print methods - S3 print methods defined in utils.R

describe("print.huerd_palette()", {
  it("runs without error for basic palette", {
    palette <- c("#FF0000", "#00FF00", "#0000FF")
    class(palette) <- c("huerd_palette", class(palette))

    expect_no_error({
      output <- capture.output({
        result <- print(palette)
      })
      expect_identical(result, palette)
    })
  })

  it("handles empty palette", {
    empty_palette <- character(0)
    class(empty_palette) <- c("huerd_palette", class(empty_palette))

    expect_no_error({
      output <- capture.output({
        result <- print(empty_palette)
      })
      expect_identical(result, empty_palette)
    })
  })

  it("handles invalid colors gracefully", {
    invalid_palette <- c("#FF0000", "#INVALID", NA_character_)
    class(invalid_palette) <- c("huerd_palette", class(invalid_palette))

    expect_no_error({
      output <- capture.output(print(invalid_palette))
    })
  })
})

describe("print.huerd_evaluation()", {
  it("works correctly", {
    evaluation <- list(
      n_colors = 3,
      distances = list(
        min = 0.567,
        mean = 0.789,
        median = 0.743,
        sd = 0.123,
        estimated_max = 0.645,
        performance_ratio = 0.88
      ),
      cvd_safety = list(
        worst_case_min_distance = 0.234,
        protan = list(min_distance = 0.345, preserved_ratio = 0.89),
        deutan = list(min_distance = 0.234, preserved_ratio = 0.76),
        tritan = list(min_distance = 0.456, preserved_ratio = 0.92)
      ),
      distribution = list(
        lightness_oklab = list(range = c(0.2, 0.8), mean = 0.5),
        chroma_oklab = list(range = c(0.05, 0.25), mean = 0.15),
        hue_oklab = list(circular_variance = 0.67)
      ),
      summary_heuristic_score = 85
    )
    class(evaluation) <- "huerd_evaluation"

    expect_no_error({
      output <- capture.output({
        result <- print(evaluation)
      })
      expect_identical(result, evaluation)
    })
  })
})

describe("print.huerd_simulation_result()", {
  it("works for single CVD type", {
    cvd_result <- c("#FF6B6B", "#4ECDC4", "#45B7D1")
    class(cvd_result) <- "huerd_simulation_result"
    attr(cvd_result, "cvd_type") <- "deutan"
    attr(cvd_result, "severity") <- 1.0

    expect_no_error({
      output <- capture.output({
        result <- print(cvd_result)
      })
      expect_identical(result, cvd_result)
    })
  })
})

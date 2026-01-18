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

  it("works for list-based path with multiple CVD types", {
    cvd_result <- list(
      original = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
      deutan = c("#FF8888", "#4ECCCC", "#45D1FF"),
      protan = c("#FF6666", "#4EBBBB", "#45CCEE")
    )
    class(cvd_result) <- "huerd_simulation_result"
    attr(cvd_result, "cvd_type") <- "all"
    attr(cvd_result, "severity") <- 1.0

    expect_no_error({
      output <- capture.output({
        result <- print(cvd_result)
      })
      expect_identical(result, cvd_result)
    })
  })
})

# =============================================================================
# ERROR TESTS
# =============================================================================

describe(".hex_to_oklab() - Invalid hex formats", {
  it("rejects invalid hex characters (GGGGGG)", {
    expect_error(.hex_to_oklab("#GGGGGG"))
  })

  it("rejects invalid hex characters (ZZZZZZ)", {
    expect_error(.hex_to_oklab("#ZZZZZZ"))
  })

  it("rejects hex with non-hex alphanumeric characters", {
    expect_error(.hex_to_oklab("#HIJKLM"))
  })

  it("rejects hex with special characters", {
    expect_error(.hex_to_oklab("#!!##@@"))
  })

  it("rejects wrong length (5 characters)", {
    expect_error(.hex_to_oklab("#FFFFF"))
  })

  it("rejects wrong length (7 characters)", {
    expect_error(.hex_to_oklab("#FFFFFF1"))
  })

  it("handles 3-character hex format gracefully", {
    # farver handles 3-character hex by expanding it
    expect_no_error(result <- .hex_to_oklab("#FFF"))
    expect_true(is.matrix(result))
    expect_equal(ncol(result), 3)
  })

  it("handles NA values gracefully", {
    # farver handles NA values - returns matrix with NA
    expect_no_error(result <- .hex_to_oklab(NA_character_))
    expect_true(is.matrix(result))
    expect_true(any(is.na(result)))
  })

  it("rejects vector with invalid colors mixed with valid ones", {
    expect_error(.hex_to_oklab(c("#FF0000", "#GGGGGG", "#0000FF")))
  })

  it("rejects whitespace in hex string", {
    expect_error(.hex_to_oklab("#FF FF00"))
  })

  it("rejects lowercase hex with invalid characters", {
    expect_error(.hex_to_oklab("#gghijk"))
  })
})

describe(".hex_to_oklab() - Edge cases", {
  it("handles all zeros (#000000)", {
    expect_no_error(result <- .hex_to_oklab("#000000"))
    expect_true(is.matrix(result))
    expect_equal(ncol(result), 3)
    expect_true(all(is.finite(result)))
  })

  it("handles all Fs (#FFFFFF)", {
    expect_no_error(result <- .hex_to_oklab("#FFFFFF"))
    expect_true(is.matrix(result))
    expect_equal(ncol(result), 3)
    expect_true(all(is.finite(result)))
  })
})

describe(".oklab_to_hex() - Invalid matrices", {
  it("rejects matrix with wrong number of columns (2)", {
    invalid_matrix <- matrix(c(0.6, 0.2, 0.4, -0.1), nrow = 2)
    expect_error(.oklab_to_hex(invalid_matrix))
  })

  it("handles matrix with 4 columns gracefully", {
    # farver takes only first 3 columns and ignores the rest
    invalid_matrix <- matrix(
      c(0.6, 0.2, -0.1, 0.5, 0.4, -0.1, 0.1, 0.2),
      nrow = 2,
      byrow = TRUE
    )
    expect_no_error(result <- .oklab_to_hex(invalid_matrix))
    expect_true(is.character(result))
    expect_equal(length(result), 2)
  })

  it("handles matrix with 0 rows gracefully", {
    # 0x3 matrix - farver handles empty matrices gracefully
    empty_matrix <- matrix(numeric(0), nrow = 0, ncol = 3)
    expect_no_error(result <- .oklab_to_hex(empty_matrix))
    expect_true(is.character(result))
    expect_equal(length(result), 0)
  })

  it("rejects non-numeric matrix", {
    char_matrix <- matrix(c("0.6", "0.2", "-0.1"), nrow = 1)
    expect_error(.oklab_to_hex(char_matrix))
  })

  it("handles matrix with NA values gracefully", {
    na_matrix <- matrix(c(0.6, NA, -0.1), nrow = 1)
    # farver handles NA values - no error thrown but result may contain NA
    expect_no_error(result <- .oklab_to_hex(na_matrix))
    expect_true(is.character(result))
  })

  it("handles matrix with Inf values gracefully", {
    inf_matrix <- matrix(c(Inf, 0.2, -0.1), nrow = 1)
    # farver handles Inf values - no error thrown
    expect_no_error(result <- .oklab_to_hex(inf_matrix))
    expect_true(is.character(result))
  })

  it("handles matrix with -Inf values gracefully", {
    neg_inf_matrix <- matrix(c(-Inf, 0.2, -0.1), nrow = 1)
    # farver handles -Inf values - no error thrown
    expect_no_error(result <- .oklab_to_hex(neg_inf_matrix))
    expect_true(is.character(result))
  })

  it("handles matrix with NaN values gracefully", {
    nan_matrix <- matrix(c(NaN, 0.2, -0.1), nrow = 1)
    # farver handles NaN values - no error thrown
    expect_no_error(result <- .oklab_to_hex(nan_matrix))
    expect_true(is.character(result))
  })

  it("handles data frame instead of matrix gracefully", {
    df_matrix <- data.frame(L = 0.6, a = 0.2, b = -0.1)
    # farver converts data frames to matrices - no error thrown
    expect_no_error(result <- .oklab_to_hex(df_matrix))
    expect_true(is.character(result))
  })

  it("rejects list instead of matrix", {
    list_input <- list(c(0.6, 0.2, -0.1))
    expect_error(.oklab_to_hex(list_input))
  })

  it("rejects vector instead of matrix", {
    vector_input <- c(0.6, 0.2, -0.1)
    expect_error(.oklab_to_hex(vector_input))
  })

  it("handles matrix with all zeros", {
    zero_matrix <- matrix(c(0, 0, 0), nrow = 1)
    # farver handles zeros - no error thrown, produces valid hex
    expect_no_error(result <- .oklab_to_hex(zero_matrix))
    expect_true(is.character(result))
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", result))
  })

  it("handles matrix with out-of-range OKLAB values", {
    # OKLAB L should be roughly 0-1, a and b roughly -0.5 to 0.5
    # farver handles out-of-range values gracefully
    out_of_range <- matrix(c(2.0, 0.2, -0.1), nrow = 1)
    expect_no_error(result <- .oklab_to_hex(out_of_range))
    expect_true(is.character(result))
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", result))
  })
})

describe(".oklab_to_hex() - Edge cases", {
  it("handles typical OKLAB values", {
    oklab_matrix <- matrix(c(0.6, 0.2, -0.1), nrow = 1)
    expect_no_error(result <- .oklab_to_hex(oklab_matrix))
    expect_true(is.character(result))
    expect_equal(length(result), 1)
  })

  it("handles zero chroma (achromatic colors)", {
    achromatic <- matrix(c(0.5, 0, 0), nrow = 1)
    expect_no_error(result <- .oklab_to_hex(achromatic))
    expect_true(is.character(result))
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", result))
  })
})

describe("print.huerd_palette() - Invalid/missing attributes", {
  it("handles palette without metrics attribute", {
    palette <- c("#FF0000", "#00FF00", "#0000FF")
    class(palette) <- c("huerd_palette", class(palette))
    # Should not fail even without metrics attribute

    expect_no_error({
      output <- capture.output(print(palette))
    })
  })

  it("handles palette with NULL metrics attribute", {
    palette <- c("#FF0000", "#00FF00", "#0000FF")
    class(palette) <- c("huerd_palette", class(palette))
    attr(palette, "metrics") <- NULL

    expect_no_error({
      output <- capture.output(print(palette))
    })
  })

  it("handles palette with NULL optimization_details attribute", {
    palette <- c("#FF0000", "#00FF00", "#0000FF")
    class(palette) <- c("huerd_palette", class(palette))
    attr(palette, "optimization_details") <- NULL

    expect_no_error({
      output <- capture.output(print(palette))
    })
  })

  it("handles palette with invalid metrics (non-huerd_evaluation)", {
    palette <- c("#FF0000", "#00FF00", "#0000FF")
    class(palette) <- c("huerd_palette", class(palette))
    attr(palette, "metrics") <- "not_a_list"

    expect_no_error({
      output <- capture.output(print(palette))
    })
  })

  it("handles palette with malformed optimization_details", {
    palette <- c("#FF0000", "#00FF00", "#0000FF")
    class(palette) <- c("huerd_palette", class(palette))
    attr(palette, "optimization_details") <- list(bad_field = "value")

    expect_no_error({
      output <- capture.output(print(palette))
    })
  })

  it("handles very long palette", {
    long_palette <- rep("#FF0000", 100)
    class(long_palette) <- c("huerd_palette", class(long_palette))

    expect_no_error({
      output <- capture.output(print(long_palette))
    })
  })

  it("handles palette with NA colors", {
    na_palette <- c("#FF0000", NA_character_, "#0000FF")
    class(na_palette) <- c("huerd_palette", class(na_palette))

    expect_no_error({
      output <- capture.output(print(na_palette))
    })
  })

  it("handles palette with all NA colors", {
    all_na_palette <- c(NA_character_, NA_character_)
    class(all_na_palette) <- c("huerd_palette", class(all_na_palette))

    expect_no_error({
      output <- capture.output(print(all_na_palette))
    })
  })

  it("handles palette with invalid hex colors", {
    invalid_palette <- c("#INVALID", "#NOTACOL", "#ZZZZZZ")
    class(invalid_palette) <- c("huerd_palette", class(invalid_palette))

    expect_no_error({
      output <- capture.output(print(invalid_palette))
    })
  })
})

describe("print.huerd_evaluation() - Invalid/missing fields", {
  it("handles minimal evaluation object", {
    minimal_eval <- list(
      n_colors = 3,
      distances = list(
        min = 0.5,
        mean = 0.6,
        median = 0.55,
        sd = 0.1,
        estimated_max = 0.8,
        performance_ratio = 0.625
      ),
      cvd_safety = list(
        worst_case_min_distance = 0.3,
        protan = list(min_distance = 0.4, preserved_ratio = 0.8),
        deutan = list(min_distance = 0.3, preserved_ratio = 0.75),
        tritan = list(min_distance = 0.5, preserved_ratio = 0.9)
      ),
      distribution = list(
        lightness_oklab = list(range = c(0.1, 0.9), mean = 0.5),
        chroma_oklab = list(range = c(0.01, 0.3), mean = 0.15),
        hue_oklab = list(circular_variance = 0.5)
      )
    )
    class(minimal_eval) <- "huerd_evaluation"

    expect_no_error({
      output <- capture.output(print(minimal_eval))
    })
  })

  it("handles missing nested fields gracefully", {
    minimal_eval <- list(
      n_colors = 3
    )
    class(minimal_eval) <- "huerd_evaluation"

    expect_no_error({
      output <- capture.output(print(minimal_eval))
    })
  })

  it("handles n_colors = 1 (limited metrics)", {
    single_eval <- list(
      n_colors = 1,
      distances = list(min = NA, mean = NA, median = NA, sd = NA),
      cvd_safety = list(worst_case_min_distance = NA),
      distribution = list()
    )
    class(single_eval) <- "huerd_evaluation"

    expect_no_error({
      output <- capture.output(print(single_eval))
    })
  })

  it("handles n_colors = 0", {
    empty_eval <- list(
      n_colors = 0,
      distances = list(),
      cvd_safety = list(),
      distribution = list()
    )
    class(empty_eval) <- "huerd_evaluation"

    expect_no_error({
      output <- capture.output(print(empty_eval))
    })
  })

  it("handles NA values in distance metrics", {
    na_eval <- list(
      n_colors = 3,
      distances = list(
        min = NA_real_,
        mean = NA_real_,
        median = NA_real_,
        sd = NA_real_,
        estimated_max = NA_real_,
        performance_ratio = NA_real_
      ),
      cvd_safety = list(
        worst_case_min_distance = NA_real_,
        protan = list(min_distance = NA_real_, preserved_ratio = NA_real_),
        deutan = list(min_distance = NA_real_, preserved_ratio = NA_real_),
        tritan = list(min_distance = NA_real_, preserved_ratio = NA_real_)
      ),
      distribution = list(
        lightness_oklab = list(range = c(NA_real_, NA_real_), mean = NA_real_),
        chroma_oklab = list(range = c(NA_real_, NA_real_), mean = NA_real_),
        hue_oklab = list(circular_variance = NA_real_)
      )
    )
    class(na_eval) <- "huerd_evaluation"

    expect_no_error({
      output <- capture.output(print(na_eval))
    })
  })

  it("handles Inf values in metrics", {
    inf_eval <- list(
      n_colors = 3,
      distances = list(
        min = Inf,
        mean = Inf,
        median = Inf,
        sd = Inf,
        estimated_max = Inf,
        performance_ratio = Inf
      ),
      cvd_safety = list(
        worst_case_min_distance = Inf,
        protan = list(min_distance = Inf, preserved_ratio = Inf),
        deutan = list(min_distance = Inf, preserved_ratio = Inf),
        tritan = list(min_distance = Inf, preserved_ratio = Inf)
      ),
      distribution = list(
        lightness_oklab = list(range = c(0, 1), mean = Inf),
        chroma_oklab = list(range = c(0, 1), mean = Inf),
        hue_oklab = list(circular_variance = Inf)
      )
    )
    class(inf_eval) <- "huerd_evaluation"

    expect_no_error({
      output <- capture.output(print(inf_eval))
    })
  })
})

describe("print.huerd_simulation_result() - Missing/invalid attributes", {
  it("handles missing cvd_type attribute", {
    cvd_result <- c("#FF6B6B", "#4ECDC4", "#45B7D1")
    class(cvd_result) <- "huerd_simulation_result"
    # No cvd_type attribute set

    expect_no_error({
      output <- capture.output(print(cvd_result))
    })
  })

  it("handles missing severity attribute", {
    cvd_result <- c("#FF6B6B", "#4ECDC4", "#45B7D1")
    class(cvd_result) <- "huerd_simulation_result"
    attr(cvd_result, "cvd_type") <- "deutan"
    # No severity attribute set

    expect_no_error({
      output <- capture.output(print(cvd_result))
    })
  })

  it("handles NULL cvd_type attribute", {
    cvd_result <- c("#FF6B6B", "#4ECDC4", "#45B7D1")
    class(cvd_result) <- "huerd_simulation_result"
    attr(cvd_result, "cvd_type") <- NULL

    expect_no_error({
      output <- capture.output(print(cvd_result))
    })
  })

  it("handles NULL severity attribute", {
    cvd_result <- c("#FF6B6B", "#4ECDC4", "#45B7D1")
    class(cvd_result) <- "huerd_simulation_result"
    attr(cvd_result, "cvd_type") <- "deutan"
    attr(cvd_result, "severity") <- NULL

    expect_no_error({
      output <- capture.output(print(cvd_result))
    })
  })

  it("handles NA severity attribute", {
    cvd_result <- c("#FF6B6B", "#4ECDC4", "#45B7D1")
    class(cvd_result) <- "huerd_simulation_result"
    attr(cvd_result, "cvd_type") <- "deutan"
    attr(cvd_result, "severity") <- NA_real_

    expect_no_error({
      output <- capture.output(print(cvd_result))
    })
  })

  it("handles severity > 1", {
    cvd_result <- c("#FF6B6B", "#4ECDC4", "#45B7D1")
    class(cvd_result) <- "huerd_simulation_result"
    attr(cvd_result, "cvd_type") <- "deutan"
    attr(cvd_result, "severity") <- 2.0

    expect_no_error({
      output <- capture.output(print(cvd_result))
    })
  })

  it("handles negative severity", {
    cvd_result <- c("#FF6B6B", "#4ECDC4", "#45B7D1")
    class(cvd_result) <- "huerd_simulation_result"
    attr(cvd_result, "cvd_type") <- "deutan"
    attr(cvd_result, "severity") <- -0.5

    expect_no_error({
      output <- capture.output(print(cvd_result))
    })
  })

  it("handles list with empty palettes", {
    cvd_result <- list(
      original = character(0),
      deutan = character(0)
    )
    class(cvd_result) <- "huerd_simulation_result"
    attr(cvd_result, "cvd_type") <- "all"
    attr(cvd_result, "severity") <- 1.0

    expect_no_error({
      output <- capture.output(print(cvd_result))
    })
  })

  it("handles list with NA colors", {
    cvd_result <- list(
      original = c("#FF0000", NA_character_, "#0000FF"),
      deutan = c("#FF0000", NA_character_, "#0000FF")
    )
    class(cvd_result) <- "huerd_simulation_result"
    attr(cvd_result, "cvd_type") <- "all"
    attr(cvd_result, "severity") <- 1.0

    expect_no_error({
      output <- capture.output(print(cvd_result))
    })
  })

  it("handles unrecognized cvd_type value", {
    cvd_result <- c("#FF6B6B", "#4ECDC4", "#45B7D1")
    class(cvd_result) <- "huerd_simulation_result"
    attr(cvd_result, "cvd_type") <- "unknown_type"
    attr(cvd_result, "severity") <- 1.0

    expect_no_error({
      output <- capture.output(print(cvd_result))
    })
  })

  it("handles empty list result", {
    cvd_result <- list()
    class(cvd_result) <- "huerd_simulation_result"
    attr(cvd_result, "cvd_type") <- "all"
    attr(cvd_result, "severity") <- 1.0

    expect_no_error({
      output <- capture.output(print(cvd_result))
    })
  })
})

describe("Round-trip conversion edge cases", {
  it("handles round-trip for black (#000000)", {
    original <- "#000000"
    oklab_result <- .hex_to_oklab(original)
    hex_result <- .oklab_to_hex(oklab_result)

    expect_no_error(hex_result)
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", hex_result))
  })

  it("handles round-trip for white (#FFFFFF)", {
    original <- "#FFFFFF"
    oklab_result <- .hex_to_oklab(original)
    hex_result <- .oklab_to_hex(oklab_result)

    expect_no_error(hex_result)
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", hex_result))
  })

  it("handles round-trip for pure red (#FF0000)", {
    original <- "#FF0000"
    oklab_result <- .hex_to_oklab(original)
    hex_result <- .oklab_to_hex(oklab_result)

    expect_no_error(hex_result)
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", hex_result))
  })

  it("handles round-trip for pure green (#00FF00)", {
    original <- "#00FF00"
    oklab_result <- .hex_to_oklab(original)
    hex_result <- .oklab_to_hex(oklab_result)

    expect_no_error(hex_result)
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", hex_result))
  })

  it("handles round-trip for pure blue (#0000FF)", {
    original <- "#0000FF"
    oklab_result <- .hex_to_oklab(original)
    hex_result <- .oklab_to_hex(oklab_result)

    expect_no_error(hex_result)
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", hex_result))
  })

  it("handles round-trip for gray tones", {
    gray_colors <- c("#333333", "#666666", "#999999", "#CCCCCC", "#EEEEEE")

    expect_no_error({
      oklab_result <- .hex_to_oklab(gray_colors)
      hex_result <- .oklab_to_hex(oklab_result)
    })

    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", hex_result)))
  })

  it("handles round-trip for similar colors", {
    # Colors that are very close should still convert without error
    similar_colors <- c("#FF0000", "#FF0100", "#FF0200", "#FE0300")

    expect_no_error({
      oklab_result <- .hex_to_oklab(similar_colors)
      hex_result <- .oklab_to_hex(oklab_result)
    })

    expect_equal(length(hex_result), length(similar_colors))
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", hex_result)))
  })

  it("handles round-trip for saturated colors", {
    saturated <- c("#FF00FF", "#00FFFF", "#FFFF00")

    expect_no_error({
      oklab_result <- .hex_to_oklab(saturated)
      hex_result <- .oklab_to_hex(oklab_result)
    })

    expect_equal(length(hex_result), length(saturated))
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", hex_result)))
  })

  it("preserves matrix dimensions after round-trip", {
    original_hex <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF")
    oklab_result <- .hex_to_oklab(original_hex)
    hex_result <- .oklab_to_hex(oklab_result)

    expect_equal(length(hex_result), length(original_hex))
    expect_true(is.character(hex_result))
  })
})

describe("print_color_vector() - Edge cases", {
  it("handles all NA colors", {
    expect_no_error({
      output <- capture.output(print_color_vector(c(
        NA_character_,
        NA_character_
      )))
    })
  })

  it("handles colors with whitespace", {
    expect_no_error({
      output <- capture.output(print_color_vector(c("#FF0000 ", " #00FF00")))
    })
  })

  it("handles very long color strings", {
    long_color <- paste0("#", paste(rep("FF", 6), collapse = ""))
    expect_no_error({
      output <- capture.output(print_color_vector(long_color))
    })
  })
})

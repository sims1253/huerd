# Tests for is_cvd_safe

describe("is_cvd_safe()", {
  it("handles single color", {
    color <- "#FF0000"

    result <- is_cvd_safe(color)

    expect_type(result, "logical")
    expect_length(result, 1)
  })

  it("handles empty input", {
    result <- is_cvd_safe(character(0))

    expect_type(result, "logical")
    expect_length(result, 1)
  })

  it("returns logical value for multiple colors", {
    colors <- c("#FF0000", "#00FF00", "#0000FF")

    result <- is_cvd_safe(colors)

    expect_type(result, "logical")
    expect_length(result, 1)
  })
})

# Tests for simulate_palette_cvd expected behavior

describe("simulate_palette_cvd()", {
  it("returns list for single CVD type", {
    colors <- c("#FF0000", "#00FF00", "#0000FF")

    result <- simulate_palette_cvd(colors, cvd_type = "deutan")

    expect_true(is.list(result))
    expect_true(inherits(result, "huerd_simulation_result"))
    expect_true("deutan" %in% names(result))
    expect_true(is.character(result$deutan))
    expect_equal(length(result$deutan), 3)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result$deutan)))
  })

  it("handles different CVD types", {
    colors <- c("#FF0000", "#00FF00")

    deutan_result <- simulate_palette_cvd(colors, cvd_type = "deutan")
    protan_result <- simulate_palette_cvd(colors, cvd_type = "protan")
    tritan_result <- simulate_palette_cvd(colors, cvd_type = "tritan")

    expect_true(is.list(deutan_result))
    expect_true(is.list(protan_result))
    expect_true(is.list(tritan_result))
    expect_true(is.character(deutan_result$deutan))
    expect_true(is.character(protan_result$protan))
    expect_true(is.character(tritan_result$tritan))
    expect_equal(length(deutan_result$deutan), 2)
    expect_equal(length(protan_result$protan), 2)
    expect_equal(length(tritan_result$tritan), 2)
  })

  it("returns list for all CVD types", {
    colors <- c("#FF0000", "#00FF00")

    result <- simulate_palette_cvd(colors, cvd_type = "all")

    expect_true(is.list(result))
    expect_true(inherits(result, "huerd_simulation_result"))
    expect_true("original" %in% names(result))
    expect_true("deutan" %in% names(result))
    expect_true("protan" %in% names(result))
    expect_true("tritan" %in% names(result))
  })

  it("handles severity parameter", {
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

    expect_true(is.list(mild_result))
    expect_true(is.list(complete_result))
    expect_true(is.character(mild_result$deutan))
    expect_true(is.character(complete_result$deutan))
    expect_equal(length(mild_result$deutan), 1)
    expect_equal(length(complete_result$deutan), 1)
    expect_true(inherits(mild_result, "huerd_simulation_result"))
    expect_true(inherits(complete_result, "huerd_simulation_result"))
  })

  it("handles empty input", {
    expect_silent({
      result <- simulate_palette_cvd(character(0), cvd_type = "deutan")
    })

    expect_true(is.list(result))
    expect_equal(length(result), 1)
    expect_true(inherits(result, "huerd_simulation_result"))
  })
})

# Tests for plot_cvd_comparison expected behavior

describe("plot_cvd_comparison()", {
  it("accepts CVD simulation results", {
    colors <- c("#FF0000", "#00FF00")
    sim_results <- simulate_palette_cvd(colors, cvd_type = "all")

    expect_silent(plot_cvd_comparison(sim_results))
  })

  it("handles single CVD type results", {
    colors <- c("#FF0000", "#00FF00")
    deutan_result <- simulate_palette_cvd(colors, cvd_type = "deutan")

    expect_silent(plot_cvd_comparison(list(
      original = colors,
      simulated = deutan_result$deutan
    )))
  })

  it("handles empty input gracefully", {
    expect_message(
      {
        expect_no_error(plot_cvd_comparison(list(original = character(0))))
      },
      "Cannot plot CVD comparison"
    )
  })
})

# ERROR CONDITION TESTS - Testing uncovered error paths in CVD functions

describe("simulate_palette_cvd() - error conditions", {
  it("handles invalid hex color formats", {
    invalid_hex_colors <- c("#INVALID", "#ZZZ", "red", "#12345", "#GGGGGG")

    expect_error(
      simulate_palette_cvd(invalid_hex_colors),
      "colors must be a character vector of valid hex codes or NA."
    )

    mixed_colors <- c("#FF0000", "#INVALID", "#00FF00")
    expect_error(
      simulate_palette_cvd(mixed_colors),
      "colors must be a character vector of valid hex codes or NA."
    )
  })

  it("handles empty colors after filtering", {
    all_na_colors <- c(NA_character_, NA_character_, NA_character_)

    expect_silent({
      result1 <- simulate_palette_cvd(all_na_colors, cvd_type = "protan")
    })

    expect_true(is.list(result1))
    expect_equal(length(result1), 1)
    expect_true(inherits(result1, "huerd_simulation_result"))
    expect_equal(attr(result1, "cvd_type"), "protan")

    expect_silent({
      result2 <- simulate_palette_cvd(all_na_colors, cvd_type = "all")
    })

    expect_true(is.list(result2))
    expect_true(inherits(result2, "huerd_simulation_result"))
    expect_equal(attr(result2, "cvd_type"), "all")
    expect_true("original" %in% names(result2))
    expect_equal(length(result2$original), 0)
  })

  it("handles invalid severity values", {
    colors <- c("#FF0000", "#00FF00", "#0000FF")

    expect_error(
      simulate_palette_cvd(colors, severity = -0.1),
      "severity must be a number between 0 and 1."
    )

    expect_error(
      simulate_palette_cvd(colors, severity = 1.5),
      "severity must be a number between 0 and 1."
    )

    expect_error(
      simulate_palette_cvd(colors, severity = "invalid"),
      "severity must be a number between 0 and 1."
    )

    expect_error(
      simulate_palette_cvd(colors, severity = NA),
      "severity must be a number between 0 and 1."
    )
  })
})

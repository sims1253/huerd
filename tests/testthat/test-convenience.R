# Tests for convenience functions

describe("quick_palette()", {
  it("generates correct number of colors", {
    pal <- quick_palette(5)
    expect_length(pal, 5)
    expect_s3_class(pal, "huerd_palette")
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
  })

  it("respects brand_colors parameter", {
    brand <- c("#FF0000", "#00FF00")
    pal <- quick_palette(5, brand_colors = brand)

    expect_length(pal, 5)
    expect_true(toupper(brand[1]) %in% toupper(pal))
    expect_true(toupper(brand[2]) %in% toupper(pal))
  })

  it("respects quality parameter", {
    expect_no_error(quick_palette(3, quality = "fast"))
    expect_no_error(quick_palette(3, quality = "balanced"))
    expect_no_error(quick_palette(3, quality = "high"))
  })

  it("respects lightness presets", {
    expect_no_error(quick_palette(3, lightness = "any"))
    expect_no_error(quick_palette(3, lightness = "light"))
    expect_no_error(quick_palette(3, lightness = "dark"))
    expect_no_error(quick_palette(3, lightness = "mid"))
  })

  it("accepts custom lightness bounds", {
    pal <- quick_palette(3, lightness = c(0.4, 0.6))
    expect_length(pal, 3)
  })

  it("errors on invalid lightness", {
    expect_error(quick_palette(3, lightness = "invalid"))
    expect_error(quick_palette(3, lightness = c(0.5)))
  })

  it("honors cvd_safe parameter", {
    set.seed(42)
    pal_safe <- quick_palette(4, cvd_safe = TRUE)
    set.seed(42)
    pal_plain <- quick_palette(4, cvd_safe = FALSE)

    expect_length(pal_safe, 4)
    expect_length(pal_plain, 4)
    # Different objectives converge to different palettes from the same start
    expect_false(identical(as.character(pal_safe), as.character(pal_plain)))
    expect_true(attr(pal_safe, "generation_metadata")$cvd_safe)
    expect_false(attr(pal_plain, "generation_metadata")$cvd_safe)
  })
})


describe("brand_palette()", {
  it("creates palette with brand colors", {
    brand <- c("#003366", "#FF6600")
    pal <- brand_palette(brand, n_total = 5)

    expect_length(pal, 5)
    expect_s3_class(pal, "huerd_palette")
    expect_true(toupper(brand[1]) %in% toupper(pal))
    expect_true(toupper(brand[2]) %in% toupper(pal))
  })

  it("errors when n_total too small", {
    expect_error(
      brand_palette(c("#FF0000", "#00FF00", "#0000FF"), n_total = 2),
      "n_total"
    )
  })

  it("errors on empty brand_colors", {
    expect_error(brand_palette(character(0), n_total = 5))
    expect_error(brand_palette(NULL, n_total = 5))
  })
})


describe("export_palette()", {
  it("returns hex colors by default", {
    pal <- generate_palette(3, progress = FALSE)
    result <- export_palette(pal)

    expect_type(result, "character")
    expect_length(result, 3)
  })

  it("generates CSS format", {
    pal <- generate_palette(3, progress = FALSE)
    result <- export_palette(pal, format = "css")

    expect_type(result, "character")
    expect_true(grepl(":root", result, fixed = TRUE))
    expect_true(grepl("--color_1:", result, fixed = TRUE))
  })

  it("generates Sass format", {
    pal <- generate_palette(3, progress = FALSE)
    result <- export_palette(pal, format = "sass")

    expect_type(result, "character")
    expect_true(grepl("\\$color_1:", result))
  })

  it("generates JSON format", {
    pal <- generate_palette(3, progress = FALSE)
    result <- export_palette(pal, format = "json")

    expect_type(result, "character")
    expect_true(grepl("\\{", result))
    expect_true(grepl("color_1", result, fixed = TRUE))

    # The emitted string must be valid, parseable JSON
    skip_if_not_installed("jsonlite")
    expect_true(jsonlite::validate(result))
    parsed <- jsonlite::fromJSON(result)
    expect_equal(names(parsed), paste0("color_", 1:3))
    expect_equal(unname(unlist(parsed)), as.character(pal))
  })

  it("JSON-escapes quotes, backslashes, and control characters", {
    # RFC 8259: quotes and backslashes get two-character escapes; control
    # characters use named escapes or \u00XX
    names <- c(
      'primary"accent',
      "back\\slash",
      "tab\tchar",
      "newline\nchar",
      paste0("ctrl", intToUtf8(1)),
      paste0("backspace", intToUtf8(8))
    )
    colors <- paste0("#12345", seq_along(names))

    result <- export_palette(colors, format = "json", names = names)

    skip_if_not_installed("jsonlite")
    expect_true(jsonlite::validate(result))
    parsed <- jsonlite::fromJSON(result)
    expect_equal(names(parsed), names)
    expect_equal(unname(unlist(parsed)), colors)
  })

  it("round-trips non-ASCII JSON names", {
    names <- c("primaire", " Hellblau ")
    result <- export_palette(
      c("#112233", "#445566"),
      format = "json",
      names = names
    )

    skip_if_not_installed("jsonlite")
    expect_true(jsonlite::validate(result))
    expect_equal(names(jsonlite::fromJSON(result)), names)
  })

  it("exports the empty palette as the empty JSON object", {
    result <- export_palette(character(0), format = "json")

    expect_equal(result, "{}")
    skip_if_not_installed("jsonlite")
    expect_equal(length(jsonlite::fromJSON(result)), 0)
  })

  it("writes valid JSON with special-character names to file", {
    temp_file <- tempfile(fileext = ".json")
    on.exit(unlink(temp_file))

    export_palette(
      c("#112233", "#445566"),
      format = "json",
      names = c('quoted"name', "ctrl\u0001"),
      file = temp_file
    )

    skip_if_not_installed("jsonlite")
    content <- paste(readLines(temp_file, warn = FALSE), collapse = "\n")
    expect_true(jsonlite::validate(content))
    expect_equal(
      names(jsonlite::fromJSON(content)),
      c('quoted"name', "ctrl\u0001")
    )
  })

  it("errors on NA names", {
    expect_error(
      export_palette(
        c("#112233", "#445566"),
        format = "json",
        names = c("ok", NA_character_)
      ),
      regexp = "NA"
    )
  })

  it("generates CSV format", {
    pal <- generate_palette(3, progress = FALSE)
    result <- export_palette(pal, format = "csv")

    expect_type(result, "character")
    expect_true(grepl("name,hex", result, fixed = TRUE))
  })

  it("respects custom names", {
    pal <- generate_palette(3, progress = FALSE)
    result <- export_palette(
      pal,
      format = "css",
      names = c("primary", "secondary", "accent")
    )

    expect_true(grepl("--primary:", result, fixed = TRUE))
    expect_true(grepl("--secondary:", result, fixed = TRUE))
    expect_true(grepl("--accent:", result, fixed = TRUE))
  })

  it("errors on wrong number of names", {
    pal <- generate_palette(3, progress = FALSE)
    expect_error(export_palette(pal, names = c("one", "two")))
  })

  it("writes to file", {
    pal <- generate_palette(3, progress = FALSE)
    temp_file <- tempfile(fileext = ".css")
    on.exit(unlink(temp_file))

    export_palette(pal, format = "css", file = temp_file)

    expect_true(file.exists(temp_file))
    content <- readLines(temp_file)
    expect_true(any(grepl(":root", content, fixed = TRUE)))
  })
})


describe("interpret_palette_quality()", {
  it("returns correct structure", {
    pal <- generate_palette(5, progress = FALSE)
    result <- interpret_palette_quality(pal)

    expect_s3_class(result, "huerd_interpretation")
    expect_true("summary" %in% names(result))
    expect_true("distinctness" %in% names(result))
    expect_true("accessibility" %in% names(result))
    expect_true("recommendations" %in% names(result))
    expect_true("metrics" %in% names(result))
  })

  it("handles single color", {
    pal <- generate_palette(1, progress = FALSE)
    result <- interpret_palette_quality(pal)

    expect_s3_class(result, "huerd_interpretation")
    expect_true(is.na(result$distinctness))
  })

  it("prints without error", {
    pal <- generate_palette(5, progress = FALSE)
    result <- interpret_palette_quality(pal)

    expect_no_error(print(result))
    expect_invisible(print(result))
  })

  it("handles palettes beyond the theoretical maximum lookup (n > 40)", {
    set.seed(123)
    pal <- quick_palette(41, quality = "fast")

    result <- interpret_palette_quality(pal)

    expect_s3_class(result, "huerd_interpretation")
    expect_match(result$summary, "not available")
    expect_true(is.na(result$metrics$performance_ratio))
  })
})


describe("plot.huerd_palette()", {
  it("works for swatches", {
    skip_if_not_installed("ragg")

    pal <- generate_palette(5, progress = FALSE)

    expect_no_error({
      withr::with_pdf(tempfile(fileext = ".pdf"), {
        plot(pal, type = "swatches")
      })
    })
  })

  it("works for analysis", {
    skip_if_not_installed("ragg")

    pal <- generate_palette(5, progress = FALSE)

    expect_no_error({
      withr::with_pdf(tempfile(fileext = ".pdf"), {
        plot(pal, type = "analysis")
      })
    })
  })

  it("warns on empty palette", {
    pal <- generate_palette(0, progress = FALSE)
    expect_warning(plot(pal))
  })
})

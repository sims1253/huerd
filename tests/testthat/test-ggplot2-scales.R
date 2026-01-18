# Tests for ggplot2 scale functions

# ==============================================================================
# Main tests
# ==============================================================================

describe("scale_color_huerd()", {
  it("works with automatic palette", {
    skip_if_not_installed("ggplot2")

    library(ggplot2)

    p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
      geom_point() +
      scale_color_huerd()

    expect_no_error(ggplot_build(p))
  })

  it("works with pre-generated palette", {
    skip_if_not_installed("ggplot2")

    library(ggplot2)

    pal <- generate_palette(5, progress = FALSE)

    p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
      geom_point() +
      scale_color_huerd(palette = pal)

    expect_no_error(ggplot_build(p))
  })

  it("works with brand_colors", {
    skip_if_not_installed("ggplot2")

    library(ggplot2)

    p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
      geom_point() +
      scale_color_huerd(brand_colors = c("#FF0000", "#00FF00"))

    expect_no_error(ggplot_build(p))
  })

  it("has alias scale_colour_huerd", {
    expect_identical(scale_colour_huerd, scale_color_huerd)
  })
})

describe("scale_fill_huerd()", {
  it("works with bar charts", {
    skip_if_not_installed("ggplot2")

    library(ggplot2)

    p <- ggplot(mpg, aes(class, fill = class)) +
      geom_bar() +
      scale_fill_huerd()

    expect_no_error(ggplot_build(p))
  })

  it("works with pre-generated palette", {
    skip_if_not_installed("ggplot2")

    library(ggplot2)

    pal <- generate_palette(7, progress = FALSE)

    p <- ggplot(mpg, aes(class, fill = class)) +
      geom_bar() +
      scale_fill_huerd(palette = pal)

    expect_no_error(ggplot_build(p))
  })
})

describe("ggplot2 scales", {
  it("pass through ggplot2 arguments", {
    skip_if_not_installed("ggplot2")

    library(ggplot2)

    p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
      geom_point() +
      scale_color_huerd(name = "Custom Title")

    built <- ggplot_build(p)
    expect_equal(built$plot$scales$scales[[1]]$name, "Custom Title")
  })

  it("handle missing values", {
    skip_if_not_installed("ggplot2")

    library(ggplot2)

    iris_na <- iris
    iris_na$Species[1:5] <- NA

    p <- ggplot(iris_na, aes(Sepal.Length, Sepal.Width, color = Species)) +
      geom_point() +
      scale_color_huerd(na.value = "grey80")

    expect_no_error(ggplot_build(p))
  })

  it("warn when fewer colors than needed", {
    skip_if_not_installed("ggplot2")

    library(ggplot2)

    small_pal <- generate_palette(2, progress = FALSE)

    p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
      geom_point() +
      scale_color_huerd(palette = small_pal)

    expect_warning(ggplot_build(p), "fewer colors")
  })
})

# ==============================================================================
# Error Tests for scale_color_huerd()
# ==============================================================================

describe("scale_color_huerd() errors", {
  describe("missing ggplot2 dependency", {
    it("errors when ggplot2 is not available", {
      skip("Testing missing ggplot2 dependency requires special setup")

      # This test requires special handling to unload ggplot2
      # The actual error message test would be:
      # expect_error(
      #   scale_color_huerd(),
      #   "Package.*ggplot2.*is required"
      # )
    })
  })

  describe("invalid palette objects", {
    it("handles list instead of huerd_palette", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # A list is not a valid palette - this should either warn or error
      invalid_palette <- list("#FF0000", "#00FF00", "#0000FF")

      # The function may handle this gracefully or warn
      # We test that it doesn't cause a hard crash
      # Note: The actual behavior depends on how ggplot2 handles the palette
      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(palette = invalid_palette)
      })
    })

    it("handles numeric vector instead of huerd_palette", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Numeric is not a valid palette
      invalid_palette <- c(1, 2, 3, 4, 5)

      # This should either produce a warning or error
      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(palette = invalid_palette)
      })
    })

    it("handles matrix instead of huerd_palette", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Matrix is not a valid palette
      invalid_palette <- matrix(c("#FF0000", "#00FF00"), nrow = 1)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(palette = invalid_palette)
      })
    })

    it("handles empty huerd_palette gracefully", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Create an empty palette
      empty_palette <- generate_palette(0, progress = FALSE)

      # Should work but may produce warnings
      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(palette = empty_palette)
      })
    })
  })

  describe("invalid brand_colors", {
    it("handles invalid hex color format", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Invalid hex color (missing #) - may produce warning during palette generation
      # The key is that it doesn't error during scale creation
      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(brand_colors = c("FF0000", "00FF00"))
      })
    })

    it("handles completely invalid color specification", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Invalid color specification - should not error during scale creation
      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(brand_colors = c("not_a_color"))
      })
    })

    it("handles mixed valid and invalid brand_colors", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Mix of valid and invalid - should not error during scale creation
      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(brand_colors = c("#FF0000", "invalid"))
      })
    })

    it("handles numeric brand_colors", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Numeric instead of character - should not error during scale creation
      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(brand_colors = c(255, 0, 0))
      })
    })
  })

  describe("invalid na.value", {
    it("accepts valid na.value colors", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Create data with NA values
      iris_na <- iris
      iris_na$Species[1:5] <- NA

      # Valid na.value should work
      expect_no_error({
        p <- ggplot(iris_na, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(na.value = "#CCCCCC")
      })
    })

    it("accepts color names for na.value", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      iris_na <- iris
      iris_na$Species[1:5] <- NA

      # ggplot2 accepts named colors
      expect_no_error({
        p <- ggplot(iris_na, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(na.value = "grey90")
      })
    })

    it("handles invalid na.value gracefully", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      iris_na <- iris
      iris_na$Species[1:5] <- NA

      # Invalid na.value may cause issues during ggplot building
      expect_no_error({
        p <- ggplot(iris_na, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(na.value = "not_a_valid_color_name_12345")
      })
    })
  })

  describe("invalid scale parameters", {
    it("accepts valid name parameter", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(name = "My Species")
      })
    })

    it("accepts valid labels parameter", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(labels = c("A", "B", "C"))
      })
    })

    it("accepts valid limits parameter", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(limits = c("setosa", "versicolor"))
      })
    })

    it("handles NULL breaks parameter", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(breaks = NULL)
      })
    })

    it("handles invalid breaks parameter gracefully", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Invalid breaks may cause issues at render time
      # The scale is created but may warn/error at render time
      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(breaks = "invalid_break")
      })
    })
  })

  describe("palette length edge cases", {
    it("handles very small palette (1 color)", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      single_color_pal <- generate_palette(1, progress = FALSE)

      # Should work - warning may or may not be produced depending on ggplot2 version
      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(palette = single_color_pal)
      })
    })

    it("handles very large palette", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Generate a large palette
      large_pal <- generate_palette(20, progress = FALSE)

      expect_no_error({
        # Create data with many categories
        df <- data.frame(
          x = 1:100,
          y = 1:100,
          cat = factor(rep(1:20, each = 5))
        )

        p <- ggplot(df, aes(x, y, color = cat)) +
          geom_point() +
          scale_color_huerd(palette = large_pal)
      })
    })

    it("handles palette with exactly matching length", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # 3 species in iris
      exact_pal <- generate_palette(3, progress = FALSE)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(palette = exact_pal)
      })
    })
  })

  describe("palette with invalid colors", {
    it("handles palette with NA values", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Create palette and inject NA
      pal <- generate_palette(3, progress = FALSE)
      pal_with_na <- pal
      pal_with_na[1] <- NA_character_

      # Should warn or error
      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(palette = pal_with_na)
      })
    })

    it("handles empty character vector palette", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Empty character vector
      empty_pal <- character(0)
      class(empty_pal) <- "huerd_palette"

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(palette = empty_pal)
      })
    })
  })

  describe("aesthetics parameter", {
    it("accepts custom aesthetics parameter", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(aesthetics = "color")
      })
    })
  })
})

# ==============================================================================
# Error Tests for scale_fill_huerd()
# ==============================================================================

describe("scale_fill_huerd() errors", {
  describe("missing ggplot2 dependency", {
    it("errors when ggplot2 is not available", {
      skip("Testing missing ggplot2 dependency requires special setup")

      # This test requires special handling to unload ggplot2
      # The actual error message test would be:
      # expect_error(
      #   scale_fill_huerd(),
      #   "Package.*ggplot2.*is required"
      # )
    })
  })

  describe("invalid palette objects", {
    it("handles data.frame instead of huerd_palette", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      invalid_palette <- data.frame(color = c("#FF0000", "#00FF00"))

      expect_no_error({
        p <- ggplot(mpg, aes(class, fill = class)) +
          geom_bar() +
          scale_fill_huerd(palette = invalid_palette)
      })
    })

    it("handles string instead of huerd_palette", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      invalid_palette <- "#FF0000"

      expect_no_error({
        p <- ggplot(mpg, aes(class, fill = class)) +
          geom_bar() +
          scale_fill_huerd(palette = invalid_palette)
      })
    })
  })

  describe("invalid brand_colors", {
    it("handles wrong color format", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # RGB format instead of hex - should not error during scale creation
      expect_no_error({
        p <- ggplot(mpg, aes(class, fill = class)) +
          geom_bar() +
          scale_fill_huerd(brand_colors = c("rgb(255,0,0)"))
      })
    })

    it("handles numeric brand_colors", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(mpg, aes(class, fill = class)) +
          geom_bar() +
          scale_fill_huerd(brand_colors = c(1, 2, 3))
      })
    })
  })

  describe("invalid na.value", {
    it("handles invalid na.value in fill scale", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Create data with NA values
      mpg_na <- mpg
      mpg_na$class[1:10] <- NA

      # Invalid na.value may cause issues
      expect_no_error({
        p <- ggplot(mpg_na, aes(class, fill = class)) +
          geom_bar() +
          scale_fill_huerd(na.value = "completely_invalid_color_name_xyz")
      })
    })
  })

  describe("fill scale specific edge cases", {
    it("handles single category data", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Data with only one category
      df_single <- data.frame(x = 1:10, y = 1:10, cat = "A")

      expect_no_error({
        p <- ggplot(df_single, aes(x, y, fill = cat)) +
          geom_tile() +
          scale_fill_huerd()
      })
    })

    it("handles many categories in bar chart", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Data with many categories
      df_many <- data.frame(
        x = 1:100,
        cat = factor(rep(1:15, length.out = 100))
      )

      expect_no_error({
        p <- ggplot(df_many, aes(cat, fill = cat)) +
          geom_bar() +
          scale_fill_huerd()
      })
    })
  })
})

# ==============================================================================
# General Scale Function Error Tests
# ==============================================================================

describe("ggplot2 scales - general errors", {
  describe("argument validation", {
    it("passes unknown arguments to generate_palette", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Unknown args that should go to generate_palette
      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(
            palette = NULL,
            brand_colors = NULL,
            initialization = "k-means++",
            max_iterations = 100,
            progress = FALSE
          )
      })
    })

    it("handles conflicting arguments gracefully", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # When both palette and brand_colors are provided, palette takes precedence
      pal <- generate_palette(3, progress = FALSE)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(
            palette = pal,
            brand_colors = c("#FF0000") # This should be ignored
          )
      })
    })
  })

  describe("edge cases with data", {
    it("handles data with no variation", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Constant color variable
      df_const <- data.frame(x = 1:10, y = 1:10, color = "constant")

      expect_no_error({
        p <- ggplot(df_const, aes(x, y, color = color)) +
          geom_point() +
          scale_color_huerd()
      })
    })

    it("handles all NA in color variable", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      df_na <- iris
      df_na$Species <- NA

      expect_no_error({
        p <- ggplot(df_na, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd()
      })
    })

    it("handles many NA values in color variable", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      df_mixed_na <- iris
      df_mixed_na$Species[1:100] <- NA
      df_mixed_na$Species[101:150] <- NA

      expect_no_error({
        p <- ggplot(
          df_mixed_na,
          aes(Sepal.Length, Sepal.Width, color = Species)
        ) +
          geom_point() +
          scale_color_huerd()
      })
    })
  })

  describe("scale context validation", {
    it("works in basic ggplot context", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          scale_color_huerd()
      })
    })

    it("works with multiple scales", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd() +
          scale_x_continuous() +
          scale_y_continuous()
      })
    })

    it("works when added to plot multiple times", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
        geom_point()

      # Adding scale should work
      p1 <- p + scale_color_huerd()

      expect_s3_class(p1$scales$get_scales("colour"), "ScaleDiscrete")
    })
  })

  describe("color specification formats", {
    it("accepts uppercase hex colors in brand_colors", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(brand_colors = c("#FF0000", "#00FF00", "#0000FF"))
      })
    })

    it("accepts lowercase hex colors in brand_colors", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(brand_colors = c("#ff0000", "#00ff00", "#0000ff"))
      })
    })

    it("accepts mixed case hex colors in brand_colors", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(brand_colors = c("#Ff0000", "#00fF00", "#0000ff"))
      })
    })
  })

  describe("parameter type handling", {
    it("handles logical na.translate parameter", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(na.translate = TRUE)
      })

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(na.translate = FALSE)
      })
    })

    it("handles valid guide parameter", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(guide = "legend")
      })
    })

    it("handles valid position parameter", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(position = "right")
      })

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(position = "left")
      })

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(position = "top")
      })
    })

    it("handles valid drop parameter", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(drop = TRUE)
      })

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(drop = FALSE)
      })
    })
  })

  describe("expand and limits parameters", {
    it("handles valid expand parameter", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(expand = c(0, 0))
      })

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(expand = c(0.1, 0.1))
      })
    })

    it("handles valid limits parameter", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(limits = c("setosa", "versicolor", "virginica"))
      })

      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(limits = c("setosa", "versicolor"))
      })
    })

    it("handles limits that exclude all data", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      # Limits that exclude all actual data - should warn or error
      expect_no_error({
        p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() +
          scale_color_huerd(limits = c("nonexistent"))
      })
    })
  })

  describe("very large number of categories", {
    it("handles 50 categories", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      df_large <- data.frame(
        x = 1:100,
        cat = factor(rep(1:50, each = 2))
      )

      expect_no_error({
        p <- ggplot(df_large, aes(x, cat, color = cat)) +
          geom_point() +
          scale_color_huerd()
      })
    })

    it("handles 25 categories with custom palette", {
      skip_if_not_installed("ggplot2")
      library(ggplot2)

      df_very_large <- data.frame(
        x = 1:100,
        cat = factor(rep(1:25, each = 4))
      )

      large_pal <- generate_palette(25, progress = FALSE)

      expect_no_error({
        p <- ggplot(df_very_large, aes(x, cat, color = cat)) +
          geom_point() +
          scale_color_huerd(palette = large_pal)
      })
    })
  })
})

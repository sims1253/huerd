# Tests for ggplot2 scale functions

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

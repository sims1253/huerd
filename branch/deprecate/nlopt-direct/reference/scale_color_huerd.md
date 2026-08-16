# Discrete color scale using huerd palettes

These scales provide perceptually optimized color palettes for ggplot2
visualizations. Colors are generated using minimax optimization in the
OKLAB color space to maximize perceptual distinctness.

## Usage

``` r
scale_color_huerd(
  palette = NULL,
  brand_colors = NULL,
  ...,
  aesthetics = "colour",
  na.value = "grey50"
)

scale_colour_huerd(
  palette = NULL,
  brand_colors = NULL,
  ...,
  aesthetics = "colour",
  na.value = "grey50"
)

scale_fill_huerd(
  palette = NULL,
  brand_colors = NULL,
  ...,
  aesthetics = "fill",
  na.value = "grey50"
)
```

## Arguments

- palette:

  A `huerd_palette` object (from
  [`generate_palette()`](https://sims1253.github.io/huerd/branch/deprecate/nlopt-direct/reference/generate_palette.md))
  to use. If `NULL`, a palette will be generated automatically based on
  the number of levels in your data.

- brand_colors:

  Character vector of hex colors that must be included in the palette.
  Only used when `palette = NULL`. These colors will be preserved and
  additional colors optimized around them.

- ...:

  Additional arguments passed to
  [`generate_palette()`](https://sims1253.github.io/huerd/branch/deprecate/nlopt-direct/reference/generate_palette.md)
  when generating palettes on-the-fly, or to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

- aesthetics:

  Character string or vector of aesthetic names to apply the scale to.
  Defaults to `"colour"` for `scale_color_huerd()` and `"fill"` for
  `scale_fill_huerd()`.

- na.value:

  Color to use for missing values. Defaults to `"grey50"`.

## Value

A ggplot2 scale object.

## Details

There are two ways to use these scales:

1.  **Pre-generated palette**: Pass a `huerd_palette` object to the
    `palette` argument. This is useful when you want to reuse the same
    palette across multiple plots or need fine control over generation
    parameters.

2.  **On-the-fly generation**: Leave `palette = NULL` and the scale will
    automatically generate an optimized palette based on the number of
    levels in your data. Use `brand_colors` to include specific colors.

## See also

[`generate_palette()`](https://sims1253.github.io/huerd/branch/deprecate/nlopt-direct/reference/generate_palette.md)
for creating palettes with custom parameters.

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Basic usage - automatic palette generation
  ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
    geom_point(size = 3) +
    scale_color_huerd()

  # With brand colors
  ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
    geom_point(size = 3) +
    scale_color_huerd(brand_colors = c("#1f77b4", "#ff7f0e"))

  # Using a pre-generated palette
  my_palette <- generate_palette(5, progress = FALSE)
  ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
    geom_point(size = 3) +
    scale_color_huerd(palette = my_palette)

  # Fill scale for bar charts
  ggplot(mpg, aes(class, fill = class)) +
    geom_bar() +
    scale_fill_huerd() +
    theme(legend.position = "none")
}

```

# Comprehensive Palette Analysis Dashboard

Creates a scicomap-inspired comprehensive diagnostic dashboard for color
palettes using the modern grid graphics system for robust cross-platform
compatibility. This function generates six visualization panels to help
assess palette quality, including perceptual distance analysis, CVD
simulation, OKLAB space distribution, and performance comparison against
established scientific palettes.

## Usage

``` r
plot_palette_analysis(colors, force_font_scale = NULL, ...)
```

## Arguments

- colors:

  A character vector of hex colors or a matrix of colors in OKLAB space.

- force_font_scale:

  Allows to force a specific font scale

- ...:

  Additional arguments reserved for future use.

## Value

Invisibly returns the evaluation result from evaluate_palette.

## Examples

``` r
colors <- c("#ff0000", "#00ff00", "#0000ff")
plot_palette_analysis(colors)
```

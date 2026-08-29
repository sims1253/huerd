# Palette Analysis Dashboard

Creates a scicomap-inspired diagnostic dashboard for color palettes
using the grid graphics system for robust cross-platform compatibility.
The function generates six panels to help assess palette quality,
including perceptual distance analysis, CVD simulation, OKLAB space
distribution, and performance comparison against established scientific
palettes.

## Usage

``` r
plot_palette_analysis(colors, force_font_scale = NULL, ...)
```

## Arguments

- colors:

  A character vector of hex colors or a matrix of colors in OKLAB space.

- force_font_scale:

  Lets you force a specific font scale.

- ...:

  Additional arguments reserved for future use.

## Value

Invisibly returns the evaluation result from evaluate_palette.

## Examples

``` r
colors <- c("#ff0000", "#00ff00", "#0000ff")
plot_palette_analysis(colors)
```

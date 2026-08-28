# Quick palette generation with sensible defaults

A simplified interface to
[`generate_palette()`](https://sims1253.github.io/huerd/branch/chore/internal-hygiene/reference/generate_palette.md)
that uses intuitive parameter names and sensible defaults. This function
is designed for users who want good results without understanding
optimization details.

## Usage

``` r
quick_palette(
  n,
  brand_colors = NULL,
  cvd_safe = TRUE,
  quality = c("balanced", "fast", "high"),
  lightness = "any"
)
```

## Arguments

- n:

  Number of colors to generate.

- brand_colors:

  Optional character vector of hex colors that must be included in the
  palette. These colors will be preserved exactly as provided, and
  additional colors will be optimized around them.

- cvd_safe:

  Logical. If `TRUE` (default), the optimizer maximizes the worst-case
  perceptual distance across color vision deficiency simulations
  (deuteranopia, protanopia, tritanopia). If `FALSE`, it optimizes for
  normal vision only.

- quality:

  Character string specifying the quality/speed tradeoff:

  - `"fast"`: Quick generation with fewer iterations (good for
    exploration)

  - `"balanced"`: Default balance of quality and speed

  - `"high"`: More iterations for better results (slower)

- lightness:

  Character string or numeric vector specifying lightness constraints:

  - `"any"`: Balanced range (L: 0.2-0.9)

  - `"light"`: Prefer lighter colors (L: 0.5-0.9)

  - `"dark"`: Prefer darker colors (L: 0.2-0.6)

  - `"mid"`: Prefer mid-range lightness (L: 0.35-0.75)

  - Numeric vector of length 2: Custom bounds (e.g., `c(0.3, 0.8)`)

## Value

A `huerd_palette` object (character vector of hex colors with additional
attributes).

## See also

[`generate_palette()`](https://sims1253.github.io/huerd/branch/chore/internal-hygiene/reference/generate_palette.md)
for full control over palette generation.

## Examples

``` r
# Simple 5-color palette
quick_palette(5)
#> 
#> -- huerd Color Palette (5 colors) --
#> Colors:
#> [ 1] #001D8A
#> [ 2] #1C8A00
#> [ 3] #FF0000
#> [ 4] #FF00CC
#> [ 5] #96E600
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.235
#> * Optimizer Performance Ratio      : 57.1%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.099
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 269
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.

# Include brand colors
quick_palette(6, brand_colors = c("#1f77b4", "#ff7f0e"))
#> 
#> -- huerd Color Palette (6 colors) --
#> Colors:
#> [ 1] #5F4E00
#> [ 2] #1F77B4
#> [ 3] #FF7F0E
#> [ 4] #83A8FF
#> [ 5] #FFB69C
#> [ 6] #FFE2FF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.141
#> * Optimizer Performance Ratio      : 38.6%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.133
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 223
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.

# Fast generation for exploration
quick_palette(8, quality = "fast")
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #000094
#> [ 2] #730000
#> [ 3] #9F4900
#> [ 4] #EC1071
#> [ 5] #DD00FF
#> [ 6] #00CF00
#> [ 7] #00E7FF
#> [ 8] #E9D7A1
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.166
#> * Optimizer Performance Ratio      : 53.7%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.122
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 202
#> * Optimizer Status: NLOPT_MAXEVAL_REACHED: Optimization stopped because maxeval (above) was reached.

# Light colors for dark backgrounds
quick_palette(5, lightness = "light")
#> 
#> -- huerd Color Palette (5 colors) --
#> Colors:
#> [ 1] #338800
#> [ 2] #FF0000
#> [ 3] #FF00C6
#> [ 4] #00D146
#> [ 5] #00E0FF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.206
#> * Optimizer Performance Ratio      : 50.1%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.099
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 333
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

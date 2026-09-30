# Quick palette generation with sensible defaults

A simplified interface to
[`generate_palette()`](https://sims1253.github.io/huerd/pr/20/reference/generate_palette.md)
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

  Character string or numeric vector specifying the OKLAB lightness
  range used to seed the initial candidate colors. These are
  initialization preferences, not hard constraints: the optimizer is
  free to move colors outside the requested range, so final lightness is
  not guaranteed to stay within it.

  - `"any"`: Balanced range (L: 0.2-0.9)

  - `"light"`: Start from lighter colors (L: 0.5-0.9)

  - `"dark"`: Start from darker colors (L: 0.2-0.6)

  - `"mid"`: Start from mid-range lightness (L: 0.35-0.75)

  - Numeric vector of length 2: Custom bounds (e.g., `c(0.3, 0.8)`)

## Value

A `huerd_palette` object (character vector of hex colors with additional
attributes).

## See also

[`generate_palette()`](https://sims1253.github.io/huerd/pr/20/reference/generate_palette.md)
for full control over palette generation.

## Examples

``` r
# Simple 5-color palette
quick_palette(5)
#> 
#> -- huerd Color Palette (5 colors) --
#> Colors:
#> [ 1] #04324D
#> [ 2] #974C8C
#> [ 3] #ED00FF
#> [ 4] #B0989F
#> [ 5] #F5C3FC
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.190
#> * Optimizer Performance Ratio      : 46.2%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.175
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 482
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.

# Include brand colors
quick_palette(6, brand_colors = c("#1f77b4", "#ff7f0e"))
#> 
#> -- huerd Color Palette (6 colors) --
#> Colors:
#> [ 1] #530076
#> [ 2] #A82F2B
#> [ 3] #1F77B4
#> [ 4] #FF7F0E
#> [ 5] #BFADEA
#> [ 6] #FFFFBB
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.241
#> * Optimizer Performance Ratio      : 65.9%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.199
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 346
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.

# Fast generation for exploration
quick_palette(8, quality = "fast")
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #4C0065
#> [ 2] #1F39A5
#> [ 3] #CA5591
#> [ 4] #FF0300
#> [ 5] #B668E0
#> [ 6] #ACC100
#> [ 7] #84B7FF
#> [ 8] #7FEAFF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.123
#> * Optimizer Performance Ratio      : 39.6%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.108
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 202
#> * Optimizer Status: NLOPT_MAXEVAL_REACHED: Optimization stopped because maxeval (above) was reached.

# Start from light colors (e.g., when plotting on a dark background)
quick_palette(5, lightness = "light")
#> 
#> -- huerd Color Palette (5 colors) --
#> Colors:
#> [ 1] #3C00BC
#> [ 2] #8C0000
#> [ 3] #DF7F00
#> [ 4] #58A4FF
#> [ 5] #FFFF00
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.299
#> * Optimizer Performance Ratio      : 72.7%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.259
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 325
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

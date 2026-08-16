# Quick palette generation with sensible defaults

A simplified interface to
[`generate_palette()`](https://sims1253.github.io/huerd/branch/sims/dev/reference/generate_palette.md)
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

[`generate_palette()`](https://sims1253.github.io/huerd/branch/sims/dev/reference/generate_palette.md)
for full control over palette generation.

## Examples

``` r
# Simple 5-color palette
quick_palette(5)
#> 
#> -- huerd Color Palette (5 colors) --
#> Colors:
#> [ 1] #340900
#> [ 2] #1D00A9
#> [ 3] #2B9500
#> [ 4] #FF00FF
#> [ 5] #00FD00
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.294
#> * Optimizer Performance Ratio      : 71.5%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.213
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 339
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.

# Include brand colors
quick_palette(6, brand_colors = c("#1f77b4", "#ff7f0e"))
#> 
#> -- huerd Color Palette (6 colors) --
#> Colors:
#> [ 1] #6C3900
#> [ 2] #1F77B4
#> [ 3] #B59298
#> [ 4] #00ADF7
#> [ 5] #FF7F0E
#> [ 6] #00E7FF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.160
#> * Optimizer Performance Ratio      : 43.8%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.138
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 366
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.

# Fast generation for exploration
quick_palette(8, quality = "fast")
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #003300
#> [ 2] #3000B7
#> [ 3] #006893
#> [ 4] #8F00FF
#> [ 5] #FF0000
#> [ 6] #FF7697
#> [ 7] #CDBBF2
#> [ 8] #00FF00
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.161
#> * Optimizer Performance Ratio      : 52.0%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.126
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 202
#> * Optimizer Status: NLOPT_MAXEVAL_REACHED: Optimization stopped because maxeval (above) was reached.

# Light colors for dark backgrounds
quick_palette(5, lightness = "light")
#> 
#> -- huerd Color Palette (5 colors) --
#> Colors:
#> [ 1] #0000FF
#> [ 2] #A872FF
#> [ 3] #D771AA
#> [ 4] #EE9BFF
#> [ 5] #D3DFD4
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.144
#> * Optimizer Performance Ratio      : 35.0%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.135
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 552
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

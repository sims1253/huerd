# Quick palette generation with sensible defaults

A simplified interface to
[`generate_palette()`](https://sims1253.github.io/huerd/branch/deprecate/nlopt-direct/reference/generate_palette.md)
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

[`generate_palette()`](https://sims1253.github.io/huerd/branch/deprecate/nlopt-direct/reference/generate_palette.md)
for full control over palette generation.

## Examples

``` r
# Simple 5-color palette
quick_palette(5)
#> 
#> -- huerd Color Palette (5 colors) --
#> Colors:
#> [ 1] #6C0000
#> [ 2] #A700FF
#> [ 3] #009E00
#> [ 4] #D09AFF
#> [ 5] #C0FFFF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.247
#> * Optimizer Performance Ratio      : 60.1%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.210
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 352
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.

# Include brand colors
quick_palette(6, brand_colors = c("#1f77b4", "#ff7f0e"))
#> 
#> -- huerd Color Palette (6 colors) --
#> Colors:
#> [ 1] #863400
#> [ 2] #1F77B4
#> [ 3] #FF7F0E
#> [ 4] #93A0FF
#> [ 5] #64EDFF
#> [ 6] #F5E17C
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.200
#> * Optimizer Performance Ratio      : 54.6%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.167
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 374
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.

# Fast generation for exploration
quick_palette(8, quality = "fast")
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #002300
#> [ 2] #760037
#> [ 3] #5900B6
#> [ 4] #5E8200
#> [ 5] #FF0000
#> [ 6] #009FFF
#> [ 7] #FF9B00
#> [ 8] #00EDFF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.215
#> * Optimizer Performance Ratio      : 69.4%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.089
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 202
#> * Optimizer Status: NLOPT_MAXEVAL_REACHED: Optimization stopped because maxeval (above) was reached.

# Light colors for dark backgrounds
quick_palette(5, lightness = "light")
#> 
#> -- huerd Color Palette (5 colors) --
#> Colors:
#> [ 1] #C44500
#> [ 2] #FF00F6
#> [ 3] #DBACFF
#> [ 4] #CDF700
#> [ 5] #88FFFF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.218
#> * Optimizer Performance Ratio      : 53.0%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.125
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 433
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

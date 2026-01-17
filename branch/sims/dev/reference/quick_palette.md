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

  Logical. If `TRUE` (default), optimizes for color vision deficiency
  safety. Currently this uses the default optimization which considers
  CVD in its metrics.

- quality:

  Character string specifying the quality/speed tradeoff:

  - `"fast"`: Quick generation with fewer iterations (good for
    exploration)

  - `"balanced"`: Default balance of quality and speed

  - `"high"`: More iterations for better results (slower)

- lightness:

  Character string or numeric vector specifying lightness constraints:

  - `"any"`: No lightness constraints (full range)

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
#> [ 1] #7C0066
#> [ 2] #C30000
#> [ 3] #FF36FF
#> [ 4] #00F8FF
#> [ 5] #FADE00
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.204
#> * Optimizer Performance Ratio      : 49.8%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.186
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 379
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.

# Include brand colors
quick_palette(6, brand_colors = c("#1f77b4", "#ff7f0e"))
#> 
#> -- huerd Color Palette (6 colors) --
#> Colors:
#> [ 1] #5D0039
#> [ 2] #B80000
#> [ 3] #1F77B4
#> [ 4] #FF7F0E
#> [ 5] #FFA8FF
#> [ 6] #B6FD00
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.218
#> * Optimizer Performance Ratio      : 59.6%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.173
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 318
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.

# Fast generation for exploration
quick_palette(8, quality = "fast")
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #570000
#> [ 2] #484D00
#> [ 3] #0000F6
#> [ 4] #686D00
#> [ 5] #408572
#> [ 6] #9369E7
#> [ 7] #00B050
#> [ 8] #FFB6C8
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.114
#> * Optimizer Performance Ratio      : 36.7%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.109
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 202
#> * Optimizer Status: NLOPT_MAXEVAL_REACHED: Optimization stopped because maxeval (above) was reached.

# Light colors for dark backgrounds
quick_palette(5, lightness = "light")
#> 
#> -- huerd Color Palette (5 colors) --
#> Colors:
#> [ 1] #CB00A1
#> [ 2] #FF00FF
#> [ 3] #FF9D00
#> [ 4] #DEA6FF
#> [ 5] #FFED00
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.166
#> * Optimizer Performance Ratio      : 40.4%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.120
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 508
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

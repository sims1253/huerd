# Create a palette with brand colors

Convenience function specifically for creating palettes that incorporate
brand or corporate colors. This is a common use case where specific
colors must be preserved while generating complementary colors.

## Usage

``` r
brand_palette(brand_colors, n_total, cvd_safe = TRUE)
```

## Arguments

- brand_colors:

  Character vector of hex colors representing your brand colors. These
  will be preserved exactly in the output.

- n_total:

  Total number of colors needed in the final palette. Must be at least
  as large as the number of brand colors.

- cvd_safe:

  Logical. If `TRUE` (default), the optimizer maximizes the worst-case
  perceptual distance across color vision deficiency simulations
  (deuteranopia, protanopia, tritanopia). If `FALSE`, it optimizes for
  normal vision only.

## Value

A `huerd_palette` object containing the brand colors plus optimized
complementary colors.

## See also

[`quick_palette()`](https://sims1253.github.io/huerd/reference/quick_palette.md),
[`generate_palette()`](https://sims1253.github.io/huerd/reference/generate_palette.md)

## Examples

``` r
# Corporate palette with 2 brand colors expanded to 6
brand_palette(
  brand_colors = c("#003366", "#FF6600"),
  n_total = 6
)
#> 
#> -- huerd Color Palette (6 colors) --
#> Colors:
#> [ 1] #21001B
#> [ 2] #003366
#> [ 3] #750000
#> [ 4] #1A78A2
#> [ 5] #FF6600
#> [ 6] #82B9FF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.195
#> * Optimizer Performance Ratio      : 53.2%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.163
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 264
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

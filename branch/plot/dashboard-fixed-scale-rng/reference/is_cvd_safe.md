# Check Palette CVD Safety

Quick check if a palette's worst-case minimum inter-color distance under
simulated common CVD conditions meets a specified threshold.

## Usage

``` r
is_cvd_safe(colors, min_cvd_distance = 0.08, ...)
```

## Arguments

- colors:

  Character vector of hex colors or an OKLAB matrix.

- min_cvd_distance:

  Numeric. The minimum acceptable perceptual distance (in OK LAB space)
  that any two colors in the palette should maintain under simulated
  deuteranopia, protanopia, and tritanopia. Default is 0.08. Recall JND
  is ~0.02.

- ...:

  Additional arguments reserved for future use.

## Value

Logical. `TRUE` if the palette's `worst_case_min_distance` from
[`evaluate_palette()`](https://sims1253.github.io/huerd/branch/plot/dashboard-fixed-scale-rng/reference/evaluate_palette.md)
is greater than or equal to `min_cvd_distance`, `FALSE` otherwise.
Returns `TRUE` if palette has fewer than 2 colors.

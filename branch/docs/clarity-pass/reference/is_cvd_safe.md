# Check Palette CVD Safety

Checks whether a palette's worst-case minimum distance between colors,
under simulated common CVD conditions, meets a specified threshold.

## Usage

``` r
is_cvd_safe(colors, min_cvd_distance = NULL, ...)
```

## Arguments

- colors:

  Character vector of hex colors or an OKLAB matrix.

- min_cvd_distance:

  Numeric. The minimum acceptable perceptual distance (in OKLAB space)
  that any two colors in the palette should maintain under simulated
  deuteranopia, protanopia, and tritanopia. Defaults to the package-wide
  distinctness threshold (0.08). JND is ~0.02.

- ...:

  Additional arguments reserved for future use.

## Value

Logical. `TRUE` if the palette's `worst_case_min_distance` from
[`evaluate_palette()`](https://sims1253.github.io/huerd/branch/docs/clarity-pass/reference/evaluate_palette.md)
is greater than or equal to `min_cvd_distance`, `FALSE` otherwise.
Returns `TRUE` if palette has fewer than 2 colors.

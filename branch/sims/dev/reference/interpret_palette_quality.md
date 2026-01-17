# Interpret palette quality in plain language

Provides a human-readable assessment of a palette's quality, translating
technical metrics into understandable language suitable for
presentations or documentation.

## Usage

``` r
interpret_palette_quality(palette)
```

## Arguments

- palette:

  A `huerd_palette` object or character vector of hex colors.

## Value

A list with class `huerd_interpretation` containing:

- `summary`: Overall quality assessment

- `distinctness`: How distinct the colors are from each other

- `accessibility`: CVD accessibility assessment

- `recommendations`: Suggestions for improvement (if any)

## Examples

``` r
pal <- generate_palette(6, progress = FALSE)
interpret_palette_quality(pal)
#> 
#> ── Palette Quality Assessment ──
#> 
#> This 6-color palette is highly optimized (60% of theoretical maximum).
#> Excellent - colors are highly distinct and easy to differentiate
#> 
#> 
#> ── Distinctness 
#> Excellent - colors are highly distinct and easy to differentiate
#> 
#> ── Accessibility 
#> Excellent - palette is safe for most color vision deficiencies
```

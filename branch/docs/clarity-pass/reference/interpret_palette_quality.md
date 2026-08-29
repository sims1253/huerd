# Interpret palette quality in plain language

Assesses a palette's quality and translates the metrics into plain
language for presentations or documentation.

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
#> This 6-color palette is moderately optimized (29% of theoretical maximum). Good
#> - colors are reasonably distinct for most uses
#> 
#> 
#> ── Distinctness 
#> Good - colors are reasonably distinct for most uses
#> 
#> ── Accessibility 
#> Good - palette should work for most viewers with CVD
```

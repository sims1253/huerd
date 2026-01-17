# Evaluate Palette Quality

Provides a comprehensive evaluation of a color palette's perceptual
properties, including its distinguishability, CVD safety, and color
distribution. Returns raw metrics without subjective scoring for
post-hoc analysis.

## Usage

``` r
evaluate_palette(colors, ...)
```

## Arguments

- colors:

  A character vector of hex colors, or a matrix of colors in OK LAB
  space.

- ...:

  Additional arguments reserved for future use.

## Value

A list of evaluation metrics with class `huerd_evaluation`. Contains raw
metrics including distances, CVD safety, and distribution for objective
analysis without subjective heuristic scoring.

## Examples

``` r
pal <- generate_palette(5, progress = FALSE)
metrics <- evaluate_palette(pal)
print(metrics) # Uses custom print method
#> 
#> -- huerd Palette Evaluation (5 colors) --
#> 
#> -- Perceptual Distances (OKLAB) --
#> * Min distance       : 0.2980
#> * Mean distance      : 0.4324
#> * Median distance    : 0.4158
#> * Std. Dev.          : 0.1187
#> * Estimated Max Min  : 0.4108 (for unconstrained palette of this size)
#> * Performance Ratio  : 72.5% (achieved min / estimated max)
#> 
#> -- CVD Safety (OKLAB distances under simulation) --
#> * Worst-case min dist: 0.2303
#>   Protanopia : min=0.240, preserved_ratio=0.81
#>   Deuteranopia: min=0.233, preserved_ratio=0.78
#>   Tritanopia : min=0.230, preserved_ratio=0.77
#> 
#> -- Color Distribution (OKLAB) --
#> * Lightness (L)    : range=[0.34, 0.96], mean=0.61
#> * Chroma (C)       : range=[0.036, 0.253], mean=0.157
#> * Hue (degrees)    : circular_variance=0.564

# The performance_ratio compares the achieved min distance to an estimated maximum
# metrics$distances$performance_ratio
```

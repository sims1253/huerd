# Evaluate Palette Quality

Provides a comprehensive evaluation of a color palette's perceptual
properties, including its distinguishability, CVD safety, and color
distribution. Returns raw metrics without subjective scoring for
post-hoc analysis.

## Usage

``` r
evaluate_palette(colors)
```

## Arguments

- colors:

  A character vector of hex colors, or a matrix of colors in OK LAB
  space.

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
#> * Min distance       : 0.2358
#> * Mean distance      : 0.3985
#> * Median distance    : 0.3423
#> * Std. Dev.          : 0.1453
#> * Estimated Max Min  : 0.4108 (for unconstrained palette of this size)
#> * Performance Ratio  : 57.4% (achieved min / estimated max)
#> 
#> -- CVD Safety (OKLAB distances under simulation) --
#> * Worst-case min dist: 0.1863
#>   Protanopia : min=0.190, preserved_ratio=0.80
#>   Deuteranopia: min=0.188, preserved_ratio=0.80
#>   Tritanopia : min=0.186, preserved_ratio=0.79
#> 
#> -- Color Distribution (OKLAB) --
#> * Lightness (L)    : range=[0.43, 0.87], mean=0.64
#> * Chroma (C)       : range=[0.058, 0.300], mean=0.216
#> * Hue (degrees)    : circular_variance=0.749

# The performance_ratio compares the achieved min distance to an estimated maximum
# metrics$distances$performance_ratio
```

# Introduction to huerd

## Introduction

The `huerd` package provides a scientifically-grounded approach to
generating categorical color palettes. It uses a pure minimax
optimization algorithm in the perceptually uniform OKLAB color space to
create palettes with maximally distinct colors.

This vignette will walk you through the core features of `huerd`, from
basic palette generation to in-depth analysis.

## Installation

You can install the development version of `huerd` from GitHub with:

``` r

# install.packages("remotes")
# remotes::install_github("sims1253/huerd")
```

## Basic Palette Generation

The simplest way to use `huerd` is with the
[`generate_palette()`](https://sims1253.github.io/huerd/pr/20/reference/generate_palette.md)
function. By default, it will create a palette of the specified size
with colors that are as distinct as possible.

``` r

library(huerd)

# Generate a palette of 5 colors
palette <- generate_palette(5, progress = FALSE)
print(palette)
#> 
#> -- huerd Color Palette (5 colors) --
#> Colors:
#> [ 1] #1D0000
#> [ 2] #781C5D
#> [ 3] #4E3BFF
#> [ 4] #FB5766
#> [ 5] #FFDBFF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.277
#> * Optimizer Performance Ratio      : 67.5%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.253
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 445
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

All palettes are automatically sorted by brightness (lightness in the
OKLAB space), making them intuitive to use.

## Constrained Palettes

A key feature of `huerd` is the ability to include fixed “brand” colors
in your palette while optimizing the remaining colors around them.

``` r

# Generate a 6-color palette that must include a specific blue and orange
brand_palette <- generate_palette(
  n = 6,
  include_colors = c("#4A6B8A", "#E5A04C"),
  progress = FALSE
)
print(brand_palette)
#> 
#> -- huerd Color Palette (6 colors) --
#> Colors:
#> [ 1] #691B4B
#> [ 2] #4A6B8A
#> [ 3] #C2312B
#> [ 4] #E5A04C
#> [ 5] #F9C3FF
#> [ 6] #AAF79A
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.209
#> * Optimizer Performance Ratio      : 57.1%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.140
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 293
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

## Palette Analysis

`huerd` includes powerful tools for analyzing the quality of your
palettes.

### `evaluate_palette()`

The
[`evaluate_palette()`](https://sims1253.github.io/huerd/pr/20/reference/evaluate_palette.md)
function provides a detailed, quantitative assessment of a palette’s
properties.

``` r

# Evaluate the brand palette we just created
evaluation <- evaluate_palette(brand_palette)
print(evaluation)
#> 
#> -- huerd Palette Evaluation (6 colors) --
#> 
#> -- Perceptual Distances (OKLAB) --
#> * Min distance       : 0.2087
#> * Mean distance      : 0.3390
#> * Median distance    : 0.3085
#> * Std. Dev.          : 0.1260
#> * Estimated Max Min  : 0.3655 (for unconstrained palette of this size)
#> * Performance Ratio  : 57.1% (achieved min / estimated max)
#> 
#> -- CVD Safety (OKLAB distances under simulation) --
#> * Worst-case min dist: 0.1398
#>   Protanopia : min=0.144, preserved_ratio=0.69
#>   Deuteranopia: min=0.142, preserved_ratio=0.68
#>   Tritanopia : min=0.140, preserved_ratio=0.67
#> 
#> -- Color Distribution (OKLAB) --
#> * Lightness (L)    : range=[0.37, 0.90], mean=0.66
#> * Chroma (C)       : range=[0.063, 0.183], mean=0.123
#> * Hue (degrees)    : circular_variance=0.685
```

This function returns a wealth of information, including:

- **Perceptual Distances**: Minimum, mean, and other statistics about
  the distances between colors.
- **CVD Safety**: How the palette performs under simulated color vision
  deficiency.
- **Color Distribution**: Statistics on the spread of lightness, chroma,
  and hue.

### `plot_palette_analysis()`

For a more visual analysis, the
[`plot_palette_analysis()`](https://sims1253.github.io/huerd/pr/20/reference/plot_palette_analysis.md)
function creates a comprehensive dashboard.

``` r

# Create the diagnostic dashboard
plot_palette_analysis(brand_palette)
```

![](introduction-to-huerd_files/figure-html/unnamed-chunk-5-1.png)

This dashboard provides six key visualizations:

1.  **Color Swatches**: An overview of the palette with key metrics.
2.  **OKLAB Color Space**: A projection of the colors in the `a*b*`
    plane of the OKLAB space, with point size indicating lightness.
3.  **Pairwise Distance Matrix**: A heatmap showing the perceptual
    distance between every pair of colors on a fixed scale, so cell
    colors are comparable across palettes.
4.  **CVD Simulation**: How the palette appears to individuals with the
    three most common types of color vision deficiency, plus greyscale.
5.  **Pairwise Distances under CVD**: The distribution of pairwise
    distances under each CVD simulation, compared with normal vision.
6.  **Comparative Palettes**: A comparison of your palette’s distance
    distribution against established palettes like Batlow, Viridis, and
    Set2.

## CVD Accessibility

`huerd` provides two main tools for working with CVD.

### `is_cvd_safe()`

This function provides a simple, programmatic check to see if a palette
meets a minimum threshold for CVD safety.

``` r

is_cvd_safe(brand_palette)
#> [1] TRUE
```

### `simulate_palette_cvd()`

This function allows you to see how your palette would appear to
individuals with different types of CVD.

``` r

# Simulate the appearance for all CVD types
cvd_simulation <- simulate_palette_cvd(brand_palette, cvd_type = "all")
print(cvd_simulation)
#> 
#> -- huerd CVD Simulation Result (Multiple Types, Severity: 1.00) --
#> Palette for: original
#>   [ 1] #691B4B
#>   [ 2] #4A6B8A
#>   [ 3] #C2312B
#>   [ 4] #E5A04C
#>   [ 5] #F9C3FF
#>   [ 6] #AAF79A
#> Palette for: protan
#>   [ 1] #25324C
#>   [ 2] #5E6C8B
#>   [ 3] #5D5429
#>   [ 4] #B7A443
#>   [ 5] #BED1FF
#>   [ 6] #FCE893
#> Palette for: deutan
#>   [ 1] #3C3F49
#>   [ 2] #566589
#>   [ 3] #817325
#>   [ 4] #C7B54E
#>   [ 5] #CAD7FD
#>   [ 6] #F0E19F
#> Palette for: tritan
#>   [ 1] #711930
#>   [ 2] #307275
#>   [ 3] #D60031
#>   [ 4] #F98F8E
#>   [ 5] #FBC9D8
#>   [ 6] #A5F1E0
```

## Conclusion

This vignette has covered the core functionality of the `huerd` package.
By combining pure minimax optimization with comprehensive analysis
tools, `huerd` provides a powerful and flexible solution for creating
high-quality, accessible color palettes.

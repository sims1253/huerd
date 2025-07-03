
<!-- README.md is generated from README.Rmd. Please edit that file -->

# huerd <a href="https://sims1253.github.io/huerd/"><img src="man/figures/logo.png" align="right" height="120" alt="huerd website" /></a>

<!-- badges: start -->

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R-CMD-check](https://github.com/sims1253/huerd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sims1253/huerd/actions/workflows/R-CMD-check.yaml)
[![Tests](https://github.com/sims1253/huerd/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/sims1253/huerd/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sims1253/huerd/graph/badge.svg)](https://app.codecov.io/gh/sims1253/huerd)
[![GH-Pages](https://github.com/sims1253/huerd/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/sims1253/huerd/actions/workflows/pkgdown.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

A discrete color palette generator with support for fixed colors,
optimized for color vision deficient viewers.

## Installation

You can install the development version of huerd from GitHub with:

``` r
# install.packages("pak")
pak::pak("sims1253/huerd")
```

## Basic Example

Generate a palette with 5 colors:

``` r
library(huerd)

palette <- generate_palette(8)
print(palette)
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #3D0000
#> [ 2] #0000E4
#> [ 3] #FF0000
#> [ 4] #0091FF
#> [ 5] #FF00E5
#> [ 6] #BCAC5E
#> [ 7] #FF85B7
#> [ 8] #00DDFF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.188
#> * Optimizer Performance Ratio      : 60.6%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.101
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 861
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

## Constrained Color Palettes

Include specific colors while optimizing the remaining colors:

``` r
library(huerd)

palette <- generate_palette(
  n = 8,
  include_colors = c("#4A6B8A", "#E5A04C")
)
print(palette)
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #4C4100
#> [ 2] #005156
#> [ 3] #4A6B8A
#> [ 4] #427AEC
#> [ 5] #A09EAF
#> [ 6] #E5A04C
#> [ 7] #8EB4FF
#> [ 8] #00EFFF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.115
#> * Optimizer Performance Ratio      : 37.1%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.106
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 573
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

## Diagnostic Dashboard

Analyze palette quality with a dashboard:

``` r
library(huerd)

palette <- generate_palette(8)
plot_palette_analysis(palette)
```

<img src="man/figures/README-dashboard-1.png" width="100%" />

## Palette Quality Evaluation

Pure data provider for detailed post-hoc analysis:

``` r
library(huerd)

palette <- generate_palette(8)
evaluation <- evaluate_palette(palette)

# Access raw metrics (no subjective scoring)
cat("Minimum distance:", evaluation$distances$min, "\n")
#> Minimum distance: 0.09164296
cat("Performance ratio:", evaluation$distances$performance_ratio * 100, "%\n")
#> Performance ratio: 29.57528 %
cat("CVD worst case:", evaluation$cvd_safety$worst_case_min_distance, "\n")
#> CVD worst case: 0.0596302
```

## Custom Parameters

Fine-tune the generation process:

``` r
library(huerd)

palette <- generate_palette(
  n = 8,
  initialization = "harmony",              # Color harmony-based initialization
  init_lightness_bounds = c(0.3, 0.8),    # Constrain lightness range
  max_iterations = 2000                    # Show progress
)
print(palette)
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #705DB7
#> [ 2] #C86AB6
#> [ 3] #9C9A2F
#> [ 4] #F17587
#> [ 5] #27C4C1
#> [ 6] #FDA700
#> [ 7] #B7CE9B
#> [ 8] #84DAFD
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.113
#> * Optimizer Performance Ratio      : 36.4%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.083
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 546
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

## Complete Workflow Example

``` r
library(huerd)

# 1. Generate brand palette
brand_palette <- generate_palette(
  n = 8,
  include_colors = c("#1f77b4", "#ff7f0e"),  # Fixed brand colors
  fixed_aesthetic_influence = 0.9,
  initialization = "harmony",
  return_metrics = TRUE,
  progress = TRUE
)
#> Preparing for palette generation...
#> Adapting initialization from fixed colors' aesthetics...
#> Initializing 6 free colors (method: harmony)...
#> Optimizing 6 free colors...
#> Finalizing palette...
#> Done.

# 2. Diagnostic analysis
plot_palette_analysis(brand_palette)
```

<img src="man/figures/README-workflow-1.png" width="100%" />

``` r

# 3. Quality evaluation
evaluation <- evaluate_palette(brand_palette)
cat("Min distance:", round(evaluation$distances$min, 3), "\n")
#> Min distance: 0.124
cat("Performance:", round(evaluation$distances$performance_ratio * 100, 1), "%\n")
#> Performance: 40.1 %

# 4. CVD accessibility check
cvd_safe <- is_cvd_safe(brand_palette)
if (cvd_safe) {
  cat("Palette is CVD-accessible\n")
} else {
  cat("Palette may challenge CVD viewers\n")
}
#> Palette is CVD-accessible

# 5. CVD simulation for verification
cvd_simulation <- simulate_palette_cvd(brand_palette, cvd_type = "all")
print(cvd_simulation)
#> 
#> -- huerd CVD Simulation Result (Multiple Types, Severity: 1.00) --
#> Palette for: original
#>   [ 1] #AC4374
#>   [ 2] #1F77B4
#>   [ 3] #827300
#>   [ 4] #4CAC8E
#>   [ 5] #AB80FF
#>   [ 6] #FF7F0E
#>   [ 7] #00CAE2
#>   [ 8] #DBE9FF
#> Palette for: protan
#>   [ 1] #535D75
#>   [ 2] #5A79B7
#>   [ 3] #806F00
#>   [ 4] #A8A28D
#>   [ 5] #5298FF
#>   [ 6] #A59100
#>   [ 7] #B6C2E3
#>   [ 8] #E2EAFF
#> Palette for: deutan
#>   [ 1] #6F7072
#>   [ 2] #456CB3
#>   [ 3] #84750F
#>   [ 4] #999790
#>   [ 5] #5C96FC
#>   [ 6] #C4AE05
#>   [ 7] #9DB0E2
#>   [ 8] #DEE6FE
#> Palette for: tritan
#>   [ 1] #B93C56
#>   [ 2] #00868D
#>   [ 3] #8D6961
#>   [ 4] #1BADA3
#>   [ 5] #9899B4
#>   [ 6] #FF616D
#>   [ 7] #00D5D1
#>   [ 8] #D3EDF0

# 6. Display final palette (colors are brightness-sorted)
print(brand_palette)
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #AC4374
#> [ 2] #1F77B4
#> [ 3] #827300
#> [ 4] #4CAC8E
#> [ 5] #AB80FF
#> [ 6] #FF7F0E
#> [ 7] #00CAE2
#> [ 8] #DBE9FF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.124
#> * Optimizer Performance Ratio      : 40.1%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.116
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 634
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

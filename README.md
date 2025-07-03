
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
#> [ 1] #50001D
#> [ 2] #2F00FF
#> [ 3] #B20000
#> [ 4] #449752
#> [ 5] #FF4000
#> [ 6] #2B9AFF
#> [ 7] #00DA4F
#> [ 8] #00ECFF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.181
#> * Optimizer Performance Ratio      : 58.5%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.103
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 608
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
#> [ 1] #512900
#> [ 2] #4A6B8A
#> [ 3] #A1701C
#> [ 4] #339B8E
#> [ 5] #A99BF4
#> [ 6] #2BC9AB
#> [ 7] #E5A04C
#> [ 8] #8FF0C6
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.130
#> * Optimizer Performance Ratio      : 41.9%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.121
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 517
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
#> Minimum distance: 0.1419445
cat("Performance ratio:", evaluation$distances$performance_ratio * 100, "%\n")
#> Performance ratio: 45.80875 %
cat("CVD worst case:", evaluation$cvd_safety$worst_case_min_distance, "\n")
#> CVD worst case: 0.1004943
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
#> [ 1] #316E99
#> [ 2] #86614A
#> [ 3] #838F1C
#> [ 4] #009C8F
#> [ 5] #BD8ED7
#> [ 6] #6ED1BF
#> [ 7] #FCC870
#> [ 8] #E1E3FF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.138
#> * Optimizer Performance Ratio      : 44.5%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.106
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 784
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
#> Min distance: 0.131
cat("Performance:", round(evaluation$distances$performance_ratio * 100, 1), "%\n")
#> Performance: 42.1 %

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
#>   [ 1] #1F77B4
#>   [ 2] #AF6E00
#>   [ 3] #00A781
#>   [ 4] #BF8ACD
#>   [ 5] #FF7F0E
#>   [ 6] #2BDE8E
#>   [ 7] #FFA2F2
#>   [ 8] #FFDEED
#> Palette for: protan
#>   [ 1] #5A79B7
#>   [ 2] #847300
#>   [ 3] #A29B7F
#>   [ 4] #8299D0
#>   [ 5] #A59100
#>   [ 6] #DCCC89
#>   [ 7] #A0B9F5
#>   [ 8] #E0E4EE
#> Palette for: deutan
#>   [ 1] #456CB3
#>   [ 2] #938205
#>   [ 3] #918E84
#>   [ 4] #8E9FCB
#>   [ 5] #C4AE05
#>   [ 6] #C8BD93
#>   [ 7] #B7C6EF
#>   [ 8] #E7E9EC
#> Palette for: tritan
#>   [ 1] #00868D
#>   [ 2] #C05E5D
#>   [ 3] #00A89C
#>   [ 4] #C092A2
#>   [ 5] #FF616D
#>   [ 6] #00DCC9
#>   [ 7] #FFA7C0
#>   [ 8] #FFDDE3

# 6. Display final palette (colors are brightness-sorted)
print(brand_palette)
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #1F77B4
#> [ 2] #AF6E00
#> [ 3] #00A781
#> [ 4] #BF8ACD
#> [ 5] #FF7F0E
#> [ 6] #2BDE8E
#> [ 7] #FFA2F2
#> [ 8] #FFDEED
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.131
#> * Optimizer Performance Ratio      : 42.1%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.100
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 406
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

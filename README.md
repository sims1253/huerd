
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
optimized for color vision deficient viewers. Features different
optimization algorithms and a multi-objective optimization framework for
advanced color palette generation.

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

palette <- generate_palette(8, progress = FALSE)
print(palette)
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #6C0000
#> [ 2] #007F77
#> [ 3] #0084FF
#> [ 4] #FF0000
#> [ 5] #E900FF
#> [ 6] #9792B2
#> [ 7] #00B1EA
#> [ 8] #00DBFF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.115
#> * Optimizer Performance Ratio      : 37.0%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.066
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 487
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

## Constrained Color Palettes

Include specific colors while optimizing the remaining colors:

``` r
library(huerd)

palette <- generate_palette(
  n = 8,
  include_colors = c("#4A6B8A", "#E5A04C"),
  progress = FALSE
)
print(palette)
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #700000
#> [ 2] #114661
#> [ 3] #A73800
#> [ 4] #4A6B8A
#> [ 5] #CE673B
#> [ 6] #00AAAD
#> [ 7] #E5A04C
#> [ 8] #FFC7D8
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.132
#> * Optimizer Performance Ratio      : 42.7%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.130
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 533
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

## Multi-Optimizer Support

Choose from 4 different optimization algorithms based on your needs:

``` r
library(huerd)

# COBYLA: Default deterministic optimizer for general use
cobyla_palette <- generate_palette(6, optimizer = "nloptr_cobyla", progress = FALSE)

# SANN: Stochastic simulated annealing for higher quality
sann_palette <- generate_palette(6, optimizer = "sann", progress = FALSE)

# DIRECT: Global optimization for reproducibility (may need tuning)
direct_palette <- generate_palette(6, optimizer = "nlopt_direct", progress = FALSE)

# Nelder-Mead: Derivative-free local optimization
# As an alternative deterministic approach
neldermead_palette <- generate_palette(6, optimizer = "nlopt_neldermead", progress = FALSE)

cat("COBYLA:", paste(cobyla_palette, collapse = ", "), "\n")
#> COBYLA: #2600BA, #970000, #0000FF, #008A00, #FF0000, #BAA69E
cat("SANN:", paste(sann_palette, collapse = ", "), "\n")
#> SANN: #300000, #6E0000, #0000D7, #00A100, #CE74FF, #00FFFF
cat("DIRECT:", paste(direct_palette, collapse = ", "), "\n")
#> DIRECT: #636363, #636363, #636363, #636363, #636363, #636363
cat("Nelder-Mead:", paste(neldermead_palette, collapse = ", "), "\n")
#> Nelder-Mead: #9D0000, #FF0000, #FF77C5, #A7C100, #FFB8A2, #00FBFF
```

## Multi-Objective Framework

The package includes a multi-objective optimization framework:

``` r
library(huerd)

# Current: Pure distance optimization (default)
distance_palette <- generate_palette(
  n = 6,
  weights = c(distance = 1),  # Explicit distance weighting
  optimizer = "nloptr_cobyla",
  progress = FALSE
)

# Future versions will support additional objectives like:
# weights = c(distance = 0.7, aesthetics = 0.3)
# weights = c(distance = 0.8, uniformity = 0.2)

print(distance_palette)
#> 
#> -- huerd Color Palette (6 colors) --
#> Colors:
#> [ 1] #001E00
#> [ 2] #A50000
#> [ 3] #FF0000
#> [ 4] #00A5FF
#> [ 5] #00FFFF
#> [ 6] #FFF300
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.189
#> * Optimizer Performance Ratio      : 51.6%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.136
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 329
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

## Diagnostic Dashboard

Get a quick overview of your palette properties:

``` r
library(huerd)

palette <- generate_palette(8, progress = FALSE)
plot_palette_analysis(palette, force_font_scale = 0.6)
```

<img src="man/figures/README-dashboard-1.png" width="100%" />

## Palette Quality Evaluation

Or look at the numerical evaluation results:

``` r
library(huerd)

palette <- generate_palette(8, progress = FALSE)
evaluation <- evaluate_palette(palette)

# Access raw metrics (no subjective scoring)
cat("Minimum distance:", evaluation$distances$min, "\n")
#> Minimum distance: 0.2073131
cat("Performance ratio:", evaluation$distances$performance_ratio * 100, "%\n")
#> Performance ratio: 66.9047 %
cat("CVD worst case:", evaluation$cvd_safety$worst_case_min_distance, "\n")
#> CVD worst case: 0.08815642
```

## Custom Parameters

Fine-tune the generation process with advanced options:

``` r
library(huerd)

palette <- generate_palette(
  n = 8,
  initialization = "harmony",              # Color harmony-based initialization
  init_lightness_bounds = c(0.3, 0.8),    # Constrain lightness range
  max_iterations = 2000,                   # Increased iterations
  optimizer = "nloptr_cobyla",             # Use COBYLA for optimization
  progress = FALSE
)
print(palette)
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #CE4572
#> [ 2] #00AC8F
#> [ 3] #00BACF
#> [ 4] #A6AF5F
#> [ 5] #B9AFFC
#> [ 6] #D0BE8F
#> [ 7] #FFB9E7
#> [ 8] #67FFA4
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.093
#> * Optimizer Performance Ratio      : 29.9%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.065
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 573
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

## Complete Workflow Example

``` r
library(huerd)

# 1. Generate brand palette with advanced optimization
brand_palette <- generate_palette(
  n = 8,
  include_colors = c("#1f77b4", "#ff7f0e"),  # Fixed brand colors
  fixed_aesthetic_influence = 0.9,
  initialization = "harmony",
  optimizer = "sann",
  max_iterations = 5000,
  weights = c(distance = 1),
  return_metrics = TRUE,
  progress = TRUE
)
#> Preparing for palette generation...
#> Adapting initialization from fixed colors' aesthetics...
#> Initializing 6 free colors (method: harmony)...
#> Optimizing 6 free colors using sann...
#> Finalizing palette...
#> Done.

# 2. Diagnostic analysis
plot_palette_analysis(brand_palette, force_font_scale = 0.6)
```

<img src="man/figures/README-workflow-1.png" width="100%" />

``` r

# 3. Quality evaluation
evaluation <- evaluate_palette(brand_palette)
cat("Min distance:", round(evaluation$distances$min, 3), "\n")
#> Min distance: 0.18
cat("Performance:", round(evaluation$distances$performance_ratio * 100, 1), "%\n")
#> Performance: 57.9 %

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
#>   [ 1] #450000
#>   [ 2] #005800
#>   [ 3] #3100DD
#>   [ 4] #1F77B4
#>   [ 5] #FF0055
#>   [ 6] #FF7F0E
#>   [ 7] #F5B7FF
#>   [ 8] #45FF00
#> Palette for: protan
#>   [ 1] #181400
#>   [ 2] #5A4E00
#>   [ 3] #004EE2
#>   [ 4] #5A79B7
#>   [ 5] #666355
#>   [ 6] #A59100
#>   [ 7] #B0C8FF
#>   [ 8] #FFE600
#> Palette for: deutan
#>   [ 1] #292300
#>   [ 2] #52480D
#>   [ 3] #003ADA
#>   [ 4] #456CB3
#>   [ 5] #9F914E
#>   [ 6] #C4AE05
#>   [ 7] #BECFFD
#>   [ 8] #F1D83A
#> Palette for: tritan
#>   [ 1] #4D0001
#>   [ 2] #005549
#>   [ 3] #005B81
#>   [ 4] #00868D
#>   [ 5] #FF0032
#>   [ 6] #FF616D
#>   [ 7] #F7BED1
#>   [ 8] #00F7D9

# 6. Display final palette (colors are brightness-sorted)
print(brand_palette)
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #450000
#> [ 2] #005800
#> [ 3] #3100DD
#> [ 4] #1F77B4
#> [ 5] #FF0055
#> [ 6] #FF7F0E
#> [ 7] #F5B7FF
#> [ 8] #45FF00
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.180
#> * Optimizer Performance Ratio      : 57.9%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.093
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 5000
#> * Optimizer Status: Optimization converged
```

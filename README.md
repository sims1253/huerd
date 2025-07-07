
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
#> [ 1] #420031
#> [ 2] #0000F0
#> [ 3] #008B00
#> [ 4] #FF0000
#> [ 5] #F800FF
#> [ 6] #FF9A00
#> [ 7] #00CCFF
#> [ 8] #00FFFF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.143
#> * Optimizer Performance Ratio      : 46.0%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.098
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 593
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
#> [ 1] #562C08
#> [ 2] #7A5300
#> [ 3] #4A6B8A
#> [ 4] #6188E5
#> [ 5] #9EAAB1
#> [ 6] #E5A04C
#> [ 7] #D0CAFF
#> [ 8] #6DFF00
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.136
#> * Optimizer Performance Ratio      : 44.0%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.129
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 546
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

## Multi-Optimizer Support

Choose from 5 different optimization algorithms based on your needs:

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

# L-BFGS: Gradient-based optimization for smooth objectives (v0.5.0+)
lbfgs_palette <- generate_palette(6, optimizer = "nlopt_lbfgs", 
                                  weights = c(smooth_repulsion = 1), progress = FALSE)

cat("COBYLA:", paste(cobyla_palette, collapse = ", "), "\n")
#> COBYLA: #000064, #830000, #0000FF, #FF0000, #FFB900, #A3D0FF
cat("SANN:", paste(sann_palette, collapse = ", "), "\n")
#> SANN: #0F0C00, #4C006B, #004500, #DD6A00, #AD88AE, #74DDFF
cat("DIRECT:", paste(direct_palette, collapse = ", "), "\n")
#> DIRECT: #636363, #636363, #636363, #636363, #636363, #636363
cat("Nelder-Mead:", paste(neldermead_palette, collapse = ", "), "\n")
#> Nelder-Mead: #AB0000, #FF3000, #FF00FF, #FF89FF, #00FFFF, #FFE100
cat("L-BFGS:", paste(lbfgs_palette, collapse = ", "), "\n")
#> L-BFGS: #2E0079, #000092, #293700, #FF0000, #00FF00, #00FFFF
```

## Multi-Objective Framework

The package includes a multi-objective optimization framework with both
discrete and smooth optimization support:

``` r
library(huerd)

# Discrete distance optimization (default)
distance_palette <- generate_palette(
  n = 6,
  weights = c(distance = 1),  # Explicit distance weighting
  optimizer = "nloptr_cobyla",
  progress = FALSE
)

# Smooth optimization for faster convergence (v0.5.0+)
smooth_palette <- generate_palette(
  n = 8,
  weights = c(smooth_repulsion = 1),  # Smooth repulsion objective
  optimizer = "nlopt_lbfgs",          # L-BFGS for gradient-based optimization
  progress = FALSE
)

# Alternative smooth objective using log-sum-exp
logsumexp_palette <- generate_palette(
  n = 6,
  weights = c(smooth_logsumexp = 1),
  optimizer = "nlopt_lbfgs",
  progress = FALSE
)

print(distance_palette)
#> 
#> -- huerd Color Palette (6 colors) --
#> Colors:
#> [ 1] #460113
#> [ 2] #912F00
#> [ 3] #008AFF
#> [ 4] #FF0087
#> [ 5] #F1E200
#> [ 6] #00FFFF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.206
#> * Optimizer Performance Ratio      : 56.4%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.192
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 470
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
#> Minimum distance: 0.1671957
cat("Performance ratio:", evaluation$distances$performance_ratio * 100, "%\n")
#> Performance ratio: 53.95788 %
cat("CVD worst case:", evaluation$cvd_safety$worst_case_min_distance, "\n")
#> CVD worst case: 0.1004561
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
#> [ 1] #009B72
#> [ 2] #BA61C1
#> [ 3] #DF6056
#> [ 4] #8881FF
#> [ 5] #FF83A9
#> [ 6] #EDB580
#> [ 7] #85F8FF
#> [ 8] #FDFDC9
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.130
#> * Optimizer Performance Ratio      : 42.0%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.080
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 1011
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
#> Min distance: 0.191
cat("Performance:", round(evaluation$distances$performance_ratio * 100, 1), "%\n")
#> Performance: 61.8 %

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
#>   [ 1] #00322D
#>   [ 2] #3700B4
#>   [ 3] #1F77B4
#>   [ 4] #DD0000
#>   [ 5] #A000FF
#>   [ 6] #FF7F0E
#>   [ 7] #D9A1FF
#>   [ 8] #A8FF00
#> Palette for: protan
#>   [ 1] #2F2E2D
#>   [ 2] #003FB8
#>   [ 3] #5A79B7
#>   [ 4] #5D5100
#>   [ 5] #0068FF
#>   [ 6] #A59100
#>   [ 7] #8FB4FF
#>   [ 8] #FFEB00
#> Palette for: deutan
#>   [ 1] #282A2E
#>   [ 2] #0032B1
#>   [ 3] #456CB3
#>   [ 4] #8D7C00
#>   [ 5] #006BFB
#>   [ 6] #C4AE05
#>   [ 7] #9CB8FC
#>   [ 8] #FFE537
#> Palette for: tritan
#>   [ 1] #003330
#>   [ 2] #004868
#>   [ 3] #00868D
#>   [ 4] #F4000B
#>   [ 5] #8C6196
#>   [ 6] #FF616D
#>   [ 7] #D5AEC4
#>   [ 8] #ACF3D9

# 6. Display final palette (colors are brightness-sorted)
print(brand_palette)
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #00322D
#> [ 2] #3700B4
#> [ 3] #1F77B4
#> [ 4] #DD0000
#> [ 5] #A000FF
#> [ 6] #FF7F0E
#> [ 7] #D9A1FF
#> [ 8] #A8FF00
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.191
#> * Optimizer Performance Ratio      : 61.8%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.109
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 5000
#> * Optimizer Status: Optimization converged
```

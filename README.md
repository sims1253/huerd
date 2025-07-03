
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
#> [ 1] #170600
#> [ 2] #640097
#> [ 3] #9A0038
#> [ 4] #0044FF
#> [ 5] #FF0000
#> [ 6] #00ABFF
#> [ 7] #00FFEB
#> [ 8] #EFFF00
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.206
#> * Optimizer Performance Ratio      : 66.5%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.169
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 677
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
#> [ 1] #13345B
#> [ 2] #9A3A34
#> [ 3] #4A6B8A
#> [ 4] #4E9558
#> [ 5] #C07FA2
#> [ 6] #E5A04C
#> [ 7] #70CFA4
#> [ 8] #D3BCFF
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.169
#> * Optimizer Performance Ratio      : 54.5%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.095
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 392
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
#> Minimum distance: 0.1539379
cat("Performance ratio:", evaluation$distances$performance_ratio * 100, "%\n")
#> Performance ratio: 49.67927 %
cat("CVD worst case:", evaluation$cvd_safety$worst_case_min_distance, "\n")
#> CVD worst case: 0.1403357
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
#> [ 1] #498F5D
#> [ 2] #9965BC
#> [ 3] #A294F7
#> [ 4] #E39890
#> [ 5] #00D1D9
#> [ 6] #D1D770
#> [ 7] #00F7FF
#> [ 8] #FFF5BC
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.106
#> * Optimizer Performance Ratio      : 34.2%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.102
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 665
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
#> Min distance: 0.134
cat("Performance:", round(evaluation$distances$performance_ratio * 100, 1), "%\n")
#> Performance: 43.3 %

# 4. CVD accessibility check
cvd_safe <- is_cvd_safe(brand_palette)
if (cvd_safe) {
  cat("Palette is CVD-accessible\n")
} else {
  cat("Palette may challenge CVD viewers\n")
}
#> Palette may challenge CVD viewers

# 5. CVD simulation for verification
cvd_simulation <- simulate_palette_cvd(brand_palette, cvd_type = "all")
print(cvd_simulation)
#> 
#> -- huerd CVD Simulation Result (Multiple Types, Severity: 1.00) --
#> Palette for: original
#>   [ 1] #1F77B4
#>   [ 2] #BA3E9D
#>   [ 3] #009F69
#>   [ 4] #A581CA
#>   [ 5] #FF7F0E
#>   [ 6] #F28BC5
#>   [ 7] #00FCE9
#>   [ 8] #EFEA52
#> Palette for: protan
#>   [ 1] #5A79B7
#>   [ 2] #4364A0
#>   [ 3] #9D9266
#>   [ 4] #728FCD
#>   [ 5] #A59100
#>   [ 6] #93A2C7
#>   [ 7] #EEEDE8
#>   [ 8] #FDE13C
#> Palette for: deutan
#>   [ 1] #456CB3
#>   [ 2] #6A779A
#>   [ 3] #8D866D
#>   [ 4] #7991C8
#>   [ 5] #C4AE05
#>   [ 6] #AEB3C2
#>   [ 7] #D3D9EB
#>   [ 8] #FFE75C
#> Palette for: tritan
#>   [ 1] #00868D
#>   [ 2] #C44368
#>   [ 3] #009E91
#>   [ 4] #A08C9C
#>   [ 5] #FF616D
#>   [ 6] #FE8AA0
#>   [ 7] #00FFF6
#>   [ 8] #FFDBCB

# 6. Display final palette (colors are brightness-sorted)
print(brand_palette)
#> 
#> -- huerd Color Palette (8 colors) --
#> Colors:
#> [ 1] #1F77B4
#> [ 2] #BA3E9D
#> [ 3] #009F69
#> [ 4] #A581CA
#> [ 5] #FF7F0E
#> [ 6] #F28BC5
#> [ 7] #00FCE9
#> [ 8] #EFEA52
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.134
#> * Optimizer Performance Ratio      : 43.3%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.073
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 727
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.
```

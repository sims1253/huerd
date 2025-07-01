# huerd: Herding hues

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R-CMD-check](https://github.com/sims1253/huerd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sims1253/huerd/actions/workflows/R-CMD-check.yaml)
[![Tests](https://github.com/sims1253/huerd/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/sims1253/huerd/actions/workflows/test-coverage.yaml)
[![Codecov test coverage](https://codecov.io/gh/sims1253/huerd/graph/badge.svg)](https://app.codecov.io/gh/sims1253/huerd)
[![GH-Pages](https://github.com/sims1253/huerd/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/sims1253/huerd/actions/workflows/pkgdown.yaml)

A discrete color palette generator with support for fixed colors optimized for both normal and color vision deficient viewers.

## Installation

```R
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("sims1253/huerd")
```

## Basic Usage

Generate a palette of maximally distinct colors using pure minimax optimization:

```R
library(huerd)

# Generate 5 colors (automatically sorted by brightness)
palette <- generate_palette(5)
print(palette)

# The package uses pure minimax optimization to maximize the minimum 
# perceptual distance between any two colors in the palette
```

## Constrained Color Palettes

Include specific colors while optimizing the remaining colors:

```R
# Generate 6 colors including fixed colors
palette <- generate_palette(
  n = 6,
  include_colors = c("#4A6B8A", "#E5A04C")
)
print(palette)
```

## Diagnostic Dashboard

Analyze palette quality with a dashboard:

```R
# Generate a palette
palette <- generate_palette(6)

# Create diagnostic dashboard
plot_palette_analysis(palette)
```

## Palette Quality Evaluation

Pure data provider for detailed post-hoc analysis:

```R
# Generate and evaluate a palette
palette <- generate_palette(4)
evaluation <- evaluate_palette(palette)

# Access raw metrics (no subjective scoring)
cat("Minimum distance:", evaluation$distances$min, "\n")
cat("Performance ratio:", evaluation$distances$performance_ratio * 100, "%\n")
cat("CVD worst case:", evaluation$cvd_safety$worst_case_min_distance, "\n")
```

## Custom Parameters

Fine-tune the generation process:

```R
palette <- generate_palette(
  n = 8,
  initialization = "harmony",              # Color harmony-based initialization
  init_lightness_bounds = c(0.3, 0.8),    # Constrain lightness range
  max_iterations = 2000                    # Show progress
)
```

## Complete Workflow Example

```R
library(huerd)

# 1. Generate brand-constrained palette with pure minimax optimization
brand_palette <- generate_palette(
  n = 8,
  include_colors = c("#1f77b4", "#ff7f0e"),  # Fixed brand colors
  return_metrics = TRUE,
  progress = TRUE
)

# 2. Comprehensive diagnostic analysis
plot_palette_analysis(brand_palette)

# 3. Detailed quality evaluation
evaluation <- evaluate_palette(brand_palette)
cat("Min distance:", round(evaluation$distances$min, 3), "\n")
cat("Performance:", round(evaluation$distances$performance_ratio * 100, 1), "%\n")

# 4. CVD accessibility check
cvd_safe <- is_cvd_safe(brand_palette)
if (cvd_safe) {
  cat("Palette is CVD-accessible\n")
} else {
  cat("Palette may challenge CVD viewers\n")
}

# 5. CVD simulation for verification
cvd_simulation <- simulate_palette_cvd(brand_palette, cvd_type = "all")
print(cvd_simulation)

# 6. Display final palette (colors are brightness-sorted)
print(brand_palette)
```

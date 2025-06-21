# huerd: Herding hues

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R-CMD-check](https://github.com/sims1253/huerd/workflows/R-CMD-check/badge.svg)](https://github.com/sims1253/huerd/actions)
[![Actions Status](https://github.com/sims1253/huerd/workflows/Tests/badge.svg)](https://github.com/sims1253/huerd/actions)
[![Codecov test coverage](https://codecov.io/gh/sims1253/huerd/graph/badge.svg)](https://app.codecov.io/gh/sims1253/huerd)

A discrete color palette generation tool that creates perceptually distinct colors optimized for both normal and color vision deficient viewers.

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

## Key Features

### Pure Minimax Optimization

Scientifically-grounded approach that maximizes the minimum perceptual distance:

```R
# Pure minimax optimization - no complex parameters needed
palette <- generate_palette(n = 6)

# Evaluate the optimization quality
evaluation <- evaluate_palette(palette)
cat("Min distance:", evaluation$distances$min, "\n")
cat("Performance ratio:", evaluation$distances$performance_ratio * 100, "%\n")
```

### Constrained Color Palettes

Include specific brand colors while optimizing the remaining colors:

```R
# Generate 6 colors including fixed brand colors
palette <- generate_palette(
  n = 6,
  include_colors = c("#4A6B8A", "#E5A04C")  # Fixed brand colors
)
print(palette)
```

### Automatic Brightness Sorting

All palettes are automatically sorted by lightness for intuitive ordering:

```R
palette <- generate_palette(5)
# Colors are automatically ordered from dark to light
# This makes palettes more intuitive and consistent
```

## Advanced Usage

### Comprehensive Diagnostic Dashboard

Analyze palette quality with a scientific visualization dashboard:

```R
# Generate a palette
palette <- generate_palette(6)

# Create comprehensive diagnostic dashboard
plot_palette_analysis(palette)

# The dashboard shows:
# - Color swatches with metrics
# - Perceptual distance heatmap  
# - OKLAB color space projection
# - CVD simulations
# - Comparison with reference palettes
```

### Palette Quality Evaluation

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

### CVD Analysis

Analyze color vision deficiency safety:

```R
# Simulate CVD appearance
simulation <- simulate_palette_cvd(
  colors = palette,
  cvd_type = "all",
  severity = 1.0
)
print(simulation)

# Check safety programmatically
safety_check <- is_cvd_safe(palette)
cat("CVD safe:", safety_check, "\n")
```

### Custom Parameters

Fine-tune the generation process:

```R
palette <- generate_palette(
  n = 8,
  initialization = "harmony",              # Color harmony-based initialization
  init_lightness_bounds = c(0.3, 0.8),    # Constrain lightness range
  max_iterations = 2000,                   # More optimization iterations
  progress = TRUE                          # Show progress
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
  cat("✓ Palette is CVD-accessible\n")
} else {
  cat("⚠ Palette may challenge CVD viewers\n")
}

# 5. CVD simulation for verification
cvd_simulation <- simulate_palette_cvd(brand_palette, cvd_type = "all")
print(cvd_simulation)

# 6. Display final palette (colors are brightness-sorted)
print(brand_palette)
```

## Technical Details

- **Algorithm**: Pure minimax optimization maximizing minimum perceptual distance
- **Color Space**: All optimization performed in perceptually uniform OKLAB space  
- **Brightness Sorting**: Automatic sorting by OKLAB lightness with gamut clamping compensation
- **CVD Analysis**: Uses colorspace package's protan/deutan/tritan functions
- **Optimization Engine**: Box-constrained optimization via nloptr package
- **Diagnostics**: Comprehensive base R graphics dashboard (zero plotting dependencies)
- **Quality Metrics**: Pure data provider for objective post-hoc analysis
- **Dependencies**: farver, colorspace, nloptr, cli

## Scientific Approach

The package implements scientifically-grounded color palette generation:

1. **Pure Minimax Objective**: Focuses solely on maximizing the worst-case perceptual distance
2. **Perceptual Uniformity**: OKLAB color space ensures perceptual consistency  
3. **Automatic Sorting**: Brightness-ordered palettes for intuitive interpretation
4. **Objective Evaluation**: No subjective scoring - pure quantitative metrics
5. **Comprehensive Diagnostics**: Multi-panel analysis including comparative benchmarking

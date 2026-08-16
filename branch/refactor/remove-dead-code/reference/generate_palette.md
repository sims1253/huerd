# Generate Optimal Color Palette using Pure Minimax Optimization

Creates a scientifically-grounded color palette that maximizes the
minimum perceptual distance between any two colors using pure minimax
optimization in OKLAB color space. Colors are automatically sorted by
brightness and can include fixed brand colors.

## Usage

``` r
generate_palette(
  n,
  include_colors = NULL,
  initialization = c("k-means++", "harmony"),
  init_lightness_bounds = c(0.2, 0.9),
  init_hcl_bounds = list(C = c(40, 80), L = c(50, 80)),
  fixed_aesthetic_influence = 0.75,
  aesthetic_init_config = NULL,
  max_iterations = 1000,
  return_metrics = TRUE,
  progress = interactive(),
  weights = NULL,
  optimizer = "nloptr_cobyla",
  cvd_safe = TRUE,
  ...
)
```

## Arguments

- n:

  Integer. Total number of colors in the palette.

- include_colors:

  Character vector. Hex colors that must be included in the palette
  unchanged. Default is NULL.

- initialization:

  Character. Initialization method for free colors: "k-means++" or
  "harmony". Default is "k-means++".

- init_lightness_bounds:

  Numeric vector of length 2. For k-means++ initialization, target
  lightness (L in OKLAB) range for initial candidates. Default
  `c(0.2, 0.9)`.

- init_hcl_bounds:

  List. For harmony-based initialization, target `C` (Chroma) and `L`
  (Luminance) ranges for initial HCL colors. Default
  `list(C = c(40, 80), L = c(50, 80))`.

- fixed_aesthetic_influence:

  Numeric, 0 to 1. If `include_colors` are provided, controls how
  strongly their aesthetic properties influence the initialization for
  free colors. 0 = no influence, 1 = strong influence. Default is 0.75.

- aesthetic_init_config:

  List. Advanced configuration for aesthetic initialization. Use `NULL`
  (default) for built-in defaults.

- max_iterations:

  Integer. Maximum optimization iterations. Default is 1000.

- return_metrics:

  Logical. Whether to return evaluation metrics as attributes. Default
  is TRUE.

- progress:

  Logical. Show progress messages. Default is
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- weights:

  Named numeric vector. Weights for multi-objective optimization.
  Supports: `c(distance = 1)` for discrete distance optimization,
  `c(smooth_repulsion = 1)` for smooth repulsion objective using inverse
  squared distances, or `c(smooth_logsumexp = 1)` for smooth log-sum-exp
  objective. Default is NULL, which is internally equivalent to
  `c(distance = 1)` for most optimizers. For "nlopt_lbfgs", NULL
  defaults to `smooth_repulsion`.

- optimizer:

  Character. Optimization algorithm to use. Currently supported:
  "nloptr_cobyla" (default) for deterministic optimization with
  constraint handling, "sann" for stochastic simulated annealing
  (excellent quality but not perfectly reproducible without a seed),
  "nlopt_direct" for deterministic global optimization using the DIRECT
  algorithm (best choice for scientific reproducibility and high
  quality, though may be slower), "nlopt_neldermead" for derivative-free
  local optimization using the Nelder-Mead simplex algorithm (good
  alternative to COBYLA for robust local optimization), "nlopt_lbfgs"
  for gradient-based L-BFGS optimization (fastest convergence for smooth
  objectives; works best with `smooth_repulsion` or `smooth_logsumexp`
  weights). The framework is designed to easily support additional
  optimizers in future versions.

- cvd_safe:

  Logical. If `TRUE` (default), the objective maximizes the minimum
  perceptual distance in the worst case across deuteranopia, protanopia,
  and tritanopia simulations, producing palettes that are
  distinguishable for viewers with color vision deficiencies. If
  `FALSE`, the objective maximizes the minimum perceptual distance for
  normal vision only. Has no effect when `optimizer = "nlopt_lbfgs"`
  because the smooth objectives are normal-vision only.

- ...:

  Additional arguments reserved for future use.

## Value

A character vector of hex colors with class `huerd_palette`,
automatically sorted by brightness (lightness). If
`return_metrics = TRUE`, includes evaluation metrics as attributes.

## Details

This function implements pure minimax optimization to create color
palettes with maximum worst-case perceptual distinguishability. The
approach is scientifically grounded and focuses on a single, clear
objective.

The process:

1.  Initialize free colors using k-means++ or harmony-based methods

2.  Optimize using box-constrained nloptr to maximize the minimum
    perceptual distance (worst case across CVD simulations when
    `cvd_safe = TRUE`, the default)

3.  Sort final palette by OKLAB lightness for intuitive ordering

4.  Apply gamut compensation during brightness sorting

The pure minimax approach ensures optimal categorical color palettes
without complex multi-objective trade-offs. Quality can be assessed
using
[`evaluate_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/evaluate_palette.md)
and visualized with
[`plot_palette_analysis()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/plot_palette_analysis.md).

## Performance Tips

- For low performance ratios, try increasing `max_iterations`

- Use `progress = TRUE` to monitor optimization convergence

- Include 2-3 fixed colors maximum for best optimization

- Use diagnostic dashboard
  [`plot_palette_analysis()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/plot_palette_analysis.md)
  for analysis

## Examples

``` r
# Simple optimal palette
palette <- generate_palette(5, progress = FALSE)
print(palette)
#> 
#> -- huerd Color Palette (5 colors) --
#> Colors:
#> [ 1] #5600AD
#> [ 2] #F40000
#> [ 3] #F853FF
#> [ 4] #C7AA00
#> [ 5] #00FF00
#> 
#> -- Quality Metrics Summary --
#> * Min. Perceptual Distance (OKLAB): 0.251
#> * Optimizer Performance Ratio      : 61.1%
#> * Min. CVD-Safe Distance (OKLAB)  : 0.122
#> 
#> -- Generation Details --
#> * Optimizer Iterations: 382
#> * Optimizer Status: NLOPT_XTOL_REACHED: Optimization stopped because xtol_rel or xtol_abs (above) was reached.

# Brand-constrained palette
brand_palette <- generate_palette(
  n = 6,
  include_colors = c("#4A6B8A", "#E5A04C"),
  progress = FALSE
)

# Using specific optimizer (deterministic)
optimizer_palette <- generate_palette(
  n = 4,
  optimizer = "nloptr_cobyla",
  progress = FALSE
)

# Using simulated annealing (stochastic, excellent quality)
set.seed(42)  # For reproducibility
sann_palette <- generate_palette(
  n = 4,
  optimizer = "sann",
  progress = FALSE
)

# Using DIRECT algorithm (deterministic global, best for scientific
# reproducibility)
direct_palette <- generate_palette(
  n = 4,
  optimizer = "nlopt_direct",
  progress = FALSE
)

# Using Nelder-Mead algorithm (derivative-free local, good alternative
# to COBYLA)
neldermead_palette <- generate_palette(
  n = 4,
  optimizer = "nlopt_neldermead",
  progress = FALSE
)

# Using smooth optimization with L-BFGS (efficient for larger palettes)
smooth_palette <- generate_palette(
  n = 12,
  weights = c(smooth_repulsion = 1),
  optimizer = "nlopt_lbfgs",
  progress = FALSE
)

# Using alternative smooth objective
logsumexp_palette <- generate_palette(
  n = 6,
  weights = c(smooth_logsumexp = 1),
  optimizer = "nlopt_lbfgs",
  progress = FALSE
)

# Evaluate quality
evaluation <- evaluate_palette(brand_palette)
cat("Min distance:", evaluation$distances$min, "\n")
#> Min distance: 0.1237911 
cat("Performance:", evaluation$distances$performance_ratio * 100, "%\n")
#> Performance: 33.86589 %

# Comprehensive analysis
plot_palette_analysis(brand_palette)

```

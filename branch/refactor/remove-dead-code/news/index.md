# Changelog

## huerd (development version)

### Internal Changes

- Removed dead internal code: the unused `validate_color_input_smart()`
  / `validate_hex_colors()` / `validate_oklab_matrix()` validation
  subsystem, the legacy multi-objective
  `objective_function_aggregator()`, the unused
  `.validate_balance_weights()`, `.validate_aesthetic_weights()`, and
  `.normalize_weights()` helpers, and the unused `track`-free print
  indirection `.print_color_item()`
- Removed three `aesthetic_init_config` keys that were accepted but
  never used by any code (`kmeans_C_sd_multiplier`,
  `harmony_hcl_L_min_sd`, `harmony_hcl_C_min_sd`); passing them now
  errors with an “Invalid name” message instead of being silently
  ignored
- Cleaned unused NAMESPACE imports
  ([`colorspace::simulate_cvd`](https://colorspace.R-Forge.R-project.org/reference/simulate_cvd.html),
  [`stats::setNames`](https://rdrr.io/r/stats/setNames.html),
  [`stats::var`](https://rdrr.io/r/stats/cor.html),
  [`grDevices::rgb`](https://rdrr.io/r/grDevices/rgb.html))

------------------------------------------------------------------------

## huerd 0.6.2 (2026-08-16)

### Bug Fixes

- [`interpret_palette_quality()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/interpret_palette_quality.md)
  no longer errors on palettes with more than 40 colors, where the
  theoretical-maximum distance lookup is undefined and
  `performance_ratio` is `NA`; the summary now reports that the
  percentage is not available
- `validate_color_input_smart()` now reports the actual problem (“Colors
  must be a character vector (hex codes) or an OKLAB matrix”) for
  wrong-type inputs instead of an unrelated “no base colors provided”
  message
- [`export_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/export_palette.md)
  documentation now correctly states that `"hex"` returns a character
  vector, not a single string

### New Features

- [`generate_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/generate_palette.md)
  gains a `cvd_safe` argument (default `TRUE`): when `TRUE`, the
  objective maximizes the worst-case perceptual distance across
  deuteranopia, protanopia, and tritanopia simulations; when `FALSE`, it
  optimizes normal-vision distance only. Applies to all derivative-free
  optimizers; documented as having no effect for `"nlopt_lbfgs"`, whose
  smooth objectives are normal-vision only
- The previously ignored `cvd_safe` arguments of
  [`quick_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/quick_palette.md)
  and
  [`brand_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/brand_palette.md)
  are now forwarded to
  [`generate_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/generate_palette.md)
  and take effect
- [`reproduce_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/reproduce_palette.md)
  forwards the stored `cvd_safe` setting; palettes generated before
  0.6.2 reproduce with the previous default behavior

### Infrastructure

- Moved `withr` from Suggests to Imports:
  [`reproduce_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/reproduce_palette.md)
  calls
  [`withr::with_preserve_seed()`](https://withr.r-lib.org/reference/with_seed.html)
  on its default code path, which must not fail for installations
  without suggested packages
- Removed the committed `R/.tldr` tool cache from version control
  (already excluded from the built tarball via `.Rbuildignore`)
- Removed redundant `Author`/`Maintainer` fields from `DESCRIPTION`
  (regenerated automatically from `Authors@R`)

------------------------------------------------------------------------

## huerd 0.6.1 (2026-01-26)

### Bug Fixes

- [`plot_palette_analysis()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/plot_palette_analysis.md)
  now returns `invisible(NULL)` without warning when fewer than two
  colors are supplied.
- [`simulate_palette_cvd()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/simulate_palette_cvd.md)
  returns empty results silently when input contains no valid colors.
- `plot_cvd_comparison()` now preserves the fallback border color when
  color conversion fails.
- Optimizers validate inputs and return structured results instead of
  failing on invalid inputs (`optimize_colors_constrained()`,
  `optimize_colors_sann()`).

### Internal Changes

- Improved robustness in `calculate_perceptual_distances()` for
  NULL/empty/NA inputs.
- Strengthened initialization parameter validation for aesthetic
  initialization bounds and influence settings.
- Expanded `.Rbuildignore` to exclude additional development artifacts.

------------------------------------------------------------------------

## huerd 0.6.0 (2026-01-18)

### Bug Fixes

- Fixed hex color validation regex in
  [`evaluate_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/evaluate_palette.md)
  to properly reject literal “NA” strings while allowing actual NA
  values
- Fixed flaky test in `test-optimization-core.R` for nlopt_neldermead
  optimizer performance expectations
- Fixed
  [`reproduce_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/reproduce_palette.md)
  to properly restore RNG state using
  [`withr::with_preserve_seed()`](https://withr.r-lib.org/reference/with_seed.html)
  instead of direct `.Random.seed` manipulation
- Fixed `validate_oklab_matrix()` test errors by properly initializing
  validation_result structure

### Code Quality Improvements

- Added comprehensive documentation to internal helper functions with
  [@param](https://github.com/param) descriptions
- Improved error handling and validation in
  [`export_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/export_palette.md)
  for different export formats (JSON, CSV, CSS, Sass)
- Enhanced L-BFGS optimizer state consistency by adding `colors_oklab`
  field and defensive clamping
- Updated all tests to use testthat 3rd edition describe/it BDD pattern
  (1,006+ tests)
- Fixed CVD analysis tests to handle list structure return from
  [`simulate_palette_cvd()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/simulate_palette_cvd.md)

### New Features

- Added
  [`quick_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/quick_palette.md)
  for one-line palette generation
- Added
  [`brand_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/brand_palette.md)
  for brand color integration
- Added
  [`export_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/export_palette.md)
  with support for JSON, CSV, CSS, and Sass formats
- Added
  [`interpret_palette_quality()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/interpret_palette_quality.md)
  for palette quality assessment
- Added
  [`plot.huerd_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/plot.huerd_palette.md)
  method with “swatches” and “analysis” plot types
- Added ggplot2 integration:
  [`scale_color_huerd()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/scale_color_huerd.md),
  [`scale_fill_huerd()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/scale_color_huerd.md),
  [`scale_colour_huerd()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/scale_color_huerd.md)

### Infrastructure

- Added `withr` to Suggests for proper RNG state management
- Updated .gitignore with .venv/ and pkgdown/ patterns
- Updated .Rbuildignore with corrected .venv exclusion pattern
- Added comprehensive vignettes for data scientist, designer, and
  package developer workflows

------------------------------------------------------------------------

## huerd 0.5.3

### Breaking Changes

- [`simulate_palette_cvd()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/simulate_palette_cvd.md)
  with a single `cvd_type` now returns a named list instead of a
  character vector; access the simulated colors via `result$simulated`.
  The print method was updated accordingly

### Code Quality Improvements

This release focuses on code quality, maintainability, and consistency
improvements identified through comprehensive code review.

#### User Messaging

- Replaced [`cat()`](https://rdrr.io/r/base/cat.html) calls with
  semantic `cli` messaging functions (`cli_alert_info()`,
  `cli_alert_warning()`, `cli_alert_success()`, `cli_inform()`) for
  better user experience
- Added `cli` to package Imports

#### Type Safety & Performance

- Replaced [`sapply()`](https://rdrr.io/r/base/lapply.html) with
  [`vapply()`](https://rdrr.io/r/base/lapply.html) for type-stable
  return values
- Replaced [`is.na()`](https://rdrr.io/r/base/NA.html) with
  [`anyNA()`](https://rdrr.io/r/base/NA.html) for more efficient NA
  detection
- Fixed style issues: `=` → `<-` for assignment, `1:n` → `seq_len(n)`
  for safer indexing, `T`/`F` → `TRUE`/`FALSE`

#### Code Organization

- Added named constants for magic numbers: `.CANDIDATE_POOL_BASE`,
  `.MIN_DISTANCE_THRESHOLD`, `.OKLAB_TOLERANCE`
- Extracted repeated clamping code to `.clamp_to_bounds()` helper
  function
- Added `@param` documentation to internal helper functions
- Removed dead/commented-out code

#### API Consistency

- Added `...` parameter to exported functions for future extensibility
- [`simulate_palette_cvd()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/simulate_palette_cvd.md)
  return structure unified across single and multiple `cvd_type` values
  (see Breaking Changes above)

### Bug Fixes

- Corrected gradient formulas in smooth optimization objectives
  (`gradient_smooth_repulsion()` and `gradient_smooth_logsumexp()`)
- Added input validation to gradient functions
- Added numerical gradient verification tests to ensure correctness

------------------------------------------------------------------------

## huerd 0.5.2 (2025-07-21)

### Bug Fixes

- Corrected distance matrix calculation in smooth optimization
  objectives where diagonal masking affected only element \[1,1\] rather
  than the full diagonal
- Eliminated redundant distance calculations by using
  [`dist()`](https://rdrr.io/r/stats/dist.html) output directly instead
  of symmetric matrix conversion
- Applied log-sum-exp numerical stability technique for robust
  computation under extreme parameter values
- Added regression tests for distance calculation correctness

------------------------------------------------------------------------

## huerd 0.5.0 (2025-07-07)

### New Features

#### Smooth Optimization Support

- Added L-BFGS optimizer for gradient-based optimization
- Implemented two smooth objective functions: `smooth_repulsion` and
  `smooth_logsumexp`
- Added analytical gradient computation for faster convergence
- Speed improvements of 5-20x for larger palettes (8+ colors)

#### Multi-Objective Framework Updates

- Extended framework to support smooth and discrete objectives
- Support for weighted combinations of smooth objectives

### API Changes

- Added `optimizer = "nlopt_lbfgs"` option for L-BFGS optimization
- Added `weights = c(smooth_repulsion = 1)` and
  `weights = c(smooth_logsumexp = 1)` for smooth objectives

### Bug Fixes

- Fixed critical bug where L-BFGS optimizer always used
  `smooth_repulsion` objective regardless of `weights` parameter
- Fixed crash in `evaluate_palette_quality()` when called directly with
  hex colors (“argument is of length zero” error)
- Both smooth objectives (`smooth_repulsion` and `smooth_logsumexp`) now
  work correctly and produce different optimization results

### Internal Changes

- Added analytical gradient functions for smooth objectives
- Extended multi-objective framework to handle smooth and discrete
  objectives
- Improved input validation in `evaluate_palette_quality()` function

------------------------------------------------------------------------

## huerd 0.4.2 (2025-07-07)

### New Features

- Added L-BFGS optimizer (`optimizer = "nlopt_lbfgs"`) for
  gradient-based optimization
- Added support for smooth optimization using analytical gradients

### Internal Changes

- Added `optimize_colors_lbfgs()` function using `NLOPT_LD_LBFGS`
  algorithm

------------------------------------------------------------------------

## huerd 0.4.1 (2025-07-07)

### Internal Changes

- Added smooth differentiable objective functions for future
  gradient-based optimization

------------------------------------------------------------------------

## huerd 0.4.0 (2025-07-06)

### Major Changes

- Replaced base R graphics with grid graphics system.
  [`plot_palette_analysis()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/plot_palette_analysis.md)
  now uses
  [`gridExtra::arrangeGrob()`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html)
  for layout.

- Converted all plotting functions to return grob objects instead of
  drawing directly.

### New Features

- Added reproducibility system for palette generation with metadata
  storage.

- Added hue order reversal detection for CVD analysis.

### Breaking Changes

- Removed `new_device`, `device_width`, and `device_height` parameters
  from
  [`plot_palette_analysis()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/plot_palette_analysis.md).

- Moved `gridExtra` from Suggests to Imports, added `grid` to Imports.

- Removed helper functions: `.create_graphics_device`,
  `.is_ragg_available`, `.is_interactive_ide`, `calculate_safe_margins`.

## huerd 0.3.1 (2025-07-04)

### New Features

- Added automated font size scaling to
  [`plot_palette_analysis()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/plot_palette_analysis.md)
  that automatically adjusts text sizes based on device dimensions.

## huerd 0.3.0 (2025-07-04)

### Major Changes

- Multi-optimizer framework: The
  [`generate_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/generate_palette.md)
  function now supports four different optimization algorithms through
  the new `optimizer` parameter:
  - `"nloptr_cobyla"` (default): Deterministic optimization with
    constraint handling
  - `"sann"`: Stochastic simulated annealing
  - `"nlopt_direct"`: Deterministic global optimization using DIRECT
    algorithm
  - `"nlopt_neldermead"`: Derivative-free local optimization using
    Nelder-Mead simplex
- Added automatic font scaling to
  [`plot_palette_analysis()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/plot_palette_analysis.md)
  that prevents text overlap and out-of-frame issues.

### Minor Changes

- Added internal utility functions `.hex_to_oklab()` and
  `.oklab_to_hex()` to eliminate repetitive color conversion patterns.

- Fixed failing visualization tests and added test coverage for new
  utility functions.

- Updated test files to use utility functions.

## huerd 0.1.1 (2025-07-01)

### Minor Changes

- Removed unused cli and crayon dependencies.

## huerd 0.1.0 (2025-07-01)

This is a major refactoring and simplification of `huerd`, focusing the
package on a single, scientifically-grounded objective: pure minimax
color palette generation.

### Major Changes

- Pure minimax optimization: The core
  [`generate_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/generate_palette.md)
  function has been streamlined to use a pure minimax objective,
  maximizing the minimum perceptual distance between colors in the OKLAB
  space. All complex multi-objective parameters (`optimize_for`,
  `balance_weights`, `aesthetic_penalty_weights_LC`) have been removed,
  simplifying the API and aligning the package with a clear, defensible
  scientific goal.

- Comprehensive diagnostic dashboard: A new
  [`plot_palette_analysis()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/plot_palette_analysis.md)
  function has been added. It provides a comprehensive, six-panel
  diagnostic dashboard inspired by `scicomap` for in-depth palette
  analysis. This feature uses only base R graphics and introduces zero
  new dependencies.

- Automatic brightness sorting: All palettes generated by
  [`generate_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/generate_palette.md)
  are now automatically sorted by their OKLAB lightness value, ensuring
  intuitive and consistent ordering from darkest to lightest.

- Simplified evaluation: The
  [`evaluate_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/evaluate_palette.md)
  function is now a pure data provider, returning raw, objective metrics
  (distances, CVD safety, color distribution) without any subjective
  heuristic scoring.

### Minor Changes

- The
  [`is_cvd_safe()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/is_cvd_safe.md)
  function now uses the more robust
  [`evaluate_palette()`](https://sims1253.github.io/huerd/branch/refactor/remove-dead-code/reference/evaluate_palette.md)
  function for its calculations.
- The print methods for `huerd_palette` and `huerd_evaluation` have been
  updated to reflect the new, simplified data structures.

### Removals

The following experimental and non-essential features have been removed
to streamline the package:

- Force-field and repulsion-based optimization algorithms.
- Palette animation framework.
- Algorithm comparison tools.
- Post-hoc CVD refinement steps.
- 3D color space visualizations.
- All functionality related to the CAM16 and CIECAM16 color models.

------------------------------------------------------------------------

## huerd 0.0.3 (2025-06-22)

- Added a post-hoc `cvd_adjustment` step to improve palettes for color
  vision deficiency.
- Added a `README.md` with basic usage instructions.

## huerd 0.0.2 (2025-01-12)

- Switched the core optimization algorithm to an inverse-square law
  repulsion model.
- Removed all functionality related to the CIECAM16 color model.
- Added a `swatchplot` to the animation feature.

## huerd 0.0.1 (2025-01-04)

- Initial release.
- Core functionality for palette generation using the CAM16 and CIECAM16
  color models.
- Features for color space conversion and animation of the optimization
  process.

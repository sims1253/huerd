# huerd 0.3.0 (2025-07-04)

## Major Changes

*   **Multi-Optimizer Framework**: The `generate_palette()` function now supports four different optimization algorithms through the new `optimizer` parameter:
    - `"nloptr_cobyla"` (default): Deterministic optimization with constraint handling
    - `"sann"`: Stochastic simulated annealing for excellent quality
    - `"nlopt_direct"`: Deterministic global optimization using DIRECT algorithm
    - `"nlopt_neldermead"`: Derivative-free local optimization using Nelder-Mead simplex

*   **Multi-Objective Optimization Foundation**: Added infrastructure for multi-objective optimization with the new `weights` parameter. Currently supports distance-based optimization with framework for future expansion to other objectives.

# huerd 0.2.0 (2025-07-03)

## Major Changes

*   **Grid Graphics Visualization System**: Replaced base R graphics with grid graphics for robust, cross-platform visualization in `plot_palette_analysis()`.

*   **Automatic Font Scaling**: Added responsive font scaling to `plot_palette_analysis()` that automatically adjusts text sizes based on device dimensions, preventing text overlap and out-of-frame issues.

*   **Enhanced Dashboard Panels**: Improved all six diagnostic panels with better layout, styling, and readability.

## Minor Changes

*   **DRY Code Improvements**: Added internal utility functions `.hex_to_oklab()` and `.oklab_to_hex()` to eliminate repetitive color conversion patterns throughout the codebase.

*   **Test Suite Improvements**: Fixed all failing visualization tests and added comprehensive test coverage for new utility functions.

*   **Package Dependencies**: Moved `gridExtra` from Suggests to Imports for reliable grid graphics support.

# huerd 0.1.1 (2025-07-01)

## Minor Changes
*   Removed unused cli and crayon dependencies. 

# huerd 0.1.0 (2025-07-01)

This is a major refactoring and simplification of `huerd`, focusing the package on a single, scientifically-grounded objective: pure minimax color palette generation.

## Major Changes

*   **Pure Minimax Optimization**: The core `generate_palette()` function has been streamlined to use a pure minimax objective, maximizing the minimum perceptual distance between colors in the OKLAB space. All complex multi-objective parameters (`optimize_for`, `balance_weights`, `aesthetic_penalty_weights_LC`) have been removed, simplifying the API and aligning the package with a clear, defensible scientific goal.
*   **Comprehensive Diagnostic Dashboard**: A new `plot_palette_analysis()` function has been added. It provides a comprehensive, six-panel diagnostic dashboard inspired by `scicomap` for in-depth palette analysis. This feature uses only **base R graphics** and introduces zero new dependencies.
*   **Automatic Brightness Sorting**: All palettes generated by `generate_palette()` are now automatically sorted by their OKLAB lightness value, ensuring intuitive and consistent ordering from darkest to lightest.
*   **Simplified Evaluation**: The `evaluate_palette()` function is now a pure data provider, returning raw, objective metrics (distances, CVD safety, color distribution) without any subjective heuristic scoring.

## Minor Changes

*   The `is_cvd_safe()` function now uses the more robust `evaluate_palette()` function for its calculations.
*   The print methods for `huerd_palette` and `huerd_evaluation` have been updated to reflect the new, simplified data structures.

## Removals

The following experimental and non-essential features have been removed to streamline the package:

*   Force-field and repulsion-based optimization algorithms.
*   Palette animation framework.
*   Algorithm comparison tools.
*   Post-hoc CVD refinement steps.
*   3D color space visualizations.
*   All functionality related to the CAM16 and CIECAM16 color models.

---

# huerd 0.0.3 (2025-06-22)

*   Added a post-hoc `cvd_adjustment` step to improve palettes for color vision deficiency.
*   Added a `README.md` with basic usage instructions.

# huerd 0.0.2 (2025-01-12)

*   Switched the core optimization algorithm to an inverse-square law repulsion model.
*   Removed all functionality related to the CIECAM16 color model.
*   Added a `swatchplot` to the animation feature.

# huerd 0.0.1 (2025-01-04)

*   Initial release.
*   Core functionality for palette generation using the CAM16 and CIECAM16 color models.
*   Features for color space conversion and animation of the optimization process.

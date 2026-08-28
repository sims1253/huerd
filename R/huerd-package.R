#' huerd: Constrained Color Palette Generation with Aesthetic Guidance
#'
#' The huerd package provides tools for generating perceptually distinct color
#' palettes with constraints. Its main feature lets you specify fixed colors
#' that must appear in the final palette unchanged. The optimizer tunes the
#' remaining colors for distinctness and CVD safety; they can optionally
#' adhere to the aesthetic "vibe" (Lightness and Chroma profile) of the
#' fixed colors.
#'
#' @section Main Functions:
#' \describe{
#'   \item{[generate_palette()]}{Generate a color palette with constraints.}
#'   \item{[evaluate_palette()]}{Evaluate the quality of a color palette.}
#'   \item{[simulate_palette_cvd()]}{Simulate how a palette appears under CVD.}
#'   \item{[is_cvd_safe()]}{Quick check for CVD safety against a threshold.}
#' }
#'
#' @section Color Spaces:
#' The package uses the OKLAB color space by default for its perceptual
#' uniformity.
#' Euclidean distance in OKLAB corresponds well to perceived color differences.
#' A Just Noticeable Difference (JND) in OKLAB is approximately 0.02.
#'
#' @section Optimization Method:
#' `huerd` treats palette generation as a box-constrained optimization problem
#' and solves it with the `nloptr` package. The goal is to find an arrangement
#' of colors that maximizes a score based on perceptual distance, CVD safety,
#' and optional penalties for aesthetic or gamut deviations.
#'
#' @keywords internal
"_PACKAGE"

# The following block is used by Roxygen to automatically manage imports
# in the NAMESPACE file.
## usethis namespace: start
## usethis namespace: end
NULL

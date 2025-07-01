#' huerd: Constrained Color Palette Generation with Aesthetic Guidance
#'
#' The huerd package provides tools for generating perceptually distinct color
#' palettes with constraints. The main feature is the ability to specify fixed
#' colors that must be included in the final palette unchanged. Generated
#' colors are optimized for distinctness and CVD safety, and can optionally
#' adhere to the aesthetic "vibe" (Lightness and Chroma profile) of the fixed colors.
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
#' The package uses the OK LAB color space by default for its perceptual uniformity.
#' Euclidean distance in OK LAB corresponds well to perceived color differences.
#' A Just Noticeable Difference (JND) in OK LAB is approximately 0.02.
#'
#' @section Optimization Method:
#' `huerd` treats palette generation as a box-constrained optimization problem,
#' handled by the `nloptr` package. The goal is to find an arrangement of
#' colors that maximizes a score based on perceptual distance, CVD safety, and
#' optional penalties for aesthetic or gamut deviations.
#'
#' @keywords internal
"_PACKAGE"

# The following block is used by Roxygen to automatically manage imports
# in the NAMESPACE file.
## usethis namespace: start
#' @importFrom colorspace simulate_cvd
#' @importFrom farver encode_colour decode_colour convert_colour
#' @importFrom nloptr nloptr
#' @importFrom stats runif dist optim var sd quantile median setNames
#' @importFrom utils modifyList packageVersion head str
#' @importFrom grDevices rgb col2rgb
## usethis namespace: end
NULL

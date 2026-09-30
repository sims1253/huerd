# Analytic OKLAB <-> sRGB transforms
# ==============================================================================
#
# These implement Björn Ottosson's published Oklab reference transform for
# sRGB. They exist for the L-BFGS optimizer, which is parameterized directly
# in sRGB space so every candidate color is in the sRGB gamut by construction:
# the objective and its gradient must share one exactly consistent transform,
# which farver's compiled conversion cannot provide derivatives for.
#
# farver remains the authority at the optimizer boundaries (projecting the
# starting point, converting the final solution) and for all delivered colors.
# The analytic transform here matches farver to within ~1e-4 in OKLAB, an
# order of magnitude below the 8-bit quantization of a delivered hex palette,
# so the scored palette and the delivered palette agree to quantization
# precision.

# lms^(1/3) -> OKLAB (Ottosson M1)
# nolint start: object_name_linter (L/a/b are standard color science notation)
.OKLAB_M1 <- matrix(
  c(
    0.2104542553,
    0.7936177850,
    -0.0040720468,
    1.9779984951,
    -2.4285922050,
    0.4505937099,
    0.0259040371,
    0.7827717662,
    -0.8086757660
  ),
  nrow = 3,
  byrow = TRUE
)

# linear sRGB -> lms (Ottosson M2)
.OKLAB_M2 <- matrix(
  c(
    0.4122214708,
    0.5363325363,
    0.0514459929,
    0.2119034982,
    0.6806995451,
    0.1073969566,
    0.0883024619,
    0.2817188376,
    0.6299787005
  ),
  nrow = 3,
  byrow = TRUE
)

# OKLAB -> lms^(1/3) (rows of Ottosson's published inverse)
.OKLAB_M1_INV <- matrix(
  c(
    1.0,
    0.3963377774,
    0.2158037573,
    1.0,
    -0.1055613458,
    -0.0638541728,
    1.0,
    -0.0894841775,
    -1.2914855480
  ),
  nrow = 3,
  byrow = TRUE
)

# lms -> linear sRGB (columns of Ottosson's published inverse)
.OKLAB_M2_INV <- matrix(
  c(
    4.0767416621,
    -3.3077115913,
    0.2309699292,
    -1.2684380046,
    2.6097574011,
    -0.3413193965,
    -0.0041960863,
    -0.7034186147,
    1.7076147010
  ),
  nrow = 3,
  byrow = TRUE
)

# Smallest lms value with a usable cbrt derivative; below this the Jacobian
# entry is treated as 0 to keep gradients finite at the black corner
.OKLAB_JACOBIAN_LMS_EPS <- 1e-8

#' sRGB gamma decoding (sRGB in 0..1 -> linear sRGB)
#' @noRd
.srgb_decode_gamma <- function(srgb) {
  ifelse(srgb <= 0.04045, srgb / 12.92, ((srgb + 0.055) / 1.055)^2.4)
}

#' sRGB gamma encoding (linear sRGB in 0..1 -> sRGB)
#' @noRd
.srgb_encode_gamma <- function(linear) {
  ifelse(linear <= 0.0031308, 12.92 * linear, 1.055 * linear^(1 / 2.4) - 0.055)
}

#' Convert sRGB colors (matrix with channels in 0..1) to OKLAB
#'
#' Analytic implementation of the Oklab forward transform for sRGB inputs;
#' every input inside the unit cube maps to an in-gamut OKLAB color.
#' @noRd
.srgb_to_oklab <- function(srgb_colors) {
  linear <- .srgb_decode_gamma(srgb_colors)
  lms <- linear %*% t(.OKLAB_M2)
  lms^(1 / 3) %*% t(.OKLAB_M1)
}

#' Convert OKLAB colors to sRGB (matrix with channels in 0..1)
#'
#' Linear sRGB values outside 0..1 (out-of-gamut OKLAB inputs) are clamped
#' before gamma encoding, mirroring farver's channel clamping.
#' @noRd
.oklab_to_srgb <- function(oklab_colors) {
  lms_root <- oklab_colors %*% t(.OKLAB_M1_INV)
  linear <- (lms_root^3) %*% t(.OKLAB_M2_INV)
  # Clamp before reshaping: pmin/pmax drop matrix dims
  linear <- matrix(pmin(1, pmax(0, linear)), ncol = 3)
  .srgb_encode_gamma(linear)
}

#' Chain an OKLAB-space gradient through to sRGB parameter space
#'
#' For each color, applies the transpose Jacobian
#' `d(oklab)/d(srgb) = M1 D(lms) M2 D(gamma')` to the OKLAB-space gradient
#' `grad_oklab`, where `D(lms)` is diagonal with entries `1/(3 lms^(2/3))`
#' and `D(gamma')` is diagonal with the sRGB decode derivative.
#'
#' @param srgb_colors Matrix of sRGB colors the gradients are taken at.
#' @param grad_oklab Matrix of objective gradients w.r.t. OKLAB coordinates,
#'   rows aligned with `srgb_colors`.
#' @return Matrix of gradients w.r.t. sRGB channel values.
#' @noRd
.oklab_grad_wrt_srgb <- function(srgb_colors, grad_oklab) {
  linear <- .srgb_decode_gamma(srgb_colors)
  lms <- linear %*% t(.OKLAB_M2)

  # d(linear)/d(srgb): piecewise sRGB decode derivative
  d_gamma <- ifelse(
    srgb_colors <= 0.04045,
    1 / 12.92,
    (2.4 / 1.055) * ((srgb_colors + 0.055) / 1.055)^1.4
  )

  # d(lms_root)/d(lms) = 1/(3 lms^(2/3)); zeroed near lms = 0 where the
  # derivative diverges (black corner)
  d_lms <- ifelse(
    lms > .OKLAB_JACOBIAN_LMS_EPS,
    1 / (3 * lms^(2 / 3)),
    0
  )

  # J^T g = D(gamma') M2^T D(lms) M1^T g, applied row-wise
  u <- grad_oklab %*% .OKLAB_M1
  u <- d_lms * u
  u <- u %*% .OKLAB_M2
  d_gamma * u
}
# nolint end

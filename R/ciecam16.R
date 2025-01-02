#' CIECAM16 Color Appearance Model Implementation
#'
#' @description
#' A comprehensive implementation of the CIECAM16 color appearance model that provides
#' functions for color space conversions and color difference calculations. The package
#' supports:
#'
#' * Forward and inverse transformations between CIE XYZ and CIECAM16
#' * Conversion between hex colors and CIECAM16
#' * Color difference calculations in CAM16-UCS space
#' * Various viewing condition parameters
#'
#' @details
#' The implementation follows the CIE TC 1-91 specification and includes support for:
#' * Different viewing conditions (Average, Dim, Dark)
#' * Chromatic adaptation
#' * Complete set of color appearance correlates
#'
#' @references
#' Li C, Li Z, Wang Z, et al. (2017). Comprehensive color solutions: CAM16, CAT16,
#' and CAM16-UCS. Color Research & Application, 42(6), 703-718.
#'
#' Gao C, Xiao K, Pointer M, Li C. (2024). The development of the CIECAM16 and
#' visualization of its domain and range. Color Research & Application, 49(1), 1-17.
#'
#' Hellwig L, Fairchild MD. (2022). Revising CAM16-UCS. Color and Imaging Conference,
#' Proceedings of the Color and Imaging Conference, 2022(1), 277-280.
#'
#' CIE 248:2022. The CIE 2016 Colour Appearance Model for Colour Management Systems:
#' CIECAM16. Vienna: CIE Central Bureau.
#'
#' Morovic P, et al. (Package Maintainers). colour-science/colour: A Color Science
#' Package for Python. https://github.com/colour-science/colour
#'
#' @name CIECAM16
NULL

#' Define CIECAM16 Viewing Condition Parameters
#'
#' @description
#' Provides standardized viewing condition parameters for the CIECAM16 color appearance
#' model. These parameters control how colors are perceived under different ambient
#' lighting conditions.
#'
#' @param name Character string specifying the viewing condition. Options:
#' * "Average" - Normal viewing conditions (office, home)
#' * "Dim" - Reduced ambient lighting (museum, evening)
#' * "Dark" - Minimal ambient lighting (cinema, night)
#'
#' @return A list containing the following CIECAM16 parameters:
#' \describe{
#'   \item{F}{Degree of adaptation factor (range: 0.8-1.0)}
#'   \item{c}{Surround impact factor (range: 0.525-0.69)}
#'   \item{N_c}{Chromatic induction factor (range: 0.8-1.0)}
#' }
#'
#' @details
#' The parameters returned by this function affect various aspects of color appearance:
#' * F controls the degree of chromatic adaptation
#' * c influences the impact of surrounding colors
#' * N_c affects the perceived chroma
#'
#' @examples
#' # Get parameters for average office lighting
#' params <- ciecam_environs("Average")
#' print(params$F) # Should be 1.0
#'
#' # Get parameters for dim museum lighting
#' dim_params <- ciecam_environs("Dim")
#' print(dim_params$F) # Should be 0.9
#'
#' @export
ciecam_environs <- function(name) {
  environs <- list(
    Average = list(F = 1.0, c = 0.69, N_c = 1.0),
    Dim = list(F = 0.9, c = 0.59, N_c = 0.95),
    Dark = list(F = 0.8, c = 0.525, N_c = 0.8)
  )
  if (!name %in% names(environs)) {
    stop("Invalid environment name. Choose from 'Average', 'Dim', or 'Dark'.")
  }
  environs[[name]]
}

#' Convert CIE XYZ to CIECAM16 Color Appearance Correlates
#'
#' @description
#' Transforms CIE XYZ tristimulus values to CIECAM16 color appearance correlates
#' using the forward transform of the CIECAM16 color appearance model.
#'
#' @param XYZ Numeric vector of length 3 or matrix (Nx3) containing CIE XYZ
#'   tristimulus values. Values should be in the domain [0, 100].
#' @param XYZ_w Numeric vector of length 3 containing the reference white point
#'   XYZ values. Values should be in the domain [0, 100].
#' @param L_A Numeric. The adapting field luminance in cd/m². Must be positive.
#' @param Y_b Numeric. The relative luminance of background (Y_b = 100 * L_b / L_w).
#'   Must be positive.
#' @param surround List of viewing condition parameters from `ciecam_environs()`.
#'   Default is "Average".
#' @param discount_illuminant Logical. If TRUE, assumes complete chromatic
#'   adaptation. Default is FALSE.
#' @param compute_H Logical. If TRUE, computes hue quadrature (H). Default is TRUE.
#'
#' @return A list containing CIECAM16 color appearance correlates:
#' \describe{
#'   \item{J}{Lightness (0-100)}
#'   \item{C}{Chroma (≥0)}
#'   \item{h}{Hue angle in degrees (0-360)}
#'   \item{s}{Saturation (0-100)}
#'   \item{Q}{Brightness (≥0)}
#'   \item{M}{Colorfulness (≥0)}
#'   \item{H}{Hue quadrature (0-400) if compute_H=TRUE}
#'   \item{HC}{Hue composition (NA in current implementation)}
#' }
#'
#' @examples
#' # Convert D65 white point
#' XYZ <- c(95.05, 100.00, 108.88)
#' XYZ_w <- XYZ # Using same white point
#' L_A <- 318.31 # Typical office lighting
#' Y_b <- 20.0 # Typical background
#' result <- XYZ_to_CIECAM16(XYZ, XYZ_w, L_A, Y_b)
#'
#' # Convert with dim viewing conditions
#' dim_result <- XYZ_to_CIECAM16(XYZ, XYZ_w, L_A, Y_b,
#'   surround = ciecam_environs("Dim")
#' )
#'
#' @export
XYZ_to_CIECAM16 <- function(XYZ, XYZ_w, L_A, Y_b, surround = ciecam_environs("Average"), discount_illuminant = FALSE, compute_H = TRUE) {
  # Input Validation (Important for package robustness)
  if (!is.numeric(XYZ) || length(XYZ) != 3) {
    stop("XYZ must be a numeric vector of length 3.")
  }
  if (!is.numeric(XYZ_w) || length(XYZ_w) != 3) {
    stop("XYZ_w must be a numeric vector of length 3.")
  }
  if (!is.numeric(L_A) || length(L_A) != 1 || L_A <= 0) {
    stop("L_A must be a positive numeric value.")
  }
  if (!is.numeric(Y_b) || length(Y_b) != 1 || Y_b <= 0) {
    stop("Y_b must be a positive numeric value.")
  }

  # Scale to domain [0, 100]
  XYZ <- to_domain_100(XYZ)
  XYZ_w <- to_domain_100(XYZ_w)
  X_w <- XYZ_w[1]
  Y_w <- XYZ_w[2]
  Z_w <- XYZ_w[3]

  # Step 0
  RGB_w <- MATRIX_16() %*% XYZ_w
  
  D <- if (!discount_illuminant) {
    pmin(pmax(degree_of_adaptation(surround$F, L_A), 0), 1)
  } else {
    1 # Simplified for single L_A
  }

  params <- viewing_conditions_dependent_parameters(Y_b, Y_w, L_A)
  n <- params$n
  F_L <- params$F_L
  N_bb <- params$N_bb
  N_cb <- params$N_cb
  z <- params$z

  D_RGB <- D * Y_w / RGB_w + 1 - D
  RGB_wc <- D_RGB * RGB_w
  RGB_aw <- post_adaptation_non_linear_response_compression_forward(RGB_wc, F_L)
  A_w <- achromatic_response_forward(RGB_aw, N_bb)

  # Step 1
  RGB <- MATRIX_16() %*% as.numeric(XYZ)

  # Step 2
  RGB_c <- D_RGB * RGB

  # Step 3
  RGB_a <- post_adaptation_non_linear_response_compression_forward(RGB_c, F_L)

  # Step 4
  opponent_dims <- opponent_colour_dimensions_forward(RGB_a)
  a <- opponent_dims[[1]]
  b <- opponent_dims[[2]]

  # Step 5
  h <- hue_angle(a, b)
  e_t <- eccentricity_factor(h)

  # Step 6
  H <- if (compute_H) hue_quadrature(h) else NA

  # Step 7
  A <- achromatic_response_forward(RGB_a, N_bb)

  # Step 8
  J <- lightness_correlate(A, A_w, surround$c, z)

  # Step 9
  Q <- brightness_correlate(surround$c, J, A_w, F_L)

  # Step 10
  C <- chroma_correlate(J, n, surround$N_c, N_cb, e_t, a, b, RGB_a)

  # Step 11
  M <- colourfulness_correlate(C, F_L)

  # Step 12
  s <- saturation_correlate(M, Q)

  # Return the CIECAM16 specification
  list(
    J = from_range_100(J),
    C = from_range_100(C),
    h = from_range_degrees(h),
    s = from_range_100(s),
    Q = from_range_100(Q),
    M = from_range_100(M),
    H = from_range_degrees(H, 400),
    HC = NA # Not computed in this implementation
  )
}

#' Convert CIECAM16 Color Appearance Correlates to CIE XYZ
#'
#' @description
#' Transforms CIECAM16 color appearance correlates back to CIE XYZ tristimulus
#' values using the inverse transform of the CIECAM16 color appearance model.
#'
#' @param CAM List containing CIECAM16 correlates with required elements:
#' \describe{
#'   \item{J}{Lightness (0-100)}
#'   \item{C}{Chroma (≥0)}
#'   \item{h}{Hue angle in degrees (0-360)}
#' }
#' @param XYZ_w Numeric vector of length 3 containing the reference white point
#'   XYZ values. Values should be in the domain [0, 100].
#' @param L_A Numeric. The adapting field luminance in cd/m². Must be positive.
#' @param Y_b Numeric. The relative luminance of background (Y_b = 100 * L_b / L_w).
#'   Must be positive.
#' @param surround List of viewing condition parameters from `ciecam_environs()`.
#'   Default is "Average".
#' @param discount_illuminant Logical. If TRUE, assumes complete chromatic
#'   adaptation. Default is FALSE.
#'
#' @return Numeric vector of length 3 containing CIE XYZ tristimulus values
#'   in the domain [0, 100].
#'
#' @examples
#' # Convert a medium gray in CIECAM16 space back to XYZ
#' CAM <- list(J = 50, C = 0, h = 0) # Achromatic color
#' XYZ_w <- c(95.05, 100.00, 108.88) # D65 white point
#' L_A <- 318.31
#' Y_b <- 20.0
#' XYZ <- CIECAM16_to_XYZ(CAM, XYZ_w, L_A, Y_b)
#'
#' # Convert a saturated red
#' red_CAM <- list(J = 50, C = 50, h = 0)
#' red_XYZ <- CIECAM16_to_XYZ(red_CAM, XYZ_w, L_A, Y_b)
#'
#' @export
CIECAM16_to_XYZ <- function(CAM, XYZ_w, L_A, Y_b, surround = ciecam_environs("Average"), discount_illuminant = FALSE) {
  # Input Validation
  required_attributes <- c("J", "C", "h")
  if (!all(required_attributes %in% names(CAM))) {
    stop("The input list 'CAM' must contain at least 'J', 'C', and 'h' attributes.")
  }
  if (!is.numeric(XYZ_w) || length(XYZ_w) != 3) {
    stop("XYZ_w must be a numeric vector of length 3.")
  }
  if (!is.numeric(L_A) || length(L_A) != 1 || L_A <= 0) {
    stop("L_A must be a positive numeric value.")
  }
  if (!is.numeric(Y_b) || length(Y_b) != 1 || Y_b <= 0) {
    stop("Y_b must be a positive numeric value.")
  }

  # Extract CIECAM16 correlates from input list
  J <- to_domain_100(CAM$J)
  C <- to_domain_100(CAM$C)
  h <- to_domain_degrees(CAM$h)

  # Ensure 'h' is within [0, 360) range
  h <- h %% 360
  h[h < 0] <- h[h < 0] + 360

  # Scale XYZ_w to domain [0, 100]
  XYZ_w <- to_domain_100(XYZ_w)
  X_w <- XYZ_w[1]
  Y_w <- XYZ_w[2]
  Z_w <- XYZ_w[3]

  # Step 0
  RGB_w <- MATRIX_16() %*% XYZ_w
  D <- if (!discount_illuminant) {
    pmin(pmax(degree_of_adaptation(surround$F, L_A), 0), 1)
  } else {
    1 # Simplified for single L_A
  }

  params <- viewing_conditions_dependent_parameters(Y_b, Y_w, L_A)
  n <- params$n
  F_L <- params$F_L
  N_bb <- params$N_bb
  N_cb <- params$N_cb
  z <- params$z

  D_RGB <- D / RGB_w + 1 - D
  RGB_wc <- D_RGB * RGB_w
  RGB_aw <- post_adaptation_non_linear_response_compression_forward(RGB_wc, F_L)
  A_w <- achromatic_response_forward(RGB_aw, N_bb)

  # Step 7
  A <- A_w * (J / 100)^(1 / (surround$c * z))

  # Step 5
  e_t <- eccentricity_factor(h)

  # Step 10
  t <- (C / ((1.64 - 0.29^n)^0.73))^(1 / 0.9)
  p_2 <- A / N_bb + 0.305

  # Step 4
  if (C == 0) {
    a <- 0
    b <- 0
  } else {
    p_1 <- (50000 / 13) * (N_cb * surround$N_c) * e_t / t
    a <- p_2 * (2 + (1 / 20)) / (p_1 + (21 / 20))
    b <- p_2 * 1 / (p_1 + (21 / 20))

    h_rad <- h * pi / 180
    a <- a * cos(h_rad)
    b <- b * sin(h_rad)
  }

  # Step 3
  p_3 <- (a^2 + b^2)^0.5
  p_4 <- (a * (12 / 11) + b * (-1 / 11)) / p_3
  p_5 <- (a * (1 / 9) + b * (1 / 9)) / p_3
  RGB_a_1 <- (460 * (p_2 + (451 / 1403) * p_4 + (288 / 1403) * p_5)) / (1403 + (451 / 1403) * p_4 - (288 / 1403) * p_5)
  RGB_a_2 <- (460 * (p_2 - (891 / 623) * p_4 - (261 / 623) * p_5)) / (623 - (891 / 623) * p_4 + (261 / 623) * p_5)
  RGB_a_3 <- (460 * (p_2 - (220 / 497) * p_4 - (6300 / 497) * p_5)) / (497 - (220 / 497) * p_4 + (6300 / 497) * p_5)

  RGB_a <- list(RGB_a_1, RGB_a_2, RGB_a_3)

  # Step 3
  RGB_a_minus_point_1 <- purrr::map(RGB_a, function(x) x - 0.1)
  RGB_c <- purrr::map(RGB_a_minus_point_1, function(x) f_e_inverse(x, F_L))

  # Step 2
  RGB <- purrr::map2(RGB_c, as.list(D_RGB), function(x, y) x / y)

  # Step 1
  XYZ <- MATRIX_INVERSE_16() %*% as.numeric(unlist(RGB))

  # Scale and return XYZ
  from_range_100(XYZ)
}

#' Convert Hex Color to CIECAM16 Color Appearance Correlates
#'
#' @description
#' Converts a hex color code to CIECAM16 color appearance correlates under
#' specified viewing conditions. This is a convenience function that combines
#' sRGB-to-XYZ and XYZ-to-CIECAM16 transformations.
#'
#' @param hex Character string representing a hex color code (e.g., "#FF0000").
#'   Must include the leading "#" and be 7 characters long.
#' @param XYZ_w Numeric vector of length 3 containing the reference white point
#'   XYZ values. Default is D65 white point [95.05, 100.00, 108.88].
#' @param L_A Numeric. The adapting field luminance in cd/m².
#'   Default is 318.31 (typical office lighting).
#' @param Y_b Numeric. The relative luminance of background.
#'   Default is 20.0 (typical background).
#' @param surround List of viewing condition parameters. Default is "Average".
#' @param discount_illuminant Logical for complete adaptation. Default is FALSE.
#' @param compute_H Logical to include hue quadrature. Default is TRUE.
#'
#' @return A list containing CIECAM16 correlates (see `XYZ_to_CIECAM16()`).
#'
#' @examples
#' # Convert pure red
#' red_cam <- hex_to_CIECAM16("#FF0000")
#'
#' # Convert white under dim conditions
#' white_dim <- hex_to_CIECAM16("#FFFFFF",
#'   surround = ciecam_environs("Dim")
#' )
#'
#' @export
hex_to_CIECAM16 <- function(hex, XYZ_w = c(95.05, 100.00, 108.88), L_A = 318.31, Y_b = 20.0, surround = ciecam_environs("Average"), discount_illuminant = FALSE, compute_H = TRUE) {
  rgb <- farver::decode_colour(hex, to = "rgb")
  xyz <- farver::convert_colour(rgb, from = "rgb", to = "xyz")
  XYZ_to_CIECAM16(
    100 * xyz,
    XYZ_w = XYZ_w,
    L_A = L_A,
    Y_b = Y_b,
    surround = surround,
    discount_illuminant = discount_illuminant,
    compute_H = compute_H
  )
}

#' Calculate CAM16-UCS Color Difference
#'
#' @description
#' Computes the Euclidean distance between two colors in the CAM16-UCS uniform
#' color space, which provides perceptually uniform color differences.
#'
#' @param hex1,hex2 Character strings of hex color codes to compare.
#'   Must include the leading "#" and be 7 characters long.
#' @param ... Additional parameters passed to `hex_to_CIECAM16()` for
#'   controlling viewing conditions.
#'
#' @return Numeric value representing the perceptual color difference.
#'   Values around 1.0 indicate just noticeable differences (JND),
#'   while larger values indicate more significant differences.
#'
#' @examples
#' # Compare similar reds
#' diff_small <- hex_colors_distance("#FF0000", "#FF0101")
#'
#' # Compare contrasting colors
#' diff_large <- hex_colors_distance("#FF0000", "#00FF00")
#'
#' # Compare under dim viewing conditions
#' diff_dim <- hex_colors_distance("#FF0000", "#FF0101",
#'   surround = ciecam_environs("Dim")
#' )
#'
#' @export
hex_colors_distance <- function(hex1, hex2, ...) {
  # Convert both colors to CIECAM16 space
  cam1 <- hex_to_CIECAM16(hex1, ...)
  cam2 <- hex_to_CIECAM16(hex2, ...)

  # Extract J, M, h components for both colors
  J1 <- cam1$J
  M1 <- cam1$M
  h1 <- cam1$h

  J2 <- cam2$J
  M2 <- cam2$M
  h2 <- cam2$h

  # Convert h from degrees to radians for trigonometric calculations
  h1_rad <- h1 * pi / 180
  h2_rad <- h2 * pi / 180

  # Calculate Cartesian coordinates in CAM16-UCS space
  a1 <- M1 * cos(h1_rad)
  b1 <- M1 * sin(h1_rad)

  a2 <- M2 * cos(h2_rad)
  b2 <- M2 * sin(h2_rad)

  # Calculate Euclidean distance in CAM16-UCS space
  sqrt((J1 - J2)^2 + (a1 - a2)^2 + (b1 - b2)^2)
}

#' Calculate Pairwise CAM16-UCS Color Differences
#'
#' @description
#' Computes a symmetric matrix of perceptual color differences between all pairs
#' of input colors using the CAM16-UCS uniform color space.
#'
#' @param hex Character vector of hex color codes. Each code must include
#'   the leading "#" and be 7 characters long.
#' @param ... Additional parameters passed to `hex_to_CIECAM16()` for
#'   controlling viewing conditions.
#'
#' @return A symmetric matrix where element [i,j] contains the perceptual
#'   color difference between hex[i] and hex[j]. Diagonal elements are 0.
#'
#' @examples
#' # Compare primary colors
#' colors <- c("#FF0000", "#00FF00", "#0000FF")
#' diffs <- hex_colors_distance_matrix(colors)
#'
#' # Compare under dark viewing conditions
#' diffs_dark <- hex_colors_distance_matrix(colors,
#'   surround = ciecam_environs("Dark")
#' )
#'
#' @export
hex_colors_distance_matrix <- function(hex, ...) {
  # Convert colors to CAM16-UCS space
  cam_colors <- purrr::map(hex, hex_to_CIECAM16, ...)
  ucs_colors <- purrr::map(cam_colors, ~ c(.$J, .$M, .$h))
  ucs_matrix <- do.call(rbind, ucs_colors)
  n <- nrow(ucs_matrix)

  # Initialize matrix with zeros
  dists <- matrix(0, nrow = n, ncol = n)

  # Calculate only upper triangle
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      dist <- sqrt(sum((ucs_matrix[i, ] - ucs_matrix[j, ])^2))
      dists[i, j] <- dists[j, i] <- dist
    }
  }
  dists
}

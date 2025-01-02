#' Validate color input values
#'
#' Performs comprehensive validation of color values for different color spaces.
#' Checks input type, dimensions, value ranges, and space-specific constraints.
#'
#' @param points Matrix or data frame of color values with 3 columns
#' @param space Character string specifying color space: "lab", "rgb", or "xyz"
#' @param distance Logical indicating if points will be used for distance calculations
#' @return Numeric matrix of validated color values
#' @keywords internal
validate_color_input <- function(points, space = "lab", distance = FALSE) {
  # Check if input is matrix or data frame
  if (!is.matrix(points) && !is.data.frame(points)) {
    stop("Input must be a matrix or data frame")
  }

  # Convert data frame to matrix if needed
  if (is.data.frame(points)) {
    points <- as.matrix(points)
  }

  # Check dimensions
  if (ncol(points) != 3) {
    stop("Input must have exactly 3 columns")
  }

  if (distance && nrow(points) < 2) {
    stop("Input must have at least 2 rows for distance calculation")
  }

  # Check for missing values
  if (any(is.na(points))) {
    stop("Input contains missing values")
  }

  # Space-specific validation
  if (space == "lab") {
    # L should be between 0 and 100
    if (any(points[, 1] < 0) || any(points[, 1] > 100)) {
      stop("L values must be between 0 and 100")
    }
    # a and b can theoretically be any value but extreme values are suspicious
    if (any(abs(points[, 2:3]) > 100)) {
      warning("Extreme a* or b* values detected (>100 or <-100)")
    }
  } else if (space == "rgb") {
    if (any(points < 0) || any(points > 255)) {
      stop("RGB values must be between 0 and 255")
    }
  } else if (space == "xyz") {
    if (any(points < 0)) {
      stop("XYZ values cannot be negative")
    }
    # Check for extreme values that may cause numerical instability
    if (any(points > 150)) {
      warning("XYZ values > 150 detected. Results may be numerically unstable.")
    }
    if (any(points > 0 & points < 0.5)) {
      warning("Very small non-zero XYZ values detected. Results may be numerically unstable.")
    }
  }

  # Convert to numeric matrix if not already
  storage.mode(points) <- "numeric"

  points
}

#' Scale value to [0, 100] domain
#'
#' Linearly scales a numeric value from [0, 1] range to [0, 100] range.
#' Useful for normalizing color values to a standard range.
#'
#' @param x Numeric value in [0, 1] range
#' @return Numeric value in [0, 100] range
#' @keywords internal
#' @examples
#' to_domain_100(0.5) # Returns 50
to_domain_100 <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  x * 100
}

#' Scale value from [0, 100] to [0, 1] range
#'
#' Linearly transforms a value from [0, 100] range to [0, 1] range.
#' Useful for converting normalized color values back to unit range.
#'
#' @param x Numeric value in [0, 100] range
#' @return Numeric value in [0, 1] range
#' @keywords internal
#' @examples
#' from_range_100(50) # Returns 0.5
from_range_100 <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  if (x < 0 || x > 100) {
    stop("Input must be in range [0, 100]")
  }
  x / 100
}

#' Convert radians to degrees
#'
#' Converts an angle from radians to degrees using the formula:
#' degrees = radians * (180 / π)
#'
#' @param x Numeric angle in radians
#' @return Numeric angle in degrees [0, 360)
#' @keywords internal
#' @examples
#' to_domain_degrees(pi) # Returns 180
to_domain_degrees <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  x * 180 / pi
}

#' Convert degrees to radians
#'
#' Converts an angle from degrees to radians using the formula:
#' radians = degrees * (π / 180)
#'
#' @param x Numeric angle in degrees
#' @param scale Numeric full circle scale (default 360 for degrees)
#' @return Numeric angle in radians
#' @keywords internal
#' @examples
#' from_range_degrees(180) # Returns pi
from_range_degrees <- function(x, scale = 360) {
  if (!is.numeric(x) || !is.numeric(scale)) {
    stop("Inputs must be numeric")
  }
  if (scale <= 0) {
    stop("Scale must be positive")
  }
  x * scale / 360
}

#' Calculate degree of adaptation
#'
#' Computes the degree of adaptation for color appearance models
#' using the given adaptation factor and luminance.
#'
#' @param F Numeric adaptation factor in [0, 1] range
#' @param L_A Numeric luminance of adapting field in cd/m²
#' @return Numeric degree of adaptation in [0, 1] range
#' @keywords internal
#' @examples
#' degree_of_adaptation(1.0, 318.31)
degree_of_adaptation <- function(F, L_A) {
  if (!is.numeric(F) || !is.numeric(L_A)) {
    stop("Inputs must be numeric")
  }
  F * (0.2 * L_A^0.25 * (1.0 / (5 * L_A + 1))^0.25)
}

#' Calculate viewing condition dependent parameters
#'
#' Computes parameters needed for color appearance models based on
#' viewing conditions and luminance values.
#'
#' @param Y_b Numeric luminance factor of background (Y_b = 100 * L_b / L_w)
#' @param Y_w Numeric luminance factor of reference white (typically 100)
#' @param L_A Numeric luminance of adapting field in cd/m²
#' @return List containing:
#'   \item{n}{Numeric background luminance factor}
#'   \item{F_L}{Numeric luminance adaptation factor}
#'   \item{N_bb}{Numeric chromatic induction factor}
#'   \item{N_cb}{Numeric chromatic induction factor for background}
#'   \item{z}{Numeric base exponential nonlinearity}
#' @keywords internal
#' @examples
#' viewing_conditions_dependent_parameters(20, 100, 318.31)
viewing_conditions_dependent_parameters <- function(Y_b, Y_w, L_A) {
  if (!is.numeric(Y_b) || !is.numeric(Y_w) || !is.numeric(L_A)) {
    stop("Inputs must be numeric")
  }
  k <- 1 / (5 * L_A + 1)
  F_L <- 0.2 * k^4 * (5 * L_A) + 0.1 * (1 - k^4)^2 * (5 * L_A)^(1 / 3)
  n <- Y_b / Y_w
  N_bb <- 0.725 * (1 / n)^0.2
  N_cb <- N_bb
  z <- 1.48 + sqrt(n)
  list(n = n, F_L = F_L, N_bb = N_bb, N_cb = N_cb, z = z)
}

#' Apply post-adaptation non-linear response compression (forward)
#'
#' @param RGB Numeric vector of RGB values
#' @param F_L Numeric luminance adaptation factor
#' @return Numeric vector of compressed RGB values
#' @keywords internal
#' @examples
#' post_adaptation_non_linear_response_compression_forward(c(100, 150, 200), 1.0)
post_adaptation_non_linear_response_compression_forward <- function(RGB, F_L) {
  if (!is.numeric(RGB) || !is.numeric(F_L)) {
    stop("Inputs must be numeric")
  }
  RGB_scaled <- F_L * RGB / 100
  RGB_comp <- (400 * RGB_scaled^0.42) / (27.13 + RGB_scaled^0.42) + 0.1
  return(RGB_comp)
}

#' Calculate achromatic response (forward)
#'
#' Computes the achromatic response in the CIECAM16 color appearance model.
#' This represents the perceived brightness of a color.
#'
#' @param RGB_a Numeric vector of adapted RGB values (length 3)
#' @param N_bb Numeric chromatic induction factor (non-negative)
#' @return Numeric achromatic response value
#' @keywords internal
#' @examples
#' achromatic_response_forward(c(100, 150, 200), 1.0)
achromatic_response_forward <- function(RGB_a, N_bb) {
  (2 * RGB_a[1] + RGB_a[2] + (1 / 20) * RGB_a[3] - 0.305) * N_bb
}

#' Calculate opponent color dimensions (forward)
#'
#' Computes the opponent color dimensions (a and b) which represent
#' the red-green and yellow-blue color opponency channels.
#'
#' @param RGB_a Numeric vector of adapted RGB values (length 3)
#' @return List containing:
#'   - a: Numeric red-green opponent dimension
#'   - b: Numeric yellow-blue opponent dimension
#' @keywords internal
#' @examples
#' opponent_colour_dimensions_forward(c(100, 150, 200))
opponent_colour_dimensions_forward <- function(RGB_a) {
  a <- RGB_a[1] - 12 * RGB_a[2] / 11 + RGB_a[3] / 11
  b <- (RGB_a[1] + RGB_a[2] - 2 * RGB_a[3]) / 9
  list(a, b)
}

#' Calculate hue angle from opponent color dimensions
#'
#' Computes the hue angle in degrees from the opponent color dimensions.
#' The hue angle represents the color's position on the color wheel.
#'
#' @param a Numeric red-green opponent color dimension
#' @param b Numeric yellow-blue opponent color dimension
#' @return Numeric hue angle in degrees [0, 360)
#' @keywords internal
#' @examples
#' hue_angle(50, 100)
hue_angle <- function(a, b) {
  rad2deg <- function(rad) (rad * 180) / (pi)
  h <- rad2deg(atan2(b, a))
  ifelse(h < 0, h + 360, h)
}

#' Calculate eccentricity factor for given hue angle
#'
#' Computes the eccentricity factor which accounts for the non-uniformity
#' of color perception across different hue angles.
#'
#' @param h Numeric hue angle in degrees [0, 360)
#' @return Numeric eccentricity factor (typically between 1.0 and 1.2714)
#' @keywords internal
#' @examples
#' eccentricity_factor(45)
eccentricity_factor <- function(h) {
  h_rad <- h * pi / 180
  (1.2714 - 0.2714 * cos(h_rad - 2 * pi / 9))
}

#' Calculate hue quadrature
#'
#' Computes the hue quadrature value based on the given hue angle.
#'
#' @param h Numeric hue angle in degrees
#' @return Numeric hue quadrature value
#' @keywords internal
#' @examples
#' hue_quadrature(45)
hue_quadrature <- function(h) {
  # Calculate the hue quadrature
  H_data <- c(0, 100, 200, 300, 400)
  h_i_data <- c(20.14, 90.00, 164.25, 237.53, 380.14)

  h_degrees <- ifelse(h < 0, h + 360, ifelse(h > 360, h - 360, h))

  if (any(h_degrees < h_i_data[1] | h_degrees >= h_i_data[5])) {
    h_degrees[h_degrees < h_i_data[1]] <- h_degrees[h_degrees < h_i_data[1]] + 360
  }

  i <- 0
  for (x in seq_along(h_degrees)) {
    for (j in seq_along(h_i_data)) {
      if (h_degrees[x] >= h_i_data[j] && h_degrees[x] < h_i_data[j + 1]) {
        i[x] <- j
        break
      }
    }
  }

  e_i <- (100 * (i - 1) +
    (1 / eccentricity_factor(h_i_data))[i] * (H_data[i + 1] - H_data[i])) /
    ((1 / eccentricity_factor(h_i_data))[i] +
      (1 / eccentricity_factor(h_i_data))[i + 1] *
        ((H_data[i + 1] - H_data[i]) / (h_i_data[i + 1] - h_i_data[i])))

  H <- H_data[i] + (h_degrees - h_i_data[i]) * (e_i - H_data[i]) / (h_degrees - h_i_data[i])

  return(H)
}

#' Calculate lightness correlate
#'
#' Computes the lightness correlate (J) which represents the perceived
#' brightness of a color relative to white.
#'
#' @param A Achromatic response of the color
#' @param A_w Achromatic response for reference white
#' @param c Chromatic induction factor (non-negative)
#' @param z Scaling factor (non-negative)
#' @return Numeric lightness correlate value [0, 100]
#' @keywords internal
lightness_correlate <- function(A, A_w, c, z) {
  100 * (A / A_w)^(c * z)
}

#' Calculate brightness correlate
#'
#' Computes the brightness correlate (Q) which represents the absolute
#' perceived brightness of a color.
#'
#' @param c Chromatic induction factor (non-negative)
#' @param J Lightness correlate [0, 100]
#' @param A_w Achromatic response for reference white
#' @param F_L Luminance adaptation factor (non-negative)
#' @return Numeric brightness correlate value
#' @keywords internal
brightness_correlate <- function(c, J, A_w, F_L) {
  4 / c * sqrt(J / 100) * (A_w + 4) * F_L^0.25
}

#' Calculate chroma correlate
#'
#' Computes the chroma correlate (C) which represents the colorfulness
#' of a stimulus relative to the brightness of a similarly illuminated white.
#'
#' @param J Lightness correlate [0, 100]
#' @param n Background luminance factor (non-negative)
#' @param N_c Chromatic induction factor (non-negative)
#' @param N_cb Chromatic induction factor for background (non-negative)
#' @param e_t Eccentricity factor (typically between 1.0 and 1.2714)
#' @param a Red-green opponent color dimension
#' @param b Yellow-blue opponent color dimension
#' @param RGB_a Numeric vector of adapted RGB values (length 3)
#' @return Numeric chroma correlate value
#' @keywords internal
chroma_correlate <- function(J, n, N_c, N_cb, e_t, a, b, RGB_a) {
  t <- ((50000 / 13) * N_c * N_cb) * e_t * sqrt(a^2 + b^2) /
    (RGB_a[1] + RGB_a[2] + (21 / 20) * RGB_a[3])
  t^0.9 * sqrt(J / 100) * (1.64 - 0.29^n)^0.73
}

#' Calculate colourfulness correlate
#'
#' Computes the colourfulness correlate (M) which represents the absolute
#' colorfulness of a stimulus.
#'
#' @param C Chroma correlate (non-negative)
#' @param F_L Luminance adaptation factor (non-negative)
#' @return Numeric colourfulness correlate value
#' @keywords internal
colourfulness_correlate <- function(C, F_L) {
  C * F_L^0.25
}

#' Calculate saturation correlate
#'
#' Computes the saturation correlate (s) which represents the colorfulness
#' of a stimulus relative to its own brightness.
#'
#' @param M Colourfulness correlate (non-negative)
#' @param Q Brightness correlate (non-negative)
#' @return Numeric saturation correlate value
#' @keywords internal
saturation_correlate <- function(M, Q) {
  100 * sqrt(M / Q)
}

#' Apply forward non-linear response compression
#'
#' Applies the forward non-linear response compression function used in
#' the CIECAM16 color appearance model.
#'
#' @param x Numeric input value
#' @param F_L Luminance adaptation factor (non-negative)
#' @return Numeric compressed value
#' @keywords internal
f_e_forward <- function(x, F_L) {
  x_scaled <- F_L * x / 100
  x_comp <- (400 * x_scaled^0.42) / (27.13 + x_scaled^0.42) + 0.1
  return(x_comp)
}

#' Apply inverse non-linear response compression
#'
#' Applies the inverse non-linear response compression function used in
#' the CIECAM16 color appearance model.
#'
#' @param x Numeric compressed value
#' @param F_L Luminance adaptation factor (non-negative)
#' @return Numeric decompressed value
#' @keywords internal
f_e_inverse <- function(x, F_L) {
  x_comp <- x - 0.1
  x_scaled <- (27.13 * x_comp) / (400 - x_comp)
  x_rescaled <- (x_scaled)^(1 / 0.42)
  x_out <- 100 * x_rescaled / F_L
  return(x_out)
}

#' Get CIECAM16 transformation matrix
#'
#' Returns the 3x3 matrix used to transform from XYZ to RGB color space
#' in the CIECAM16 color appearance model.
#'
#' @return Numeric matrix (3x3) for XYZ to RGB transformation
#' @keywords internal
#' @examples
#' MATRIX_16()
MATRIX_16 <- function() {
  matrix(c(
    0.401288, 0.650173, -0.051461,
    -0.250268, 1.204414, 0.045854,
    -0.002079, 0.048952, 0.953127
  ), nrow = 3, byrow = TRUE)
}

#' Get inverse CIECAM16 transformation matrix
#'
#' Returns the inverse of the 3x3 matrix used to transform from RGB to XYZ color space
#' in the CIECAM16 color appearance model.
#'
#' @return Numeric matrix (3x3) for RGB to XYZ transformation
#' @keywords internal
#' @examples
#' MATRIX_INVERSE_16()
MATRIX_INVERSE_16 <- function() {
  solve(matrix(c(
    0.401288, 0.650173, -0.051461,
    -0.250268, 1.204414, 0.045854,
    -0.002079, 0.048952, 0.953127
  ), nrow = 3, byrow = TRUE))
}

#' CAM16 Color Space Conversion Utilities
#'
#' A collection of functions for converting between hex colors and CAM16-UCS space.
#' Core calculations are shared between forward and inverse transforms.
#'
#' @name cam16
NULL

#' Initialize CAM16 viewing condition parameters
#'
#' @param L_A Adapting luminance in cd/m² (default 100)
#' @param Y_b Background relative luminance (default 20)
#' @param surround Surround conditions ("average", "dim", or "dark")
#' @return List of calculated parameters used in CAM16 conversions
#' @keywords internal
initialize_cam16_parameters <- function(L_A = 100, Y_b = 20, surround = "average") {
  # Input validation
  if (L_A <= 0) stop("L_A must be positive")
  if (Y_b < 0 || Y_b > 100) stop("Y_b must be between 0 and 100")
  if (!surround %in% c("average", "dim", "dark")) {
    stop("surround must be one of: average, dim, dark")
  }

  # M16 CAT matrix from the CAM16 paper
  M16 <- matrix(c(
    0.401288, 0.650173, -0.051461,
    -0.250268, 1.204414, 0.045854,
    -0.002079, 0.048952, 0.953127
  ), nrow = 3, byrow = TRUE)

  # Surround parameters from Table A1
  surround_params <- list(
    average = c(1.0, 0.69, 1.0),
    dim = c(0.9, 0.59, 0.9),
    dark = c(0.8, 0.525, 0.8)
  )

  F <- surround_params[[surround]][1]
  c <- surround_params[[surround]][2]
  Nc <- surround_params[[surround]][3]

  # D65 white point
  xyz_w <- c(95.047, 100, 108.883)
  RGB_w <- M16 %*% xyz_w

  # Calculate adaptation parameters
  k <- 1 / (5 * L_A + 1)
  D <- F * (1 - (1 / 3.6) * exp((-L_A - 42) / 92))
  D <- pmin(pmax(D, 0), 1)

  F_L <- k^4 * L_A + 0.1 * (1 - k^4)^2 * (L_A^(1 / 3))

  n <- Y_b / xyz_w[2]
  z <- 1.48 + sqrt(n)
  Nbb <- 0.725 * (1 / n)^0.2
  Ncb <- Nbb

  # Calculate adaptation matrices
  D_RGB <- D * xyz_w[2] / RGB_w + (1 - D)
  RGB_wc <- D_RGB * RGB_w

  # Calculate adapted white point responses
  R_aw <- calculate_response(RGB_wc[1], F_L)
  G_aw <- calculate_response(RGB_wc[2], F_L)
  B_aw <- calculate_response(RGB_wc[3], F_L)
  A_w <- (2 * R_aw + G_aw + (1 / 20) * B_aw - 0.305) * Nbb

  list(
    M16 = M16,
    M16_inv = solve(M16),
    F = F,
    c = c,
    Nc = Nc,
    F_L = F_L,
    n = n,
    z = z,
    Nbb = Nbb,
    Ncb = Ncb,
    D_RGB = D_RGB,
    D_RGB_inv = 1 / D_RGB,
    A_w = A_w,
    xyz_w = xyz_w
  )
}

#' Calculate post-adaptation response
#'
#' @param x Input value
#' @param FL Luminance adaptation factor
#' @return Adapted response value
#' @keywords internal
calculate_response <- function(x, FL) {
  y <- (FL * abs(x)) / 100
  sign(x) * 400 * (y^0.42) / (27.13 + y^0.42) + 0.1
}

#' Invert post-adaptation response
#'
#' @param x Adapted value
#' @param FL Luminance adaptation factor
#' @return Original value
#' @keywords internal
invert_response <- function(x, FL) {
  x <- x - 0.1
  y <- sign(x) * (27.13 * abs(x)) / (400 - abs(x))
  sign(x) * 100 * (y^(1 / 0.42)) / FL
}

#' Convert hex colors to CAM16-UCS
#'
#' @param hex Character vector of hex color codes
#' @param L_A Adapting luminance in cd/m²
#' @param Y_b Background relative luminance
#' @param surround Surround conditions
#' @return Matrix of CAM16-UCS coordinates (J', a_M', b_M')
#' @export
hex_to_cam16ucs <- function(hex, L_A = 100, Y_b = 20, surround = "average") {
  # Validate hex colors
  if (!all(grepl("^#[0-9A-Fa-f]{6}$", hex))) {
    stop("Invalid hex color format. Must be in format '#RRGGBB'")
  }

  # Initialize parameters
  params <- initialize_cam16_parameters(L_A, Y_b, surround)

  # Convert hex to RGB matrix
  rgb <- t(sapply(hex, function(h) {
    c(
      r = strtoi(substr(h, 2, 3), 16),
      g = strtoi(substr(h, 4, 5), 16),
      b = strtoi(substr(h, 6, 7), 16)
    )
  })) / 255

  # Handle special cases first
  result <- matrix(nrow = length(hex), ncol = 3)
  colnames(result) <- c("J_p", "a_M", "b_M")

  # Handle white
  white_idx <- which(abs(rgb[, 1] - 1) < 1e-6 &
    abs(rgb[, 2] - 1) < 1e-6 &
    abs(rgb[, 3] - 1) < 1e-6)
  if (length(white_idx) > 0) {
    result[white_idx, ] <- c(100, 0, 0)
  }

  # Handle black
  black_idx <- which(abs(rgb[, 1]) < 1e-6 &
    abs(rgb[, 2]) < 1e-6 &
    abs(rgb[, 3]) < 1e-6)
  if (length(black_idx) > 0) {
    result[black_idx, ] <- c(0, 0, 0)
  }

  # Process remaining colors
  remaining_idx <- setdiff(seq_len(nrow(rgb)), c(white_idx, black_idx))

  if (length(remaining_idx) > 0) {
    # Convert RGB to XYZ using sRGB transform
    rgb_linear <- ifelse(rgb[remaining_idx, ] <= 0.04045,
      rgb[remaining_idx, ] / 12.92,
      ((rgb[remaining_idx, ] + 0.055) / 1.055)^2.4
    )

    srgb_to_xyz <- matrix(c(
      0.4124564, 0.3575761, 0.1804375,
      0.2126729, 0.7151522, 0.0721750,
      0.0193339, 0.1191920, 0.9503041
    ), nrow = 3, byrow = TRUE)

    xyz <- t(apply(rgb_linear, 1, function(rgb) srgb_to_xyz %*% rgb)) * 100

    # Convert XYZ to CAM16 RGB
    RGB <- t(apply(xyz, 1, function(xyz) params$M16 %*% xyz))

    # Apply chromatic adaptation
    RGB_c <- t(apply(RGB, 1, function(rgb) params$D_RGB * rgb))

    # Calculate post-adaptation responses
    RGB_a <- t(apply(RGB_c, 1, function(rgb) {
      c(
        calculate_response(rgb[1], params$F_L),
        calculate_response(rgb[2], params$F_L),
        calculate_response(rgb[3], params$F_L)
      )
    }))

    # Calculate opponent color coordinates
    a <- RGB_a[, 1] - 12 * RGB_a[, 2] / 11 + RGB_a[, 3] / 11
    b <- (RGB_a[, 1] + RGB_a[, 2] - 2 * RGB_a[, 3]) / 9

    # Calculate hue angle with improved stability
    h <- (180 / pi) * atan2(b, a)
    h <- ifelse(h < 0, h + 360, h)

    # Handle cases where both a and b are near zero
    h <- ifelse(abs(a) < 1e-6 & abs(b) < 1e-6, 0, h)

    # Calculate achromatic response
    A <- (2 * RGB_a[, 1] + RGB_a[, 2] + (1 / 20) * RGB_a[, 3] - 0.305) * params$Nbb

    # Calculate perceptual attributes
    J <- 100 * (A / params$A_w)^(params$c * params$z)

    # Handle near-zero values for stability
    et <- 1 / 4 * (cos(h * pi / 180 + 2) + 3.8)
    ab_magnitude <- sqrt(a^2 + b^2)

    # Avoid division by zero in t calculation
    RGB_sum <- pmax(rowSums(RGB_a * c(1, 1, 21 / 20)), 1e-6)
    t <- ifelse(ab_magnitude < 1e-6, 0,
      (50000 / 13) * params$Nc * params$Ncb * et * ab_magnitude / RGB_sum
    )

    C <- t^0.9 * sqrt(J / 100) * (1.64 - 0.29^params$n)^0.73

    # Convert to CAM16-UCS
    J_p <- (1.7 * J) / (1 + 0.007 * J)
    M_p <- log(1 + 0.0228 * C) / 0.0228

    # Calculate final coordinates with improved handling of achromatic colors
    is_achromatic <- ab_magnitude < 1e-6
    result[remaining_idx, ] <- cbind(
      J_p,
      ifelse(is_achromatic, 0, M_p * cos(h * pi / 180)),
      ifelse(is_achromatic, 0, M_p * sin(h * pi / 180))
    )
  }

  result
}

#' Convert CAM16-UCS coordinates to hex colors
#'
#' @param J_p Lightness coordinate in CAM16-UCS
#' @param a_M Red-green coordinate in CAM16-UCS
#' @param b_M Blue-yellow coordinate in CAM16-UCS
#' @param L_A Adapting luminance in cd/m²
#' @param Y_b Background relative luminance
#' @param surround Surround conditions
#' @return Character vector of hex color codes
#' @export
cam16ucs_to_hex <- function(J_p, a_M, b_M, L_A = 100, Y_b = 20, surround = "average") {
  # Input validation
  if (length(J_p) != length(a_M) || length(J_p) != length(b_M)) {
    stop("J_p, a_M, and b_M must have the same length")
  }

  # Initialize parameters
  params <- initialize_cam16_parameters(L_A, Y_b, surround)

  # Process each set of coordinates
  hex <- mapply(function(J_p, a_M, b_M) {
    # Handle NaN or Inf values
    if (any(is.na(c(J_p, a_M, b_M))) || any(is.infinite(c(J_p, a_M, b_M)))) {
      return("#000000") # Return black for invalid values
    }

    # Convert from CAM16-UCS to CAM16
    J <- (J_p / 1.7) * (1 + 0.007 * J_p)
    M_p <- sqrt(a_M^2 + b_M^2)

    # Handle achromatic colors with improved precision
    if (M_p < 1e-6) {
      h <- 0
      C <- 0
    } else {
      h <- atan2(b_M, a_M) * 180 / pi
      h <- ifelse(h < 0, h + 360, h)
      C <- 0.0228 * (exp(0.0228 * M_p) - 1)
    }

    # Calculate t from C
    if (C < 1e-6) {
      t <- 0
    } else {
      t <- (C / ((sqrt(J / 100) * (1.64 - 0.29^params$n)^0.73))^(1 / 0.9))
    }

    # Calculate eccentricity factor
    et <- 1 / 4 * (cos(h * pi / 180 + 2) + 3.8)

    # Calculate achromatic response
    A <- params$A_w * (J / 100)^(1 / (params$c * params$z))

    # Calculate opponent coordinates
    p2 <- (A / params$Nbb + 0.305)
    h_rad <- h * pi / 180

    # Handle special cases with improved precision
    if (t < 1e-6) {
      a <- 0
      b <- 0
    } else if (abs(cos(h_rad)) < 1e-6) { # Near vertical
      P <- p2 * (2 + 21 / 20)
      b <- sign(sin(h_rad)) * sqrt((t * P) / (50000 * params$Nc * params$Ncb * et / 13)) * P
      a <- 0
    } else if (abs(sin(h_rad)) < 1e-6) { # Near horizontal
      P <- p2 * (2 + 21 / 20)
      a <- sign(cos(h_rad)) * sqrt((t * P) / (50000 * params$Nc * params$Ncb * et / 13)) * P
      b <- 0
    } else if (abs(sin(h_rad)) >= abs(cos(h_rad))) {
      P <- p2 * (2 + 21 / 20)
      b <- sqrt((t * P) / (50000 * params$Nc * params$Ncb * et / 13)) * P
      a <- b * cos(h_rad) / sin(h_rad)
    } else {
      P <- p2 * (2 + 21 / 20)
      a <- sqrt((t * P) / (50000 * params$Nc * params$Ncb * et / 13)) * P
      b <- a * sin(h_rad) / cos(h_rad)
    }

    # Calculate RGB_a values
    R_a <- (460 * p2 + 451 * a + 288 * b) / 1403
    G_a <- (460 * p2 - 891 * a - 261 * b) / 1403
    B_a <- (460 * p2 - 220 * a - 6300 * b) / 1403

    # Invert post-adaptation response
    RGB_c <- c(
      invert_response(R_a, params$F_L),
      invert_response(G_a, params$F_L),
      invert_response(B_a, params$F_L)
    )

    # Handle numerical instability
    if (any(is.na(RGB_c)) || any(is.infinite(RGB_c))) {
      return("#000000")
    }

    # Invert chromatic adaptation
    RGB <- RGB_c * params$D_RGB_inv

    # Convert to XYZ
    xyz <- params$M16_inv %*% RGB

    # Convert XYZ to sRGB
    xyz_to_srgb <- matrix(c(
      3.2404542, -1.5371385, -0.4985314,
      -0.9692660, 1.8760108, 0.0415560,
      0.0556434, -0.2040259, 1.0572252
    ), nrow = 3, byrow = TRUE)

    rgb_linear <- xyz_to_srgb %*% (xyz / 100)

    # Apply inverse gamma correction
    rgb <- ifelse(rgb_linear <= 0.0031308,
      12.92 * rgb_linear,
      1.055 * rgb_linear^(1 / 2.4) - 0.055
    )

    # Clamp to [0,1] range and convert to hex
    rgb_255 <- round(pmax(0, pmin(1, rgb)) * 255)
    sprintf("#%02X%02X%02X", rgb_255[1], rgb_255[2], rgb_255[3])
  }, J_p, a_M, b_M)

  unname(hex)
}

#' Convert matrix of CAM16-UCS coordinates to hex colors
#'
#' @param ucs Matrix of CAM16-UCS coordinates (columns: J_p, a_M, b_M)
#' @param ... Additional parameters passed to cam16ucs_to_hex()
#' @return Character vector of hex color codes
#' @export
cam16ucs_matrix_to_hex <- function(ucs, ...) {
  if (!is.matrix(ucs) || ncol(ucs) != 3) {
    stop("ucs must be a matrix with 3 columns (J_p, a_M, b_M)")
  }
  cam16ucs_to_hex(ucs[, 1], ucs[, 2], ucs[, 3], ...)
}

#' Convert hex colors to CAM16-UCS matrix
#'
#' @param hex Character vector of hex color codes
#' @param ... Additional parameters passed to hex_to_cam16ucs()
#' @return Matrix of CAM16-UCS coordinates
#' @export
hex_to_cam16ucs_matrix <- function(hex, ...) {
  result <- hex_to_cam16ucs(hex, ...)
  colnames(result) <- c("J_p", "a_M", "b_M")
  result
}

#' Check color conversion roundtrip accuracy
#'
#' @param hex Character vector of hex color codes
#' @param ... Additional parameters passed to conversion functions
#' @return Data frame with original hex, roundtrip hex, and boolean match column
#' @export
check_cam16_roundtrip <- function(hex, ...) {
  ucs <- hex_to_cam16ucs(hex, ...)
  roundtrip <- cam16ucs_matrix_to_hex(ucs, ...)
  data.frame(
    original = hex,
    roundtrip = roundtrip,
    matches = tolower(hex) == tolower(roundtrip)
  )
}

#' Calculate CAM16-UCS color difference between hex colors
#'
#' @param hex1,hex2 Hex color codes
#' @param ... Additional parameters passed to hex_to_cam16ucs()
#' @return Numeric color difference value
#' @export
hex_cam16ucs_distance <- function(hex1, hex2, ...) {
  ucs1 <- hex_to_cam16ucs(hex1, ...)
  ucs2 <- hex_to_cam16ucs(hex2, ...)
  sqrt(sum((ucs1 - ucs2)^2))
}

#' Calculate pairwise CAM16-UCS color differences for hex colors
#'
#' @param colors Vector of hex color codes
#' @param ... Additional parameters passed to hex_to_cam16ucs()
#' @return Matrix of pairwise color differences
#' @export
hex_cam16ucs_distance_matrix <- function(colors, ...) {
  # Convert colors to CAM16-UCS space
  cam16ucs <- hex_to_cam16ucs(colors, ...)
  n <- nrow(cam16ucs)

  # Initialize matrix with zeros
  dists <- matrix(0, nrow = n, ncol = n)

  # Calculate only upper triangle
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      dist <- sqrt(sum((cam16ucs[i, ] - cam16ucs[j, ])^2))
      dists[i, j] <- dist
    }
  }

  # Mirror upper triangle to lower triangle
  dists[lower.tri(dists)] <- t(dists)[lower.tri(dists)]

  dists
}

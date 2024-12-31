#' Convert XYZ to CAM16-UCS
#'
#' Implements the complete CAM16 and CAM16-UCS conversion
#' based on Li et al. 2017
#' @param xyz Matrix of XYZ values (n × 3)
#' @param xyz_w White point XYZ values (default D65)
#' @param L_A Adapting luminance in cd/m² (default 100)
#' @param Y_b Background relative luminance (default 20)
#' @param surround Surround conditions ("average", "dim", or "dark")
#' @param validate Logical, indicating whether to validate input
#' @return Matrix of CAM16-UCS coordinates (J', a_M', b_M')
#' @references Li et al. (2017) - Comprehensive color solutions: CAM16, CAT16,
#'             and CAM16-UCS
#' @export
xyz_to_cam16ucs <- function(
    xyz,
    xyz_w = NULL,
    L_A = 100,
    Y_b = 20,
    surround = "average",
    validate = TRUE) {
  # Validate input
  if (validate) {
    xyz <- validate_color_input(xyz, "xyz")
  }

  if (L_A <= 0) {
    stop("L_A must be positive")
  }
  if (Y_b < 0 || Y_b > 100) {
    stop("Y_b must be between 0 and 100")
  }
  if (!surround %in% c("average", "dim", "dark")) {
    stop("surround must be one of: average, dim, dark")
  }

  # Default D65 white point if none provided
  if (is.null(xyz_w)) {
    xyz_w <- c(95.047, 100, 108.883)
  }

  # M16 CAT matrix from the paper
  M16 <- matrix(c(
    0.401288, 0.650173, -0.051461,
    -0.250268, 1.204414, 0.045854,
    -0.002079, 0.048952, 0.953127
  ), nrow = 3, byrow = TRUE)

  # Surround parameters (F, c, Nc)
  surround_params <- list(
    average = c(1.0, 0.69, 1.0),
    dim = c(0.9, 0.59, 0.9),
    dark = c(0.8, 0.525, 0.8)
  )
  F <- surround_params[[surround]][1]
  c <- surround_params[[surround]][2]
  Nc <- surround_params[[surround]][3]

  # Step 0: Calculate all values independent of input sample
  # Convert white point to RGB using M16
  RGB_w <- M16 %*% xyz_w

  # Calculate D for degree of adaptation
  k <- 1 / (5 * L_A + 1)
  D <- F * (1 - (1 / 3.6) * exp((-L_A - 42) / 92))
  D <- pmin(pmax(D, 0), 1) # Clamp D between 0 and 1

  # Calculate FL using k and LA
  F_L <- k^4 * L_A + 0.1 * (1 - k^4)^2 * (L_A^(1 / 3))

  # Background factors
  n <- Y_b / xyz_w[2]
  z <- 1.48 + sqrt(n)
  Nbb <- 0.725 * (1 / n)^0.2
  Ncb <- Nbb

  # RGB for white point
  D_RGB <- D * xyz_w[2] / RGB_w
  RGB_wc <- D_RGB * RGB_w

  # Function to calculate post-adaptation response
  calculate_response <- function(x, FL) {
    sign(x) * 400 *
      (abs(FL * x / 100)^0.42) / (27.13 + abs(FL * x / 100)^0.42) + 0.1
  }

  # Calculate adapted response for white
  R_aw <- calculate_response(RGB_wc[1], F_L)
  G_aw <- calculate_response(RGB_wc[2], F_L)
  B_aw <- calculate_response(RGB_wc[3], F_L)

  A_w <- (2 * R_aw + G_aw + 0.05 * B_aw - 0.305) * Nbb

  # Process each input XYZ value
  process_xyz <- function(xyz_row) {
    # Convert to RGB using M16
    RGB <- M16 %*% xyz_row

    # Apply chromatic adaptation
    RGB_c <- D_RGB * RGB

    # Calculate post-adaptation responses
    R_a <- calculate_response(RGB_c[1], F_L)
    G_a <- calculate_response(RGB_c[2], F_L)
    B_a <- calculate_response(RGB_c[3], F_L)

    # Calculate a and b
    a <- R_a - 12 * G_a / 11 + B_a / 11
    b <- (R_a + G_a - 2 * B_a) / 9

    # Calculate h (hue angle)
    h <- (180 / pi) * atan2(b, a)
    h <- ifelse(h < 0, h + 360, h)

    # Calculate eccentricity (et)
    et <- 1 / 4 * (cos(h * pi / 180 + 2) + 3.8)

    # Calculate A for achromatic response
    A <- (2 * R_a + G_a + 0.05 * B_a - 0.305) * Nbb

    # Calculate J (lightness)
    J <- 100 * (A / A_w)^(c * z)

    # Calculate t for chroma
    t <- (50000 / 13) * Nc * Ncb * et * sqrt(a^2 + b^2) /
      (R_a + G_a + 1.05 * B_a + 0.305)

    # Calculate C (chroma)
    C <- t^0.9 * sqrt(J / 100) * (1.64 - 0.29^n)^0.73

    # Convert to CAM16-UCS coordinates
    J_p <- (1.7 * J) / (1 + 0.007 * J)
    M_p <- log(1 + 0.0228 * C) / 0.0228
    a_M <- M_p * cos(h * pi / 180)
    b_M <- M_p * sin(h * pi / 180)

    c(J_p, a_M, b_M)
  }

  # Apply to all input XYZ values
  if (is.matrix(xyz)) {
    result <- t(apply(xyz, 1, process_xyz))
  } else {
    result <- matrix(process_xyz(xyz), nrow = 1)
  }

  colnames(result) <- c("J_p", "a_M", "b_M")
  result
}

#' Calculate CAM16-UCS color difference
#'
#' Calculates the Euclidean distance in CAM16-UCS space between two colors
#' @param ucs1,ucs2 CAM16-UCS coordinates
#' @return Numeric color difference value
#' @export
cam16ucs_distance <- function(ucs1, ucs2) {
  sqrt(sum((ucs1 - ucs2)^2))
}

#' Calculate all CAM16-UCS color differences for a set of colors
#'
#' @param cam16ucs Matrix of CAM16-UCS coordinates (n × 3)
#' @return Matrix of pairwise color differences
#' @export
cam16ucs_distance_matrix <- function(cam16ucs) {
  n <- nrow(cam16ucs)
  dists <- matrix(0, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in i:n) {
      if (i != j) {
        dist <- cam16ucs_distance(cam16ucs[i, ], cam16ucs[j, ])
        dists[i, j] <- dist
        dists[j, i] <- dist
      }
    }
  }

  dists
}

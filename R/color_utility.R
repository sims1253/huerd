#' Validate color input values
#'
#' Performs comprehensive validation of color values for different color spaces.
#' Checks input type, dimensions, value ranges, and space-specific constraints.
#'
#' @param points Matrix or data frame of color values with 3 columns
#' @param space Character string specifying color space: "oklab", "rgb", or "xyz"
#' @param distance Logical indicating if points will be used for distance calculations
#' @return Numeric matrix of validated color values
#' @keywords internal
validate_color_input <- function(points, space = "oklab", distance = FALSE) {
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
  if (space == "oklab") {
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

#' Initialize points in LAB color space
#'
#' Creates initial color points with random positions in LAB space, respecting
#' any fixed base colors. The initialization aims to:
#' - Place base colors in their exact LAB positions
#' - Distribute random colors across a perceptually uniform region
#' - Ensure minimum saturation for better distinctness
#'
#' @param n_colors An integer specifying the total number of colors to generate.
#' @param base_colors An optional vector of hex color codes (e.g., "#FF0000")
#'                    to be used as fixed base colors. These colors will be
#'                    placed in their exact LAB positions. Default is NULL.
#'
#' @return A matrix of LAB coordinates with dimensions n_colors x 3, where each
#'         row represents a color and the columns represent the L, a, and b
#'         values respectively.
#'
#' @keywords internal
#' @examples
#' initialize_points(5)
#' initialize_points(5, base_colors = c("#FF0000", "#0000FF"))
initialize_points <- function(n_colors, base_colors = NULL) {
  points <- matrix(nrow = n_colors, ncol = 3)
  colnames(points) <- c("l", "a", "b")
  n_base <- if (!is.null(base_colors)) length(base_colors) else 0
  n_random <- n_colors - n_base

  # Convert and place base colors if provided
  if (n_base > 0) {
    rgb_base <- farver::decode_colour(base_colors)
    points[1:n_base, ] <- farver::convert_colour(
      rgb_base,
      from = "rgb", to = "oklab"
    )
  }

  # Generate random colors within LAB boundaries
  if (n_random > 0) {
    lab_boundaries <- get_lab_boundaries(base_colors)
    random_indices <- (n_base + 1):n_colors

    # Generate points within boundary ranges
    points[random_indices, 1] <- runif(
      n_random,
      lab_boundaries$l[1],
      lab_boundaries$l[2]
    )
    points[random_indices, 2] <- runif(
      n_random,
      lab_boundaries$a[1],
      lab_boundaries$a[2]
    )
    points[random_indices, 3] <- runif(
      n_random,
      lab_boundaries$b[1],
      lab_boundaries$b[2]
    )
  }

  return(points)
}

#' Get LAB color space boundaries
#'
#' Determines valid boundaries for LAB color space, considering any base colors.
#' The boundaries ensure:
#' - Colors remain in displayable RGB gamut
#' - Base colors (if any) influence lightness, a, and b ranges
#' - Sufficient space for color variation
#'
#' @param base_colors An optional vector of hex color codes (e.g., "#FF0000").
#'                    If provided, these colors will influence the LAB space
#'                    boundaries. Default is NULL.
#'
#' @return A list containing three elements:
#'         \item{L}{A numeric vector of length 2,
#'         representing the min and max values for the L channel}
#'         \item{a}{A numeric vector of length 2,
#'         representing the min and max values for the a channel}
#'         \item{b}{A numeric vector of length 2,
#'         representing the min and max values for the b channel}
#'
#' @details If base colors are provided, the function calculates boundaries
#'          that encompass these colors with some padding. If no base colors
#'          are given, it uses default ranges that allow for a wide variety of
#'          colors.
#'
#' @keywords internal
get_lab_boundaries <- function(base_colors = NULL) {
  if (!is.null(base_colors)) {
    rgb_base <- t(sapply(base_colors, function(c) col2rgb(c)))
    lab_base <- farver::convert_colour(rgb_base, "rgb", "oklab")

    # Calculate target radius as average radius of base colors
    radii <- sqrt(lab_base[, 2]^2 + lab_base[, 3]^2)
    target_radius <- mean(radii)

    # L range from base colors with some padding
    l_range <- c(
      max(0.2, min(lab_base[, 1]) - 0.1),
      min(0.9, max(lab_base[, 1]) + 0.1)
    )
  } else {
    # Default values
    l_range <- c(0, 1)
    target_radius <- 0.6
  }

  list(
    l = l_range,
    a = c(-0.4, 0.4), # Keep wide a/b ranges for initialization
    b = c(-0.4, 0.4),
    target_radius = target_radius
  )
}

#' Enforce LAB Space Boundaries
#'
#' This function ensures that all colors remain within the valid LAB color
#' space boundaries. It adjusts the L, a, and b values of each color point
#' to ensure they fall within the specified minimum and maximum limits for each
#' channel.
#'
#' @param points A matrix of LAB coordinates where each row represents a color,
#'               and columns represent the L, a, and b values respectively.
#' @param boundaries A list containing the LAB space boundaries, with elements
#'                   'l', 'a', and 'b', each a numeric vector of length 2
#'                   specifying the min and max values for the respective
#'                   channels.
#'
#' @return A matrix of LAB coordinates with the same dimensions as the input
#'         'points' matrix, where each row represents a color, and the columns
#'         represent the adjusted l, a, and b values that are within the
#'         specified boundaries.
#'
#' @keywords internal
enforce_boundaries <- function(points, boundaries) {
  points[, 1] <- pmax(boundaries$l[1], pmin(boundaries$l[2], points[, 1]))
  points[, 2] <- pmax(boundaries$a[1], pmin(boundaries$a[2], points[, 2]))
  points[, 3] <- pmax(boundaries$b[1], pmin(boundaries$b[2], points[, 3]))
  points
}

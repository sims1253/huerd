#' Input validation for color values
#'
#' @param points Matrix of color values
#' @param space Character string indicating color space ("lab", "rgb", "xyz")
#' @param distance Logical indicating whether points are used for distance
#'                 calculation
#' @return Validated matrix or error
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

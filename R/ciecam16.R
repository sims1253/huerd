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

#' CAM16 Color Specification Class
#'
#' @description
#' A class for representing colors in the CIECAM16 color appearance model space.
#' This model provides a comprehensive way to describe colors in terms of their
#' perceptual attributes like lightness, chroma, and hue.
#'
#' The constructor `CAM16_Specification()` can accept either single values or vectors
#' for its properties (J, C, h, s, Q, M, H, HC). If all arguments are either empty or length 1,
#' it returns a single `CAM16_Specification` object. If any argument has a length greater
#' than 1, the constructor returns a list of `CAM16_Specification` objects, where each
#' element in the list corresponds to a set of input values.
#'
#' @slot J A numeric value representing lightness (0-100).
#' @slot C A numeric value representing chroma (0-100).
#' @slot h A numeric value representing hue angle in degrees (0-360).
#' @slot s A numeric value representing saturation (0-100).
#' @slot Q A numeric value representing brightness (0-100).
#' @slot M A numeric value representing colorfulness (0-100).
#' @slot H A numeric value representing hue quadrature (0-400).
#' @slot HC A character string representing hue composition (e.g., "35R65Y").
#'
#' @details
#' The CIECAM16 color specification includes the following properties:
#'   \itemize{
#'     \item \code{J}: Lightness (0-100)
#'     \item \code{C}: Chroma (0-100)
#'     \item \code{h}: Hue angle in degrees (0-360)
#'     \item \code{s}: Saturation (0-100)
#'     \item \code{Q}: Brightness (0-100)
#'     \item \code{M}: Colorfulness (0-100)
#'     \item \code{H}: Hue quadrature (0-400)
#'     \item \code{HC}: Hue composition (e.g., "35R65Y")
#'   }
#'
#' @param J A numeric vector representing lightness (0-100), or a single numeric value.
#' @param C A numeric vector representing chroma (0-100), or a single numeric value.
#' @param h A numeric vector representing hue angle in degrees (0-360), or a single numeric value.
#' @param s A numeric vector representing saturation (0-100), or a single numeric value.
#' @param Q A numeric vector representing brightness (0-100), or a single numeric value.
#' @param M A numeric vector representing colorfulness (0-100), or a single numeric value.
#' @param H A numeric vector representing hue quadrature (0-400), or a single numeric value.
#' @param HC A character vector representing hue composition (e.g., "35R65Y"), or a single character value.
#'
#' @return Either a single `CAM16_Specification` object (if all inputs are of length 0 or 1), or a list of `CAM16_Specification` objects if any input has length greater than 1.
#'
#' @references
#' CIE (2022). CIE 248:2022 The CIE 2016 Colour Appearance Model for Colour
#' Management Systems: CIECAM16.
#'
#' @import S7
#' @export
#' @examples
#' # Create a single CAM16_Specification object
#' single_color <- CAM16_Specification(J = 50, C = 30, h = 180)
#'
#' # Create a list of CAM16_Specification objects
#' multiple_colors <- CAM16_Specification(J = c(50, 60), C = c(30, 40), h = c(180, 200))
#'
#' # Create an empty object
#' empty_color <- CAM16_Specification()
CAM16_Specification <- S7::new_class(
  "CAM16_Specification",
  properties = list(
    J = S7::new_property(class = class_numeric), # Lightness
    C = S7::new_property(class = class_numeric), # Chroma
    h = S7::new_property(class = class_numeric), # Hue Angle
    s = S7::new_property(class = class_numeric), # Saturation
    Q = S7::new_property(class = class_numeric), # Brightness
    M = S7::new_property(class = class_numeric), # Colorfulness
    H = S7::new_property(class = class_numeric), # Hue Quadrature
    e = S7::new_property(class = class_numeric) # Eccentricity Factor
  ),
  constructor = function(J = numeric(),
                         C = numeric(),
                         h = numeric(),
                         s = numeric(),
                         Q = numeric(),
                         M = numeric(),
                         H = numeric(),
                         e = numeric()) {
    # Find maximum length of input vectors
    max_len <- max(
      length(J),
      length(C),
      length(h),
      length(s),
      length(Q),
      length(M),
      length(H),
      length(e)
    )

    # Single object case (all arguments have length 0 or 1)
    if (max_len <= 1) {
      # Create a new object and set properties
      obj <- new_object(
        .parent = CAM16_Specification,
        J = J,
        C = C,
        h = h,
        s = s,
        Q = Q,
        M = M,
        H = H,
        e = e
      )
      return(obj)
    } else {
      # Vectorized case

      # Create a list of objects
      results <- vector(mode = "list", length = max_len)
      for (i in seq_len(max_len)) {
        results[[i]] <- new_object(
          .parent = CAM16_Specification,
          J = if (length(J) > 0) {
            J[i]
          } else {
            J # Use the default empty vector
          },
          C = if (length(C) > 0) {
            C[i]
          } else {
            C # Use the default empty vector
          },
          h = if (length(h) > 0) {
            h[i]
          } else {
            h # Use the default empty vector
          },
          s = if (length(s) > 0) {
            s[i]
          } else {
            s # Use the default empty vector
          },
          Q = if (length(Q) > 0) {
            Q[i]
          } else {
            Q # Use the default empty vector
          },
          M = if (length(M) > 0) {
            M[i]
          } else {
            M # Use the default empty vector
          },
          H = if (length(H) > 0) {
            H[i]
          } else {
            H # Use the default empty vector
          },
          e = if (length(e) > 0) {
            e[i]
          } else {
            e # Use the default empty vector
          }
        )
      }
      return(results)
    }
  },
  # Validator (simplified since HC is removed)
  validator = function(self) {
    errors <- character()

    # Helper function for numeric range validation
    validate_range <- function(value, name, min = NULL, max = NULL) {
      if (!is.null(value)) {
        if (any(is.na(value))) {
          return(sprintf("Invalid %s: Value cannot be NA", name))
        }
        if (!is.null(min) && any(value < min)) {
          return(sprintf("Invalid %s: Value must be >= %s", name, min))
        }
        if (!is.null(max) && any(value > max)) {
          return(sprintf("Invalid %s: Value must be <= %s", name, max))
        }
      }
      return(NULL)
    }

    # Validate numeric ranges
    errors <- c(
      errors,
      validate_range(self@J, "J", 0, 100),
      validate_range(self@C, "C", 0, 100),
      validate_range(self@h, "h", 0, 360),
      validate_range(self@s, "s", 0, 100),
      validate_range(self@Q, "Q", 0, 100),
      validate_range(self@M, "M", 0, 100),
      validate_range(self@H, "H", 0, 400),
      validate_range(self@e, "e", 0, 1)
    )

    # Return the errors
    errors <- errors[!sapply(errors, is.null)]
    if (length(errors) > 0) {
      return(errors)
    }

    return(NULL)
  }
)



#' @export
local({
  S7::method(as.list, CAM16_Specification) <- function(x, ...) {
    result <- list()
    if (length(x@J) > 0) result$J <- x@J
    if (length(x@C) > 0) result$C <- x@C
    if (length(x@h) > 0) result$h <- x@h
    if (length(x@s) > 0) result$s <- x@s
    if (length(x@Q) > 0) result$Q <- x@Q
    if (length(x@M) > 0) result$M <- x@M
    if (length(x@H) > 0) result$H <- x@H
    if (length(x@e) > 0) result$e <- x@e
    result
  }

  S7::method(as.data.frame, CAM16_Specification) <- function(
      x,
      row.names = NULL,
      optional = FALSE,
      ...) {
    if (is.list(x)) {
      # Handle list of objects
      df_list <- lapply(x, function(obj) {
        as.data.frame(as.list(obj))
      })
      do.call(rbind, df_list)
    } else {
      # Handle single object
      as.data.frame(as.list(x))
    }
  }


  S7::method(format, CAM16_Specification) <- function(x, ...) {
    # Create a string representation of the CAM16_Specification object
    # Only include properties that have values (length > 0)

    parts <- character()
    if (length(x@J) > 0) {
      parts <- c(parts, paste0("  Lightness (J) = ", x@J))
    }
    if (length(x@C) > 0) {
      parts <- c(parts, paste0("  Chroma (C) = ", x@C))
    }
    if (length(x@h) > 0) {
      parts <- c(parts, paste0("  Hue Angle (h) = ", x@h))
    }
    if (length(x@s) > 0) {
      parts <- c(parts, paste0("  Saturation (s) = ", x@s))
    }
    if (length(x@Q) > 0) {
      parts <- c(parts, paste0("  Brightness (Q) = ", x@Q))
    }
    if (length(x@M) > 0) {
      parts <- c(parts, paste0("  Colorfulness (M) = ", x@M))
    }
    if (length(x@H) > 0) {
      parts <- c(parts, paste0("  Hue Quadrature (H) = ", x@H))
    }
    if (length(x@e) > 0) {
      parts <- c(parts, paste0("  Eccentricity Factor (e) = ", x@e))
    }

    # Combine the parts into a single string with newlines
    paste0(
      "<CAM16_Specification:\n",
      paste(parts, collapse = ",\n"),
      "\n>"
    )
  }

  S7::method(print, CAM16_Specification) <- function(x, ...) {
    if (is.list(x)) {
      # Handle list of objects
      for (i in seq_along(x)) {
        cat("[[", i, "]]\n", sep = "")
        cat(format(x[[i]]), "\n\n") # Add extra newline between list elements
      }
    } else {
      # Handle single object
      cat(format(x), "\n")
    }
  }

  S7::method(`==`, list(CAM16_Specification, CAM16_Specification)) <- function(e1, e2) {
    # Check if all corresponding properties are equal (or both empty)
    all(
      (length(e1@J) > 0 && length(e2@J) > 0 && e1@J == e2@J) || (length(e1@J) == 0 && length(e2@J) == 0),
      (length(e1@C) > 0 && length(e2@C) > 0 && e1@C == e2@C) || (length(e1@C) == 0 && length(e2@C) == 0),
      (length(e1@h) > 0 && length(e2@h) > 0 && e1@h == e2@h) || (length(e1@h) == 0 && length(e2@h) == 0),
      (length(e1@s) > 0 && length(e2@s) > 0 && e1@s == e2@s) || (length(e1@s) == 0 && length(e2@s) == 0),
      (length(e1@Q) > 0 && length(e2@Q) > 0 && e1@Q == e2@Q) || (length(e1@Q) == 0 && length(e2@Q) == 0),
      (length(e1@M) > 0 && length(e2@M) > 0 && e1@M == e2@M) || (length(e1@M) == 0 && length(e2@M) == 0),
      (length(e1@H) > 0 && length(e2@H) > 0 && e1@H == e2@H) || (length(e1@H) == 0 && length(e2@H) == 0),
      (length(e1@e) > 0 && length(e2@e) > 0 && e1@e == e2@e) || (length(e1@e) == 0 && length(e2@e) == 0)
    )
  }

   # Overload the '-' operator for CAM16_Specification objects
   S7::method(`-`, list(CAM16_Specification, CAM16_Specification)) <- function(e1, e2) {
    # Calculate the Euclidean distance in CAM16-UCS space
    
    # Ensure that both objects have J, C, and h properties
    if (length(e1@J) == 0 || length(e1@C) == 0 || length(e1@h) == 0 ||
        length(e2@J) == 0 || length(e2@C) == 0 || length(e2@h) == 0) {
      return(NA_real_) # Or you could throw an error: stop("Both objects must have J, C, and h properties for subtraction.")
    }
    
    # Convert to CAM16-UCS coordinates
    ucs1 <- CAM16_UCS(e1)
    ucs2 <- CAM16_UCS(e2)
    
    # Calculate Euclidean distance
    sqrt((ucs1$J_prime - ucs2$J_prime)^2 + (ucs1$a - ucs2$a)^2 + (ucs1$b - ucs2$b)^2)
  }
})


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


CAM16_UCS <- function(obj) {
  # Ensure that the object has the necessary properties
  if (length(obj@J) == 0 || length(obj@C) == 0 || length(obj@h) == 0 ||
      length(obj@H) == 0 || length(obj@e) == 0) {
    stop("Object must have J, C, h, H, and e properties for CAM16-UCS conversion.")
  }
  
  J <- obj@J
  C <- obj@C
  h <- obj@h
  H <- obj@H
  e <- obj@e
  
  # CAM16-UCS formulas (from CIE 248:2022)
  J_prime <- (1 + 100 * 0.007) * J / (1 + 0.007 * J)
  M <- (1 / 0.0228) * log(1 + 0.0228 * C)
  a <- M * e * cos(h * pi / 180)
  b <- M * e * sin(h * pi / 180)
  
  list(J_prime = J_prime, a = a, b = b)
}
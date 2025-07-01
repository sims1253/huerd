#' Smart Input Validation for Color Inputs
#'
#' @param colors Input colors (hex, OKLAB matrix, etc.)
#' @param context Context for validation ("generation", "evaluation", "simulation")
#' @param strict_mode Whether to use strict validation
#' @noRd
validate_color_input_smart <- function(
  colors,
  context = "general",
  strict_mode = FALSE
) {
  validation_result <- list(
    valid = TRUE,
    warnings = character(0),
    errors = character(0),
    processed_colors = NULL
  )

  # Type and basic structure validation
  if (is.character(colors)) {
    validation_result <- validate_hex_colors(
      colors,
      validation_result,
      strict_mode
    )
  } else if (is.matrix(colors)) {
    validation_result <- validate_oklab_matrix(
      colors,
      validation_result,
      strict_mode
    )
  } else {
    validation_result$valid <- FALSE
    validation_result$errors <- c(
      validation_result$errors,
      "Colors must be character vector (hex) or numeric matrix (OKLAB)"
    )
  }

  # Context-specific validation (only if colors are of valid type)
  if (validation_result$valid) {
    if (context == "generation" && length(colors) == 0) {
      validation_result$errors <- c(
        validation_result$errors,
        "Cannot generate palette: no base colors provided and n_colors not specified"
      )
    }

    if (context == "evaluation" && length(colors) < 2) {
      validation_result$warnings <- c(
        validation_result$warnings,
        "Evaluation with fewer than 2 colors will have limited metrics"
      )
    }
  }

  # Report validation results
  if (length(validation_result$warnings) > 0) {
    for (warn in validation_result$warnings) {
      warning(warn, call. = FALSE)
    }
  }

  if (length(validation_result$errors) > 0) {
    stop(paste(validation_result$errors, collapse = "; "), call. = FALSE)
  }

  return(validation_result)
}

#' Validate hex color inputs
#' @noRd
validate_hex_colors <- function(colors, validation_result, strict_mode) {
  if (length(colors) == 0) {
    validation_result$processed_colors <- character(0)
    return(validation_result)
  }

  # Check for valid hex format
  hex_pattern <- "^#[0-9A-Fa-f]{6}$"
  valid_hex <- grepl(hex_pattern, colors, ignore.case = TRUE)
  na_colors <- is.na(colors)

  # Allow NA values but warn
  if (any(na_colors)) {
    validation_result$warnings <- c(
      validation_result$warnings,
      paste("Found", sum(na_colors), "NA color values")
    )
  }

  # Check for invalid hex
  invalid_hex <- !valid_hex & !na_colors
  if (any(invalid_hex)) {
    if (strict_mode) {
      validation_result$valid <- FALSE
      validation_result$errors <- c(
        validation_result$errors,
        paste(
          "Invalid hex colors:",
          paste(colors[invalid_hex], collapse = ", ")
        )
      )
    } else {
      validation_result$warnings <- c(
        validation_result$warnings,
        paste(
          "Invalid hex colors will be ignored:",
          paste(
            colors[invalid_hex][1:min(3, sum(invalid_hex))],
            collapse = ", "
          )
        )
      )
    }
  }

  # Test gamut validity for valid hex colors
  valid_colors <- colors[valid_hex]
  if (length(valid_colors) > 0) {
    tryCatch(
      {
        rgb_matrix <- farver::decode_colour(valid_colors)
        oklab_matrix <- farver::convert_colour(
          rgb_matrix,
          from = 'rgb',
          to = 'oklab'
        )

        # Check for reasonable OKLAB ranges
        L_values <- oklab_matrix[, 1]
        if (any(L_values < 0 | L_values > 1)) {
          validation_result$warnings <- c(
            validation_result$warnings,
            "Some colors have unusual lightness values (outside 0-1 range)"
          )
        }

        validation_result$processed_colors <- colors
      },
      error = function(e) {
        validation_result$warnings <- c(
          validation_result$warnings,
          "Some colors may be outside valid gamut"
        )
      }
    )
  }

  return(validation_result)
}

#' Validate OKLAB matrix inputs
#' @noRd
validate_oklab_matrix <- function(colors, validation_result, strict_mode) {
  # Check dimensions
  if (!is.matrix(colors) || ncol(colors) != 3) {
    validation_result$valid <- FALSE
    validation_result$errors <- c(
      validation_result$errors,
      "OKLAB matrix must have exactly 3 columns (L, a, b)"
    )
    return(validation_result)
  }

  # Check for numeric values
  if (!is.numeric(colors)) {
    validation_result$valid <- FALSE
    validation_result$errors <- c(
      validation_result$errors,
      "OKLAB matrix must contain numeric values"
    )
    return(validation_result)
  }

  # Check for missing values
  if (any(is.na(colors))) {
    if (strict_mode) {
      validation_result$valid <- FALSE
      validation_result$errors <- c(
        validation_result$errors,
        "OKLAB matrix contains NA values"
      )
    } else {
      validation_result$warnings <- c(
        validation_result$warnings,
        "OKLAB matrix contains NA values"
      )
    }
  }

  # Check OKLAB value ranges
  L_values <- colors[, 1]
  a_values <- colors[, 2]
  b_values <- colors[, 3]

  # L should be in [0, 1] for OKLAB
  if (any(L_values < 0 | L_values > 1, na.rm = TRUE)) {
    validation_result$warnings <- c(
      validation_result$warnings,
      "OKLAB L values outside typical range [0, 1]"
    )
  }

  # a and b should be reasonable (roughly [-0.5, 0.5] for most colors)
  if (any(abs(a_values) > 0.5 | abs(b_values) > 0.5, na.rm = TRUE)) {
    validation_result$warnings <- c(
      validation_result$warnings,
      "OKLAB a/b values are quite large (may be outside typical gamut)"
    )
  }

  validation_result$processed_colors <- colors
  return(validation_result)
}

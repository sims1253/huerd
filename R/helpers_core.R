# Parameter Validation Functions
# ==============================================================================

#' Validate number of colors parameter
#' @noRd
.validate_n <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || n < 0 || n != round(n)) {
    stop("'n' must be a single non-negative integer.", call. = FALSE)
  }
}

#' Validate include_colors parameter and cross-validation with n
#' @noRd
.validate_include_colors <- function(include_colors, n) {
  if (is.null(include_colors)) {
    return()
  }

  if (!is.character(include_colors)) {
    stop(
      "'include_colors' must be a character vector of hex colors or NULL.",
      call. = FALSE
    )
  }

  if (
    length(include_colors) > 0 &&
      !all(grepl("^#[0-9A-Fa-f]{6}$", include_colors, ignore.case = TRUE))
  ) {
    stop(
      "All elements in 'include_colors' must be valid hex colors (e.g., #RRGGBB).",
      call. = FALSE
    )
  }

  if (length(include_colors) > n) {
    stop(
      "Number of fixed colors ('include_colors') cannot exceed total requested colors ('n').",
      call. = FALSE
    )
  }

  if (length(unique(tolower(include_colors))) != length(include_colors)) {
    stop("'include_colors' contains duplicate values.", call. = FALSE)
  }
}

#' Validate n and include_colors consistency
#' @noRd
.validate_n_include_colors_consistency <- function(n, include_colors) {
  if (n == 0 && !is.null(include_colors) && length(include_colors) > 0) {
    stop(
      "Cannot have 'include_colors' with n=0 unless 'include_colors' is also empty or NULL.",
      call. = FALSE
    )
  }
}

#' Validate balance_weights parameter
#' @noRd
.validate_balance_weights <- function(balance_weights) {
  if (
    !is.numeric(balance_weights) ||
      length(balance_weights) != 2 ||
      any(balance_weights < 0)
  ) {
    stop(
      "'balance_weights' must be a numeric vector of two non-negative values.",
      call. = FALSE
    )
  }
}

#' Validate initialization lightness bounds
#' @noRd
.validate_init_lightness_bounds <- function(init_lightness_bounds) {
  if (
    !is.numeric(init_lightness_bounds) ||
      length(init_lightness_bounds) != 2 ||
      any(init_lightness_bounds < 0) ||
      any(init_lightness_bounds > 1) ||
      init_lightness_bounds[1] >= init_lightness_bounds[2]
  ) {
    stop(
      "'init_lightness_bounds' must be a numeric vector [min, max] for OKLAB L, where 0 <= min < max <= 1.",
      call. = FALSE
    )
  }
}

#' Validate initialization HCL bounds
#' @noRd
.validate_init_hcl_bounds <- function(init_hcl_bounds) {
  if (
    !is.list(init_hcl_bounds) ||
      !all(c("C", "L") %in% names(init_hcl_bounds)) ||
      !is.numeric(init_hcl_bounds$C) ||
      length(init_hcl_bounds$C) != 2 ||
      !is.numeric(init_hcl_bounds$L) ||
      length(init_hcl_bounds$L) != 2 ||
      any(init_hcl_bounds$C < 0) ||
      any(init_hcl_bounds$C > 150) || # HCL Chroma max bound
      init_hcl_bounds$C[1] >= init_hcl_bounds$C[2] ||
      any(init_hcl_bounds$L < 0) ||
      any(init_hcl_bounds$L > 100) || # HCL Lightness max bound
      init_hcl_bounds$L[1] >= init_hcl_bounds$L[2]
  ) {
    stop(
      "'init_hcl_bounds' must be a list with numeric vectors C=[min,max] and L=[min,max] for HCL init, with valid ranges and min < max.",
      call. = FALSE
    )
  }
}

#' Validate aesthetic influence and strength parameters
#' @noRd
.validate_aesthetic_params <- function(fixed_aesthetic_influence) {
  if (
    !is.numeric(fixed_aesthetic_influence) ||
      length(fixed_aesthetic_influence) != 1 ||
      fixed_aesthetic_influence < 0 ||
      fixed_aesthetic_influence > 1
  ) {
    stop(
      "'fixed_aesthetic_influence' must be a single numeric value between 0 and 1.",
      call. = FALSE
    )
  }
}

#' Validate aesthetic penalty weights
#' @noRd
.validate_aesthetic_penalty_weights <- function(aesthetic_penalty_weights_LC) {
  if (
    !is.numeric(aesthetic_penalty_weights_LC) ||
      length(aesthetic_penalty_weights_LC) != 2 ||
      any(aesthetic_penalty_weights_LC < 0) ||
      is.null(names(aesthetic_penalty_weights_LC)) ||
      !all(sort(names(aesthetic_penalty_weights_LC)) == c("C", "L"))
  ) {
    stop(
      "'aesthetic_penalty_weights_LC' must be a named numeric vector of two non-negative values, with names 'L' and 'C'. E.g., c(L=1.0, C=2.0).",
      call. = FALSE
    )
  }
}

#' Validate weights parameter for multi-objective optimization
#' @noRd
.validate_weights <- function(weights) {
  if (is.null(weights)) {
    return()
  }

  if (!is.numeric(weights) || length(weights) == 0) {
    stop("'weights' must be a named numeric vector or NULL.", call. = FALSE)
  }

  if (is.null(names(weights)) || any(names(weights) == "")) {
    stop(
      "'weights' must be a named numeric vector with all elements named.",
      call. = FALSE
    )
  }

  if (any(weights < 0)) {
    stop("All 'weights' values must be non-negative.", call. = FALSE)
  }

  # Check for valid objective names
  valid_objectives <- c("distance", "smooth_repulsion", "smooth_logsumexp")
  invalid_names <- names(weights)[!names(weights) %in% valid_objectives]
  if (length(invalid_names) > 0) {
    stop(
      paste0(
        "Invalid objective names in 'weights': ",
        paste(invalid_names, collapse = ", "),
        ". Valid objectives are: ",
        paste(valid_objectives, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Check that at least one weight is positive
  if (sum(weights) <= 0) {
    stop("At least one 'weights' value must be positive.", call. = FALSE)
  }
}

#' Validate aesthetic initialization config
#' @noRd
.validate_aesthetic_init_config <- function(aesthetic_init_config) {
  if (is.null(aesthetic_init_config)) {
    return()
  }

  if (!is.list(aesthetic_init_config)) {
    stop("'aesthetic_init_config' must be a list or NULL.", call. = FALSE)
  }

  valid_config_names <- names(.DEFAULT_AESTHETIC_INIT_CONFIG)
  for (name in names(aesthetic_init_config)) {
    if (!(name %in% valid_config_names)) {
      stop(
        paste0(
          "Invalid name '",
          name,
          "' in aesthetic_init_config. Valid names are: ",
          paste(valid_config_names, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (
      !is.numeric(aesthetic_init_config[[name]]) ||
        length(aesthetic_init_config[[name]]) != 1
    ) {
      stop(
        paste0(
          "Parameter '",
          name,
          "' in aesthetic_init_config must be a single numeric value."
        ),
        call. = FALSE
      )
    }
  }
}

#' Validate optimizer parameter
#' @noRd
.validate_optimizer <- function(optimizer) {
  if (!is.character(optimizer) || length(optimizer) != 1) {
    stop("'optimizer' must be a single character string.", call. = FALSE)
  }

  # Currently supported optimizers
  valid_optimizers <- c(
    "nloptr_cobyla",
    "sann",
    "nlopt_direct",
    "nlopt_neldermead",
    "nlopt_lbfgs"
  )

  if (!optimizer %in% valid_optimizers) {
    stop(
      paste0(
        "Invalid optimizer '",
        optimizer,
        "'. ",
        "Valid optimizers are: ",
        paste(valid_optimizers, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

#' Validate Input Parameters for generate_palette
#'
#' This helper function checks all user-provided parameters for correctness
#' before the main `generate_palette` logic begins.
#' @noRd
validate_inputs <- function(
  n,
  include_colors,
  init_lightness_bounds,
  init_hcl_bounds,
  fixed_aesthetic_influence,
  aesthetic_init_config,
  weights = NULL,
  optimizer = "nloptr_cobyla"
) {
  .validate_n(n)
  .validate_include_colors(include_colors, n)
  .validate_n_include_colors_consistency(n, include_colors)
  .validate_init_lightness_bounds(init_lightness_bounds)
  .validate_init_hcl_bounds(init_hcl_bounds)
  .validate_aesthetic_params(fixed_aesthetic_influence)
  .validate_aesthetic_init_config(aesthetic_init_config)
  .validate_weights(weights)
  .validate_optimizer(optimizer)
}

#' Handle cases with no free colors to generate
#'
#' @param n Total number of colors originally requested.
#' @param include_colors The vector of fixed colors.
#' @param optimize_for The optimization target string.
#' @param return_metrics Logical, whether metrics should be calculated.
#' @param progress Logical, whether to show `cli` messages.
#' @param status_msg A message explaining why optimization was skipped.
#' @return A `huerd_palette` object.
#' @noRd
.handle_no_free_colors <- function(
  n,
  include_colors,
  return_metrics,
  progress,
  status_msg = "All colors fixed",
  generation_metadata = NULL
) {
  if (progress) {
    cat("No free colors to generate; skipping optimization.\n")
  }

  final_hex_colors <- if (n > 0) include_colors else character(0)
  class(final_hex_colors) <- c("huerd_palette", class(final_hex_colors))

  if (return_metrics) {
    oklab_eval <- if (n > 0) {
      farver::decode_colour(final_hex_colors, to = "oklab")
    } else {
      matrix(numeric(0), ncol = 3)
    }
    metrics <- evaluate_palette(oklab_eval)
    attr(final_hex_colors, "metrics") <- metrics
  }

  attr(final_hex_colors, "optimization_details") <- list(
    target = "minimax",
    iterations = 0,
    status_message = status_msg,
    nloptr_status = NA_integer_,
    final_objective_value = NA_real_
  )

  # Store generation metadata for reproducibility
  if (!is.null(generation_metadata)) {
    attr(final_hex_colors, "generation_metadata") <- generation_metadata
  }

  if (progress) {
    cat("Done.\n")
  }
  return(final_hex_colors)
}

#' Normalize weight vectors
#'
#' Ensures a numeric vector sums to 1.0 if its sum is positive.
#' @param weights A numeric vector.
#' @param name The name of the parameter, for use in warning messages.
#' @param progress Logical, whether to show `cli` messages.
#' @return A numeric vector that sums to 1.0 or 0.
#' @noRd
.normalize_weights <- function(weights, name, progress) {
  if (
    sum(weights, na.rm = TRUE) > 0 &&
      !identical(sum(weights, na.rm = TRUE), 1.0)
  ) {
    if (progress) {
      cat(
        "Warning: ",
        name,
        " do not sum to 1 and will be normalized.\n",
        sep = ""
      )
    }
    return(weights / sum(weights, na.rm = TRUE))
  }
  return(weights)
}

#' Merge user-provided aesthetic config with defaults
#'
#' @param user_config The list provided by the user.
#' @return The final, merged configuration list.
#' @noRd
.merge_aesthetic_config <- function(user_config) {
  config <- .DEFAULT_AESTHETIC_INIT_CONFIG
  if (!is.null(user_config)) {
    # Check for version mismatch as a forward-compatibility measure
    if (
      !is.null(user_config$config_version) &&
        user_config$config_version != config$config_version
    ) {
      cat(
        "Warning: The provided aesthetic_init_config has version ",
        user_config$config_version,
        ", but the package expects version ",
        config$config_version,
        ". Parameters may be misinterpreted.\n",
        sep = ""
      )
    }
    config <- utils::modifyList(config, user_config)
  }
  return(config)
}


#' Calculate Pairwise Perceptual Distances in OK LAB
#'
#' Uses `stats::dist` for efficient calculation of the Euclidean distance
#' between all pairs of colors in a matrix.
#'
#' @param colors_oklab Matrix of colors in OK LAB space (N x 3).
#' @return A square distance matrix (N x N).
#' @noRd
calculate_perceptual_distances <- function(colors_oklab) {
  # Input validation for robustness
  if (
    is.null(colors_oklab) || !is.matrix(colors_oklab) || ncol(colors_oklab) != 3
  ) {
    nr <- tryCatch(nrow(as.matrix(colors_oklab)), error = function(e) 0)
    return(matrix(NA_real_, nrow = max(0, nr), ncol = max(0, nr)))
  }
  if (nrow(colors_oklab) < 1) {
    return(matrix(numeric(0), nrow = 0, ncol = 0))
  }
  if (nrow(colors_oklab) < 2) {
    mat_out <- matrix(0, nrow = nrow(colors_oklab), ncol = nrow(colors_oklab))
    # If the single color has NA components, distance to self is NA
    if (any(is.na(colors_oklab))) {
      mat_out[,] <- NA_real_
    }
    return(mat_out)
  }
  # `stats::dist` is highly optimized (written in C)
  dist_obj <- stats::dist(colors_oklab, method = "euclidean")
  return(as.matrix(dist_obj))
}


#' Infix helper for providing a default for NA/NULL/Inf
#'
#' A helper to provide a default value for NULL, NA, or non-finite numeric
#' values, which is useful for preventing errors in metric and objective calculations.
#'
#' @param x The value to check.
#' @param y The default value to return if `x` is invalid.
#' @return `x` if it is valid, otherwise `y`.
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }

  # Handle empty vectors
  if (length(x) == 0) {
    return(y)
  }

  # For numeric values, check if ALL values are NA or non-finite
  if (is.numeric(x) && all(is.na(x) | !is.finite(x))) {
    return(y)
  }

  # For character values, check if ALL values are NA
  if (is.character(x) && all(is.na(x))) {
    return(y)
  }

  # For single-value cases (scalars), use original logic for backwards compatibility
  if (length(x) == 1) {
    if (is.numeric(x) && (is.na(x) || !is.finite(x))) {
      return(y)
    }
    if (is.character(x) && is.na(x)) {
      return(y)
    }
  }

  return(x)
}

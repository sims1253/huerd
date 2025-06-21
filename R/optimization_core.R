#' Optimize Color Palette using Pure Minimax Box-Constrained Optimization
#'
#' This function takes an initial set of colors and optimizes the positions of
#' the "free" colors to maximize the minimum perceptual distance between any
#' two colors (pure minimax objective).
#'
#' @param initial_colors_oklab Matrix of all colors (fixed and initial free) in OK LAB space.
#' @param fixed_mask Logical vector indicating which rows in `initial_colors_oklab` are fixed.
#' @param max_iterations Integer. Maximum iterations for nloptr.
#' @param track_states Logical. Whether to track optimization states.
#' @param save_every Integer. Frequency for saving optimization states.
#' @param return_states Logical. Whether to return optimization states.
#' @return A list containing the optimized color matrix `palette` and `details` about the optimization.
#' @noRd
optimize_colors_constrained <- function(
  initial_colors_oklab,
  fixed_mask,
  max_iterations,
  track_states = FALSE,
  save_every = 50,
  return_states = FALSE
) {
  n_free_colors <- sum(!fixed_mask)
  initial_free_params <- as.vector(t(initial_colors_oklab[
    !fixed_mask,
    ,
    drop = FALSE
  ]))

  # Environment to hold iteration count, accessible by the objective function
  eval_f_env <- new.env(parent = emptyenv())
  eval_f_env$iter <- 0

  # Initialize state tracking if enabled
  optimization_states <- NULL
  if (track_states) {
    optimization_states <- list()
  }

  # Objective function to be minimized by nloptr (pure minimax)
  eval_f <- function(free_params_vec) {
    eval_f_env$iter <- eval_f_env$iter + 1
    current_free_colors_oklab <- matrix(free_params_vec, ncol = 3, byrow = TRUE)

    # Pure minimax objective: maximize minimum perceptual distance
    temp_all_colors_oklab <- initial_colors_oklab
    temp_all_colors_oklab[!fixed_mask, ] <- current_free_colors_oklab
    objective_value <- -objective_min_perceptual_dist(temp_all_colors_oklab)

    # Capture state if tracking enabled
    if (track_states && eval_f_env$iter %% save_every == 0) {
      temp_all_colors_oklab <- initial_colors_oklab
      temp_all_colors_oklab[!fixed_mask, ] <- current_free_colors_oklab

      current_state <- list(
        iteration = eval_f_env$iter,
        colors_oklab = temp_all_colors_oklab,
        objective_value = if (is.finite(objective_value)) {
          objective_value
        } else {
          1e10
        },
        timestamp = Sys.time()
      )
      optimization_states[[length(optimization_states) + 1]] <<- current_state
    }

    return(if (is.finite(objective_value)) objective_value else 1e10) # Fallback for non-finite objectives
  }

  # Setup and run nloptr
  # Box constraints for OKLAB space
  lower_bounds <- rep(c(0.001, -0.4, -0.4), n_free_colors)
  upper_bounds <- rep(
    c(
      0.999,
      0.4,
      0.4
    ),
    n_free_colors
  )

  opts <- list(
    "algorithm" = "NLOPT_LN_COBYLA",
    "xtol_rel" = 1.0e-5,
    "maxeval" = max_iterations,
    "print_level" = 0
  )

  result <- tryCatch(
    nloptr::nloptr(
      x0 = initial_free_params,
      eval_f = eval_f,
      # No `eval_g_ineq` constraint function needed
      lb = lower_bounds,
      ub = upper_bounds,
      opts = opts
    ),
    error = function(e) {
      initial_obj_val <- tryCatch(
        eval_f(initial_free_params),
        error = function(e_obj) NA_real_
      )
      list(
        solution = initial_free_params,
        status = -999,
        message = paste("Error in nloptr:", e$message),
        objective = initial_obj_val
      )
    }
  )

  # Process and return results
  optimized_free_colors_oklab <- matrix(result$solution, ncol = 3, byrow = TRUE)
  # Final clamp to ensure solution is strictly within bounds
  optimized_free_colors_oklab[, 1] <- pmax(
    lower_bounds[1],
    pmin(upper_bounds[1], optimized_free_colors_oklab[, 1])
  )
  optimized_free_colors_oklab[, 2] <- pmax(
    lower_bounds[2],
    pmin(upper_bounds[2], optimized_free_colors_oklab[, 2])
  )
  optimized_free_colors_oklab[, 3] <- pmax(
    lower_bounds[3],
    pmin(upper_bounds[3], optimized_free_colors_oklab[, 3])
  )

  final_colors_oklab <- initial_colors_oklab
  final_colors_oklab[!fixed_mask, ] <- optimized_free_colors_oklab

  # Prepare return value
  return_value <- list(
    palette = final_colors_oklab,
    details = list(
      iterations = eval_f_env$iter,
      status_message = result$message,
      nloptr_status = result$status,
      final_objective_value = result$objective
    )
  )

  # Add optimization states if requested
  if (return_states && track_states) {
    return_value$optimization_states <- optimization_states
  }

  return(return_value)
}

#' Aggregate Objective Function for Optimization
#' This function computes the score to be maximized (so it returns a positive value).
#' The main optimization function will take the negative of this.
#' @noRd
objective_function_aggregator <- function(
  colors_oklab,
  optimize_for,
  balance_weights
) {
  if (nrow(colors_oklab) < 2) {
    return(Inf)
  }

  if (optimize_for == "perceptual") {
    return(objective_min_perceptual_dist(colors_oklab))
  }
  if (optimize_for == "cvd_safe") {
    return(objective_min_cvd_safe_dist(colors_oklab))
  }

  # Balanced
  perceptual_score <- objective_min_perceptual_dist(colors_oklab)
  cvd_score <- objective_min_cvd_safe_dist(colors_oklab)

  perceptual_score <- perceptual_score %||% 0
  cvd_score <- cvd_score %||% 0

  # If weights are c(0,0), value will be 0.
  return(balance_weights[1] * perceptual_score + balance_weights[2] * cvd_score)
}

#' Objective: Maximize Minimum Perceptual Distance
#' @noRd
objective_min_perceptual_dist <- function(colors_oklab) {
  if (nrow(colors_oklab) < 2) {
    return(Inf)
  }
  dist_matrix <- calculate_perceptual_distances(colors_oklab)

  valid_distances <- dist_matrix[upper.tri(dist_matrix)]
  valid_distances <- valid_distances[is.finite(valid_distances)]

  return(if (length(valid_distances) == 0) 0 else min(valid_distances))
}

#' Objective: Maximize Minimum CVD-Safe Distance
#' @noRd
objective_min_cvd_safe_dist <- function(colors_oklab) {
  if (nrow(colors_oklab) < 2) {
    return(Inf)
  }

  lab_colors <- farver::convert_colour(colors_oklab, from = "oklab", to = "lab")
  rgb_matrix_255 <- farver::convert_colour(
    lab_colors,
    from = "lab",
    to = "rgb"
  )

  # Convert to 0-1 range (colorspace expects 0-1, not 0-255)
  rgb_matrix_01 <- rgb_matrix_255 / 255.0

  if (any(is.na(rgb_matrix_01))) {
    return(0)
  }

  # Ensure matrix structure is preserved - defensive programming
  if (!is.matrix(rgb_matrix_01) || ncol(rgb_matrix_01) != 3) {
    # Force matrix structure if lost during conversion
    rgb_matrix_01 <- as.matrix(rgb_matrix_01)
    if (ncol(rgb_matrix_01) != 3) {
      return(0) # Invalid color data
    }
  }

  # Clamp RGB values while preserving matrix structure
  rgb_colors_clamped <- rgb_matrix_01
  rgb_colors_clamped[rgb_colors_clamped < 0] <- 0
  rgb_colors_clamped[rgb_colors_clamped > 1] <- 1

  # Convert to colorspace's expected format (sRGB object)
  # Ensure we pass a proper matrix with exactly 3 columns
  srgb_obj <- colorspace::sRGB(rgb_colors_clamped)

  cvd_types <- c("deutan", "protan", "tritan")
  worst_case_min_dist <- Inf

  for (cvd_type in cvd_types) {
    cvd_simulated_srgb <- switch(
      cvd_type,
      "deutan" = colorspace::deutan(srgb_obj, severity = 1),
      "protan" = colorspace::protan(srgb_obj, severity = 1),
      "tritan" = colorspace::tritan(srgb_obj, severity = 1)
    )
    cvd_simulated_rgb_01 <- cvd_simulated_srgb@coords

    cvd_simulated_lab <- farver::convert_colour(
      cvd_simulated_rgb_01 * 255.0,
      from = "rgb",
      to = "lab"
    )
    cvd_simulated_oklab <- farver::convert_colour(
      cvd_simulated_lab,
      from = "lab",
      to = "oklab"
    )

    min_dist_this_cvd <- objective_min_perceptual_dist(cvd_simulated_oklab)
    worst_case_min_dist <- min(
      worst_case_min_dist,
      min_dist_this_cvd,
      na.rm = TRUE
    )
  }
  return(worst_case_min_dist)
}

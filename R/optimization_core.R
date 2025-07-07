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
    objective_value <- -objective_min_cvd_safe_dist(temp_all_colors_oklab)

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
    #"algorithm" = "NLOPT_GN_DIRECT_L_RAND",
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

  rgb_matrix_255 <- farver::convert_colour(
    colors_oklab,
    from = "oklab",
    to = "rgb"
  )

  # Convert to 0-1 range (colorspace expects 0-1, farver returns 0-255)
  rgb_matrix_01 <- rgb_matrix_255 / 255

  if (any(is.na(rgb_matrix_01))) {
    return(0)
  }

  #if (!is.matrix(rgb_matrix_01) || ncol(rgb_matrix_01) != 3) {
  # Force matrix structure if lost during conversion
  #  rgb_matrix_01 <- as.matrix(rgb_matrix_01)
  #  if (ncol(rgb_matrix_01) != 3) {
  #    return(0) # Invalid color data
  #  }
  #}

  srgb_obj <- colorspace::sRGB(rgb_matrix_01)

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

    cvd_simulated_oklab <- farver::convert_colour(
      # Convert to 0-255 range (colorspace returns 0-1, farver expects 0-255)
      cvd_simulated_rgb_01 * 255,
      from = "rgb",
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

#' Optimize Color Palette using Simulated Annealing
#'
#' This function takes an initial set of colors and optimizes the positions of
#' the "free" colors to maximize the minimum perceptual distance between any
#' two colors using simulated annealing from stats::optim.
#'
#' @param initial_colors_oklab Matrix of all colors (fixed and initial free) in OK LAB space.
#' @param fixed_mask Logical vector indicating which rows in `initial_colors_oklab` are fixed.
#' @param max_iterations Integer. Maximum iterations for simulated annealing.
#' @param track_states Logical. Whether to track optimization states.
#' @param save_every Integer. Frequency for saving optimization states.
#' @param return_states Logical. Whether to return optimization states.
#' @return A list containing the optimized color matrix `palette` and `details` about the optimization.
#' @noRd
optimize_colors_sann <- function(
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

  # Box constraints for OKLAB space
  lower_bounds <- rep(c(0.001, -0.4, -0.4), n_free_colors)
  upper_bounds <- rep(c(0.999, 0.4, 0.4), n_free_colors)

  # Objective function to be minimized by optim (pure minimax with penalty for constraint violations)
  eval_f <- function(free_params_vec) {
    eval_f_env$iter <- eval_f_env$iter + 1
    current_free_colors_oklab <- matrix(free_params_vec, ncol = 3, byrow = TRUE)

    # Pure minimax objective: maximize minimum perceptual distance
    temp_all_colors_oklab <- initial_colors_oklab
    temp_all_colors_oklab[!fixed_mask, ] <- current_free_colors_oklab
    objective_value <- -objective_min_cvd_safe_dist(temp_all_colors_oklab)

    # Add penalty for constraint violations (box constraints)
    penalty <- 0
    violations <- pmax(0, lower_bounds - free_params_vec) +
      pmax(0, free_params_vec - upper_bounds)
    penalty <- sum(violations^2) * 1e6 # Large penalty for constraint violations

    # Capture state if tracking enabled
    if (track_states && eval_f_env$iter %% save_every == 0) {
      current_state <- list(
        iteration = eval_f_env$iter,
        colors_oklab = temp_all_colors_oklab,
        objective_value = if (is.finite(objective_value)) {
          objective_value
        } else {
          1e10
        },
        penalty = penalty,
        timestamp = Sys.time()
      )
      optimization_states[[length(optimization_states) + 1]] <<- current_state
    }

    final_objective <- if (is.finite(objective_value)) {
      objective_value + penalty
    } else {
      1e10 + penalty
    }

    return(final_objective)
  }

  # Setup and run simulated annealing
  result <- tryCatch(
    stats::optim(
      par = initial_free_params,
      fn = eval_f,
      method = "SANN",
      control = list(
        maxit = max_iterations,
        temp = 10, # Initial temperature
        tmax = 10 # Number of function evaluations at each temperature
      )
    ),
    error = function(e) {
      initial_obj_val <- tryCatch(
        eval_f(initial_free_params),
        error = function(e_obj) NA_real_
      )
      list(
        par = initial_free_params,
        convergence = -999,
        message = paste("Error in optim SANN:", e$message),
        value = initial_obj_val
      )
    }
  )

  # Process and return results
  optimized_free_colors_oklab <- matrix(result$par, ncol = 3, byrow = TRUE)

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
      status_message = if (result$convergence == 0) {
        "Optimization converged"
      } else if (!is.null(result$message)) {
        result$message
      } else {
        paste("Optimization status:", result$convergence)
      },
      sann_convergence = result$convergence,
      final_objective_value = result$value
    )
  )

  # Add optimization states if requested
  if (return_states && track_states) {
    return_value$optimization_states <- optimization_states
  }

  return(return_value)
}

#' Optimize Color Palette using NLopt DIRECT Algorithm
#'
#' This function takes an initial set of colors and optimizes the positions of
#' the "free" colors to maximize the minimum perceptual distance between any
#' two colors using the DIRECT (Dividing Rectangles) global optimization algorithm.
#' This is a deterministic global optimizer that provides excellent scientific
#' reproducibility, though it may be slower than local optimization methods.
#'
#' @param initial_colors_oklab Matrix of all colors (fixed and initial free) in OK LAB space.
#' @param fixed_mask Logical vector indicating which rows in `initial_colors_oklab` are fixed.
#' @param max_iterations Integer. Maximum iterations for nloptr DIRECT.
#' @param track_states Logical. Whether to track optimization states.
#' @param save_every Integer. Frequency for saving optimization states.
#' @param return_states Logical. Whether to return optimization states.
#' @return A list containing the optimized color matrix `palette` and `details` about the optimization.
#' @noRd
optimize_colors_nlopt_direct <- function(
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
    objective_value <- -objective_min_cvd_safe_dist(temp_all_colors_oklab)

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

  # Setup and run nloptr with DIRECT algorithm
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
    "algorithm" = "NLOPT_GN_DIRECT",
    "maxeval" = max_iterations,
    "print_level" = 0
  )

  result <- tryCatch(
    nloptr::nloptr(
      x0 = initial_free_params,
      eval_f = eval_f,
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
        message = paste("Error in nloptr DIRECT:", e$message),
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

#' Optimize Color Palette using NLopt Nelder-Mead Algorithm
#'
#' This function takes an initial set of colors and optimizes the positions of
#' the "free" colors to maximize the minimum perceptual distance between any
#' two colors using the Nelder-Mead simplex algorithm from NLopt. This is a
#' local optimization method that is derivative-free and robust for non-smooth
#' objective functions, making it a good alternative to the COBYLA algorithm.
#'
#' @param initial_colors_oklab Matrix of all colors (fixed and initial free) in OK LAB space.
#' @param fixed_mask Logical vector indicating which rows in `initial_colors_oklab` are fixed.
#' @param max_iterations Integer. Maximum iterations for nloptr Nelder-Mead.
#' @param track_states Logical. Whether to track optimization states.
#' @param save_every Integer. Frequency for saving optimization states.
#' @param return_states Logical. Whether to return optimization states.
#' @return A list containing the optimized color matrix `palette` and `details` about the optimization.
#' @noRd
optimize_colors_nlopt_neldermead <- function(
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
    objective_value <- -objective_min_cvd_safe_dist(temp_all_colors_oklab)

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

  # Setup and run nloptr with Nelder-Mead algorithm
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
    "algorithm" = "NLOPT_LN_NELDERMEAD",
    "xtol_rel" = 1.0e-5,
    "maxeval" = max_iterations,
    "print_level" = 0
  )

  result <- tryCatch(
    nloptr::nloptr(
      x0 = initial_free_params,
      eval_f = eval_f,
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
        message = paste("Error in nloptr Nelder-Mead:", e$message),
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

#' L-BFGS Optimization Implementation
#'
#' High-performance gradient-based optimization using L-BFGS algorithm
#' paired with smooth differentiable objective functions.
#'
#' @param initial_colors_oklab Initial color matrix in OKLAB space
#' @param fixed_mask Logical vector indicating which colors are fixed
#' @param max_iterations Maximum optimization iterations
#' @param track_states Whether to track optimization states
#' @param save_every Save state every N iterations
#' @param return_states Whether to return optimization states
#' @return List with optimized palette and details
#' @noRd
optimize_colors_lbfgs <- function(
  initial_colors_oklab,
  fixed_mask,
  max_iterations,
  weights = NULL,
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

  # Track iteration count
  eval_f_env <- new.env()
  eval_f_env$iter <- 0
  optimization_states <- list()

  # Define bounds for free colors (OKLAB space)
  lower_bounds <- rep(c(0, -0.4, -0.4), n_free_colors)
  upper_bounds <- rep(c(1, 0.4, 0.4), n_free_colors)

  # Determine which smooth objective to use based on weights
  use_logsumexp <- !is.null(weights) &&
    "smooth_logsumexp" %in% names(weights) &&
    weights["smooth_logsumexp"] > 0

  # Select objective and gradient functions
  objective_func <- if (use_logsumexp) {
    objective_smooth_logsumexp
  } else {
    objective_smooth_repulsion
  }
  gradient_func <- if (use_logsumexp) {
    gradient_smooth_logsumexp
  } else {
    gradient_smooth_repulsion
  }

  # Objective function using selected smooth objective
  eval_f <- function(free_params_vec) {
    eval_f_env$iter <- eval_f_env$iter + 1
    current_free_colors_oklab <- matrix(free_params_vec, ncol = 3, byrow = TRUE)

    # Reconstruct full color matrix
    temp_all_colors_oklab <- initial_colors_oklab
    temp_all_colors_oklab[!fixed_mask, ] <- current_free_colors_oklab

    # Use selected smooth objective
    objective_value <- objective_func(temp_all_colors_oklab)

    # Track states if requested
    if (track_states && eval_f_env$iter %% save_every == 0) {
      current_state <- list(
        iteration = eval_f_env$iter,
        palette = farver::encode_colour(temp_all_colors_oklab, from = "oklab"),
        objective_value = objective_value
      )
      optimization_states[[length(optimization_states) + 1]] <<- current_state
    }

    return(objective_value)
  }

  # Gradient function using selected gradient function
  eval_grad_f <- function(free_params_vec) {
    current_free_colors_oklab <- matrix(free_params_vec, ncol = 3, byrow = TRUE)

    # Reconstruct full color matrix
    temp_all_colors_oklab <- initial_colors_oklab
    temp_all_colors_oklab[!fixed_mask, ] <- current_free_colors_oklab

    # Calculate gradient for all colors using selected gradient function
    full_gradient <- gradient_func(temp_all_colors_oklab)

    # Extract gradient for free colors only
    free_gradient <- full_gradient[!fixed_mask, , drop = FALSE]

    return(as.vector(t(free_gradient)))
  }

  # L-BFGS optimization using nloptr
  tryCatch(
    {
      nloptr_result <- nloptr::nloptr(
        x0 = initial_free_params,
        eval_f = eval_f,
        eval_grad_f = eval_grad_f,
        lb = lower_bounds,
        ub = upper_bounds,
        opts = list(
          "algorithm" = "NLOPT_LD_LBFGS",
          "xtol_rel" = 1.0e-8,
          "maxeval" = max_iterations,
          "print_level" = 0
        )
      )

      # Reconstruct final color matrix
      optimized_free_colors <- matrix(
        nloptr_result$solution,
        ncol = 3,
        byrow = TRUE
      )
      optimized_all_colors_oklab <- initial_colors_oklab
      optimized_all_colors_oklab[!fixed_mask, ] <- optimized_free_colors

      return_value <- list(
        palette = optimized_all_colors_oklab,
        details = list(
          algorithm = "L-BFGS",
          iterations = eval_f_env$iter,
          nloptr_status = nloptr_result$status,
          final_objective_value = nloptr_result$objective,
          status_message = nloptr_result$message
        )
      )
    },
    error = function(e) {
      # Fallback to initial colors on error
      return_value <- list(
        palette = initial_colors_oklab,
        details = list(
          algorithm = "L-BFGS (failed)",
          iterations = eval_f_env$iter,
          nloptr_status = -1,
          final_objective_value = NA_real_,
          status_message = paste("L-BFGS optimization failed:", e$message)
        )
      )
    }
  )

  # Add optimization states if requested
  if (return_states && track_states) {
    return_value$optimization_states <- optimization_states
  }

  return(return_value)
}

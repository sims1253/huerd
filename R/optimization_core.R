#' Clamp values to bounds
#' @param values Numeric vector to clamp
#' @param lower Lower bound
#' @param upper Upper bound
#' @return Clamped values
#' @noRd
.clamp_to_bounds <- function(values, lower, upper) {
  pmax(lower, pmin(upper, values))
}

#' Make list result with proper classes
#' @param palette Palette matrix
#' @param details Details list
#' @return Result list with classes set
#' @noRd
.make_list_result <- function(palette, details) {
  palette <- as.matrix(palette)
  class(palette) <- c("huerd_optimization_palette", "matrix")
  res <- list(palette = palette, details = details)
  class(res) <- c("huerd_optimization_result", "list")
  class(res$details) <- c("huerd_optimization_details", "list")
  res
}

#' Objective: Maximize Minimum Perceptual Distance
#' @noRd
objective_min_perceptual_dist <- function(colors_oklab) {
  if (!is.matrix(colors_oklab) || ncol(colors_oklab) != 3) {
    return(0)
  }
  if (anyNA(colors_oklab)) {
    return(0)
  }
  if (nrow(colors_oklab) < 2) {
    return(Inf)
  }
  dist_matrix <- calculate_perceptual_distances(colors_oklab)

  valid_distances <- dist_matrix[upper.tri(dist_matrix)]
  valid_distances <- valid_distances[is.finite(valid_distances)]

  if (length(valid_distances) == 0) 0 else min(valid_distances)
}

#' Objective: Maximize Minimum CVD-Safe Distance
#' @noRd
objective_min_cvd_safe_dist <- function(colors_oklab) {
  if (is.null(colors_oklab) || !is.matrix(colors_oklab)) {
    stop("colors_oklab must be a matrix")
  }
  if (ncol(colors_oklab) != 3) {
    stop("colors_oklab must have 3 columns (L, a, b)")
  }
  if (anyNA(colors_oklab)) {
    return(0)
  }
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

  if (anyNA(rgb_matrix_01)) {
    return(0)
  }

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
  worst_case_min_dist
}

#' Select the minimax objective based on CVD safety
#' @noRd
.select_palette_objective <- function(cvd_safe) {
  if (isTRUE(cvd_safe)) {
    objective_min_cvd_safe_dist
  } else {
    objective_min_perceptual_dist
  }
}

#' Build an error handler for a solver tryCatch block
#'
#' Falls back to the initial free parameters with status -999 and, unless
#' `eval_f` is NULL, evaluates the objective at the initial parameters to
#' report the initial objective value. The fallback list uses the raw
#' solver's field names (e.g. "par"/"convergence"/"value" for stats::optim)
#' so it flows through result normalization like a solver return value.
#' @param prefix Character prefix for the error message.
#' @param initial_free_params Initial free parameters to fall back to.
#' @param eval_f Objective used to evaluate the initial objective value,
#'   or NULL to skip the evaluation (L-BFGS behavior).
#' @param solution_name,status_name,objective_name Field names for the
#'   fallback list (raw solver names).
#' @return A function suitable as the `error` handler of tryCatch.
#' @noRd
.solver_error_handler <- function(
  prefix,
  initial_free_params,
  eval_f = NULL,
  solution_name = "solution",
  status_name = "status",
  objective_name = "objective"
) {
  function(e) {
    initial_obj_val <- if (is.null(eval_f)) {
      NA_real_
    } else {
      tryCatch(
        eval_f(initial_free_params),
        error = function(e_obj) NA_real_
      )
    }
    fallback <- list()
    fallback[[solution_name]] <- initial_free_params
    fallback[[status_name]] <- -999
    fallback$message <- paste0(prefix, e$message)
    fallback[[objective_name]] <- initial_obj_val
    fallback
  }
}

#' Normalize a raw solver result to the common optimizer shape
#'
#' Maps a raw solver return value (nloptr or optim) to the common
#' `list(solution, status, message, objective)` shape, applying the
#' defensive defaults of the original per-optimizer implementations:
#' non-list results get status -999 and an "invalid result" message,
#' invalid solutions fall back to the initial free parameters, and a
#' missing message defaults to "Optimization completed".
#' @param res Raw solver result (or error fallback list).
#' @param initial_free_params Initial free parameters to fall back to.
#' @param invalid_message Message used when `res` is not a valid list.
#' @param solution_name,status_name,objective_name Field names in `res`
#'   ("solution"/"status"/"objective" for nloptr results,
#'   "par"/"convergence"/"value" for optim results).
#' @return Normalized list with elements `solution`, `status`, `message`,
#'   and `objective`.
#' @noRd
.normalize_optimization_result <- function(
  res,
  initial_free_params,
  invalid_message,
  solution_name = "solution",
  status_name = "status",
  objective_name = "objective"
) {
  # Ensure result is a valid list; otherwise use defaults
  if (is.null(res) || !is.list(res)) {
    res <- list()
    res[[solution_name]] <- initial_free_params
    res[[status_name]] <- -999
    res$message <- invalid_message
    res[[objective_name]] <- NA_real_
  }
  # Ensure solution is valid; otherwise use initial parameters
  solution <- res[[solution_name]]
  if (is.null(solution) || length(solution) != length(initial_free_params)) {
    solution <- initial_free_params
  }
  # Ensure message is present
  message <- res$message
  if (is.null(message)) {
    message <- "Optimization completed"
  }
  list(
    solution = solution,
    status = res[[status_name]],
    message = message,
    objective = res[[objective_name]]
  )
}

#' Run nloptr with the shared error handling and result normalization
#'
#' Invokes `nloptr::nloptr()` with the given options, catching solver
#' errors with `.solver_error_handler()` (message prefixed with
#' `error_prefix`; the initial objective is evaluated unless
#' `evaluate_initial_objective` is FALSE) and normalizing the raw result
#' with `.normalize_optimization_result()`.
#' @param initial_free_params Initial free parameters (x0 and fallback).
#' @param eval_f Objective function passed to nloptr.
#' @param eval_grad_f Gradient function passed to nloptr, or NULL for
#'   derivative-free algorithms.
#' @param lower_bounds,upper_bounds Box constraints passed to nloptr.
#' @param opts Options list passed to nloptr (algorithm, tolerances,
#'   maxeval, print_level).
#' @param error_prefix Character prefix used for both the caught-error
#'   message and the invalid-result message.
#' @param evaluate_initial_objective Logical. Whether the error handler
#'   evaluates the objective at the initial parameters (FALSE for L-BFGS).
#' @return Normalized list with elements `solution`, `status`, `message`,
#'   and `objective`.
#' @noRd
.run_nloptr_solver <- function(
  initial_free_params,
  eval_f,
  eval_grad_f = NULL,
  lower_bounds,
  upper_bounds,
  opts,
  error_prefix,
  evaluate_initial_objective = TRUE
) {
  error_handler <- if (evaluate_initial_objective) {
    .solver_error_handler(error_prefix, initial_free_params, eval_f)
  } else {
    .solver_error_handler(error_prefix, initial_free_params)
  }
  raw <- tryCatch(
    nloptr::nloptr(
      x0 = initial_free_params,
      eval_f = eval_f,
      eval_grad_f = eval_grad_f,
      lb = lower_bounds,
      ub = upper_bounds,
      opts = opts
    ),
    error = error_handler
  )
  .normalize_optimization_result(
    raw,
    initial_free_params,
    invalid_message = paste0(error_prefix, "invalid result")
  )
}

#' Shared driver for the palette optimization algorithms
#'
#' Implements the validation prologue, iteration counting, bounds setup,
#' solver invocation, error normalization, per-column clamping, and result
#' assembly common to all palette optimizers. Method-specific behavior is
#' selected via `method`: the objective function, the solver call, and the
#' naming/ordering of the `details` fields.
#'
#' @param method Character. One of "cobyla", "sann", "direct",
#'   "neldermead", or "lbfgs".
#' @param initial_colors_oklab Matrix of all colors (fixed and initial free)
#'   in OKLAB space.
#' @param fixed_mask Logical vector indicating which rows in
#'   `initial_colors_oklab` are fixed.
#' @param max_iterations Integer. Maximum iterations for the solver.
#' @param cvd_safe Logical. If `TRUE`, maximize the worst-case minimum
#'   distance across CVD simulations; if `FALSE`, maximize the minimum
#'   normal-vision perceptual distance. Ignored for "lbfgs", which always
#'   uses a smooth objective.
#' @param weights Named numeric vector specifying which smooth objective to
#'   use for "lbfgs". If it contains "smooth_logsumexp" with a finite
#'   positive value, the log-sum-exp objective is used; otherwise the
#'   smooth repulsion objective. Ignored by the other methods.
#' @return A list containing optimized color matrix `palette` and
#'   `details` about optimization.
#' @noRd
.run_optimization <- function(
  method,
  initial_colors_oklab,
  fixed_mask,
  max_iterations,
  cvd_safe = TRUE,
  weights = NULL
) {
  # Shared validation prologue (identical across all optimizers)
  if (is.null(initial_colors_oklab)) {
    stop("initial_colors_oklab must be a matrix")
  }
  if (!is.matrix(initial_colors_oklab)) {
    stop("initial_colors_oklab must be a matrix")
  }
  if (ncol(initial_colors_oklab) != 3) {
    stop("initial_colors_oklab must have 3 columns (L, a, b)")
  }
  if (!is.numeric(initial_colors_oklab)) {
    return(.make_list_result(
      matrix(
        numeric(0),
        ncol = 3,
        dimnames = list(NULL, c("L", "a", "b"))
      ),
      list(status_message = "initial_colors_oklab must be numeric")
    ))
  }
  if (is.null(fixed_mask)) {
    return(.make_list_result(
      initial_colors_oklab,
      list(status_message = "fixed_mask cannot be NULL")
    ))
  }
  if (!is.logical(fixed_mask)) {
    return(.make_list_result(
      initial_colors_oklab,
      list(status_message = "fixed_mask must be logical")
    ))
  }
  if (length(fixed_mask) != nrow(initial_colors_oklab)) {
    return(.make_list_result(
      initial_colors_oklab,
      list(
        status_message = "fixed_mask length must match initial_colors_oklab rows"
      )
    ))
  }
  if (anyNA(fixed_mask)) {
    return(.make_list_result(
      initial_colors_oklab,
      list(status_message = "fixed_mask contains NA values")
    ))
  }
  if (is.na(max_iterations) || max_iterations < 0) {
    max_iterations <- 0
  }

  n_free_colors <- sum(!fixed_mask)

  if (n_free_colors == 0) {
    # Method-specific details fields for the "nothing to optimize" case
    details <- switch(
      method,
      sann = list(
        iterations = as.integer(0),
        status_message = "No free colors to optimize",
        sann_convergence = as.double(0),
        final_objective_value = NA_real_
      ),
      lbfgs = list(
        iterations = as.integer(0),
        status_message = "No free colors to optimize",
        nloptr_status = as.double(0),
        final_objective_value = NA_real_,
        algorithm = "L-BFGS"
      ),
      list(
        iterations = as.integer(0),
        status_message = "No free colors to optimize",
        nloptr_status = as.double(0),
        final_objective_value = NA_real_
      )
    )
    return(.make_list_result(as.matrix(initial_colors_oklab), details))
  }

  initial_free_params <- as.vector(t(initial_colors_oklab[
    !fixed_mask,
    ,
    drop = FALSE
  ]))

  # Environment to hold iteration count, accessible by the objective function
  eval_f_env <- new.env(parent = emptyenv())
  eval_f_env$iter <- 0

  # Box constraints for OKLAB space
  # Using 0.001/0.999 to avoid numerical issues at exact boundaries
  lower_bounds <- rep(c(0.001, -0.4, -0.4), n_free_colors)
  upper_bounds <- rep(c(0.999, 0.4, 0.4), n_free_colors)

  # Method-specific objective setup
  if (method == "lbfgs") {
    # Determine which smooth objective to use based on weights
    use_logsumexp <- !is.null(weights) &&
      "smooth_logsumexp" %in% names(weights) &&
      is.finite(weights["smooth_logsumexp"]) &&
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
      current_free_colors_oklab <- matrix(
        free_params_vec,
        ncol = 3,
        byrow = TRUE
      )

      # Reconstruct full color matrix
      temp_all_colors_oklab <- initial_colors_oklab
      temp_all_colors_oklab[!fixed_mask, ] <- current_free_colors_oklab

      objective_func(temp_all_colors_oklab)
    }

    # Gradient function using selected gradient function
    eval_grad_f <- function(free_params_vec) {
      current_free_colors_oklab <- matrix(
        free_params_vec,
        ncol = 3,
        byrow = TRUE
      )

      # Reconstruct full color matrix
      temp_all_colors_oklab <- initial_colors_oklab
      temp_all_colors_oklab[!fixed_mask, ] <- current_free_colors_oklab

      # Calculate gradient for all colors using selected gradient function
      full_gradient <- gradient_func(temp_all_colors_oklab)

      # Extract gradient for free colors only
      free_gradient <- full_gradient[!fixed_mask, , drop = FALSE]

      as.vector(t(free_gradient))
    }
  } else if (method == "sann") {
    # Minimax objective with penalty for constraint violations
    objective_fn <- .select_palette_objective(cvd_safe)
    eval_f <- function(free_params_vec) {
      eval_f_env$iter <- eval_f_env$iter + 1
      current_free_colors_oklab <- matrix(
        free_params_vec,
        ncol = 3,
        byrow = TRUE
      )

      # Minimax objective: maximize the minimum distance under the selected
      # objective (worst case across CVD simulations, or normal vision)
      temp_all_colors_oklab <- initial_colors_oklab
      temp_all_colors_oklab[!fixed_mask, ] <- current_free_colors_oklab
      objective_value <- -objective_fn(temp_all_colors_oklab)

      # Add penalty for constraint violations (box constraints)
      penalty <- sum(
        (pmax(0, lower_bounds - free_params_vec) +
          pmax(0, free_params_vec - upper_bounds))^2
      ) *
        1e6 # Large penalty for constraint violations

      if (is.finite(objective_value)) {
        objective_value + penalty
      } else {
        1e10 + penalty
      }
    }
  } else {
    # Minimax objective (COBYLA, DIRECT, Nelder-Mead)
    objective_fn <- .select_palette_objective(cvd_safe)
    eval_f <- function(free_params_vec) {
      eval_f_env$iter <- eval_f_env$iter + 1
      current_free_colors_oklab <- matrix(
        free_params_vec,
        ncol = 3,
        byrow = TRUE
      )

      # Minimax objective: maximize the minimum distance under the selected
      # objective (worst case across CVD simulations, or normal vision)
      temp_all_colors_oklab <- initial_colors_oklab
      temp_all_colors_oklab[!fixed_mask, ] <- current_free_colors_oklab
      objective_value <- -objective_fn(temp_all_colors_oklab)

      if (is.finite(objective_value)) objective_value else 1e10 # Fallback
    }
  }

  # Method-specific solver invocation and normalization to the common
  # list(solution, status, message, objective) shape
  result <- switch(
    method,
    cobyla = .run_nloptr_solver(
      initial_free_params,
      eval_f,
      lower_bounds = lower_bounds,
      upper_bounds = upper_bounds,
      opts = list(
        "algorithm" = "NLOPT_LN_COBYLA",
        "xtol_rel" = 1.0e-5,
        "maxeval" = max_iterations,
        "print_level" = 0
      ),
      error_prefix = "Error in nloptr: "
    ),
    direct = .run_nloptr_solver(
      initial_free_params,
      eval_f,
      lower_bounds = lower_bounds,
      upper_bounds = upper_bounds,
      opts = list(
        "algorithm" = "NLOPT_GN_DIRECT",
        "maxeval" = max_iterations,
        "print_level" = 0
      ),
      error_prefix = "Error in nloptr DIRECT: "
    ),
    neldermead = .run_nloptr_solver(
      initial_free_params,
      eval_f,
      lower_bounds = lower_bounds,
      upper_bounds = upper_bounds,
      opts = list(
        "algorithm" = "NLOPT_LN_NELDERMEAD",
        "xtol_rel" = 1.0e-5,
        "maxeval" = max_iterations,
        "print_level" = 0
      ),
      error_prefix = "Error in nloptr Nelder-Mead: "
    ),
    lbfgs = .run_nloptr_solver(
      initial_free_params,
      eval_f,
      eval_grad_f,
      lower_bounds = lower_bounds,
      upper_bounds = upper_bounds,
      opts = list(
        "algorithm" = "NLOPT_LD_LBFGS",
        "xtol_rel" = 1.0e-8,
        "maxeval" = max_iterations,
        "print_level" = 0
      ),
      error_prefix = "Error in nloptr L-BFGS: ",
      evaluate_initial_objective = FALSE
    ),
    sann = {
      raw <- tryCatch(
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
        error = .solver_error_handler(
          "Error in optim SANN: ",
          initial_free_params,
          eval_f,
          solution_name = "par",
          status_name = "convergence",
          objective_name = "value"
        )
      )
      .normalize_optimization_result(
        raw,
        initial_free_params,
        invalid_message = "Error in optim SANN: invalid result",
        solution_name = "par",
        status_name = "convergence",
        objective_name = "value"
      )
    },
    stop("Unsupported optimization method: ", method)
  )

  # Shared post-processing: clamp solution columns to the OKLAB bounds
  optimized_free_colors_oklab <- matrix(result$solution, ncol = 3, byrow = TRUE)
  # Final clamp to ensure solution is strictly within bounds
  optimized_free_colors_oklab[, 1] <- .clamp_to_bounds(
    optimized_free_colors_oklab[, 1],
    lower_bounds[1],
    upper_bounds[1]
  )
  optimized_free_colors_oklab[, 2] <- .clamp_to_bounds(
    optimized_free_colors_oklab[, 2],
    lower_bounds[2],
    upper_bounds[2]
  )
  optimized_free_colors_oklab[, 3] <- .clamp_to_bounds(
    optimized_free_colors_oklab[, 3],
    lower_bounds[3],
    upper_bounds[3]
  )

  final_colors_oklab <- initial_colors_oklab
  final_colors_oklab[!fixed_mask, ] <- optimized_free_colors_oklab

  # Shared status/objective values for the details list (defensive
  # NA_real_ fallbacks for missing solver fields)
  status_value <- if (is.null(result$status)) {
    NA_real_
  } else {
    as.double(result$status)
  }
  objective_value <- if (is.null(result$objective)) {
    NA_real_
  } else {
    as.double(result$objective)
  }

  # Method-specific details field naming and ordering
  details <- switch(
    method,
    sann = list(
      iterations = as.integer(eval_f_env$iter),
      status_message = if (result$status == 0) {
        "Optimization converged"
      } else {
        result$message
      },
      sann_convergence = status_value,
      final_objective_value = objective_value
    ),
    lbfgs = list(
      algorithm = "L-BFGS",
      iterations = as.integer(eval_f_env$iter),
      nloptr_status = status_value,
      final_objective_value = objective_value,
      status_message = result$message
    ),
    list(
      iterations = as.integer(eval_f_env$iter),
      status_message = result$message,
      nloptr_status = status_value,
      final_objective_value = objective_value
    )
  )

  .make_list_result(as.matrix(final_colors_oklab), details)
}

#' Optimize Color Palette using Pure Minimax Box-Constrained Optimization
#'
#' This function takes an initial set of colors and optimizes positions of
#' "free" colors to maximize the minimum perceptual distance between any
#' two colors (pure minimax objective).
#'
#' @param initial_colors_oklab Matrix of all colors (fixed and initial free)
#'   in OK LAB space.
#' @param fixed_mask Logical vector indicating which rows in
#'   `initial_colors_oklab` are fixed.
#' @param max_iterations Integer. Maximum iterations for nloptr.
#' @param cvd_safe Logical. If `TRUE`, maximize the worst-case minimum
#'   distance across CVD simulations; if `FALSE`, maximize the minimum
#'   normal-vision perceptual distance.
#' @return A list containing optimized color matrix `palette` and
#'   `details` about optimization.
#' @noRd
optimize_colors_constrained <- function(
  initial_colors_oklab,
  fixed_mask,
  max_iterations,
  cvd_safe = TRUE
) {
  .run_optimization(
    method = "cobyla",
    initial_colors_oklab = initial_colors_oklab,
    fixed_mask = fixed_mask,
    max_iterations = max_iterations,
    cvd_safe = cvd_safe
  )
}

#' Optimize Color Palette using Simulated Annealing
#'
#' This function takes an initial set of colors and optimizes positions of
#' "free" colors to maximize the minimum perceptual distance between any
#' two colors using simulated annealing from stats::optim.
#'
#' @param initial_colors_oklab Matrix of all colors (fixed and initial free)
#'   in OK LAB space.
#' @param fixed_mask Logical vector indicating which rows in
#'   `initial_colors_oklab` are fixed.
#' @param max_iterations Integer. Maximum iterations for simulated annealing.
#' @param cvd_safe Logical. If `TRUE`, maximize the worst-case minimum
#'   distance across CVD simulations; if `FALSE`, maximize the minimum
#'   normal-vision perceptual distance.
#' @return A list containing optimized color matrix `palette` and
#'   `details` about optimization.
#' @noRd
optimize_colors_sann <- function(
  initial_colors_oklab,
  fixed_mask,
  max_iterations,
  cvd_safe = TRUE
) {
  .run_optimization(
    method = "sann",
    initial_colors_oklab = initial_colors_oklab,
    fixed_mask = fixed_mask,
    max_iterations = max_iterations,
    cvd_safe = cvd_safe
  )
}

#' Optimize Color Palette using NLopt DIRECT Algorithm
#'
#' This function takes an initial set of colors and optimizes positions of
#' "free" colors to maximize the minimum perceptual distance between any
#' two colors using DIRECT (Dividing Rectangles) global optimization algorithm.
#'
#' Deprecated: exposed via `optimizer = "nlopt_direct"` in
#' `generate_palette()`, which warns on use. DIRECT's center-lattice
#' sampling cannot reliably find all-distinct color configurations in this
#' parameterization, so it returns degenerate palettes for most palette
#' sizes. Retained only for backwards compatibility.
#'
#' @param initial_colors_oklab Matrix of all colors (fixed and initial free)
#'   in OK LAB space.
#' @param fixed_mask Logical vector indicating which rows in
#'   `initial_colors_oklab` are fixed.
#' @param max_iterations Integer. Maximum iterations for nloptr DIRECT.
#' @param cvd_safe Logical. If `TRUE`, maximize the worst-case minimum
#'   distance across CVD simulations; if `FALSE`, maximize the minimum
#'   normal-vision perceptual distance.
#' @return A list containing optimized color matrix `palette` and
#'   `details` about optimization.
#' @noRd
optimize_colors_nlopt_direct <- function(
  initial_colors_oklab,
  fixed_mask,
  max_iterations,
  cvd_safe = TRUE
) {
  .run_optimization(
    method = "direct",
    initial_colors_oklab = initial_colors_oklab,
    fixed_mask = fixed_mask,
    max_iterations = max_iterations,
    cvd_safe = cvd_safe
  )
}

#' Optimize Color Palette using NLopt Nelder-Mead Algorithm
#'
#' This function takes an initial set of colors and optimizes positions of
#' "free" colors to maximize the minimum perceptual distance between any
#' two colors using Nelder-Mead simplex algorithm from NLopt. This is a
#' local optimization method that is derivative-free and robust for non-smooth
#' objective functions, making it a good alternative to COBYLA algorithm.
#'
#' @param initial_colors_oklab Matrix of all colors (fixed and initial free)
#'   in OK LAB space.
#' @param fixed_mask Logical vector indicating which rows in
#'   `initial_colors_oklab` are fixed.
#' @param max_iterations Integer. Maximum iterations for nloptr Nelder-Mead.
#' @param cvd_safe Logical. If `TRUE`, maximize the worst-case minimum
#'   distance across CVD simulations; if `FALSE`, maximize the minimum
#'   normal-vision perceptual distance.
#' @return A list containing optimized color matrix `palette` and
#'   `details` about optimization.
#' @noRd
optimize_colors_nlopt_neldermead <- function(
  initial_colors_oklab,
  fixed_mask,
  max_iterations,
  cvd_safe = TRUE
) {
  .run_optimization(
    method = "neldermead",
    initial_colors_oklab = initial_colors_oklab,
    fixed_mask = fixed_mask,
    max_iterations = max_iterations,
    cvd_safe = cvd_safe
  )
}

#' L-BFGS Optimization Implementation
#'
#' High-performance gradient-based optimization using L-BFGS algorithm
#' paired with smooth differentiable objective functions.
#'
#' @param initial_colors_oklab Initial color matrix in OKLAB space.
#' @param fixed_mask Logical vector indicating which colors are fixed.
#' @param max_iterations Maximum optimization iterations.
#' @param weights Named numeric vector specifying which smooth objective to use.
#'   If contains "smooth_logsumexp" with positive value, uses log-sum-exp
#'   objective. Otherwise uses smooth repulsion objective. Default is NULL
#'   (uses repulsion).
#' @return List with optimized palette and details
#' @noRd
optimize_colors_lbfgs <- function(
  initial_colors_oklab,
  fixed_mask,
  max_iterations,
  weights = NULL
) {
  .run_optimization(
    method = "lbfgs",
    initial_colors_oklab = initial_colors_oklab,
    fixed_mask = fixed_mask,
    max_iterations = max_iterations,
    weights = weights
  )
}

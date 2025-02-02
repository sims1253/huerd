#' Optimize Color Distribution Using Repulsion Forces
#'
#' This function implements a color repulsion algorithm in the CIELAB color space to
#' generate visually distinct color sets. It uses a physics-inspired approach where
#' colors repel each other while being constrained within the LAB color boundaries.
#' The algorithm is particularly useful for generating color palettes where maximum
#' perceptual difference between colors is desired.
#'
#' @param n_colors Integer. Total number of colors to generate. Must be >= 1.
#' @param max_iterations Integer. Maximum number of iterations for the optimization
#'        process. Default: 5000. Must be >= 1.
#' @param base_colors Character vector. Optional fixed colors in hex format (e.g. "#FF0000")
#'        that will be included in the palette and used as anchor points. Length must be
#'        less than n_colors.
#' @param save_every Integer. Interval for saving intermediate states. Default: 5.
#'        Must be >= 1. Smaller values increase memory usage but provide more detailed
#'        animation data.
#' @param color_force_factor Numeric. Strength of repulsion between colors. Default: 200.
#'        Must be positive. Higher values create more separation between colors.
#' @param boundary_force_factor Numeric. Strength of repulsion from LAB boundaries.
#'        Default: 400. Must be positive.
#' @param center_force_factor Numeric. Strength of repulsion from the LAB center.
#'        Default: 150. Must be positive.
#' @param return_states Logical. If TRUE, returns all intermediate states for animation.
#'        If FALSE, returns only the final state. Default: FALSE.
#' @param convergence_threshold Numeric. Movement threshold for convergence. If NULL,
#'        automatically calculated based on n_colors. Must be positive.
#' @param min_iterations Integer. Minimum number of iterations before convergence
#'        checking begins. Default: 100. Must be >= 1.
#'
#' @return If return_states is FALSE, returns a matrix of final color positions in LAB
#'         space (n_colors x 3). If return_states is TRUE, returns a list of matrices
#'         representing the color positions at each saved state.
#'
#' @export
#'
#' @examples
#' # Generate 10 distinct colors
#' colors <- sqrt_repulsion(n_colors = 10)
#' 
#' # Generate colors with 2 fixed base colors
#' colors <- sqrt_repulsion(n_colors = 10, base_colors = c("#FF0000", "#00FF00"))
#' 
#' # Get animation states for visualization
#' states <- sqrt_repulsion(n_colors = 5, return_states = TRUE, save_every = 10)
sqrt_repulsion <- function(
    n_colors,
    max_iterations = 5000,
    base_colors = NULL,
    save_every = 5,
    color_force_factor = 200,
    boundary_force_factor = 400,
    center_force_factor = 150,
    return_states = FALSE,
    convergence_threshold = NULL,
    min_iterations = 100) {
  # Input validation
  if (n_colors < 1) stop("n_colors must be >= 1")
  if (max_iterations < 1) stop("max_iterations must >= 1")
  if (save_every < 1) stop("save_every must be >= 1")
  if (color_force_factor <= 0) stop("color_force_factor must be positive")
  if (boundary_force_factor <= 0) stop("boundary_force_factor must be positive")
  if (center_force_factor <= 0) stop("middle_force_factor must be positive")
  if (min_iterations < 1) stop("min_iterations must be >= 1")
  if (!is.null(convergence_threshold) && convergence_threshold <= 0) {
    stop("convergence_threshold must be positive")
  }

  if (!is.null(base_colors)) {
    if (!all(grepl("^#[0-9A-Fa-f]{6}$", base_colors))) {
      stop("base_colors must be valid hex colors (e.g. '#FF0000')")
    }
    if (length(base_colors) >= n_colors) {
      stop("Number of base_colors must be less than n_colors")
    }
  }

  # Set default convergence threshold based on number of colors if not provided
  if (is.null(convergence_threshold)) {
    convergence_threshold <- 0.001 / sqrt(n_colors) # Scales with number of colors
  }

  # Initialize color positions and state tracking
  points <- initialize_points(n_colors, base_colors)
  states <- vector(mode = "list", length = floor(max_iterations / save_every))
  states[[1]] <- points
  boundaries <- get_lab_boundaries(base_colors)
  n_base <- if (!is.null(base_colors)) length(base_colors) else 0

  # Initialize convergence tracking
  prev_points <- points
  movement_history <- numeric(5) # Track last 5 movements for stability
  history_index <- 1

  # Main optimization loop
  for (iter in seq_len(max_iterations)) {
    forces <- matrix(0, nrow = n_colors, ncol = 3)

    # Point Repulsion - Vectorized calculation
    distances <- farver::compare_colour(
      points,
      from_space = "lab",
      method = "cie2000"
    )
    distances[lower.tri(distances)] <- t(distances)[lower.tri(distances)]

    # point repulsion forces
    for (i in (n_base + 1):n_colors) {
      diff_matrix <- -sweep(points, 2, points[i, ])
      dist_matrix <- distances[i, ]

      # Calculate force magnitudes directly
      force_magnitudes <- color_force_factor / (dist_matrix^2)
      force_magnitudes[!is.finite(force_magnitudes)] <- 0 # Handle zero distances

      # Calculate direction vectors in a-b plane only
      ab_components <- cbind(0, diff_matrix[, 2:3]) # Zero L component
      ab_norms <- sqrt(rowSums(ab_components^2))
      direction_vectors <- ab_components / ifelse(ab_norms > 0, ab_norms, 1)
      direction_vectors[is.na(direction_vectors)] <- 0

      # Combine magnitude and direction
      forces[i, ] <- colSums(sweep(direction_vectors, 1, force_magnitudes, "*"))
    }

    # boundary and center repulsion forces
    for (i in (n_base + 1):n_colors) {
      point <- points[i, ]
      boundary_force <- c(0, 0, 0)

      # L-axis boundaries - pure repulsion from each wall
      l_min_dist <- point[1] - boundaries$l[1] # Distance to lower L boundary
      l_max_dist <- boundaries$l[2] - point[1] # Distance to upper L boundary

      # Force pointing up from lower boundary (positive direction)
      boundary_force[1] <- boundary_force[1] + boundary_force_factor / (l_min_dist^2)

      # Force pointing down from upper boundary (negative direction)
      boundary_force[1] <- boundary_force[1] - boundary_force_factor / (l_max_dist^2)

      # a-b plane boundaries
      a_min_dist <- point[2] - boundaries$a[1] # Distance to -100
      a_max_dist <- boundaries$a[2] - point[2] # Distance to +100
      b_min_dist <- point[3] - boundaries$b[1] # Distance to -100
      b_max_dist <- boundaries$b[2] - point[3] # Distance to +100

      # Repulsion from a boundaries
      boundary_force[2] <- boundary_force[2] + boundary_force_factor / (a_min_dist^2)
      boundary_force[2] <- boundary_force[2] - boundary_force_factor / (a_max_dist^2)

      # Repulsion from b boundaries
      boundary_force[3] <- boundary_force[3] + boundary_force_factor / (b_min_dist^2)
      boundary_force[3] <- boundary_force[3] - boundary_force_factor / (b_max_dist^2)

      # Repulsion from center
      radius <- sqrt(point[2]^2 + point[3]^2)
      if (!is.na(radius) && radius > 0) {
        # Force gets stronger as points get closer to center
        force_magnitude <- center_force_factor / (radius^2)

        # Push outward from center - positive direction in both a and b
        boundary_force[2] <- boundary_force[2] + force_magnitude * point[2] / radius
        boundary_force[3] <- boundary_force[3] + force_magnitude * point[3] / radius
      }

      # Check for NaN/Inf in final forces
      boundary_force[!is.finite(boundary_force)] <- 0
      forces[i, ] <- forces[i, ] + boundary_force
    }

    # Update points positions - only non-base colors
    if (n_base < n_colors) {
      points[(n_base + 1):n_colors, ] <- points[
        (n_base + 1):n_colors,
      ] + forces[(n_base + 1):n_colors, ]
      points <- enforce_boundaries(points = points, boundaries = boundaries)
    }

    # Calculate convergence metric (root mean square movement)
    if (iter >= min_iterations) {
      movement <- sqrt(mean((points - prev_points)^2))
      movement_history[history_index] <- movement
      history_index <- (history_index %% 5) + 1

      # Check for convergence using moving average
      if (mean(movement_history) < convergence_threshold &&
        max(movement_history) < convergence_threshold * 1.5) {
        # Save final state before breaking
        current_save_index <- floor(iter / save_every) + 1
        if (current_save_index <= length(states)) {
          states[[current_save_index]] <- points
        }
        break
      }
    }

    prev_points <- points

    # Save state if needed
    if (iter %% save_every == 0) {
      states[[iter / save_every + 1]] <- points
    }
  }

  # Always save final state
  if (max_iterations %% save_every != 0) {
    states[[floor(iter / save_every + 2)]] <- points
  }

  if (return_states) {
    return(states[!sapply(states, is.null)])
  }
  return(tail(states[!sapply(states, is.null)], n = 1)[[1]])
}

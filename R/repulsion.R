#' Simulate Color Repulsion in LAB Space
#'
#' Uses a force-directed layout algorithm to distribute colors evenly in LAB
#' color space. The algorithm works by:
#' 1. Initializing colors randomly in LAB space (or using provided base colors)
#' 2. Simulating repulsive forces between colors to maximize perceptual
#'    separation
#' 3. Applying boundary forces to keep colors within valid RGB gamut
#' 4. Iteratively updating color positions based on these forces
#'
#' The force simulation helps achieve:
#' - Maximum perceptual distinctness between colors
#' - Even distribution across color space
#' - Valid RGB colors that can be displayed
#' - Preservation of any provided base colors
#'
#' @param n_colors Number of colors to generate in the palette
#' @param max_iterations Maximum number of iterations for force simulation.
#'                       More iterations allow colors to spread out more but
#'                       increase computation time. Default: 100
#' @param learning_rate Controls magnitude of color movements per iteration.
#'                      Higher values mean larger movements but may cause
#'                      instability. Default: 50
#' @param base_colors Optional vector of fixed base colors in hex format
#'                    (e.g. "#FF0000"). These colors will remain unchanged
#'                    during optimization. Default: NULL
#' @param save_every Save state every n iterations for animation purposes.
#'                   Lower values capture more intermediate states but use more
#'                   memory. Default: 5
#' @param boundary_force Strength of force keeping colors in valid RGB space.
#'                       Higher values enforce RGB boundaries more strictly.
#'                       Default: 0.3
#' @param return_states If TRUE, returns list of states for animation. If FALSE,
#'                      returns only final state. Default: FALSE
#'
#' @return If return_states=FALSE, returns a matrix of LAB coordinates for the
#'         final color palette. If return_states=TRUE, returns a list of
#'         matrices containing LAB coordinates at each saved state.
#'
#' @examples
#' # Generate 5 colors
#' colors <- simulate_color_repulsion(5)
#'
#' # Generate 8 colors with 2 fixed base colors
#' base <- c("#FF0000", "#0000FF")
#' colors <- simulate_color_repulsion(8, base_colors = base)
#'
#' # Get animation states for 6 colors
#' states <- simulate_color_repulsion(6,
#'   max_iterations = 50,
#'   save_every = 2,
#'   return_states = TRUE
#' )
#' @export
simulate_color_repulsion <- function(n_colors,
                                     max_iterations = 100,
                                     learning_rate = 1,
                                     base_colors = NULL,
                                     save_every = 5,
                                     boundary_force = 1,
                                     return_states = FALSE) {
  # Input validation
  if (n_colors < 1) stop("n_colors must be positive")
  if (max_iterations < 1) stop("max_iterations must be positive")
  if (learning_rate <= 0) stop("learning_rate must be positive")
  if (save_every < 1) stop("save_every must be positive")
  if (boundary_force < 0) stop("boundary_force must be non-negative")

  if (!is.null(base_colors)) {
    if (!all(grepl("^#[0-9A-Fa-f]{6}$", base_colors))) {
      stop("base_colors must be valid hex colors (e.g. '#FF0000')")
    }
    if (length(base_colors) >= n_colors) {
      stop("Number of base_colors must be less than n_colors")
    }
  }

  # Initialize color positions and state tracking
  points <- initialize_points(n_colors, base_colors)
  states <- vector(mode = "list", length = floor(max_iterations / save_every))
  states[[1]] <- points
  boundaries <- get_lab_boundaries(base_colors)

  # Main optimization loop
  for (iter in seq_len(max_iterations)) {
    points <- update_points(
      points,
      base_colors,
      boundaries,
      learning_rate,
      boundary_force,
      iteration = iter,
      max_iterations = max_iterations
    )

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
    return(states)
  }
  return(tail(states, n = 1)[[1]])
}

#' Calculate repulsive forces between colors
#'
#' This function computes forces that push colors apart in LAB space. It
#' calculates color-to-color repulsion, boundary forces, and saturation forces
#' for each color point, excluding the fixed base colors.
#'
#' @param points A matrix of LAB coordinates where each row represents a color
#'               and columns represent L, A, and B values respectively.
#' @param n_base An integer specifying the number of fixed base colors that
#'               should not be moved.
#' @param n_colors An integer specifying the total number of colors, including
#'                 both base and non-base colors.
#' @param boundary_force A numeric value representing the strength of the
#'                       boundary forces that keep colors within the valid LAB
#'                       space.
#' @param boundaries A list containing the LAB space boundaries, typically with
#'                   elements 'L', 'a', and 'b', each a vector of length 2
#'                   specifying the min and max values.
#'
#' @return A matrix of force vectors with dimensions n_colors x 3, where each
#'         row represents the force acting on a color in the L, A, and B
#'         dimensions.
#'
#' @keywords internal
calculate_forces <- function(
    points,
    n_base, n_colors, boundary_force, boundaries) {
  forces <- matrix(0, nrow = n_colors, ncol = 3)

  # Get perceptual distances
  distances <- farver::compare_colour(
    points,
    from_space = "oklab",
    method = "cie2000"
  )

  # Calculate 2D distances in a-b plane for saturation control
  ab_distances <- matrix(0, nrow = n_colors, ncol = n_colors)
  for (i in 1:n_colors) {
    for (j in 1:n_colors) {
      ab_distances[i, j] <- sqrt(
        (points[i, 2] - points[j, 2])^2 +
          (points[i, 3] - points[j, 3])^2
      )
    }
  }

  for (i in (n_base + 1):n_colors) {
    # 1. Color-to-color repulsion with adaptive force
    repulsion_force <- numeric(3)
    for (j in 1:n_colors) {
      if (i != j) {
        diff <- points[i, ] - points[j, ]
        perceptual_dist <- distances[i, j]
        ab_dist <- ab_distances[i, j]

        # Normalize direction vector
        direction <- diff / sqrt(sum(diff^2))

        # Combined force with multiple components
        # Short-range strong repulsion
        short_range <- 5 / (1 + (ab_dist / 20)^2)

        # Medium-range component for even spreading
        medium_range <- 2 / (1 + (ab_dist / 40)^1.5)

        # Long-range weak attraction for global structure
        long_range <- -0.5 / (1 + (ab_dist / 80)^1.2)

        # Combine forces with distance-dependent weights
        total_force <- short_range + medium_range + long_range

        # Extra boost for very close colors
        if (perceptual_dist < 15) {
          total_force <- total_force * 2
        }

        repulsion_force <- repulsion_force + direction * total_force
      }
    }

    # 2. Boundary forces (circular in a-b plane)
    radius <- sqrt(sum(points[i, 2:3]^2))
    target_radius <- boundaries$target_radius

    if (radius > 0) {
      # Radial force toward target radius
      radial_force <- boundary_force * (target_radius - radius) / target_radius

      # Convert to a,b components
      forces[i, 2] <- forces[i, 2] + radial_force * points[i, 2] / radius
      forces[i, 3] <- forces[i, 3] + radial_force * points[i, 3] / radius
    }

    # 3. L-axis specific forces
    l_center <- mean(boundaries$l)
    l_range <- diff(boundaries$l)
    l_force <- -boundary_force * (points[i, 1] - l_center) / (l_range / 2)

    # Combine all forces
    forces[i, ] <- c(
      l_force + repulsion_force[1],
      forces[i, 2] + repulsion_force[2],
      forces[i, 3] + repulsion_force[3]
    )
  }

  # Normalize extreme forces
  force_magnitudes <- sqrt(rowSums(forces^2))
  large_forces <- force_magnitudes > 1
  if (any(large_forces)) {
    forces[large_forces, ] <- forces[large_forces, ] /
      force_magnitudes[large_forces]
  }

  return(forces)
}

#' Calculate boundary forces for a color point in LAB space
#'
#' This function computes forces that keep colors within valid LAB space
#' boundaries. It calculates repulsive forces near the boundaries to prevent
#' colors from moving outside the valid range.
#'
#' @param point A numeric vector of length 3 representing a single point in LAB
#'              space, where the elements correspond to L, a, and b values
#'              respectively.
#' @param boundary_force A numeric value representing the strength of the
#'                       boundary force. Higher values result in stronger
#'                       repulsion near boundaries.
#' @param boundaries A list containing the LAB space boundaries, with elements
#'                  'l', 'a', and 'b', each a numeric vector of length 2
#'                   specifying the min and max values. The 'a' boundaries are
#'                   used for both a and b channels.
#'
#' @return A numeric vector of length 3 representing the boundary forces in the
#'         l, a, and b dimensions. Positive values push the point away from the
#'         lower boundary, while negative values push it away from the upper
#'         boundary.
#'
#' @keywords internal
calculate_boundary_forces <- function(point, boundary_force, boundaries) {
  force <- numeric(3)

  # L channel still uses regular boundaries
  l_dist_min <- point[1] - boundaries$l[1]
  l_dist_max <- boundaries$l[2] - point[1]
  l_margin <- 10

  force[1] <- boundary_force * (
    1 / (1 + (l_dist_min / l_margin)^2) -
      1 / (1 + (l_dist_max / l_margin)^2)
  )

  # For a and b channels, use circular force field
  # Calculate distance from center (a=0, b=0)
  radius <- sqrt(point[2]^2 + point[3]^2)

  # Target radius based on base colors or default
  target_radius <- if (!is.null(boundaries$target_radius)) {
    boundaries$target_radius
  } else {
    60 # Default target radius
  }

  if (radius > 0) { # Avoid division by zero
    # Force magnitude based on difference from target radius
    # Positive force pushes outward, negative pulls inward
    radial_force <- boundary_force * (target_radius - radius) / target_radius

    # Convert radial force to a and b components
    force[2] <- radial_force * point[2] / radius
    force[3] <- radial_force * point[3] / radius
  } else {
    # If at center, push in random direction
    angle <- runif(1, 0, 2 * pi)
    force[2] <- boundary_force * cos(angle)
    force[3] <- boundary_force * sin(angle)
  }

  return(force)
}

#' Calculate Saturation Forces for a Color Point in LAB Space
#'
#' This function computes forces that maintain minimum color saturation in LAB
#' color space. It calculates a force vector that pushes under-saturated colors
#' outward, pulls over-saturated colors inward, and applies a small force to
#' maintain position for colors within the desired saturation range.
#'
#' @param point A numeric vector of length 3 representing a single point in LAB
#'              space, where the elements correspond to L, a, and b values
#'              respectively.
#'
#' @return A numeric vector of length 2 representing the saturation forces in
#'         the a and b dimensions of LAB space. Positive values push the color
#'         outward (increasing saturation), while negative values pull it
#'         inward (decreasing saturation).
#'
#' @details The function uses predefined minimum (30) and maximum (80) desired
#'          saturation values. It calculates the distance from the neutral axis
#'          and applies forces based on this distance relative to the desired
#'          saturation range.
#'
#' @keywords internal
calculate_saturation_forces <- function(
    point,
    min_saturation = 30,
    force_magnitude = 0.5) {
  # Calculate distance from center (a=0, b=0)
  dist_from_center <- sqrt(point[2]^2 + point[3]^2)

  # Only apply force if color is under-saturated
  if (dist_from_center < min_saturation) {
    # Apply force to push under-saturated colors outward
    dist_to_min <- min_saturation - dist_from_center
    # Use inverse square law for force decay
    saturation_force <- force_magnitude / (1 + dist_to_min^2)

    if (dist_from_center < 0.1) {
      # For colors very close to center, push in random direction
      # to avoid unstable oscillations
      angle <- runif(1, 0, 2 * pi)
      return(c(
        saturation_force * cos(angle),
        saturation_force * sin(angle)
      ))
    }

    # Push along current a,b direction to increase saturation
    return(c(
      saturation_force * point[2],
      saturation_force * point[3]
    ) / dist_from_center)
  }

  # No force if color is sufficiently saturated
  c(0, 0)
}

#' Update Color Positions in LAB Space
#'
#' This function applies calculated forces to update the positions of colors
#' in LAB space. It adjusts the positions based on the forces acting on each
#' color, while respecting the boundaries of the LAB color space and any fixed
#' base colors.
#'
#' @param points A matrix of LAB coordinates where each row represents a color,
#'               and columns represent the L, a, and b values respectively.
#' @param base_colors An optional vector of hex color codes (e.g., "#FF0000")
#'                    representing fixed base colors that should not be moved.
#'                    Default is NULL.
#' @param boundaries A list containing the LAB space boundaries, with elements
#'                   'l', 'a', and 'b', each a numeric vector of length 2
#'                   specifying the min and max values for the respective
#'                   channels.
#' @param learning_rate A numeric value representing the learning rate for
#'                      updates. It controls the magnitude of position changes
#'                      per iteration.
#' @param boundary_force A numeric value representing the strength of the
#'                       boundary forces that keep colors within the valid LAB
#'                       space.
#' @param iteration The current iteration number.
#' @param max_iterations The total number of iterations.
#' @param target_distance The target distance for color separation.
#' @param distance_scale The scale factor for distance calculations.
#' @param min_lr_factor The minimum learning rate factor.
#' @param min_distance_boost The minimum distance boost.
#'
#' @return A matrix of updated LAB coordinates with the same dimensions as the
#'         input points' matrix. Each row represents a color, and the columns
#'         represent the updated L, a, and b values.
#'
#' @keywords internal
update_points <- function(
    points,
    base_colors,
    boundaries,
    learning_rate,
    boundary_force,
    iteration = 1,
    max_iterations = 200,
    target_distance = 45,
    distance_scale = 10,
    min_lr_factor = 0.1,
    min_distance_boost = 10) {
  n_base <- if (!is.null(base_colors)) length(base_colors) else 0
  n_colors <- nrow(points)
  n_random <- n_colors - n_base

  if (n_random == 0) {
    return(points)
  }

  # Calculate temperature (1.0 to 0.0 over iterations)
  temperature <- max(0, 1 - (iteration / max_iterations)^1.5)

  # Calculate net forces
  forces <- calculate_forces(
    points, n_base, n_colors, boundary_force, boundaries
  )

  # Add random perturbation based on temperature
  if (temperature > 0) {
    random_magnitude <- temperature * 0.5 # Scale random forces with temperature
    random_forces <- matrix(rnorm(n_colors * 3, 0, random_magnitude), ncol = 3)
    forces <- forces + random_forces
  }

  # Normalize forces to prevent extreme movements
  force_magnitudes <- sqrt(rowSums(forces^2))
  large_forces <- force_magnitudes > 1
  if (any(large_forces)) {
    forces[large_forces, ] <- forces[
      large_forces,
    ] / sqrt(force_magnitudes[large_forces])
  }

  # Scale learning rate with temperature and force magnitude
  base_lr <- learning_rate * 0.1
  current_lr <- base_lr * (1 + temperature) # Higher learning rate early on

  # Reduce learning rate when forces are large to prevent overshooting
  force_factor <- 1 / (1 + mean(force_magnitudes))
  current_lr <- current_lr * force_factor

  # Update positions with normalized forces
  points[(n_base + 1):n_colors, ] <- points[(n_base + 1):n_colors, ] +
    forces[(n_base + 1):n_colors, ] * current_lr

  return(points)
}

#' Animate Color Repulsion Process
#'
#' Creates an animated visualization of how colors repel each other in LAB
#' color space to generate a perceptually distinct color palette.
#'
#' @param n_colors Number of colors to generate in the palette
#' @param base_colors Optional vector of base colors in hex format to include
#'                    in the palette. These colors will be fixed points in the
#'                    animation.
#' @param max_iterations Maximum number of iterations for the repulsion
#'                       simulation
#' @param learning_rate Controls how far colors move in each iteration. Higher
#'                      values mean larger movements.
#' @param save_every Save state every n iterations for animation
#' @param boundary_force Strength of the force keeping colors within valid RGB
#'                        bounds. Higher values mean stronger containment.
#' @param filename Name of the output GIF file
#' @param show_force_field Whether to show the force field visualization
#'
#' @return Creates an animated GIF showing the color repulsion process
#' @export
#'
#' @examples
#' # Generate animation for 8 colors
#' animate_repulsion(8)
#'
#' # Generate animation with fixed base colors
#' animate_repulsion(8, base_colors = c("#FF0000", "#00FF00"))
animate_repulsion <- function(n_colors,
                              base_colors = NULL,
                              max_iterations = 100,
                              learning_rate = 1,
                              save_every = 5,
                              boundary_force = 1,
                              filename = "repulsion.gif",
                              show_force_field = FALSE) {
  # Input validation
  if (n_colors <= 0) stop("n_colors must be positive")
  if (learning_rate <= 0) stop("learning_rate must be positive")
  if (save_every <= 0) stop("save_every must be positive")

  if (!is.null(base_colors)) {
    valid_hex <- grepl("^#[0-9A-Fa-f]{6}$", base_colors)
    if (!all(valid_hex)) {
      stop("Invalid color format in base_colors")
    }
  }

  # Simulate the color repulsion process and get states at regular intervals
  states <- simulate_color_repulsion(n_colors,
    max_iterations = max_iterations,
    learning_rate = learning_rate,
    base_colors = base_colors,
    save_every = save_every,
    boundary_force = boundary_force,
    return_states = TRUE
  )

  # Remove NULL entries from states list
  states <- states[!sapply(states, is.null)]

  # Get boundaries for consistent plotting
  boundaries <- get_lab_boundaries(base_colors)
  n_base <- if (!is.null(base_colors)) length(base_colors) else 0

  # Create animated GIF showing the repulsion process over time
  animation::saveGIF(
    {
      # Loop through each saved state and create a frame
      for (i in seq_along(states)) {
        points <- states[[i]]

        # Set up plot margins and create visualization
        par(mar = c(4, 4, 2, 1))

        if (show_force_field) {
          # Calculate and plot force field
          field <- visualize_force_field(
            points, n_base, n_colors,
            boundary_force, boundaries
          )

          # Plot force field with heat map
          image(field$x, field$y,
            field$forces,
            col = hcl.colors(100, "YlOrRd", rev = TRUE),
            xlab = "a", ylab = "b",
            main = sprintf("Iteration %d", (i - 1) * save_every)
          )

          # Add contour lines
          contour(field$x, field$y,
            field$forces,
            add = TRUE,
            col = "gray50",
            drawlabels = FALSE
          )
        } else {
          # Regular plot setup
          plot(NA, NA,
            xlim = c(-100, 100),
            ylim = c(-100, 100),
            xlab = "a", ylab = "b",
            main = sprintf("Iteration %d", (i - 1) * save_every)
          )
        }

        # Convert LAB color coordinates to RGB hex colors for plotting
        hex_colors <- farver::encode_colour(points, from = "lab")

        # Plot the points
        points(points[, 2], points[, 3],
          col = hex_colors, pch = 16, cex = 2
        )

        # If base colors were provided, highlight them with black circles
        if (!is.null(base_colors)) {
          points(points[seq_along(base_colors), 2],
            points[seq_along(base_colors), 3],
            col = "black", pch = 1, cex = 2.5, lwd = 2
          )
        }

        # Calculate and display average perceptual color distance
        if (i > 1) {
          distances <- calculate_color_distances(points)
          mtext(sprintf("Mean Color Distance: %.1f", distances$mean),
            side = 3, line = 0, cex = 0.8
          )
        }

        # Add reference grid
        grid()
      }
    },
    movie.name = filename,
    ani.width = 600,
    ani.height = 600,
    interval = 0.1
  )
}



visualize_force_field <- function(points, n_base, n_colors, boundary_force, boundaries, resolution = 50) {
  x <- seq(-100, 100, length.out = resolution)
  y <- seq(-100, 100, length.out = resolution)
  grid <- expand.grid(x = x, y = y)

  # Calculate repulsion potential field
  potential_field <- matrix(0, nrow = nrow(grid), ncol = 1)

  for (i in 1:nrow(grid)) {
    test_point <- c(60, grid$x[i], grid$y[i])

    # Sum up repulsion potentials from all points
    potential <- 0
    for (j in 1:n_colors) {
      diff <- test_point - points[j, ]
      distance <- sqrt(sum(diff[2:3]^2))

      if (distance > 0) {
        # Higher potential means more repulsion
        potential <- potential + 5 / (1 + (distance / 40)^1.5)
      }
    }

    # Add circular boundary potential
    radius <- sqrt(sum(test_point[2:3]^2))
    if (radius > 0) {
      target_radius <- boundaries$target_radius
      radius_diff <- abs(radius - target_radius)
      boundary_potential <- boundary_force * (radius_diff / target_radius)^2
      potential <- potential + boundary_potential
    }

    potential_field[i] <- potential
  }

  # Convert to matrix and normalize
  potential_matrix <- matrix(potential_field, resolution, resolution)
  potential_matrix <- potential_matrix / max(potential_matrix)

  return(list(
    x = x,
    y = y,
    forces = potential_matrix # renamed but keeping 'forces' for compatibility
  ))
}

plot_state <- function(points, base_colors = NULL, show_force_field = FALSE, ...) {
  if (show_force_field) {
    n_base <- if (!is.null(base_colors)) length(base_colors) else 0
    n_colors <- nrow(points)

    # Calculate force field
    field <- visualize_force_field(points, n_base, n_colors, ...)

    # Plot force field
    image(field$x, field$y,
      field$forces,
      col = hcl.colors(100, "YlOrRd", rev = TRUE),
      add = FALSE,
      xlab = "a",
      ylab = "b"
    )

    # Add contour lines
    contour(field$x, field$y,
      field$forces,
      add = TRUE,
      col = "gray50",
      drawlabels = FALSE
    )
  } else {
    # Original plotting code
    plot(NA, NA,
      xlim = c(-100, 100),
      ylim = c(-100, 100),
      xlab = "a",
      ylab = "b"
    )
  }

  # Plot points on top
  points(points[, 2], points[, 3],
    pch = 19,
    col = farver::encode_colour(points, from_space = "lab")
  )

  if (!is.null(base_colors)) {
    n_base <- length(base_colors)
    points(points[1:n_base, 2], points[1:n_base, 3],
      pch = 1,
      cex = 2
    )
  }

  grid()
}

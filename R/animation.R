#' Animate Color Repulsion Process
#'
#' Creates an animated visualization of how colors repel each other in LAB color
#' space to generate a perceptually distinct color palette.
#'
#' @param n_colors Number of colors to generate in the palette
#' @param base_colors Optional vector of base colors in hex format
#'                    (e.g. "#FF0000") that will remain fixed during
#'                    animation
#' @param max_iterations Maximum number of iterations for the repulsion
#'                       simulation
#' @param learning_rate Controls magnitude of color movements per iteration
#' @param save_every Save state every n iterations for animation frames
#' @param boundary_force Strength of containment force keeping colors within
#'                       valid RGB bounds
#' @param filename Output GIF filename
#' @param show_force_field Whether to visualize the repulsion force field
#'
#' @return Creates an animated GIF showing the color optimization process
#' @export
#'
#' @examples
#' # Generate animation for 8 colors
#' animate_repulsion(8)
#'
#' # Generate animation with fixed base colors
#' animate_repulsion(8, base_colors = c("#E69F00", "#0072B2"))
animate_repulsion <- function(n_colors,
                              base_colors = NULL,
                              max_iterations = 500,
                              save_every = 5,
                              color_force_factor = 200,
                              boundary_force_factor = 200,
                              middle_force_factor = 200,
                              filename = "repulsion.gif",
                              show_force_field = FALSE,
                              convergence_threshold = NULL,
                              min_iterations= 100) {
  # Input validation
  if (n_colors <= 0) stop("n_colors must be positive")
  if (color_force_factor <= 0) stop("color_force_factor must be positive")
  if (boundary_force_factor <= 0) stop("boundary_force_factor must be positive")
  if (middle_force_factor <= 0) stop("middle_force_factor must be positive")
  if (save_every <= 0) stop("save_every must be positive")

  if (!is.null(base_colors)) {
    valid_hex <- grepl("^#[0-9A-Fa-f]{6}$", base_colors)
    if (!all(valid_hex)) {
      stop("Invalid color format in base_colors")
    }
  }

  states <- sqrt_repulsion(n_colors,
    max_iterations,
    base_colors,
    save_every,
    color_force_factor,
    boundary_force_factor,
    middle_force_factor,
    return_states = TRUE
  )

  # Remove NULL entries
  states <- states[!sapply(states, is.null)]

  # Get boundaries for consistent plotting
  boundaries <- get_lab_boundaries(base_colors)
  n_base <- if (!is.null(base_colors)) length(base_colors) else 0

  # Get reference distances once before starting animation
  ref_distances <- get_reference_distances(n_colors)

    # Function to draw a color swatch with hex code
    draw_swatch <- function(x, y, width, height, color, hex_code) {
      rect(x, y, x + width, y + height, col = color, border = "gray30")
      # Calculate text color based on background luminance
      rgb_col <- col2rgb(color)
      text_col <- ifelse(mean(rgb_col) < 128, "white", "black")
      text(x + width/2, y + height/2, hex_code, col = text_col, 
           cex = 2, family = "mono")
    }

  # Create animated GIF with enhanced visualization
  animation::saveGIF(
    {
      # Set up a 3x2 layout with LAB plot and palette on top
      layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2, byrow = TRUE),
        heights = c(2, 1, 1)
      )

      # Set default margins for all plots
      par(mar = c(4, 4, 3, 1))

      for (i in seq_along(states)) {
        points <- states[[i]]
        distances <- calculate_all_distances(points)

        # Set consistent breaks and limits for all histograms
        all_distances <- c(
          distances$original, distances$deutan,
          distances$protan, distances$tritan
        )
        breaks <- seq(min(all_distances), max(all_distances),
          length.out = 20
        )

        # 1. Main color space plot with square aspect ratio
        par(pty = "s") # Set square plotting region
        if (show_force_field) {
          field <- visualize_force_field(
            points, n_base, n_colors,
            boundary_force_factor, boundaries
          )
          image(field$x, field$y,
            field$forces,
            col = hcl.colors(100, "YlOrRd", rev = TRUE),
            xlab = "a", ylab = "b",
            main = sprintf("Iteration %d", (i - 1) * save_every)
          )
          contour(field$x, field$y,
            field$forces,
            add = TRUE,
            col = "gray50",
            drawlabels = FALSE
          )
        } else {
          plot(NA, NA,
            xlim = c(-100, 100),
            ylim = c(-100, 100),
            xlab = "a", ylab = "b",
            main = sprintf("Iteration %d", (i - 1) * save_every)
          )
          grid()
        }

        # Plot colors
        hex_colors <- farver::encode_colour(points, from = "lab")
        points(points[, 2], points[, 3],
          col = hex_colors, pch = 16, cex = 2
        )

        if (!is.null(base_colors)) {
          points(points[seq_along(base_colors), 2],
            points[seq_along(base_colors), 3],
            col = "black", pch = 1, cex = 2.5, lwd = 2
          )
        }

        # Reset to non-square for histograms
        par(pty = "m")

        # 2. Color palette visualization
        par(mar = c(2, 1, 3, 1))
        plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1),
             type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
             main = "Color Palette")
        
        # Calculate swatch dimensions
        n_total <- length(hex_colors)
        swatch_height <- min(0.15, 0.9/n_total)
        y_positions <- seq(0.95 - swatch_height, 0.05, length.out = n_total)
        
        # Draw all swatches with hex codes
        for (j in seq_along(hex_colors)) {
          draw_swatch(0.1, y_positions[j], 0.8, swatch_height,
                     hex_colors[j], toupper(hex_colors[j]))
        }

        # 3. Original distances histogram
        hist(distances$original,
          main = "Original Color Distances",
          xlab = "Distance",
          col = "lightblue",
          breaks = breaks,
          ylim = c(0, 10),
          xlim = c(0, 100)
        )
        abline(
          v = mean_bottom_quartile(distances$original), col = "red", lwd = 3
        )
        # Add reference lines
        abline(
          v = ref_distances$batlow$original, col = "black", lwd = 2, lty = 3
        )
        abline(
          v = ref_distances$tableau$original, col = "black", lwd = 2, lty = 5
        )
        abline(
          v = ref_distances$brewer$original, col = "black", lwd = 2, lty = 4
        )
        legend("topright",
          legend = c(
            "Current meanQ25", "batlow meanQ25", "Set2 meanQ25", "Hue meanQ25"
          ),
          col = c("red", "black", "black", "black"),
          lwd = 2, lty = c(1, 3, 4, 5)
        )

        # 4. Deuteranopia distances histogram
        hist(distances$deutan,
          main = "Deuteranopia Distances",
          xlab = "Distance",
          col = "lightgreen",
          breaks = breaks,
          ylim = c(0, 10),
          xlim = c(0, 100)
        )
        abline(
          v = mean_bottom_quartile(distances$deutan), col = "red", lwd = 3
        )
        abline(
          v = ref_distances$batlow$deutan, col = "black", lwd = 2, lty = 3
        )
        abline(
          v = ref_distances$tableau$deutan, col = "black", lwd = 2, lty = 5
        )
        abline(
          v = ref_distances$brewer$deutan, col = "black", lwd = 2, lty = 4
        )

        # 5. Protanopia distances histogram
        hist(distances$protan,
          main = "Protanopia Distances",
          xlab = "Distance",
          col = "salmon",
          breaks = breaks,
          ylim = c(0, 10),
          xlim = c(0, 100)
        )
        abline(
          v = mean_bottom_quartile(distances$protan), col = "red", lwd = 3
        )
        abline(
          v = ref_distances$batlow$protan, col = "black", lwd = 2, lty = 3
        )
        abline(
          v = ref_distances$tableau$protan, col = "black", lwd = 2, lty = 5
        )
        abline(
          v = ref_distances$brewer$protan, col = "black", lwd = 2, lty = 4
        )

        # 6. Tritanopia distances histogram
        hist(distances$tritan,
          main = "Tritanopia Distances",
          xlab = "Distance",
          col = "lightblue",
          breaks = breaks,
          ylim = c(0, 10),
          xlim = c(0, 100)
        )
        abline(
          v = mean_bottom_quartile(distances$tritan), col = "red", lwd = 3
        )
        abline(
          v = ref_distances$batlow$tritan, col = "black", lwd = 2, lty = 3
        )
        abline(
          v = ref_distances$tableau$tritan, col = "black", lwd = 2, lty = 5
        )
        abline(
          v = ref_distances$brewer$tritan, col = "black", lwd = 2, lty = 4
        )
      }
    },
    movie.name = filename,
    ani.width = 800,
    ani.height = 800,
    interval = 0.1
  )
}

#' Visualize Force Field
#'
#' This function visualizes a force field based on given points,
#'  boundaries, and other parameters.
#'
#' @param points A matrix of points where each row represents a point in the
#'               format (id, x, y).
#' @param n_base An integer representing the number of base points (not used in
#'               the function).
#' @param n_colors An integer representing the number of colors (points) to
#'                 consider for repulsion.
#' @param boundary_force A numeric value representing the force applied by the
#'                       boundary.
#' @param boundaries A list containing boundary parameters, specifically
#'                  `target_radius`.
#' @param resolution An integer representing the resolution of the grid.
#'                   Default is 50.
#'
#' @return A list containing:
#' \describe{
#'   \item{x}{A sequence of x-coordinates for the grid.}
#'   \item{y}{A sequence of y-coordinates for the grid.}
#'   \item{forces}{A matrix representing the normalized potential field.}
#' }
#'
#' @examples
#' points <- matrix(c(1, 0, 0, 2, 50, 50), ncol = 3, byrow = TRUE)
#' boundaries <- list(target_radius = 100)
#' result <- visualize_force_field(
#'   points,
#'   n_base = 2,
#'   n_colors = 2,
#'   boundary_force_factor = 10,
#'   boundaries = boundaries
#' )
#' image(result$x, result$y, result$forces)
#'
#' @export
visualize_force_field <- function(
    points, n_base, n_colors, boundary_force_factor, boundaries, resolution = 50) {
  x <- seq(-100, 100, length.out = resolution)
  y <- seq(-100, 100, length.out = resolution)
  grid <- expand.grid(x = x, y = y)

  # Calculate repulsion potential field
  potential_field <- matrix(0, nrow = nrow(grid), ncol = 1)

  for (i in seq_len(nrow(grid))) {
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
      boundary_potential <- boundary_force_factor * (radius_diff / target_radius)^2
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

#' Plot State Function
#'
#' This function plots the state of a given set of points. It can optionally
#' display a force field visualization.
#'
#' @param points A matrix or data frame containing the points to be plotted.
#'   The first column should contain the point identifiers, the second column
#'   should contain the x-coordinates, and the third column should contain the
#'   y-coordinates.
#' @param base_colors A vector of base colors. If provided, the first `n_base`
#'   points will be plotted with these colors.
#' @param show_force_field A logical value indicating whether to display the
#'   force field visualization. Default is FALSE.
#' @param ... Additional arguments passed to the `visualize_force_field`
#'            function.
#'
#' @details
#' If `show_force_field` is TRUE, the function calculates and plots a force
#' field using the `visualize_force_field` function. The force field is
#' displayed using a color gradient and contour lines. If `show_force_field`
#' is FALSE, the function plots an empty plot with specified x and y limits.
#'
#' The points are plotted on top of the force field or empty plot. If
#' `base_colors` is provided, the first `n_base` points are plotted with these
#' colors and larger point characters.
#'
#' @importFrom farver encode_colour
#' @importFrom graphics image contour plot points grid
#' @importFrom grDevices hcl.colors
#'
#' @examples
#' \dontrun{
#' points <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3)
#' base_colors <- c("red", "blue")
#' plot_state(points, base_colors, show_force_field = TRUE)
#' }
#'
#' @export
plot_state <- function(
    points, base_colors = NULL, show_force_field = FALSE, ...) {
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

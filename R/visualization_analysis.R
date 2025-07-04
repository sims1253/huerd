#' Comprehensive Palette Analysis Dashboard
#'
#' Creates a scicomap-inspired comprehensive diagnostic dashboard for color palettes.
#' This function generates six visualization panels to help assess palette quality,
#' including perceptual distance analysis, CVD simulation, OKLAB space distribution,
#' and performance comparison against established scientific palettes.
#'
#' @param colors A character vector of hex colors or a matrix of colors in OKLAB space.
#' @param main_title Character. Main title for the dashboard. Default: "Palette Analysis Dashboard"
#' @param new_device Logical. Whether to create a new graphics device. Default: FALSE.
#'   When TRUE, creates a new device with optimal size for the dashboard.
#'   When FALSE, uses the current device with adaptive margins.
#' @param device_width Numeric. Width of new device in inches. Default: 12.
#' @param device_height Numeric. Height of new device in inches. Default: 9.
#' @return Invisibly returns the evaluation result from evaluate_palette.
#' @importFrom graphics abline axis barplot boxplot grid image mtext par points rect text lines
#' @importFrom grDevices hcl.colors adjustcolor
#' @export
#' @examples
#' colors <- c("#ff0000", "#00ff00", "#0000ff")
#' plot_palette_analysis(colors)
#' 
#' # For file output, you might want a new device:
#' \dontrun{
#' png("my_palette.png", width = 12, height = 9, units = "in", res = 300)
#' plot_palette_analysis(colors)
#' dev.off()
#' }
plot_palette_analysis <- function(
  colors,
  main_title = "Palette Analysis Dashboard",
  new_device = FALSE,
  device_width = 12,
  device_height = 9
) {
  # Handle device creation if requested
  if (new_device) {
    dev.new(width = device_width, height = device_height, noRStudioGD = TRUE)
    on.exit(dev.off())
  }

  evaluation <- evaluate_palette(colors)
  hex_colors <- if (is.character(colors)) {
    colors
  } else {
    farver::encode_colour(colors, from = "oklab")
  }

  layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE))
  
  # Calculate safe margins based on current device size
  safe_margins <- calculate_safe_margins(
    desired_mar = c(6, 5, 5, 5),
    desired_oma = c(2, 1, 5, 1),
    mfrow = c(2, 3)
  )
  
  par(
    oma = safe_margins$oma,
    mar = safe_margins$mar
  )

  plot_color_swatches(hex_colors, evaluation)
  plot_distance_heatmap(hex_colors, evaluation)
  plot_nearest_neighbor_distances(hex_colors, evaluation)
  plot_color_space_distribution(hex_colors)
  plot_cvd_simulation(hex_colors)
  plot_comparative_palettes(hex_colors)

  # Add main title to the outer margin
  mtext(main_title, outer = TRUE, cex = 2.0, font = 2, line = 2)

  invisible(evaluation)
}

#' Calculate Safe Margins for Current Device
#'
#' Calculates margins that will fit within the current graphics device,
#' automatically scaling down if the desired margins would cause 
#' "figure margins too large" errors.
#'
#' @param desired_mar Numeric vector of length 4. Desired margins in lines
#'   (bottom, left, top, right).
#' @param desired_oma Numeric vector of length 4. Desired outer margins in lines
#'   (bottom, left, top, right).
#' @param mfrow Numeric vector of length 2. Number of subplot rows and columns.
#' @return List with components 'mar' and 'oma' containing safe margin values.
#' @noRd
calculate_safe_margins <- function(desired_mar, desired_oma, mfrow) {
  # Get current device dimensions
  pin <- par("pin")  # Plot region size in inches
  cin <- par("cin")  # Character size in inches
  
  if (length(pin) != 2 || any(is.na(pin)) || any(pin <= 0)) {
    # Fallback to conservative margins if device info unavailable
    return(list(
      mar = pmin(desired_mar, c(4, 4, 3, 2)),
      oma = pmin(desired_oma, c(1, 1, 3, 1))
    ))
  }
  
  # Calculate available space per subplot
  n_rows <- mfrow[1]
  n_cols <- mfrow[2]
  
  # Available space for each subplot (in inches)
  available_width <- pin[1] / n_cols
  available_height <- pin[2] / n_rows
  
  # Convert desired margins from lines to inches
  desired_mar_inches <- desired_mar * cin[2]  # cin[2] is line height
  desired_oma_inches <- desired_oma * cin[2]
  
  # Calculate required space for margins
  required_width <- desired_mar_inches[2] + desired_mar_inches[4] + 
                   desired_oma_inches[2] + desired_oma_inches[4]
  required_height <- desired_mar_inches[1] + desired_mar_inches[3] + 
                    desired_oma_inches[1] + desired_oma_inches[3]
  
  # Calculate scaling factors (if < 1, we need to scale down)
  width_scale <- max(0.1, (available_width - 1) / required_width)  # Leave 1 inch for plot
  height_scale <- max(0.1, (available_height - 1) / required_height)  # Leave 1 inch for plot
  
  # Use the most restrictive scaling factor
  scale_factor <- min(1, width_scale, height_scale)
  
  # Apply scaling to margins
  safe_mar <- pmax(0.5, desired_mar * scale_factor)  # Minimum 0.5 lines
  safe_oma <- pmax(0.1, desired_oma * scale_factor)  # Minimum 0.1 lines
  
  return(list(
    mar = safe_mar,
    oma = safe_oma
  ))
}

#' Get Optimal Text Color for Background Contrast
#'
#' Determines whether black or white text provides better contrast against
#' a given background color using perceptual lightness from OKLAB space.
#'
#' @param hex_color Character. A single hex color code (e.g., "#FF0000")
#' @return Character. Either "black" or "white" for optimal contrast
#' @noRd
get_contrast_text_color <- function(hex_color) {
  # Convert hex to OKLAB to get perceptual lightness
  oklab_color <- farver::convert_colour(
    farver::decode_colour(hex_color),
    from = "rgb",
    to = "oklab"
  )

  # Use OKLAB lightness (L component) for perceptual accuracy
  lightness <- oklab_color[1, 1] # L component is first column

  # Use threshold of 0.5: if L < 0.5 use white text, if L >= 0.5 use black text
  if (lightness < 0.5) {
    return("white")
  } else {
    return("black")
  }
}

#' Plot Color Swatches with Key Metrics and Grayscale
#' @noRd
plot_color_swatches <- function(hex_colors, evaluation) {
  n <- length(hex_colors)

  # Create color swatches plot with redesigned layout
  plot(
    0,
    0,
    type = "n",
    xlim = c(0, max(1, n)),
    ylim = c(0, 4.5), # Increased for better spacing
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = paste("Color Palette (", n, " colors)", sep = ""), # Updated title
    cex.main = 1.4
  )

  if (n == 0) {
    text(0.5, 2.5, "No colors to display", cex = 1.2, col = "gray50")
    return()
  }

  # Draw larger color swatches with hex codes inside and numbers below
  for (i in 1:n) {
    # Draw larger swatch (height increased from 1.5 to 2.0)
    rect(i - 1, 1.5, i, 3.5, col = hex_colors[i], border = "black", lwd = 1.5)

    # Determine optimal text color for this background
    text_color <- get_contrast_text_color(hex_colors[i])

    # Add hex code inside the swatch (rotated text to prevent bleeding)
    text(
      i - 0.5,
      2.5,
      hex_colors[i],
      cex = 0.6,
      font = 2,
      col = text_color,
      srt = 90
    )

    # Add color index number below the swatch
    text(i - 0.5, 1.2, i, cex = 0.8, font = 2, col = "black")
  }

  # Add key metrics text if we have enough colors
  if (n >= 2) {
    min_dist <- evaluation$distances$min %||% NA_real_
    perf_ratio <- (evaluation$distances$performance_ratio %||% 0) * 100
    cvd_min <- evaluation$cvd_safety$worst_case_min_distance %||% NA_real_

    metrics_text <- sprintf(
      "Min Dist: %.3f | Performance: %.1f%% | CVD Safety: %.3f",
      min_dist,
      perf_ratio,
      cvd_min
    )
    text(n / 2, 0.8, metrics_text, cex = 1.0, font = 1) # Positioned below the numbers
  } else if (n == 1) {
    text(
      0.5,
      0.8,
      "Single color - no distance metrics",
      cex = 1.0,
      col = "gray50"
    )
  }
}

#' Plot Pairwise Distance Heatmap
#' @noRd
plot_distance_heatmap <- function(hex_colors, evaluation) {
  n <- length(hex_colors)
  if (n < 2) {
    plot.new()
    text(0.5, 0.5, "Insufficient colors")
    return()
  }

  dist_matrix <- calculate_perceptual_distances(
    farver::convert_colour(
      farver::decode_colour(hex_colors),
      from = "rgb",
      to = "oklab"
    )
  )

  image(
    1:n,
    1:n,
    dist_matrix,
    col = hcl.colors(100, "Viridis"),
    main = "Pairwise Distance Matrix\n(OKLAB Units)",
    xlab = "Color Index",
    ylab = "Color Index",
    axes = FALSE,
    cex.main = 1.4,
    cex.lab = 1.2,
    asp = 1
  )
  axis(1, at = 1:n, labels = 1:n, cex.axis = 1.0)
  axis(2, at = 1:n, labels = 1:n, cex.axis = 1.0)

  if (n <= 8) {
    text_size <- pmax(0.6, 1.1 - n * 0.05)
    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          text(i, j, sprintf("%.2f", dist_matrix[i, j]), cex = text_size)
        }
      }
    }
  }

  # --- Safe Legend Drawing (with cleanup) ---
  old_xpd <- par("xpd")
  on.exit(par(xpd = old_xpd))
  par(xpd = NA)

  dist_range <- range(dist_matrix[dist_matrix > 0], na.rm = TRUE)
  if (any(is.finite(dist_range))) {
    usr <- par("usr")
    # ** ADJUSTMENT: Slightly less aggressive positioning **
    legend_x1 <- usr[2] + 0.5
    legend_x2 <- legend_x1 + 0.5

    legend_colors <- hcl.colors(100, "Viridis")
    for (i in 1:100) {
      y1 <- usr[3] + (usr[4] - usr[3]) * (i - 1) / 100
      y2 <- usr[3] + (usr[4] - usr[3]) * i / 100
      rect(legend_x1, y1, legend_x2, y2, col = legend_colors[i], border = NA)
    }
    rect(legend_x1, usr[3], legend_x2, usr[4], border = "black")

    tick_values <- pretty(dist_range, n = 5)
    tick_pos <- usr[3] +
      (usr[4] - usr[3]) * (tick_values - dist_range[1]) / diff(dist_range)

    for (i in seq_along(tick_values)) {
      lines(c(legend_x2, legend_x2 + 0.1), c(tick_pos[i], tick_pos[i]))
      text(
        legend_x2 + 0.15,
        tick_pos[i],
        sprintf("%.2f", tick_values[i]),
        pos = 4,
        cex = 0.9
      )
    }

    text(mean(c(legend_x1, legend_x2)), usr[4], "Distance", pos = 3, font = 2)
  }
}

#' Plot Nearest Neighbor Distances
#' @noRd
plot_nearest_neighbor_distances <- function(hex_colors, evaluation) {
  n <- length(hex_colors)

  if (n < 2) {
    plot(
      0,
      0,
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = "Nearest Neighbor Distances\n(Need >=2 colors)"
    )
    text(0, 0, "Insufficient colors", cex = 1.2, col = "gray50")
    return()
  }

  # Calculate distance matrix
  oklab_colors <- farver::convert_colour(
    farver::decode_colour(hex_colors),
    from = "rgb",
    to = "oklab"
  )
  dist_matrix <- calculate_perceptual_distances(oklab_colors)

  # Find nearest neighbor distance for each color
  nn_distances <- numeric(n)
  for (i in 1:n) {
    other_distances <- dist_matrix[i, -i]
    nn_distances[i] <- min(other_distances)
  }

  # Create bar plot with increased text sizes
  barplot(
    nn_distances,
    names.arg = 1:n,
    col = hex_colors,
    border = "black",
    main = "Nearest Neighbor Distances",
    xlab = "Color Index",
    ylab = "Distance (OKLAB)",
    ylim = c(0, max(nn_distances) * 1.1),
    cex.main = 1.4,
    cex.lab = 1.2,
    cex.axis = 1.0,
    cex.names = 1.0
  )

  # No redundant min line or text - this information is available in bottom right panel
}

#' Plot Color Space Distribution
#' @noRd
plot_color_space_distribution <- function(hex_colors) {
  n <- length(hex_colors)

  if (n == 0) {
    plot(
      0,
      0,
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = "Color Space Distribution\n(No colors)"
    )
    text(0, 0, "No colors to display", cex = 1.2, col = "gray50")
    return()
  }

  # Convert to OKLAB
  oklab_colors <- farver::convert_colour(
    farver::decode_colour(hex_colors),
    from = "rgb",
    to = "oklab"
  )

  L <- oklab_colors[, 1]
  a_star <- oklab_colors[, 2]
  b_star <- oklab_colors[, 3]

  # Calculate point sizes based on lightness (L dimension)
  # Map L values to circle sizes: lighter colors = larger circles
  if (n > 1) {
    L_normalized <- (L - min(L)) / (max(L) - min(L))
  } else {
    L_normalized <- 0.5 # Default for single color
  }
  point_sizes <- 1.5 + 2 * L_normalized # Range from 1.5 to 3.5

  plot(
    a_star,
    b_star,
    col = hex_colors,
    pch = 19,
    cex = point_sizes, # Use variable sizes to represent lightness
    main = "OKLAB Color Space\n(a* vs b* projection, size = lightness)",
    xlab = "a* (green <- -> red)",
    ylab = "b* (blue <- -> yellow)",
    xlim = range(c(-0.4, 0.4, a_star)) * 1.1,
    ylim = range(c(-0.4, 0.4, b_star)) * 1.1,
    cex.main = 1.4,
    cex.lab = 1.2,
    cex.axis = 1.0
  )

  abline(h = 0, v = 0, col = "gray60", lty = 2)
  grid(col = "lightgray", lty = 1)

  # Removed overlapping color index numbers for better readability

  # Simplified text with better positioning
  stats_text <- sprintf("Colors distributed across OKLAB space (n=%d)", n)
  mtext(stats_text, side = 1, line = 4.5, cex = 1.0) # Increased line spacing to prevent cutoff
}

#' Plot CVD Simulation
#' @noRd
plot_cvd_simulation <- function(hex_colors) {
  n <- length(hex_colors)

  if (n == 0) {
    plot(
      0,
      0,
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = "CVD Simulation\n(No colors)"
    )
    text(0, 0, "No colors to display", cex = 1.2, col = "gray50")
    return()
  }

  protan_colors <- colorspace::protan(hex_colors, severity = 1.0)
  deutan_colors <- colorspace::deutan(hex_colors, severity = 1.0)
  tritan_colors <- colorspace::tritan(hex_colors, severity = 1.0)
  gray_colors <- colorspace::desaturate(hex_colors)

  plot(
    0,
    0,
    type = "n",
    xlim = c(0, n),
    ylim = c(0, 5),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = "CVD Simulation & Grayscale\n(Severity = 1.0)",
    cex.main = 1.4
  )

  y_coords <- 0:4
  colors_list <- list(
    gray_colors,
    tritan_colors,
    deutan_colors,
    protan_colors,
    hex_colors
  )
  labels <- c("Gray", "Trit", "Deut", "Prot", "Orig")

  for (i in 1:n) {
    for (j in 1:5) {
      rect(
        i - 1,
        y_coords[j],
        i,
        y_coords[j] + 1,
        col = colors_list[[j]][i],
        border = "black"
      )
    }
  }

  mtext(labels, side = 2, at = y_coords + 0.5, las = 1, cex = 1.0, line = 0.5)
}

#' Plot Distance Distribution Comparison
#' @noRd
plot_comparative_palettes <- function(hex_colors) {
  n <- length(hex_colors)

  if (n < 2) {
    plot(
      0,
      0,
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = "Distance Comparison\n(Need >=2 colors)"
    )
    text(
      0,
      0,
      "Insufficient colors for distance analysis",
      cex = 1.0,
      col = "gray50"
    )
    return()
  }

  # Helper function to calculate distances for any palette
  calc_distances <- function(colors) {
    oklab_colors <- farver::convert_colour(
      farver::decode_colour(colors),
      from = "rgb",
      to = "oklab"
    )
    dist_matrix <- calculate_perceptual_distances(oklab_colors)
    # Extract upper triangle (unique pairwise distances)
    dist_matrix[upper.tri(dist_matrix)]
  }

  # Generate comparison palettes with same number of colors
  viridis_colors <- hcl.colors(n, "Viridis")
  dark2_colors <- hcl.colors(n, "Set 2")
  batlow_colors <- hcl.colors(n, "Batlow")

  # Calculate distance distributions
  huerd_distances <- calc_distances(hex_colors)
  viridis_distances <- calc_distances(viridis_colors)
  dark2_distances <- calc_distances(dark2_colors)
  batlow_distances <- calc_distances(batlow_colors)

  # Combine into list for boxplot
  distance_list <- list(
    "huerd" = huerd_distances,
    "Set 2" = dark2_distances,
    "Viridis" = viridis_distances,
    "Batlow" = batlow_distances
  )

  # Create boxplot comparison with better sizing and axis labels
  boxplot(
    distance_list,
    main = "Distance Distribution Comparison\n(OKLAB Perceptual Units)",
    ylab = "Perceptual Distance",
    xlab = "Palette Type",
    col = c("#E8F4FD", "#FFF2CC", "#E1D5E7", "#D5E8D4"),
    border = "black",
    las = 2, # Rotate x-axis labels to prevent cutoff
    cex.main = 1.4,
    cex.lab = 1.2,
    cex.axis = 1.0,
    cex.names = 1.0 # Increased x-axis labels
  )

  # Add jittered points to show underlying distribution
  # Calculate appropriate jitter width (proportion of box width)
  jitter_width <- 0.15 # 15% of unit width for good spread without overlap
  point_alpha <- 0.5 # Semi-transparent points
  point_size <- 0.7 # Smaller points to avoid visual clutter

  # Add jittered points for each palette
  for (i in 1:4) {
    # Generate jittered x-positions around the box center
    n_points <- length(distance_list[[i]])
    jittered_x <- i + runif(n_points, -jitter_width, jitter_width)

    # Add points with transparency and appropriate size
    points(
      jittered_x,
      distance_list[[i]],
      pch = 19,
      col = adjustcolor("gray30", alpha.f = point_alpha),
      cex = point_size
    )
  }

  # Add grid for easier reading
  grid(col = "lightgray", lty = 1, lwd = 0.5)

  # Highlight the minimum distances with points
  min_distances <- sapply(distance_list, min)
  points(1:4, min_distances, pch = 19, col = "black", cex = 1.2)

  # Add text showing minimum distances
  text(
    1:4,
    min_distances,
    sprintf("%.3f", min_distances),
    pos = 3,
    cex = 1.0,
    font = 2,
    col = "black"
  )

  # Add explanation with better sizing and positioning
  mtext(
    "Black dots show minimum distances (higher is better for categorical palettes)",
    side = 1,
    line = 5.0, # Increased to accommodate rotated labels
    cex = 0.9,
    col = "gray50"
  )
}

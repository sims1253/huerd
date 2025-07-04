#' Calculate Safe Margins for Plotting
#'
#' Calculates plot margins that are safe for the current plotting device,
#' adapting to available space while maintaining publication quality when possible.
#'
#' @param desired_mar Numeric vector of desired margins (bottom, left, top, right)
#' @param desired_oma Numeric vector of desired outer margins
#' @param mfrow Numeric vector of layout (rows, columns)
#' @return List with 'mar' and 'oma' elements containing safe margin values
#' @noRd
calculate_safe_margins <- function(desired_mar = c(6.5, 6.0, 4.0, 3.0),
                                  desired_oma = c(1.5, 0, 4, 0),
                                  mfrow = c(2, 3)) {
  # Get current device dimensions in inches
  dev_size <- par("din")
  
  # Calculate available space per subplot
  available_width <- dev_size[1] / mfrow[2]
  available_height <- dev_size[2] / mfrow[1]
  
  # Convert margins from lines to inches (approximate)
  # Default line height is about 0.2 inches, but varies by device
  line_height <- par("cin")[2] * par("cex")
  
  # Calculate total margin space needed in inches
  total_mar_width <- (desired_mar[2] + desired_mar[4]) * line_height
  total_mar_height <- (desired_mar[1] + desired_mar[3]) * line_height
  total_oma_width <- (desired_oma[2] + desired_oma[4]) * line_height
  total_oma_height <- (desired_oma[1] + desired_oma[3]) * line_height
  
  # Calculate available space for actual plot area
  plot_width <- available_width - total_mar_width - total_oma_width/mfrow[2]
  plot_height <- available_height - total_mar_height - total_oma_height/mfrow[1]
  
  # If plot area would be too small, scale down margins proportionally
  min_plot_size <- 1.5 # minimum 1.5 inches for plot area
  
  scale_factor <- 1.0
  if (plot_width < min_plot_size) {
    scale_factor <- min(scale_factor, (available_width - min_plot_size) / (total_mar_width + total_oma_width/mfrow[2]))
  }
  if (plot_height < min_plot_size) {
    scale_factor <- min(scale_factor, (available_height - min_plot_size) / (total_mar_height + total_oma_height/mfrow[1]))
  }
  
  # Apply scaling, but don't go below minimum usable margins
  min_mar <- c(3.0, 3.0, 2.0, 1.0)  # minimum margins for readability
  min_oma <- c(0.5, 0, 2.0, 0)      # minimum outer margins
  
  safe_mar <- pmax(desired_mar * scale_factor, min_mar)
  safe_oma <- pmax(desired_oma * scale_factor, min_oma)
  
  return(list(mar = safe_mar, oma = safe_oma))
}

#' Comprehensive Palette Analysis Dashboard
#'
#' Creates a scicomap-inspired comprehensive diagnostic dashboard for color palettes.
#' This function generates six visualization panels to help assess palette quality,
#' including perceptual distance analysis, CVD simulation, OKLAB space distribution,
#' and performance comparison against established scientific palettes.
#'
#' @param colors A character vector of hex colors or a matrix of colors in OKLAB space.
#' @param main_title Character. Main title for the dashboard. Default: "Palette Analysis Dashboard"
#' @return Invisibly returns the evaluation result from evaluate_palette.
#' @importFrom graphics abline axis barplot boxplot grid image mtext par points rect text lines
#' @importFrom grDevices hcl.colors adjustcolor
#' @export
#' @examples
#' colors <- c("#ff0000", "#00ff00", "#0000ff")
#' plot_palette_analysis(colors)
plot_palette_analysis <- function(
  colors,
  main_title = "Palette Analysis Dashboard"
) {
  # Evaluate the palette first
  evaluation <- evaluate_palette(colors)

  # Convert colors to consistent format
  if (is.character(colors)) {
    hex_colors <- colors
  } else {
    # Assume OKLAB matrix, convert to hex
    hex_colors <- farver::encode_colour(colors, from = "oklab")
  }

  n_colors <- length(hex_colors)

  # Store current graphics parameters to restore later
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Calculate safe margins based on device size
  safe_margins <- calculate_safe_margins(
    desired_mar = c(6.5, 6.0, 4.0, 3.0),
    desired_oma = c(1.5, 0, 4, 0),
    mfrow = c(2, 3)
  )

  # Set up 2x3 layout for 6 panels with adaptive margins
  par(mfrow = c(2, 3), mar = safe_margins$mar, oma = safe_margins$oma)

  # Panel 1: Color Swatches with Key Metrics
  plot_color_swatches(hex_colors, evaluation)

  # Panel 2: Pairwise Distance Heatmap
  plot_distance_heatmap(hex_colors, evaluation)

  # Panel 3: Nearest Neighbor Distances
  plot_nearest_neighbor_distances(hex_colors, evaluation)

  # Panel 4: Color Space Distribution
  plot_color_space_distribution(hex_colors)

  # Panel 5: CVD Simulation
  plot_cvd_simulation(hex_colors)

  # Panel 6: Comparative Palette Analysis
  plot_comparative_palettes(hex_colors)

  # Add main title with increased size for publication quality
  mtext(main_title, outer = TRUE, cex = 2.0, font = 2)

  invisible(evaluation)
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
  lightness <- oklab_color[1, 1]  # L component is first column
  
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
    text(i - 0.5, 2.5, hex_colors[i], cex = 0.6, font = 2, col = text_color, srt = 90)
    
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
    plot(
      0,
      0,
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = "Pairwise Distances\n(Need >=2 colors)"
    )
    text(0, 0, "Insufficient colors", cex = 1.2, col = "gray50")
    return()
  }
  
  # Store current margins and set expanded margins for legend and Y-axis label
  old_mar <- par("mar")
  
  # Calculate safe margins for this subplot, accounting for legend space
  desired_mar <- c(6.5, 6.0, 4.0, 6.5)  # Extra space on right for legend
  
  # Get current device dimensions and calculate safe margins
  dev_size <- par("din")
  line_height <- par("cin")[2] * par("cex")
  
  # Calculate available space (considering we're in a 2x3 layout)
  available_width <- dev_size[1] / 3  # 3 columns
  available_height <- dev_size[2] / 2  # 2 rows
  
  # Ensure minimum space for plot and legend
  min_plot_width <- 1.5  # minimum plot area
  min_legend_width <- 0.8  # minimum legend width
  required_width <- min_plot_width + min_legend_width
  
  # Scale margins if necessary
  scale_factor <- 1.0
  if (available_width < required_width + (desired_mar[2] + desired_mar[4]) * line_height) {
    scale_factor <- (available_width - required_width) / ((desired_mar[2] + desired_mar[4]) * line_height)
    scale_factor <- max(scale_factor, 0.3)  # Don't scale below 30% of desired
  }
  
  # Apply scaling with minimums
  min_mar <- c(3.0, 3.0, 2.0, 3.0)  # minimum margins including legend space
  safe_mar <- pmax(desired_mar * scale_factor, min_mar)
  
  par(mar = safe_mar)
  on.exit(par(mar = old_mar))

  # Calculate distance matrix
  oklab_colors <- farver::convert_colour(
    farver::decode_colour(hex_colors),
    from = "rgb",
    to = "oklab"
  )
  dist_matrix <- calculate_perceptual_distances(oklab_colors)

  viridis_colors <- hcl.colors(100, "Viridis")
  image(
    1:n,
    1:n,
    dist_matrix,
    col = viridis_colors,
    main = "Pairwise Distance Matrix\n(OKLAB Units)",
    xlab = "Color Index",
    ylab = "Color Index",
    axes = FALSE,
    cex.main = 1.4,
    cex.lab = 1.2,
    asp = 1  # Force square aspect ratio to prevent squishing
  )

  axis(1, at = 1:n, labels = 1:n, cex.axis = 1.0)
  axis(2, at = 1:n, labels = 1:n, cex.axis = 1.0)

  # Add enhanced color bar legend
  dist_range <- range(dist_matrix[dist_matrix > 0], na.rm = TRUE)
  if (diff(dist_range) > 0) {
    # Create color bar in the right margin with improved positioning
    usr <- par("usr")
    legend_width <- (usr[2] - usr[1]) * 0.12  # Slightly narrower
    legend_x1 <- usr[2] + (usr[2] - usr[1]) * 0.08  # Better spacing from plot
    legend_x2 <- legend_x1 + legend_width
    legend_y1 <- usr[3] + (usr[4] - usr[3]) * 0.15  # Better vertical positioning
    legend_y2 <- usr[4] - (usr[4] - usr[3]) * 0.15

    # Draw color bar with border for professional appearance
    legend_vals <- seq(dist_range[1], dist_range[2], length.out = 25)  # More segments for smoother gradient
    legend_colors <- hcl.colors(25, "Viridis")

    # Draw individual color segments
    for (i in 1:25) {
      rect(
        legend_x1,
        legend_y1 + (i - 1) * (legend_y2 - legend_y1) / 25,
        legend_x2,
        legend_y1 + i * (legend_y2 - legend_y1) / 25,
        col = legend_colors[i],
        border = NA
      )
    }
    
    # Add border around entire legend
    rect(legend_x1, legend_y1, legend_x2, legend_y2, 
         col = NA, border = "black", lwd = 1)
    
    # Calculate tick positions and values for better readability
    n_ticks <- 5  # Number of tick marks including min and max
    tick_positions <- seq(legend_y1, legend_y2, length.out = n_ticks)
    tick_values <- seq(dist_range[1], dist_range[2], length.out = n_ticks)
    
    # Add tick marks
    tick_length <- (usr[2] - usr[1]) * 0.015
    for (i in 1:n_ticks) {
      # Draw tick mark
      lines(c(legend_x2, legend_x2 + tick_length), 
            c(tick_positions[i], tick_positions[i]), 
            col = "black", lwd = 1)
      
      # Add value label
      text(
        legend_x2 + tick_length + (usr[2] - usr[1]) * 0.01,
        tick_positions[i],
        sprintf("%.2f", tick_values[i]),
        cex = 1.0,
        pos = 4,
        font = 1
      )
    }
    
    # Add legend title with better positioning
    text(
      legend_x1 + legend_width / 2,
      legend_y2 + (usr[4] - usr[3]) * 0.06,
      "Distance",
      cex = 1.0,
      pos = 1,  # Center above the legend
      font = 2,  # Bold font
      adj = 0.5  # Center alignment
    )
  }

  if (n <= 8) {
    text_size <- pmax(0.6, 1.1 - n * 0.05)  # Increased minimum text size
    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          text(i, j, sprintf("%.2f", dist_matrix[i, j]), cex = text_size)
        }
      }
    }
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
  mtext(stats_text, side = 1, line = 4.5, cex = 1.0)  # Increased line spacing to prevent cutoff
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

  # Simulate CVD and grayscale
  tryCatch(
    {
      protan_colors <- colorspace::protan(hex_colors, severity = 1.0)
      deutan_colors <- colorspace::deutan(hex_colors, severity = 1.0)
      tritan_colors <- colorspace::tritan(hex_colors, severity = 1.0)
      gray_colors <- colorspace::desaturate(hex_colors)

      # Create comparison plot (increased height for grayscale row)
      plot(
        0,
        0,
        type = "n",
        xlim = c(0, max(1, n)),
        ylim = c(0, 5), # Increased from 4 to 5 for grayscale row
        axes = FALSE,
        xlab = "",
        ylab = "",
        main = "CVD Simulation & Grayscale\n(Severity = 1.0)",
        cex.main = 1.4
      )

      # Draw color strips
      for (i in 1:n) {
        # Original
        rect(i - 1, 4, i, 5, col = hex_colors[i], border = "black")
        # Protanopia
        rect(i - 1, 3, i, 4, col = protan_colors[i], border = "black")
        # Deuteranopia
        rect(i - 1, 2, i, 3, col = deutan_colors[i], border = "black")
        # Tritanopia
        rect(i - 1, 1, i, 2, col = tritan_colors[i], border = "black")
        # Grayscale
        rect(i - 1, 0, i, 1, col = gray_colors[i], border = "black")
      }

      # Add labels with more spacing from swatches and increased font size
      text(-0.25, 4.5, "Orig", srt = 90, adj = 0.5, cex = 1.2)
      text(-0.25, 3.5, "Prot", srt = 90, adj = 0.5, cex = 1.2)
      text(-0.25, 2.5, "Deut", srt = 90, adj = 0.5, cex = 1.2)
      text(-0.25, 1.5, "Trit", srt = 90, adj = 0.5, cex = 1.2)
      text(-0.25, 0.5, "Gray", srt = 90, adj = 0.5, cex = 1.2)
    },
    error = function(e) {
      plot(
        0,
        0,
        type = "n",
        axes = FALSE,
        xlab = "",
        ylab = "",
        main = "CVD Simulation\n(Error)"
      )
      text(0, 0, "CVD simulation failed", cex = 1.2, col = "red")
    }
  )
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
    las = 2,  # Rotate x-axis labels to prevent cutoff
    cex.main = 1.4,
    cex.lab = 1.2,
    cex.axis = 1.0,
    cex.names = 1.0  # Increased x-axis labels
  )

  # Add jittered points to show underlying distribution
  # Calculate appropriate jitter width (proportion of box width)
  jitter_width <- 0.15  # 15% of unit width for good spread without overlap
  point_alpha <- 0.5    # Semi-transparent points
  point_size <- 0.7     # Smaller points to avoid visual clutter
  
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

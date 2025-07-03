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
#' @importFrom graphics abline axis barplot boxplot grid image mtext par points rect text
#' @importFrom grDevices hcl.colors
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

  # Set up 2x3 layout for 6 panels with better spacing
  par(mfrow = c(2, 3), mar = c(3.5, 3.5, 2.5, 1.5), oma = c(0, 0, 3, 0))

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

  # Add main title
  mtext(main_title, outer = TRUE, cex = 1.4, font = 2)

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

  # Create color swatches plot with updated layout
  plot(
    0,
    0,
    type = "n",
    xlim = c(0, max(1, n)),
    ylim = c(0, 4), # Reduced from 5 to 4 since removing grayscale row
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = paste("Color Palette (", n, " colors)", sep = "") # Updated title
  )

  if (n == 0) {
    text(0.5, 2, "No colors to display", cex = 1.2, col = "gray50")
    return()
  }

  # Draw color swatches (larger now that grayscale row is removed)
  for (i in 1:n) {
    rect(i - 1, 2, i, 3.5, col = hex_colors[i], border = "black", lwd = 1)
    
    # Determine optimal text color for this background
    text_color <- get_contrast_text_color(hex_colors[i])
    
    # Add color index on the swatch with optimal contrast
    text(i - 0.5, 2.75, i, cex = 0.8, font = 2, col = text_color)
    # Add hex code below the swatch
    text(i - 0.5, 1.5, hex_colors[i], cex = 0.6, font = 1, srt = 90)
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
    text(n / 2, 1.1, metrics_text, cex = 0.7, font = 1) # Moved down from 4.7 to 1.1
  } else if (n == 1) {
    text(
      0.5,
      1.1,
      "Single color - no distance metrics",
      cex = 0.8,
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
  
  # Store current margins and set expanded right margin for legend
  old_mar <- par("mar")
  par(mar = c(3.5, 3.5, 2.5, 4.5))  # Increased right margin from 1.5 to 4.5
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
    axes = FALSE
  )

  axis(1, at = 1:n, labels = 1:n)
  axis(2, at = 1:n, labels = 1:n)

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
        cex = 0.65,
        pos = 4,
        font = 1
      )
    }
    
    # Add legend title with better positioning
    text(
      legend_x1 + legend_width / 2,
      legend_y2 + (usr[4] - usr[3]) * 0.06,
      "Distance",
      cex = 0.7,
      pos = 1,  # Center above the legend
      font = 2,  # Bold font
      adj = 0.5  # Center alignment
    )
  }

  if (n <= 8) {
    text_size <- pmax(0.4, 0.9 - n * 0.05)
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

  # Create bar plot
  barplot(
    nn_distances,
    names.arg = 1:n,
    col = hex_colors,
    border = "black",
    main = "Nearest Neighbor Distances",
    xlab = "Color Index",
    ylab = "Distance (OKLAB)",
    ylim = c(0, max(nn_distances) * 1.1)
  )

  # Add horizontal line for minimum
  min_distance <- min(nn_distances)
  max_distance <- max(nn_distances)
  abline(h = min_distance, col = "red", lwd = 2, lty = 2)
  
  # Intelligent positioning for "Min" text label
  # Get actual bar positions from barplot layout
  # R's barplot uses 1.2 spacing between bar centers by default
  bar_centers <- seq(from = 0.7, by = 1.2, length.out = n)
  bar_width <- 1.0  # Standard barplot bar width
  
  # Calculate plot dimensions for positioning
  plot_width <- n * 1.2  # Approximate plot width based on barplot spacing
  plot_height <- max(nn_distances) * 1.1  # From ylim calculation
  
  # Enhanced collision detection parameters
  min_text_clearance <- plot_height * 0.04  # Minimum vertical clearance around text
  text_width_estimate <- plot_width * 0.15  # Estimate text width based on plot size
  collision_threshold <- pmax(max_distance * 0.06, min_distance * 0.12)  # Tighter threshold
  
  # Default text position
  default_text_x <- plot_width / 2
  default_text_y <- min_distance + min_text_clearance
  
  # Check for collisions at default position
  colliding_bars <- which(
    abs(nn_distances - min_distance) <= collision_threshold &
    abs(bar_centers - default_text_x) <= (text_width_estimate / 2 + bar_width / 2)
  )
  
  # Choose optimal text position based on collision analysis
  if (length(colliding_bars) == 0) {
    # No collisions - use default position
    text_x <- default_text_x
    text_y <- default_text_y
  } else {
    # Collisions detected - find optimal alternative position
    
    # Strategy 1: Try positioning below the line if sufficient space
    below_y <- min_distance - min_text_clearance
    if (below_y > 0 && below_y > plot_height * 0.05) {
      # Check if positioning below avoids collisions
      below_collisions <- which(
        abs(nn_distances - min_distance) <= collision_threshold &
        abs(bar_centers - default_text_x) <= (text_width_estimate / 2 + bar_width / 2)
      )
      
      if (length(below_collisions) <= length(colliding_bars)) {
        text_x <- default_text_x
        text_y <- below_y
      } else {
        # Strategy 2: Find best horizontal position
        text_x <- default_text_x
        text_y <- default_text_y
        
        # Try multiple horizontal positions
        candidate_x_positions <- c(
          plot_width * 0.2,   # Far left
          plot_width * 0.35,  # Left-center
          plot_width * 0.65,  # Right-center
          plot_width * 0.8    # Far right
        )
        
        best_x <- default_text_x
        min_collisions <- length(colliding_bars)
        
        for (candidate_x in candidate_x_positions) {
          # Ensure candidate position is within plot bounds
          if (candidate_x > text_width_estimate/2 && candidate_x < (plot_width - text_width_estimate/2)) {
            candidate_collisions <- which(
              abs(nn_distances - min_distance) <= collision_threshold &
              abs(bar_centers - candidate_x) <= (text_width_estimate / 2 + bar_width / 2)
            )
            
            if (length(candidate_collisions) < min_collisions) {
              best_x <- candidate_x
              min_collisions <- length(candidate_collisions)
            }
          }
        }
        
        text_x <- best_x
        
        # Strategy 3: If still collisions, move further above
        if (min_collisions > 0) {
          text_y <- min_distance + min_text_clearance * 2
        } else {
          text_y <- default_text_y
        }
      }
    } else {
      # Strategy 2 only: Find best horizontal position (no space below)
      text_y <- default_text_y
      
      # Try multiple horizontal positions
      candidate_x_positions <- c(
        plot_width * 0.2,   # Far left
        plot_width * 0.35,  # Left-center
        plot_width * 0.65,  # Right-center
        plot_width * 0.8    # Far right
      )
      
      best_x <- default_text_x
      min_collisions <- length(colliding_bars)
      
      for (candidate_x in candidate_x_positions) {
        # Ensure candidate position is within plot bounds
        if (candidate_x > text_width_estimate/2 && candidate_x < (plot_width - text_width_estimate/2)) {
          candidate_collisions <- which(
            abs(nn_distances - min_distance) <= collision_threshold &
            abs(bar_centers - candidate_x) <= (text_width_estimate / 2 + bar_width / 2)
          )
          
          if (length(candidate_collisions) < min_collisions) {
            best_x <- candidate_x
            min_collisions <- length(candidate_collisions)
          }
        }
      }
      
      text_x <- best_x
      
      # If still collisions, move further above
      if (min_collisions > 0) {
        text_y <- min_distance + min_text_clearance * 2
      }
    }
  }
  
  # Draw the text label at the calculated optimal position
  text(
    text_x,
    text_y,
    sprintf("Min: %.3f", min_distance),
    col = "red",
    font = 2,
    cex = 0.9  # Slightly smaller text for better fit
  )
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
    ylim = range(c(-0.4, 0.4, b_star)) * 1.1
  )

  abline(h = 0, v = 0, col = "gray60", lty = 2)
  grid(col = "lightgray", lty = 1)

  if (n <= 12) {
    label_size <- pmax(0.5, 1.0 - n * 0.03)
    text(
      a_star,
      b_star,
      1:n,
      pos = 3,
      cex = label_size,
      font = 2,
      col = "white"
    )
  }

  if (n > 1) {
    stats_text <- sprintf(
      "L: %.2f+/-%.2f | a*: %.3f+/-%.3f | b*: %.3f+/-%.3f",
      mean(L, na.rm = TRUE),
      sd(L, na.rm = TRUE),
      mean(a_star, na.rm = TRUE),
      sd(a_star, na.rm = TRUE),
      mean(b_star, na.rm = TRUE),
      sd(b_star, na.rm = TRUE)
    )
  } else {
    stats_text <- sprintf(
      "L: %.2f | a*: %.3f | b*: %.3f",
      L[1],
      a_star[1],
      b_star[1]
    )
  }
  mtext(stats_text, side = 1, line = 2.5, cex = 0.8)
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
        main = "CVD Simulation & Grayscale\n(Severity = 1.0)"
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

      # Add labels
      text(-0.1, 4.5, "Orig", srt = 90, adj = 0.5, cex = 0.7)
      text(-0.1, 3.5, "Prot", srt = 90, adj = 0.5, cex = 0.7)
      text(-0.1, 2.5, "Deut", srt = 90, adj = 0.5, cex = 0.7)
      text(-0.1, 1.5, "Trit", srt = 90, adj = 0.5, cex = 0.7)
      text(-0.1, 0.5, "Gray", srt = 90, adj = 0.5, cex = 0.7)
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

  # Create boxplot comparison with better sizing for various devices
  boxplot(
    distance_list,
    main = "Distance Distribution Comparison\n(OKLAB Perceptual Units)",
    ylab = "Perceptual Distance",
    xlab = "Palette Type",
    col = c("#E8F4FD", "#FFF2CC", "#E1D5E7", "#D5E8D4"),
    border = "black",
    las = 1,
    cex.main = 0.9, # Reduced from 1.0
    cex.lab = 0.8, # Reduced from 0.9
    cex.axis = 0.7 # Reduced from 0.8
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
  points(1:4, min_distances, pch = 19, col = "red", cex = 1.2)

  # Add text showing minimum distances
  text(
    1:4,
    min_distances,
    sprintf("%.3f", min_distances),
    pos = 3,
    cex = 0.7,
    font = 2,
    col = "red"
  )

  # Add explanation with better sizing
  mtext(
    "Red dots show minimum distances (higher is better for categorical palettes)",
    side = 1,
    line = 2.5, # Reduced from 3
    cex = 0.6, # Reduced from 0.7
    col = "gray50"
  )
}

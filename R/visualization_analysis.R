# Helper function for scaling font sizes
.scale_font <- function(base_size, font_scale) {
  max(8, round(base_size * font_scale)) # Minimum font size of 8
}

#' Comprehensive Palette Analysis Dashboard
#'
#' Creates a scicomap-inspired comprehensive diagnostic dashboard for color palettes
#' using the modern grid graphics system for robust cross-platform compatibility.
#' This function generates six visualization panels to help assess palette quality,
#' including perceptual distance analysis, CVD simulation, OKLAB space distribution,
#' and performance comparison against established scientific palettes.
#'
#' @param colors A character vector of hex colors or a matrix of colors in OKLAB space.
#' @param force_font_scale Allows to force a specific font scale
#' @return Invisibly returns the evaluation result from evaluate_palette.
#' @importFrom grid gpar grid.rect grid.text grobTree textGrob rectGrob
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices hcl.colors adjustcolor
#' @export
#' @examples
#' colors <- c("#ff0000", "#00ff00", "#0000ff")
#' plot_palette_analysis(colors)
plot_palette_analysis <- function(
  colors,
  force_font_scale = NULL
) {
  if (length(colors) < 2) {
    warning("Need at least two colors for a palette.")
    return()
  }

  evaluation <- evaluate_palette(colors)
  hex_colors <- if (is.character(colors)) {
    colors
  } else {
    farver::encode_colour(colors, from = "oklab")
  }

  # Calculate min distance for current palette
  current_oklab <- farver::convert_colour(
    farver::decode_colour(hex_colors),
    from = "rgb",
    to = "oklab"
  )

  current_dist_matrix <- calculate_perceptual_distances(current_oklab)

  # Calculate font scaling based on device dimensions
  current_vp <- grid::current.viewport()

  if (!is.null(force_font_scale)) {
    # If a scale is forced, use it directly.
    font_scale <- force_font_scale
  } else {
    # Otherwise, use the automatic detection for interactive plots.
    current_vp <- grid::current.viewport()
    dev_width <- as.numeric(grid::convertWidth(grid::unit(1, "npc"), "inches"))
    dev_height <- as.numeric(grid::convertHeight(
      grid::unit(1, "npc"),
      "inches"
    ))

    ref_width <- 4
    ref_height <- 3

    width_scale <- dev_width / ref_width
    height_scale <- dev_height / ref_height
    font_scale <- pmin(width_scale, height_scale, 1.5)
    #font_scale <- pmax(font_scale, 0.5)
  }

  # Create all subplot grobs with scaled fonts
  grob1 <- create_color_swatches(hex_colors, evaluation, font_scale)
  grob2 <- create_color_space(hex_colors, font_scale)
  grob3 <- create_distance_heatmap(hex_colors, evaluation, font_scale)
  grob4 <- create_cvd_simulation(hex_colors, font_scale)

  distance_data <- list(
    "huerd" = current_dist_matrix,
    "Protan" = calculate_perceptual_distances(.hex_to_oklab(colorspace::protan(
      hex_colors
    ))),
    "Deutan" = calculate_perceptual_distances(.hex_to_oklab(colorspace::deutan(
      hex_colors
    ))),
    "Tritan" = calculate_perceptual_distances(.hex_to_oklab(colorspace::tritan(
      hex_colors
    ))),
    "Greyscale" = calculate_perceptual_distances(.hex_to_oklab(colorspace::desaturate(
      hex_colors
    )))
  )

  grob5 <- create_comparative_palettes(
    distance_data,
    "Pairwise distances under CVD",
    font_scale
  )

  # Compare with reference palettes
  if (length(colors) < 9) {
    distance_data <- list(
      "huerd" = current_dist_matrix,
      "batlow" = get_ref_palette_distances("batlow", length(hex_colors)),
      "Viridis" = get_ref_palette_distances("viridis", length(hex_colors)),
      "Set 2" = get_ref_palette_distances("set2", length(hex_colors))
    )
  } else {
    distance_data <- list(
      "huerd" = current_dist_matrix,
      "batlow" = get_ref_palette_distances("batlow", length(hex_colors)),
      "Viridis" = get_ref_palette_distances("viridis", length(hex_colors)),
      "Harmonic" = get_ref_palette_distances("Harmonic", length(hex_colors))
    )
  }

  grob6 <- create_comparative_palettes(
    distance_data,
    "Comparison to other palettes",
    font_scale
  )

  gridExtra::grid.arrange(
    grob1,
    grob2,
    grob3,
    grob4,
    grob5,
    grob6,
    ncol = 3,
    nrow = 2
  )

  invisible(evaluation)
}

#' Create Color Swatches Grob
#' @noRd
create_color_swatches <- function(hex_colors, evaluation, font_scale = 0.8) {
  n <- length(hex_colors)

  rgp_colors = farver::decode_colour(hex_colors)
  oklab_colors = farver::convert_colour(rgp_colors, from = "rgb", to = "oklab")

  # Create a viewport for this subplot
  vp <- grid::viewport(name = "swatches")

  # Create grobs for the color swatches
  grobs <- list()

  # Title
  grobs[[1]] <- grid::textGrob(
    "Palette",
    y = 0.95,
    gp = grid::gpar(fontsize = .scale_font(16, font_scale), fontface = "bold")
  )

  # Color swatches
  swatch_width <- 0.9 / n
  for (i in 1:n) {
    x_pos <- (i - 0.5) / n

    # Color rectangle
    grobs[[length(grobs) + 1]] <- grid::rectGrob(
      x = x_pos,
      y = 0.6,
      width = swatch_width,
      height = 0.35,
      gp = grid::gpar(fill = hex_colors[i], col = "black")
    )

    # Color index
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      as.character(i),
      x = x_pos,
      y = 0.38,
      gp = grid::gpar(fontsize = .scale_font(16, font_scale), fontface = "bold")
    )

    # Hex code
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      rot = 90,
      hex_colors[i],
      x = x_pos,
      y = 0.6,
      gp = grid::gpar(
        fontsize = .scale_font(16, font_scale),
        fontface = "bold",
        col = ifelse(oklab_colors[i, 1] < 0.5, "white", "black")
      )
    )
  }

  # Add metrics if available
  if (n >= 2) {
    min_dist <- evaluation$distances$min %||% NA_real_
    perf_ratio <- (evaluation$distances$performance_ratio %||% 0) * 100
    cvd_min <- evaluation$cvd_safety$worst_case_min_distance %||% NA_real_

    metrics_text <- sprintf(
      "Min Dist: %.3f\nRelative performance: %.1f%%\nCVD Safety: %.3f",
      min_dist,
      perf_ratio,
      cvd_min
    )

    grobs[[length(grobs) + 1]] <- grid::textGrob(
      metrics_text,
      y = 0.2,
      gp = grid::gpar(fontsize = .scale_font(16, font_scale))
    )
  }

  # Combine all grobs
  do.call(grid::grobTree, grobs)
}

#' Create Distance Heatmap Grob
#' @noRd
create_distance_heatmap <- function(hex_colors, evaluation, font_scale = 0.8) {
  n <- length(hex_colors)

  # Calculate distance matrix
  dist_matrix <- calculate_perceptual_distances(
    farver::convert_colour(
      farver::decode_colour(hex_colors),
      from = "rgb",
      to = "oklab"
    )
  )

  grobs <- list()

  # Title
  grobs[[1]] <- grid::textGrob(
    "Pairwise Distance Matrix",
    y = 0.95,
    gp = grid::gpar(fontsize = .scale_font(16, font_scale), fontface = "bold")
  )
  grobs[[2]] <- grid::textGrob(
    "(OKLAB units)",
    y = 0.91,
    gp = grid::gpar(fontsize = .scale_font(12, font_scale))
  )

  # Create colored rectangles for the heatmap
  cell_size <- 0.8 / n # Available space for the matrix
  colors <- hcl.colors(100, "Viridis")

  # Normalize distances for color mapping
  max_dist <- max(dist_matrix, na.rm = TRUE)
  # min_dist <- min(dist_matrix[dist_matrix > 0], na.rm = TRUE)
  min_dist <- 0.08

  # TODO use the 0.08 threshold for meaningful difference as a lower bound and something reasonable as upper bound.
  # 0.08 # Reasonable multiple of the OKLAB JND of ~0.02
  # And same for the max dist. The scale should be fixed to be able to compare palettes.

  # TODO automatically center the matrix

  for (i in 1:n) {
    for (j in 1:n) {
      x_pos <- 0.1 + (j - 0.5) * cell_size
      y_pos <- 0.88 - (i - 0.5) * cell_size

      # Map distance to color
      if (i == j) {
        color_val <- "white" # Diagonal
      } else {
        color_idx <- round(
          99 * (dist_matrix[i, j] - min_dist) / (max_dist - min_dist)
        ) +
          1
        color_val <- colors[max(1, min(100, color_idx))]
      }

      grobs[[length(grobs) + 1]] <- grid::rectGrob(
        x = x_pos,
        y = y_pos,
        width = cell_size * 0.9,
        height = cell_size * 0.9,
        gp = grid::gpar(fill = color_val, col = "black")
      )

      # Add distance text for small matrices
      if (n <= 8 && i != j) {
        grobs[[length(grobs) + 1]] <- grid::textGrob(
          sprintf("%.2f", dist_matrix[i, j]),
          x = x_pos,
          y = y_pos,
          gp = grid::gpar(
            fontsize = .scale_font(9, font_scale),
            col = ifelse(
              farver::convert_colour(
                farver::decode_colour(color_val),
                from = "rgb",
                to = "oklab"
              )[1, 1] <
                0.5,
              "white",
              "black"
            )
          )
        )
      }
    }
  }

  # Add axis labels
  for (i in 1:n) {
    # X-axis labels
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      as.character(i),
      x = 0.1 + (i - 0.5) * cell_size,
      y = 0.88 - n * cell_size - 0.02,
      gp = grid::gpar(fontsize = .scale_font(14, font_scale), fontface = "bold")
    )

    # Y-axis labels
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      as.character(i),
      x = 0.1 - 0.02,
      y = 0.88 - (i - 0.5) * cell_size,
      gp = grid::gpar(fontsize = .scale_font(14, font_scale), fontface = "bold")
    )
  }

  do.call(grid::grobTree, grobs)
}

#' Create Color Space Distribution Grob
#' @noRd
create_color_space <- function(hex_colors, font_scale = 0.8) {
  n <- length(hex_colors)

  # Convert to OKLAB
  oklab_colors <- .hex_to_oklab(hex_colors)

  grobs <- list()

  # Title
  grobs[[1]] <- grid::textGrob(
    "OKLAB Color Space\n",
    y = 0.92,
    gp = grid::gpar(fontsize = .scale_font(16, font_scale), fontface = "bold")
  )

  grobs[[2]] <- grid::textGrob(
    "(a vs b), size indicates L",
    y = 0.91,
    gp = grid::gpar(fontsize = .scale_font(12, font_scale))
  )

  # Plot area boundaries
  plot_left <- 0.1
  plot_right <- 0.9
  plot_bottom <- 0.1
  plot_top <- 0.85

  # Data ranges with some padding
  #a_range <- range(oklab_colors[, 2], na.rm = TRUE)
  #b_range <- range(oklab_colors[, 3], na.rm = TRUE)

  a_range = c(-0.35, 0.35)
  b_range = c(-0.35, 0.35)

  # Expand ranges slightly for better visualization
  a_padding <- diff(a_range) * 0.1
  b_padding <- diff(b_range) * 0.1
  a_range <- a_range + c(-a_padding, a_padding)
  b_range <- b_range + c(-b_padding, b_padding)

  # Plot border
  grobs[[length(grobs) + 1]] <- grid::rectGrob(
    x = (plot_left + plot_right) / 2,
    y = (plot_bottom + plot_top) / 2,
    width = plot_right - plot_left,
    height = plot_top - plot_bottom,
    gp = grid::gpar(fill = NA, col = "black")
  )

  # Scale coordinates to plot area
  scale_a <- function(a) {
    plot_left + (a - a_range[1]) / diff(a_range) * (plot_right - plot_left)
  }
  scale_b <- function(b) {
    plot_bottom + (b - b_range[1]) / diff(b_range) * (plot_top - plot_bottom)
  }

  # Plot points
  for (i in 1:n) {
    x_pos <- scale_a(oklab_colors[i, 2])
    y_pos <- scale_b(oklab_colors[i, 3])

    # Color circle
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x = x_pos,
      y = y_pos,
      r = (0.015 + oklab_colors[i, 1] * 0.02),
      gp = grid::gpar(fill = hex_colors[i], col = "black")
    )
  }

  # Axis labels
  grobs[[length(grobs) + 1]] <- grid::textGrob(
    "a (green-red)",
    x = (plot_left + plot_right) / 2,
    y = 0.07,
    gp = grid::gpar(fontsize = .scale_font(14, font_scale))
  )

  grobs[[length(grobs) + 1]] <- grid::textGrob(
    "b (blue-yellow)",
    x = 0.05,
    y = (plot_bottom + plot_top) / 2,
    rot = 90,
    gp = grid::gpar(fontsize = .scale_font(14, font_scale))
  )

  # Add origin lines if 0 is in range
  x_zero <- scale_a(0)
  grobs[[length(grobs) + 1]] <- grid::linesGrob(
    x = c(x_zero, x_zero),
    y = c(plot_bottom, plot_top),
    gp = grid::gpar(col = "gray70", lty = 2)
  )

  y_zero <- scale_b(0)
  grobs[[length(grobs) + 1]] <- grid::linesGrob(
    x = c(plot_left, plot_right),
    y = c(y_zero, y_zero),
    gp = grid::gpar(col = "gray70", lty = 2)
  )

  do.call(grid::grobTree, grobs)
}

#' Create CVD Simulation Grob
#' @noRd
create_cvd_simulation <- function(hex_colors, font_scale = 0.8) {
  n <- length(hex_colors)

  grobs <- list()

  # Title
  grobs[[1]] <- grid::textGrob(
    "CVD Simulation\n",
    y = 0.95,
    gp = grid::gpar(fontsize = .scale_font(16, font_scale), fontface = "bold")
  )

  row_labels <- c(
    "Normal",
    "Protanopia",
    "Deuteranopia",
    "Tritanopia",
    "Greyscale"
  )

  for (row in 1:5) {
    y_pos <- 0.85 - (row - 1) * 0.16

    # Row label
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      row_labels[row],
      x = 0.13,
      y = y_pos,
      gp = grid::gpar(fontsize = .scale_font(12, font_scale))
    )

    # Get colors for this row
    if (row == 1) {
      row_colors <- hex_colors
    } else {
      # Apply CVD simulation
      if (row == 2) {
        row_colors <- colorspace::protan(hex_colors)
      } else if (row == 3) {
        row_colors <- colorspace::deutan(hex_colors)
      } else if (row == 4) {
        row_colors <- colorspace::tritan(hex_colors)
      } else {
        row_colors <- colorspace::desaturate(hex_colors)
      }
    }

    # Color swatches
    swatch_width <- 0.7 / n
    for (i in 1:n) {
      x_pos <- 0.25 + (i - 0.5) * swatch_width

      # Color rectangle
      grobs[[length(grobs) + 1]] <- grid::rectGrob(
        x = x_pos,
        y = y_pos,
        width = swatch_width * 0.9,
        height = 0.15,
        gp = grid::gpar(fill = row_colors[i], col = "black")
      )
    }
  }

  # Add color indices below
  for (i in 1:n) {
    x_pos <- 0.2 + (i - 0.5) * swatch_width
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      as.character(i),
      x = x_pos,
      y = 0.1,
      gp = grid::gpar(fontsize = .scale_font(14, font_scale), fontface = "bold")
    )
  }

  do.call(grid::grobTree, grobs)
}

#' Create Comparative Palettes Grob
#' @noRd
create_comparative_palettes <- function(
  distance_data,
  title,
  font_scale = 0.8
) {
  # TODO this should be boxplots with the distances

  grobs <- list()

  for (i in seq_along(distance_data)) {
    distance_data[[i]] = distance_data[[i]][distance_data[[i]] != 0]
  }

  n_palettes = length(distance_data)

  plot_area_x <- c(0.15, 0.95)
  plot_area_y <- c(0.15, 0.85)

  all_distances <- unlist(distance_data)
  y_range <- c(0, max(all_distances, na.rm = TRUE) * 1.05)

  scale_y <- function(val) {
    plot_area_y[1] +
      (val - y_range[1]) / (y_range[2] - y_range[1]) * diff(plot_area_y)
  }

  grobs[[length(grobs) + 1]] <- grid::textGrob(
    title,
    y = 0.95,
    gp = grid::gpar(fontsize = .scale_font(16, font_scale), fontface = "bold")
  )
  # Y-axis Label
  grobs[[length(grobs) + 1]] <- grid::textGrob(
    "Pairwise Perceptual Distance (Delta E)",
    x = 0.03,
    rot = 90,
    gp = grid::gpar(fontsize = .scale_font(14, font_scale))
  )

  y_ticks <- pretty(y_range)
  y_ticks <- y_ticks[y_ticks >= y_range[1] & y_ticks <= y_range[2]]

  if (length(y_ticks) > 0) {
    grobs[[length(grobs) + 1]] <- grid::segmentsGrob(
      x0 = plot_area_x[1],
      y0 = scale_y(y_ticks),
      x1 = plot_area_x[1] - 0.01,
      y1 = scale_y(y_ticks)
    )
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = y_ticks,
      x = plot_area_x[1] - 0.02,
      y = scale_y(y_ticks),
      just = "right",
      gp = grid::gpar(fontsize = .scale_font(9, font_scale))
    )
  }

  box_width <- 0.6 / n_palettes # Relative width of each boxplot

  for (i in seq_along(distance_data)) {
    palette_nm <- names(distance_data)[i]
    distances <- distance_data[[i]]

    # X-position for the center of the current boxplot
    x_center <- plot_area_x[1] + (i - 0.5) / n_palettes * diff(plot_area_x)

    # 1. Jittered Points (drawn first to be in the background)
    jitter_x <- stats::runif(length(distances), -box_width / 4, box_width / 4)
    grobs[[length(grobs) + 1]] <- grid::pointsGrob(
      x = x_center + jitter_x,
      y = scale_y(distances),
      pch = 19, # Solid circle
      size = grid::unit(3, "mm"),
      gp = grid::gpar(col = "black", alpha = 0.25)
    )

    # 2. Boxplot Statistics and Components
    stats <- grDevices::boxplot.stats(distances)$stats
    names(stats) <- c(
      "lower_whisker",
      "q1",
      "median",
      "q3",
      "upper_whisker"
    )

    # Scale stats to plot coordinates
    scaled_stats <- sapply(stats, scale_y)

    # Colors for highlighting
    is_input_palette <- palette_nm == "huerd"
    box_fill <- "#B0B0B0"
    median_col <- "black"

    # Whiskers
    grobs[[length(grobs) + 1]] <- grid::segmentsGrob(
      x0 = x_center,
      y0 = scaled_stats["lower_whisker"],
      x1 = x_center,
      y1 = scaled_stats["upper_whisker"]
    )
    grobs[[length(grobs) + 1]] <- grid::segmentsGrob(
      x0 = x_center - 0.04,
      y0 = scaled_stats["lower_whisker"],
      x1 = x_center + 0.04,
      y1 = scaled_stats["lower_whisker"]
    )
    grobs[[length(grobs) + 1]] <- grid::segmentsGrob(
      x0 = x_center - 0.04,
      y0 = scaled_stats["upper_whisker"],
      x1 = x_center + 0.04,
      y1 = scaled_stats["upper_whisker"]
    )

    # Box
    grobs[[length(grobs) + 1]] <- grid::rectGrob(
      x = x_center,
      y = scaled_stats["q1"],
      width = box_width,
      height = scaled_stats["q3"] - scaled_stats["q1"],
      just = c("center", "bottom"),
      gp = grid::gpar(fill = box_fill, col = "black", alpha = 0.7)
    )

    # Median Line
    grobs[[length(grobs) + 1]] <- grid::segmentsGrob(
      x0 = x_center - box_width / 2,
      y0 = scaled_stats["median"],
      x1 = x_center + box_width / 2,
      y1 = scaled_stats["median"],
      gp = grid::gpar(col = median_col, lwd = 3)
    )

    # 3. X-axis Label (Palette Name)
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = palette_nm,
      x = x_center,
      y = plot_area_y[1] - 0.04,
      gp = grid::gpar(
        fontsize = .scale_font(12, font_scale),
        fontface = "bold"
      )
    )
  }

  do.call(grid::grobTree, grobs)
}

#' Get reference palette minimum distance
#' @noRd
get_ref_palette_distances <- function(palette_name, n) {
  ref_colors <- hcl.colors(n, palette_name)

  ref_oklab <- farver::convert_colour(
    farver::decode_colour(ref_colors),
    from = "rgb",
    to = "oklab"
  )

  return(ref_dist_matrix <- calculate_perceptual_distances(ref_oklab))
}

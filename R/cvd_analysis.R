#' Simulate Palette Under Color Vision Deficiency
#'
#' Shows how a color palette appears to individuals with different types
#' of color vision deficiency using physiologically accurate models from `farver`.
#'
#' @param colors Character vector of hex colors.
#' @param cvd_type Character. Type of CVD: "protan", "deutan", "tritan", or "all".
#'   Default is "all".
#' @param severity Numeric. Severity of CVD from 0 (none) to 1 (complete).
#'   Default is 1.0.
#' @param plot Logical. Whether to plot a comparison using base R graphics.
#'   Default is FALSE.
#'
#' @return If `cvd_type` is "all", returns a list with simulated palettes for
#'   each type (and original). Otherwise returns a character vector of simulated hex colors.
#'   The output object also inherits from `huerd_simulation_result`.
#'
#' @examples
#' palette_ex <- generate_palette(6, progress = FALSE)
#'
#' # Simulate complete deuteranopia
#' deutan_palette <- simulate_palette_cvd(palette_ex, "deutan", severity = 1.0)
#' print(deutan_palette)
#'
#' # See all CVD types and plot them
#' if (interactive() && length(palette_ex) > 0) {
#'   all_cvd <- simulate_palette_cvd(palette_ex, "all", plot = TRUE)
#' }
#'
#' @export
simulate_palette_cvd <- function(
  colors,
  cvd_type = c("all", "protan", "deutan", "tritan"),
  severity = 1.0,
  plot = FALSE
) {
  cvd_type_arg <- match.arg(cvd_type)

  if (
    !is.character(colors) ||
      (length(colors) > 0 &&
        !all(
          grepl("^#[0-9A-Fa-f]{6}$|^NA$", colors, ignore.case = TRUE) |
            is.na(colors)
        ))
  ) {
    stop("colors must be a character vector of valid hex codes or NA.")
  }

  valid_colors <- colors[!is.na(colors)]

  if (length(valid_colors) == 0) {
    warning(
      "Input 'colors' contains no valid colors. Returning an empty result.",
      call. = FALSE
    )
    res <- if (cvd_type_arg == "all") {
      list(original = character(0))
    } else {
      character(0)
    }
    class(res) <- c("huerd_simulation_result", class(res))
    attr(res, "cvd_type") <- cvd_type_arg
    attr(res, "severity") <- severity
    return(res)
  }

  if (!is.numeric(severity) || severity < 0 || severity > 1) {
    stop("severity must be a number between 0 and 1.")
  }

  # colorspace functions work directly with hex colors
  if (cvd_type_arg == "all") {
    sim_types <- c("protan", "deutan", "tritan")
    results_list <- list(original = colors)

    for (current_sim_type in sim_types) {
      cvd_fun <- switch(
        current_sim_type,
        "protan" = colorspace::protan,
        "deutan" = colorspace::deutan,
        "tritan" = colorspace::tritan
      )
      simulated_colors <- cvd_fun(valid_colors, severity = severity)

      # Map back to original positions (handling NAs)
      result_vec <- colors
      result_vec[!is.na(colors)] <- simulated_colors
      results_list[[current_sim_type]] <- result_vec
    }
    output_results <- results_list
  } else {
    cvd_fun <- switch(
      cvd_type_arg,
      "protan" = colorspace::protan,
      "deutan" = colorspace::deutan,
      "tritan" = colorspace::tritan
    )
    simulated_colors <- cvd_fun(valid_colors, severity = severity)

    # Map back to original positions (handling NAs)
    result_vec <- colors
    result_vec[!is.na(colors)] <- simulated_colors
    output_results <- result_vec
  }

  class(output_results) <- c("huerd_simulation_result", class(output_results))
  attr(output_results, "cvd_type") <- cvd_type_arg
  attr(output_results, "severity") <- severity

  if (plot) {
    plot_list <- if (is.list(output_results)) {
      output_results
    } else {
      list(original = colors, simulated = output_results)
    }
    plot_cvd_comparison(plot_list)
  }

  return(output_results)
}

#' Plot CVD Comparison (Base R Graphics)
#'
#' Internal helper to create a visual comparison of original and simulated palettes.
#' @param palette_list A list of palettes to compare.
#' @noRd
plot_cvd_comparison <- function(palette_list) {
  if (!is.list(palette_list) || length(palette_list) == 0) {
    warning(
      "Cannot plot CVD comparison: input is not a valid list or is empty.",
      call. = FALSE
    )
    return(invisible(NULL))
  }

  n_colors <- length(palette_list[[1]])
  if (n_colors == 0) {
    message("Cannot plot CVD comparison: palette has no colors.")
    return(invisible(NULL))
  }

  n_palettes <- length(palette_list)
  old_par <- graphics::par(
    mar = c(4, 8, 3, 1),
    mfrow = c(1, 1),
    no.readonly = TRUE
  )
  on.exit(graphics::par(old_par))

  graphics::plot(
    0,
    0,
    type = "n",
    xlim = c(0.5, n_colors + 0.5),
    ylim = c(0.5, n_palettes + 0.5),
    xlab = "",
    ylab = "",
    axes = FALSE,
    main = "CVD Simulation Comparison"
  )

  palette_names <- names(palette_list) %||%
    paste("Palette", seq_along(palette_list))

  for (i in seq_along(palette_list)) {
    current_palette <- palette_list[[i]]
    y_plot <- n_palettes - i + 1

    if (is.character(current_palette) && length(current_palette) == n_colors) {
      for (j in seq_along(current_palette)) {
        fill_col <- current_palette[j] %||% "#DDDDDD" # Default to light gray if NA
        border_col <- "grey50"

        tryCatch(
          {
            fill_col_rgb_raw <- grDevices::col2rgb(fill_col)
            # Use sRGB luminance formula for perceived brightness for text contrast
            srgb_red_weight <- 0.2126
            srgb_green_weight <- 0.7152
            srgb_blue_weight <- 0.0722
            luminance_srgb <- (srgb_red_weight *
              fill_col_rgb_raw[1, 1] +
              srgb_green_weight * fill_col_rgb_raw[2, 1] +
              srgb_blue_weight * fill_col_rgb_raw[3, 1]) /
              255
            border_col <- if (luminance_srgb > 0.5) "black" else "white"
          },
          error = function(e) {
            border_col <- "red"
          }
        )

        graphics::rect(
          j - 0.45,
          y_plot - 0.45,
          j + 0.45,
          y_plot + 0.45,
          col = fill_col,
          border = border_col,
          lwd = 1.5
        )
      }
    }
  }

  graphics::axis(
    2,
    at = seq(n_palettes, 1),
    labels = palette_names,
    las = 1,
    tick = FALSE
  )
  graphics::axis(1, at = seq_len(n_colors), labels = seq_len(n_colors))
  graphics::mtext("Color Index", side = 1, line = 2.5)
}


#' Check Palette CVD Safety
#'
#' Quick check if a palette's worst-case minimum inter-color distance under
#' simulated common CVD conditions meets a specified threshold.
#'
#' @param colors Character vector of hex colors or an OKLAB matrix.
#' @param min_cvd_distance Numeric. The minimum acceptable perceptual distance
#'   (in OK LAB space) that any two colors in the palette should maintain
#'   under simulated deuteranopia, protanopia, and tritanopia. Default is 0.08.
#'   Recall JND is ~0.02.
#'
#' @return Logical. `TRUE` if the palette's `worst_case_min_distance` from
#'   `evaluate_palette()` is greater than or equal to `min_cvd_distance`,
#'   `FALSE` otherwise. Returns `TRUE` if palette has fewer than 2 colors.
#' @export
is_cvd_safe <- function(colors, min_cvd_distance = 0.08) {
  if (!is.numeric(min_cvd_distance) || min_cvd_distance <= 0) {
    stop("min_cvd_distance must be a positive number.")
  }

  metrics <- evaluate_palette(colors)

  if (metrics$n_colors < 2) {
    return(TRUE) # No pairs to check, so vacuously true
  }

  # If worst_case_min_distance is NA (e.g., due to out-of-gamut colors),
  # it means safety cannot be confirmed, so treat as unsafe (0).
  actual_worst_case_min <- metrics$cvd_safety$worst_case_min_distance %||% 0

  return(actual_worst_case_min >= min_cvd_distance)
}

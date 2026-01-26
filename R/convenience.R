# Convenience Functions for huerd
#
# User-friendly wrapper functions that provide simpler interfaces to huerd's
# core functionality. These functions prioritize ease of use over fine-grained
# control.

#' Quick palette generation with sensible defaults
#'
#' A simplified interface to [generate_palette()] that uses intuitive parameter
#' names and sensible defaults. This function is designed for users who want
#' good results without understanding optimization details.
#'
#' @param n Number of colors to generate.
#' @param brand_colors Optional character vector of hex colors that must be
#'   included in the palette. These colors will be preserved exactly as
#'   provided, and additional colors will be optimized around them.
#' @param cvd_safe Logical. If `TRUE` (default), optimizes for color vision
#'   deficiency safety. Currently this uses the default optimization which
#'   considers CVD in its metrics.
#' @param quality Character string specifying the quality/speed tradeoff:
#'   - `"fast"`: Quick generation with fewer iterations (good for exploration)
#'   - `"balanced"`: Default balance of quality and speed
#'   - `"high"`: More iterations for better results (slower)
#' @param lightness Character string or numeric vector specifying lightness
#'   constraints:
#'   - `"any"`: Balanced range (L: 0.2-0.9)
#'   - `"light"`: Prefer lighter colors (L: 0.5-0.9)
#'   - `"dark"`: Prefer darker colors (L: 0.2-0.6)
#'   - `"mid"`: Prefer mid-range lightness (L: 0.35-0.75)
#'   - Numeric vector of length 2: Custom bounds (e.g., `c(0.3, 0.8)`)
#'
#' @return A `huerd_palette` object (character vector of hex colors with
#'   additional attributes).
#'
#' @examples
#' # Simple 5-color palette
#' quick_palette(5)
#'
#' # Include brand colors
#' quick_palette(6, brand_colors = c("#1f77b4", "#ff7f0e"))
#'
#' # Fast generation for exploration
#' quick_palette(8, quality = "fast")
#'
#' # Light colors for dark backgrounds
#' quick_palette(5, lightness = "light")
#'
#' @seealso [generate_palette()] for full control over palette generation.
#' @export
quick_palette <- function(
  n,
  brand_colors = NULL,
  cvd_safe = TRUE,
  quality = c("balanced", "fast", "high"),
  lightness = "any"
) {
  quality <- match.arg(quality)

  # Map quality to iterations
  max_iterations <- switch(
    quality,
    "fast" = 200,
    "balanced" = 1000,
    "high" = 3000
  )

  # Map lightness to bounds
  if (is.character(lightness)) {
    valid_presets <- c("any", "light", "dark", "mid")
    if (!lightness %in% valid_presets) {
      cli::cli_abort(c(
        "{.arg lightness} must be a valid preset or numeric vector of ",
        "length 2.",
        "i" = "Valid presets: {.val {valid_presets}}"
      ))
    }
    lightness_bounds <- switch(
      lightness,
      "any" = c(0.2, 0.9),
      "light" = c(0.5, 0.9),
      "dark" = c(0.2, 0.6),
      "mid" = c(0.35, 0.75)
    )
  } else if (is.numeric(lightness) && length(lightness) == 2) {
    lightness_bounds <- lightness
  } else {
    cli::cli_abort(c(
      "{.arg lightness} must be a preset name or numeric vector of length 2.",
      "i" = "Valid presets: {.val any}, {.val light}, {.val dark}, {.val mid}"
    ))
  }

  generate_palette(
    n = n,
    include_colors = brand_colors,
    init_lightness_bounds = lightness_bounds,
    max_iterations = max_iterations,
    cvd_safe = cvd_safe,
    progress = FALSE
  )
}


#' Create a palette with brand colors
#'
#' Convenience function specifically for creating palettes that incorporate
#' brand or corporate colors. This is a common use case where specific colors
#' must be preserved while generating complementary colors.
#'
#' @param brand_colors Character vector of hex colors representing your brand
#'   colors. These will be preserved exactly in the output.
#' @param n_total Total number of colors needed in the final palette. Must be
#'   at least as large as the number of brand colors.
#' @param cvd_safe Logical. If `TRUE` (default), prioritizes CVD safety in
#'   optimization.
#'
#' @return A `huerd_palette` object containing the brand colors plus
#'   optimized complementary colors.
#'
#' @examples
#' # Corporate palette with 2 brand colors expanded to 6
#' brand_palette(
#'   brand_colors = c("#003366", "#FF6600"),
#'   n_total = 6
#' )
#'
#' @seealso [quick_palette()], [generate_palette()]
#' @export
brand_palette <- function(brand_colors, n_total, cvd_safe = TRUE) {
  if (!is.character(brand_colors) || length(brand_colors) == 0) {
    cli::cli_abort("{.arg brand_colors} must be a non-empty character vector.")
  }

  if (n_total < length(brand_colors)) {
    cli::cli_abort(c(
      "{.arg n_total} ({n_total}) must be >= number of brand colors \\
      ({length(brand_colors)}).",
      "i" = "You have {length(brand_colors)} brand color{?s}, \\
      so {.arg n_total} must be at least {length(brand_colors)}."
    ))
  }

  generate_palette(
    n = n_total,
    include_colors = brand_colors,
    fixed_aesthetic_influence = 0.9, # Strong influence from brand colors
    cvd_safe = cvd_safe,
    progress = FALSE
  )
}


#' Export palette to various formats
#'
#' Export a huerd palette to common formats used in design and development
#' workflows.
#'
#' @param palette A `huerd_palette` object or character vector of hex colors.
#' @param format Output format. One of:
#'   - `"hex"`: Character vector of hex colors (default)
#'   - `"css"`: CSS custom properties (variables)
#'   - `"sass"`: Sass/SCSS variables
#'   - `"json"`: JSON object keyed by color names
#'   - `"csv"`: CSV format with color names
#' @param names Optional character vector of names for the colors. If `NULL`,
#'   colors are named `color_1`, `color_2`, etc.
#' @param file Optional file path to write the output. If `NULL`, returns the
#'   formatted string. If specified, the function writes the formatted palette
#'   to the given file path and (invisibly) returns the file path as a
#'   character string.
#'
#' @return If `file` is `NULL`, returns the formatted palette as a character
#'   string (invisibly for `"hex"`). If `file` is specified, the function
#'   writes the formatted palette to the given file and (invisibly) returns the
#'   file path as a character string.
#' @examples
#' pal <- generate_palette(5, progress = FALSE)
#'
#' # Get as hex vector (default)
#' export_palette(pal)
#'
#' # CSS custom properties
#' export_palette(pal, format = "css")
#'
#' # With custom names
#' export_palette(pal, format = "css",
#'                names = c("primary", "secondary", "accent", "bg", "text"))
#'
#' # JSON for web applications
#' export_palette(pal, format = "json")
#'
#' @export
export_palette <- function(
  palette,
  format = c("hex", "css", "sass", "json", "csv"),
  names = NULL,
  file = NULL
) {
  format <- match.arg(format)
  colors <- as.character(palette)
  n <- length(colors)

  if (is.null(names)) {
    names <- paste0("color_", seq_len(n))
  } else if (length(names) != n) {
    cli::cli_abort(
      "{.arg names} must have length {n} (same as palette)."
    )
  }

  # Validate/sanitize names based on format
  if (format == "json") {
    # Escape JSON special characters in names
    names <- gsub("\\\\", "\\\\\\\\", names) # Escape backslashes first
    # Escape control characters
    names <- gsub("\n", "\\\\n", names)
    names <- gsub("\r", "\\\\r", names)
    names <- gsub("\t", "\\\\t", names)
    names <- gsub("\f", "\\\\f", names)
    names <- gsub('"', '\\\\"', names, fixed = TRUE) # Escape quotes
  } else if (format == "csv") {
    # Escape CSV special characters (quotes and commas)
    names <- gsub('"', '""', names, fixed = TRUE) # Double up quotes
    needs_quoting <- grepl('[",\n\r]', names) | names == ""
    names[needs_quoting] <- paste0('"', names[needs_quoting], '"')
  } else if (format %in% c("css", "sass")) {
    # Validate CSS/Sass variable names
    valid_pattern <- "^[a-zA-Z_][a-zA-Z0-9_-]*$"
    invalid <- !grepl(valid_pattern, names)
    if (any(invalid)) {
      cli::cli_abort(c(
        "{.arg names} must be valid CSS/Sass variable names.",
        "i" = "Invalid names: {.val {names[invalid]}}",
        "x" = "Names must start with a letter or underscore and ",
        "contain only letters, numbers, underscores, and hyphens."
      ))
    }
  }

  output <- switch(
    format,
    "hex" = colors,
    "css" = {
      lines <- paste0("  --", names, ": ", colors, ";")
      paste0(":root {\n", paste(lines, collapse = "\n"), "\n}")
    },
    "sass" = {
      lines <- paste0("$", names, ": ", colors, ";")
      paste(lines, collapse = "\n")
    },
    "json" = {
      # JSON object keyed by color names
      items <- paste0('    "', names, '": "', colors, '"')
      paste0("{\n", paste(items, collapse = ",\n"), "\n}")
    },
    "csv" = {
      lines <- paste0(names, ",", colors)
      paste(c("name,hex", lines), collapse = "\n")
    }
  )

  if (!is.null(file)) {
    writeLines(output, file)
    cli::cli_alert_success("Palette exported to {.file {file}}")
    return(invisible(file))
  }

  if (format == "hex") {
    return(invisible(colors))
  }

  output
}


#' Interpret palette quality in plain language
#'
#' Provides a human-readable assessment of a palette's quality, translating
#' technical metrics into understandable language suitable for presentations
#' or documentation.
#'
#' @param palette A `huerd_palette` object or character vector of hex colors.
#'
#' @return A list with class `huerd_interpretation` containing:
#'   - `summary`: Overall quality assessment
#'   - `distinctness`: How distinct the colors are from each other
#'   - `accessibility`: CVD accessibility assessment
#'   - `recommendations`: Suggestions for improvement (if any)
#'
#' @examples
#' pal <- generate_palette(6, progress = FALSE)
#' interpret_palette_quality(pal)
#'
#' @export
interpret_palette_quality <- function(palette) {
  colors <- as.character(palette)
  n <- length(colors)

  if (n < 2) {
    result <- list(
      summary = "Single color - no comparison possible",
      distinctness = NA_character_,
      accessibility = NA_character_,
      recommendations = character(0)
    )
    class(result) <- c("huerd_interpretation", "list")
    return(result)
  }

  eval <- evaluate_palette(colors)
  recommendations <- character(0)

  # Interpret minimum distance
  min_dist <- eval$distances$min
  distinctness <- if (min_dist >= 0.15) {
    "Excellent - colors are highly distinct and easy to differentiate"
  } else if (min_dist >= 0.10) {
    "Good - colors are reasonably distinct for most uses"
  } else if (min_dist >= 0.06) {
    recommendations <- c(
      recommendations,
      "Consider reducing the number of colors or adjusting the palette"
    )
    "Moderate - some color pairs may be difficult to distinguish"
  } else {
    recommendations <- c(
      recommendations,
      "Palette has very similar colors that may be confused"
    )
    "Limited - colors are quite similar; use with caution"
  }

  # Interpret performance ratio
  perf_ratio <- eval$distances$performance_ratio
  perf_text <- if (perf_ratio >= 0.6) {
    "highly optimized"
  } else if (perf_ratio >= 0.4) {
    "well optimized"
  } else if (perf_ratio >= 0.25) {
    "moderately optimized"
  } else {
    "has room for improvement"
  }

  # Handle NA performance ratio for summary
  percent_text <- if (is.na(perf_ratio)) {
    "not available"
  } else {
    sprintf("%.0f%% of theoretical maximum", perf_ratio * 100)
  }

  # Interpret CVD safety
  cvd_min <- eval$cvd_safety$worst_case_min_distance
  accessibility <- if (is.null(cvd_min) || is.na(cvd_min)) {
    recommendations <- c(
      recommendations,
      "CVD safety could not be assessed"
    )
    "Unknown - could not assess color vision deficiency safety"
  } else if (cvd_min >= 0.10) {
    "Excellent - palette is safe for most color vision deficiencies"
  } else if (cvd_min >= 0.06) {
    "Good - palette should work for most viewers with CVD"
  } else if (cvd_min >= 0.04) {
    recommendations <- c(
      recommendations,
      "Consider testing with CVD simulation before final use"
    )
    "Moderate - some CVD viewers may have difficulty"
  } else {
    recommendations <- c(
      recommendations,
      "Palette may be problematic for colorblind viewers"
    )
    "Limited - colors may be confusing for viewers with CVD"
  }

  # Create summary
  summary <- sprintf(
    "This %d-color palette is %s (%s). %s",
    n,
    perf_text,
    percent_text,
    distinctness
  )

  result <- list(
    summary = summary,
    distinctness = distinctness,
    accessibility = accessibility,
    recommendations = recommendations,
    metrics = list(
      min_distance = min_dist,
      performance_ratio = perf_ratio,
      cvd_worst_case = cvd_min
    )
  )
  class(result) <- c("huerd_interpretation", "list")
  result
}


#' @export
print.huerd_interpretation <- function(x, ...) {
  cli::cli_h2("Palette Quality Assessment")

  cli::cli_text(x$summary)
  cli::cli_text("")

  cli::cli_h3("Distinctness")
  cli::cli_text(x$distinctness)

  cli::cli_h3("Accessibility")
  cli::cli_text(x$accessibility)

  if (length(x$recommendations) > 0) {
    cli::cli_h3("Recommendations")
    cli::cli_ul(x$recommendations)
  }

  invisible(x)
}


#' Plot method for huerd palettes
#'
#' Display a visual representation of a huerd palette. By default shows color
#' swatches; use `type = "analysis"` for the full diagnostic dashboard.
#'
#' @param x A `huerd_palette` object.
#' @param type Type of plot: `"swatches"` for simple color display or
#'   `"analysis"` for the full diagnostic dashboard.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return Invisibly returns the palette.
#'
#' @examples
#' pal <- generate_palette(6, progress = FALSE)
#'
#' # Simple swatch display
#' plot(pal)
#'
#' # Full analysis dashboard
#' plot(pal, type = "analysis")
#'
#' @export
plot.huerd_palette <- function(x, type = c("swatches", "analysis"), ...) {
  type <- match.arg(type)

  if (type == "analysis") {
    plot_palette_analysis(x, ...)
    return(invisible(x))
  }

  # Simple swatch plot
  colors <- as.character(x)
  n <- length(colors)

  if (n == 0) {
    cli::cli_warn("Empty palette - nothing to plot")
    return(invisible(x))
  }

  # Create a simple swatch display using base graphics
  old_par <- graphics::par(mar = c(1, 1, 2, 1))
  on.exit(graphics::par(old_par))

  graphics::image(
    x = seq_len(n),
    y = 1,
    z = matrix(seq_len(n), nrow = n),
    col = colors,
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    main = sprintf("huerd palette (%d colors)", n)
  )

  # Add hex labels below
  graphics::mtext(
    colors,
    side = 1,
    at = seq_len(n),
    las = 2,
    cex = 0.7
  )

  invisible(x)
}

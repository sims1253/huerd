# ggplot2 Scale Functions for huerd Palettes
#
# This file provides ggplot2 integration for huerd palettes, allowing users to
# easily use optimized color palettes in their visualizations.

# Palette generator function for ggplot2 scales
#
# Creates a palette function compatible with ggplot2's discrete_scale().
#
# @param palette Either a huerd_palette object or NULL to generate on-the-fly.
# @param brand_colors Optional colors to include in generated palettes.
# @param ... Additional arguments passed to generate_palette().
# @return A function that takes n and returns n colors.
# @noRd
.huerd_pal <- function(palette = NULL, brand_colors = NULL, ...) {
  function(n) {
    if (!is.null(palette)) {
      # Use provided palette
      colors <- as.character(palette)
      if (length(colors) < n) {
        cli::cli_warn(c(
          "Palette has fewer colors ({length(colors)}) than needed ({n}).",
          "i" = "Generate a larger palette or reduce the number of categories."
        ))
        # Recycle colors if necessary
        colors <- rep_len(colors, n)
      }
      return(colors[seq_len(n)])
    }

    # Generate palette on-the-fly
    generated <- generate_palette(
      n = n,
      include_colors = brand_colors,
      progress = FALSE,
      ...
    )
    as.character(generated)
  }
}


#' Discrete color scale using huerd palettes
#'
#' These scales provide perceptually optimized color palettes for ggplot2
#' visualizations. Colors are generated using minimax optimization in the
#' OKLAB color space to maximize perceptual distinctness.
#'
#' @param palette A `huerd_palette` object (from [generate_palette()]) to use.
#'   If `NULL`, a palette will be generated automatically based on the number
#'   of levels in the data.
#' @param brand_colors Character vector of hex colors that must be included
#'   in the palette. Only used when `palette = NULL`. These colors will be
#'   preserved and additional colors optimized around them.
#' @param ... Additional arguments passed to [generate_palette()] when
#'   generating palettes on-the-fly, or to [ggplot2::discrete_scale()].
#' @param aesthetics Character string or vector of aesthetic names to apply

#'   the scale to. Defaults to `"colour"` for `scale_color_huerd()` and
#'
#'   `"fill"` for `scale_fill_huerd()`.
#' @param na.value Color to use for missing values. Defaults to `"grey50"`.
#'
#' @return A ggplot2 scale object.
#'
#' @details
#' There are two ways to use these scales:
#'
#' 1
#' **Pre-generated palette**: Pass a `huerd_palette` object to the `palette`
#'    argument. This is useful when you want to reuse the same palette across
#'
#'    multiple plots or need fine control over generation parameters.
#'
#' 2. **On-the-fly generation**: Leave `palette = NULL` and the scale will
#'    automatically generate an optimized palette based on the number of
#'    levels in your data. Use `brand_colors` to include specific colors.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'
#'   # Basic usage - automatic palette generation
#'   ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'     geom_point(size = 3) +
#'     scale_color_huerd()
#'
#'   # With brand colors
#'   ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'     geom_point(size = 3) +
#'     scale_color_huerd(brand_colors = c("#1f77b4", "#ff7f0e"))
#'
#'   # Using a pre-generated palette
#'   my_palette <- generate_palette(5, progress = FALSE)
#'   ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
#'     geom_point(size = 3) +
#'     scale_color_huerd(palette = my_palette)
#'
#'   # Fill scale for bar charts
#'   ggplot(mpg, aes(class, fill = class)) +
#'     geom_bar() +
#'     scale_fill_huerd() +
#'     theme(legend.position = "none")
#' }
#'
#' @seealso [generate_palette()] for creating palettes with custom parameters.
#' @export
scale_color_huerd <- function(
  palette = NULL,
  brand_colors = NULL,
  ...,
  aesthetics = "colour",
  na.value = "grey50"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg ggplot2} is required for scale functions.",
      "i" = "Install it with {.code install.packages(\"ggplot2\")}"
    ))
  }

  # Separate ggplot2 scale args from generate_palette args
  scale_args <- list(...)
  generate_args <- list()

  # Known discrete_scale arguments

  scale_arg_names <- c(
    "name",
    "breaks",
    "labels",
    "limits",
    "expand",
    "na.translate",
    "drop",
    "guide",
    "position"
  )

  for (arg_name in names(scale_args)) {
    if (arg_name %in% scale_arg_names) {
      # Keep in scale_args
    } else {
      generate_args[[arg_name]] <- scale_args[[arg_name]]
      scale_args[[arg_name]] <- NULL
    }
  }
  scale_args <- scale_args[!vapply(scale_args, is.null, logical(1))]

  pal_fun <- do.call(
    .huerd_pal,
    c(list(palette = palette, brand_colors = brand_colors), generate_args)
  )

  do.call(
    ggplot2::discrete_scale,
    c(
      list(
        aesthetics = aesthetics,
        palette = pal_fun,
        na.value = na.value
      ),
      scale_args
    )
  )
}


#' @rdname scale_color_huerd
#' @export
scale_colour_huerd <- scale_color_huerd


#' @rdname scale_color_huerd
#' @export
scale_fill_huerd <- function(
  palette = NULL,
  brand_colors = NULL,
  ...,
  aesthetics = "fill",
  na.value = "grey50"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg ggplot2} is required for scale functions.",
      "i" = "Install it with {.code install.packages(\"ggplot2\")}"
    ))
  }

  # Separate ggplot2 scale args from generate_palette args
  scale_args <- list(...)
  generate_args <- list()

  scale_arg_names <- c(
    "name",
    "breaks",
    "labels",
    "limits",
    "expand",
    "na.translate",
    "drop",
    "guide",
    "position"
  )

  for (arg_name in names(scale_args)) {
    if (arg_name %in% scale_arg_names) {
      # Keep in scale_args
    } else {
      generate_args[[arg_name]] <- scale_args[[arg_name]]
      scale_args[[arg_name]] <- NULL
    }
  }
  scale_args <- scale_args[!vapply(scale_args, is.null, logical(1))]

  pal_fun <- do.call(
    .huerd_pal,
    c(list(palette = palette, brand_colors = brand_colors), generate_args)
  )

  do.call(
    ggplot2::discrete_scale,
    c(
      list(
        aesthetics = aesthetics,
        palette = pal_fun,
        na.value = na.value
      ),
      scale_args
    )
  )
}

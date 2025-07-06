# Evaluate palette quality metrics for post-hoc analysis
# Pure data provider without subjective heuristic scoring

#' Evaluate Palette Quality
#'
#' Provides a comprehensive evaluation of a color palette's perceptual properties,
#' including its distinguishability, CVD safety, and color distribution.
#' Returns raw metrics without subjective scoring for post-hoc analysis.
#'
#' @param colors A character vector of hex colors, or a matrix of colors in OK LAB space.
#' @return A list of evaluation metrics with class `huerd_evaluation`.
#'         Contains raw metrics including distances, CVD safety, and distribution
#'         for objective analysis without subjective heuristic scoring.
#' @export
#' @examples
#' pal <- generate_palette(5, progress = FALSE)
#' metrics <- evaluate_palette(pal)
#' print(metrics) # Uses custom print method
#'
#' # The performance_ratio compares the achieved min distance to an estimated maximum
#' # metrics$distances$performance_ratio
evaluate_palette <- function(colors) {
  # --- Input Validation and Pre-processing ---
  if (is.character(colors)) {
    if (length(colors) == 0) {
      oklab_colors <- matrix(
        numeric(0),
        ncol = 3,
        dimnames = list(NULL, c("L", "a", "b"))
      )
    } else {
      if (
        !all(
          grepl("^#[0-9A-Fa-f]{6}$|^NA$", colors, ignore.case = TRUE) |
            is.na(colors)
        )
      ) {
        stop(
          "If 'colors' is character, all elements must be valid hex codes or NA."
        )
      }
      valid_colors <- colors[!is.na(colors)]
      if (length(valid_colors) == 0) {
        oklab_colors <- matrix(numeric(0), ncol = 3)
      } else {
        oklab_colors <- .hex_to_oklab(valid_colors)
      }
    }
  } else if (is.matrix(colors) && ncol(colors) == 3) {
    oklab_colors <- colors
  } else if (
    is.matrix(colors) &&
      nrow(colors) == 0 &&
      (ncol(colors) == 0 || ncol(colors) == 3)
  ) {
    oklab_colors <- matrix(
      numeric(0),
      ncol = 3,
      dimnames = list(NULL, c("L", "a", "b"))
    )
  } else {
    stop(
      "colors must be a character vector of hex colors or an OKLAB matrix (N x 3 or 0x0/0x3)."
    )
  }

  result <- evaluate_palette_quality(oklab_colors)
  class(result) <- c("huerd_evaluation", "list")
  return(result)
}


#' Internal Palette Quality Evaluation
#' @noRd
evaluate_palette_quality <- function(oklab_colors) {
  n <- nrow(oklab_colors)

  # Use NA_real_ for metrics that cannot be computed
  default_dist_stats <- list(
    min = NA_real_,
    mean = NA_real_,
    median = NA_real_,
    sd = NA_real_,
    q25 = NA_real_,
    q75 = NA_real_,
    bottom_quartile_mean = NA_real_,
    estimated_max = NA_real_,
    performance_ratio = NA_real_
  )
  default_cvd_stats <- list(
    protan = list(
      min_distance = NA_real_,
      mean_distance = NA_real_,
      preserved_ratio = NA_real_,
      hue_order_preserved = TRUE
    ),
    deutan = list(
      min_distance = NA_real_,
      mean_distance = NA_real_,
      preserved_ratio = NA_real_,
      hue_order_preserved = TRUE
    ),
    tritan = list(
      min_distance = NA_real_,
      mean_distance = NA_real_,
      preserved_ratio = NA_real_,
      hue_order_preserved = TRUE
    ),
    worst_case_min_distance = NA_real_
  )

  if (n < 2) {
    dist_stats <- default_dist_stats
    cvd_stats <- default_cvd_stats
  } else {
    dist_matrix <- calculate_perceptual_distances(oklab_colors)
    distances <- dist_matrix[upper.tri(dist_matrix)]
    distances_finite <- distances[is.finite(distances)]

    if (length(distances_finite) == 0) {
      dist_stats <- default_dist_stats
    } else {
      dist_quantiles <- stats::quantile(
        distances_finite,
        probs = c(0.25, 0.75),
        na.rm = TRUE
      )
      dist_stats <- list(
        min = min(distances_finite),
        mean = mean(distances_finite),
        median = stats::median(distances_finite),
        sd = if (length(distances_finite) > 1) {
          stats::sd(distances_finite)
        } else {
          NA_real_
        },
        q25 = dist_quantiles[1],
        q75 = dist_quantiles[2],
        bottom_quartile_mean = if (
          length(distances_finite) > 0 && is.finite(dist_quantiles[1])
        ) {
          mean(
            distances_finite[distances_finite <= dist_quantiles[1]],
            na.rm = TRUE
          )
        } else {
          NA_real_
        },
        estimated_max = NA_real_,
        performance_ratio = NA_real_ # Placeholders
      )
    }
    cvd_stats <- analyze_cvd_safety_metrics(
      oklab_colors,
      dist_stats$min %||% NA_real_
    )
  }

  # --- Add Optimizer Performance Ratio ---
  estimated_max <- .get_estimated_max_dist(n) # Helper to get value from lookup
  if (
    is.finite(dist_stats$min) && is.finite(estimated_max) && estimated_max > 0
  ) {
    dist_stats$performance_ratio <- dist_stats$min / estimated_max
    dist_stats$estimated_max <- estimated_max
  }

  distribution_stats <- analyze_color_distribution(oklab_colors)

  return(list(
    n_colors = n,
    distances = dist_stats,
    cvd_safety = cvd_stats,
    distribution = distribution_stats
  ))
}

#' Analyze CVD Safety Metrics (for evaluation)
#'
#' This is the **complete implementation**. It simulates CVD and calculates
#' perceptual distances in the simulated space.
#' @noRd
analyze_cvd_safety_metrics <- function(oklab_colors, original_min_distance) {
  na_cvd_result <- list(
    protan = list(
      min_distance = NA_real_,
      mean_distance = NA_real_,
      preserved_ratio = NA_real_,
      hue_order_preserved = TRUE
    ),
    deutan = list(
      min_distance = NA_real_,
      mean_distance = NA_real_,
      preserved_ratio = NA_real_,
      hue_order_preserved = TRUE
    ),
    tritan = list(
      min_distance = NA_real_,
      mean_distance = NA_real_,
      preserved_ratio = NA_real_,
      hue_order_preserved = TRUE
    ),
    worst_case_min_distance = NA_real_
  )

  if (nrow(oklab_colors) < 2) {
    return(na_cvd_result)
  }

  # Convert OKLAB to hex colors for CVD simulation
  hex_colors <- .oklab_to_hex(oklab_colors)

  if (any(is.na(hex_colors))) {
    warning(
      "CVD metrics: Some input colors are outside sRGB gamut; results may be NA.",
      call. = FALSE
    )
    return(na_cvd_result)
  }

  cvd_types <- c("protan", "deutan", "tritan")
  results <- list()
  all_cvd_min_distances <- rep(NA_real_, length(cvd_types))
  
  # Store original lightness order for comparison
  original_lightness_order <- order(oklab_colors[, 1])

  for (i in seq_along(cvd_types)) {
    cvd_type <- cvd_types[i]

    # Use colorspace functions for CVD simulation
    cvd_fun <- switch(
      cvd_type,
      "protan" = colorspace::protan,
      "deutan" = colorspace::deutan,
      "tritan" = colorspace::tritan
    )
    cvd_hex_sim <- cvd_fun(hex_colors, severity = 1.0)

    # Convert back to OKLAB for distance calculations
    cvd_oklab_sim <- .hex_to_oklab(cvd_hex_sim)

    if (!is.matrix(cvd_oklab_sim) || nrow(cvd_oklab_sim) < 2) {
      current_min_dist_cvd <- NA_real_
      current_mean_dist_cvd <- NA_real_
      hue_order_preserved <- TRUE  # Default to TRUE for insufficient data
    } else {
      dist_matrix_cvd <- calculate_perceptual_distances(cvd_oklab_sim)
      distances_cvd_finite <- dist_matrix_cvd[upper.tri(dist_matrix_cvd)]
      distances_cvd_finite <- distances_cvd_finite[is.finite(
        distances_cvd_finite
      )]
      current_min_dist_cvd <- if (length(distances_cvd_finite) > 0) {
        min(distances_cvd_finite)
      } else {
        NA_real_
      }
      current_mean_dist_cvd <- if (length(distances_cvd_finite) > 0) {
        mean(distances_cvd_finite)
      } else {
        NA_real_
      }
      
      # Check hue order reversal using lightness-based ordering
      cvd_lightness_order <- order(cvd_oklab_sim[, 1])
      hue_order_preserved <- identical(original_lightness_order, cvd_lightness_order)
    }
    all_cvd_min_distances[i] <- current_min_dist_cvd

    preserved_ratio <- if (
      is.finite(original_min_distance) &&
        original_min_distance > 1e-6 && # OKLAB tolerance for meaningful distance
        is.finite(current_min_dist_cvd)
    ) {
      current_min_dist_cvd / original_min_distance
    } else {
      NA_real_
    }

    results[[cvd_type]] <- list(
      min_distance = current_min_dist_cvd,
      mean_distance = current_mean_dist_cvd,
      preserved_ratio = preserved_ratio,
      hue_order_preserved = hue_order_preserved
    )
  }

  finite_min_distances <- all_cvd_min_distances[is.finite(
    all_cvd_min_distances
  )]
  results$worst_case_min_distance <- if (length(finite_min_distances) > 0) {
    min(finite_min_distances)
  } else {
    NA_real_
  }
  return(results)
}

#' Analyze Color Distribution Metrics
#'
#' This is the **complete implementation**. It calculates statistics for
#' Lightness, Chroma, and Hue.
#' @noRd
analyze_color_distribution <- function(oklab_colors) {
  n <- nrow(oklab_colors)
  na_range <- c(NA_real_, NA_real_)

  if (n == 0 || any(is.na(oklab_colors))) {
    return(list(
      lightness_oklab = list(range = na_range, mean = NA_real_, sd = NA_real_),
      chroma_oklab = list(range = na_range, mean = NA_real_, sd = NA_real_),
      hue_oklab = list(circular_variance = NA_real_, range_degrees = na_range)
    ))
  }

  L_values <- oklab_colors[, 1]
  chroma <- sqrt(oklab_colors[, 2]^2 + oklab_colors[, 3]^2)
  hue_rad <- atan2(oklab_colors[, 3], oklab_colors[, 2])
  hue_deg <- (hue_rad * 180 / pi) %% 360

  circ_var <- if (n > 1) {
    mean_cos_hue <- mean(cos(hue_rad), na.rm = TRUE)
    mean_sin_hue <- mean(sin(hue_rad), na.rm = TRUE)
    if (is.finite(mean_cos_hue) && is.finite(mean_sin_hue)) {
      1 - sqrt(mean_cos_hue^2 + mean_sin_hue^2)
    } else {
      NA_real_
    }
  } else {
    0 # No variance for a single point
  }

  return(list(
    lightness_oklab = list(
      range = if (n > 0) range(L_values, na.rm = T) else na_range,
      mean = if (n > 0) mean(L_values, na.rm = T) else NA_real_,
      sd = if (n > 1) stats::sd(L_values, na.rm = T) else NA_real_
    ),
    chroma_oklab = list(
      range = if (n > 0) range(chroma, na.rm = T) else na_range,
      mean = if (n > 0) mean(chroma, na.rm = T) else NA_real_,
      sd = if (n > 1) stats::sd(chroma, na.rm = T) else NA_real_
    ),
    hue_oklab = list(
      circular_variance = circ_var,
      range_degrees = if (n > 0) range(hue_deg, na.rm = T) else na_range
    )
  ))
}

#' Helper to get estimated max distance from internal lookup table
#' @noRd
.get_estimated_max_dist <- function(n) {
  # Better input validation from utils.R version
  if (!is.numeric(n) || length(n) != 1 || n < 2) {
    return(NA_real_)
  }

  # Access the internal data object
  # The names of the vector are characters "2", "3", etc.
  n_char <- as.character(n)

  if (!n_char %in% names(MAX_MIN_DIST_ESTIMATES)) {
    return(NA_real_)
  }

  dist_val <- MAX_MIN_DIST_ESTIMATES[n_char]
  return(unname(dist_val))
}

# The `%||%` helper would typically live in R/utils.R or R/helpers_core.R
# `%||%` <- function(x, y) if (is.null(x) || !is.finite(x) || is.na(x)) y else x

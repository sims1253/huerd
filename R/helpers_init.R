#' Calculate Aesthetic Profile from Fixed Colors
#'
#' Computes the mean and standard deviation of Lightness (L) and Chroma (C)
#' from a set of fixed colors in OKLAB space.
#' @param fixed_oklab A matrix of fixed colors in OKLAB space, or NULL.
#' @return A list with `mean_L`, `sd_L`, `mean_C`, and `sd_C`. Returns `NA` for all
#'   if input is NULL or has no rows.
#' @noRd
.calculate_aesthetic_profile <- function(fixed_oklab) {
  if (is.null(fixed_oklab) || nrow(fixed_oklab) < 1) {
    return(list(
      mean_L = NA_real_,
      sd_L = NA_real_,
      mean_C = NA_real_,
      sd_C = NA_real_
    ))
  }

  fixed_L <- fixed_oklab[, 1]
  fixed_C <- sqrt(fixed_oklab[, 2]^2 + fixed_oklab[, 3]^2)

  # Use a small fallback SD if there's only one color or no variance
  sd_L <- if (nrow(fixed_oklab) > 1) stats::sd(fixed_L, na.rm = TRUE) else 0.1
  sd_C <- if (nrow(fixed_oklab) > 1) stats::sd(fixed_C, na.rm = TRUE) else 0.05

  list(
    mean_L = mean(fixed_L, na.rm = TRUE),
    sd_L = sd_L %||% 0.1,
    mean_C = mean(fixed_C, na.rm = TRUE),
    sd_C = sd_C %||% 0.05
  )
}

#' Adapt Initialization Parameters based on Aesthetic Profile
#'
#' Blends user-provided/default initialization bounds with bounds derived from
#' the aesthetic profile of fixed colors.
#' @param aesthetic_profile A list from `.calculate_aesthetic_profile`.
#' @param influence Numeric (0-1), `fixed_aesthetic_influence` parameter.
#' @param init_lightness_bounds,init_hcl_bounds The base bounds from user.
#' @param config The merged aesthetic initialization configuration list.
#' @param progress Logical, whether to show `cli` messages.
#' @return A list containing adapted `lightness_bounds`, `hcl_bounds`, and
#'   `kmeans_chroma_filter_params`.
#' @noRd
.adapt_init_params <- function(
  aesthetic_profile,
  influence,
  init_lightness_bounds,
  init_hcl_bounds,
  config,
  progress
) {
  adapted_lightness_bounds <- init_lightness_bounds
  adapted_hcl_bounds <- init_hcl_bounds
  kmeans_chroma_filter <- list(apply_filter = FALSE)

  if (influence > 0 && is.finite(aesthetic_profile$mean_L)) {
    if (progress) {
      cat("Adapting initialization from fixed colors' aesthetics...\n")
    }

    # --- Adapt k-means++ OKLAB L-bounds using mean +/- (sd * multiplier) ---
    l_window_half <- (aesthetic_profile$sd_L %||% 0.1) *
      config$kmeans_L_sd_multiplier
    derived_L_min <- max(0.01, aesthetic_profile$mean_L - l_window_half)
    derived_L_max <- min(0.99, aesthetic_profile$mean_L + l_window_half)
    if (derived_L_min >= derived_L_max) {
      derived_L_min <- init_lightness_bounds[1]
      derived_L_max <- init_lightness_bounds[2]
    }

    final_L_min <- (1 - influence) *
      init_lightness_bounds[1] +
      influence * derived_L_min
    final_L_max <- (1 - influence) *
      init_lightness_bounds[2] +
      influence * derived_L_max
    adapted_lightness_bounds <- c(
      max(0.01, final_L_min),
      min(0.99, final_L_max)
    )
    if (adapted_lightness_bounds[1] >= adapted_lightness_bounds[2]) {
      adapted_lightness_bounds <- init_lightness_bounds
    }

    # --- Adapt k-means++ OKLAB C-filter using mean and sd ---
    max_C_dev_from_mean <- (aesthetic_profile$sd_C %||% 0.05) +
      config$kmeans_C_base_deviation
    # `influence` makes the allowed deviation smaller
    max_C_dev_final <- max_C_dev_from_mean *
      (1 - config$kmeans_C_influence_tightening_factor * influence)

    kmeans_chroma_filter <- list(
      apply_filter = TRUE,
      target_C_mean = aesthetic_profile$mean_C,
      max_C_deviation = max(0.01, max_C_dev_final), # Ensure some minimal deviation is allowed
      relaxation_factor = config$kmeans_C_filter_relaxation_factor
    )

    # --- Adapt harmony HCL L/C bounds ---
    # Convert fixed OKLAB profile to HCL for harmony init adaptation
    # This requires converting mean L/C, which is a simplification. A more accurate way
    # is to convert the original fixed colors to HCL space and get mean/sd there.
    # We will do that for robustness.
    fixed_oklab_for_hcl <- cbind(
      L = aesthetic_profile$mean_L,
      a = aesthetic_profile$mean_C, # Simplification: treat mean C as `a` coord
      b = 0
    )
    temp_lab_fixed <- farver::convert_colour(
      fixed_oklab_for_hcl,
      from = "oklab",
      to = "lab"
    )
    temp_hcl_fixed <- farver::convert_colour(
      temp_lab_fixed,
      from = "lab",
      to = "hcl"
    )

    mean_L_fixed_hcl <- temp_hcl_fixed[, "l"]
    sd_L_fixed_hcl <- config$harmony_hcl_sd_fallback *
      config$harmony_hcl_sd_multiplier

    derived_L_min_hcl <- max(5, mean_L_fixed_hcl - sd_L_fixed_hcl)
    derived_L_max_hcl <- min(95, mean_L_fixed_hcl + sd_L_fixed_hcl)
    if (
      derived_L_min_hcl >= derived_L_max_hcl || !is.finite(derived_L_min_hcl)
    ) {
      derived_L_min_hcl = init_hcl_bounds$L[1]
      derived_L_max_hcl = init_hcl_bounds$L[2]
    }

    mean_C_fixed_hcl <- temp_hcl_fixed[, "c"]
    sd_C_fixed_hcl <- config$harmony_hcl_sd_fallback *
      config$harmony_hcl_sd_multiplier
    derived_C_min_hcl <- max(10, mean_C_fixed_hcl - sd_C_fixed_hcl)
    derived_C_max_hcl <- min(120, mean_C_fixed_hcl + sd_C_fixed_hcl)
    if (
      derived_C_min_hcl >= derived_C_max_hcl || !is.finite(derived_C_min_hcl)
    ) {
      derived_C_min_hcl = init_hcl_bounds$C[1]
      derived_C_max_hcl = init_hcl_bounds$C[2]
    }

    final_L_min_hcl <- (1 - influence) *
      init_hcl_bounds$L[1] +
      influence * derived_L_min_hcl
    final_L_max_hcl <- (1 - influence) *
      init_hcl_bounds$L[2] +
      influence * derived_L_max_hcl
    current_L_hcl <- c(max(5, final_L_min_hcl), min(95, final_L_max_hcl))
    if (
      current_L_hcl[1] >= current_L_hcl[2] || !all(is.finite(current_L_hcl))
    ) {
      current_L_hcl <- init_hcl_bounds$L
    }

    final_C_min_hcl <- (1 - influence) *
      init_hcl_bounds$C[1] +
      influence * derived_C_min_hcl
    final_C_max_hcl <- (1 - influence) *
      init_hcl_bounds$C[2] +
      influence * derived_C_max_hcl
    current_C_hcl <- c(max(10, final_C_min_hcl), min(120, final_C_max_hcl))
    if (
      current_C_hcl[1] >= current_C_hcl[2] || !all(is.finite(current_C_hcl))
    ) {
      current_C_hcl <- init_hcl_bounds$C
    }

    adapted_hcl_bounds <- list(L = current_L_hcl, C = current_C_hcl)
  }

  list(
    lightness_bounds = adapted_lightness_bounds,
    hcl_bounds = adapted_hcl_bounds,
    kmeans_chroma_filter_params = kmeans_chroma_filter
  )
}

#' Initialize Free Colors Using Specified Method
#'
#' A dispatcher for different initialization strategies.
#' @param n_free Number of colors to generate.
#' @param fixed_colors_oklab OKLAB matrix of fixed colors.
#' @param method "k-means++" or "harmony".
#' @param adapted_init_params List of adapted bounds from `.adapt_init_params`.
#' @param base_init_lightness_bounds Original user-provided L-bounds for fallback.
#' @return A matrix of initialized free colors in OKLAB space.
#' @noRd
initialize_colors <- function(
  n_free,
  fixed_colors_oklab,
  method,
  adapted_init_params,
  base_init_lightness_bounds
) {
  if (n_free == 0) {
    return(matrix(
      numeric(0),
      ncol = 3,
      dimnames = list(NULL, c("L", "a", "b"))
    ))
  }

  if (method == "k-means++") {
    return(initialize_kmeans_plus_plus(
      n_free,
      fixed_colors_oklab,
      adapted_init_params$lightness_bounds,
      adapted_init_params$kmeans_chroma_filter_params,
      base_init_lightness_bounds
    ))
  } else if (method == "harmony") {
    return(initialize_harmony_based(
      n_free,
      fixed_colors_oklab,
      adapted_init_params$hcl_bounds
    ))
  }
}

#' Initialize Using k-means++ Algorithm (Adapted)
#' @noRd
initialize_kmeans_plus_plus <- function(
  n_free,
  fixed_colors_oklab,
  lightness_bounds,
  chroma_filter_params,
  base_init_lightness_bounds
) {
  oklab_ab_gen_bounds <- list(a = c(-0.4, 0.4), b = c(-0.4, 0.4))
  # Calculate initial candidate pool size: base pool size or proportional to free colors
  candidate_pool_base <- 2000
  candidate_multiplier <- 200
  n_candidates_initial_pool <- max(
    candidate_pool_base,
    n_free * candidate_multiplier
  )

  generate_candidates <- function(num_cand, l_b, ab_b) {
    matrix(
      c(
        stats::runif(num_cand, l_b[1], l_b[2]),
        stats::runif(num_cand, ab_b$a[1], ab_b$a[2]),
        stats::runif(num_cand, ab_b$b[1], ab_b$b[2])
      ),
      ncol = 3,
      dimnames = list(NULL, c("L", "a", "b"))
    )
  }

  candidates <- generate_candidates(
    n_candidates_initial_pool,
    lightness_bounds,
    oklab_ab_gen_bounds
  )

  # Check gamut validity by attempting to encode to hex
  # Invalid/out-of-gamut colors will result in NA hex values
  hex_candidates <- .oklab_to_hex(candidates)
  valid_gamut <- !is.na(hex_candidates)
  candidates <- candidates[valid_gamut, , drop = FALSE]

  # Check if we need more candidates: require at least 5x free colors and 100 minimum
  candidate_density_factor <- 5
  min_candidate_threshold <- 100
  if (
    nrow(candidates) < n_free * candidate_density_factor &&
      nrow(candidates) < min_candidate_threshold
  ) {
    cat(
      "Info: Gamut filter reduced candidates. Broadening L-bounds for candidate generation using base init_lightness_bounds.\n"
    )
    broader_l_bounds <- base_init_lightness_bounds
    candidates_fallback <- generate_candidates(
      n_candidates_initial_pool,
      broader_l_bounds,
      oklab_ab_gen_bounds
    )
    # Use round-trip conversion to detect true gamut membership
    # farver clamps out-of-gamut colors, so RGB bounds checking is ineffective
    rgb_fb <- farver::convert_colour(
      candidates_fallback,
      from = "oklab",
      to = "rgb"
    )
    oklab_fb_roundtrip <- farver::convert_colour(
      rgb_fb,
      from = "rgb",
      to = "oklab"
    )
    # OKLAB tolerance for gamut validation via round-trip conversion
    tolerance <- 1e-6
    valid_fb <- !is.na(rgb_fb[, 1]) &
      abs(candidates_fallback[, 1] - oklab_fb_roundtrip[, 1]) < tolerance &
      abs(candidates_fallback[, 2] - oklab_fb_roundtrip[, 2]) < tolerance &
      abs(candidates_fallback[, 3] - oklab_fb_roundtrip[, 3]) < tolerance
    candidates <- candidates_fallback[valid_fb, , drop = FALSE]
  }

  if (chroma_filter_params$apply_filter && nrow(candidates) > 0) {
    cand_C <- sqrt(candidates[, 2]^2 + candidates[, 3]^2)
    target_C <- chroma_filter_params$target_C_mean
    max_dev <- chroma_filter_params$max_C_deviation

    valid_chroma <- abs(cand_C - target_C) <= max_dev

    if (
      sum(valid_chroma) < n_free && sum(valid_chroma) < nrow(candidates) * 0.1
    ) {
      cat("Info: Chroma filter is very restrictive. Relaxing it slightly.\n")
      max_dev_relaxed <- max_dev * chroma_filter_params$relaxation_factor
      valid_chroma <- abs(cand_C - target_C) <= max_dev_relaxed
    }
    candidates <- candidates[valid_chroma, , drop = FALSE]
  }

  if (nrow(candidates) == 0 && n_free > 0) {
    cat(
      "Warning: No valid candidate colors found after all filters. Returning empty set.\n"
    )
    return(matrix(
      numeric(0),
      ncol = 3,
      dimnames = list(NULL, c("L", "a", "b"))
    ))
  }

  current_centers_oklab <- fixed_colors_oklab
  if (is.null(fixed_colors_oklab) && n_free > 0) {
    first_idx <- sample.int(nrow(candidates), 1)
    current_centers_oklab <- candidates[first_idx, , drop = FALSE]
    candidates <- candidates[-first_idx, , drop = FALSE]
  }

  newly_selected_centers <- matrix(
    numeric(0),
    ncol = 3,
    dimnames = list(NULL, c("L", "a", "b"))
  )

  for (i in seq_len(n_free)) {
    if (nrow(candidates) == 0) {
      break
    }

    min_dists_sq <- apply(candidates, 1, function(cand_point) {
      dist_sq_vec <- apply(current_centers_oklab, 1, function(center_row) {
        sum((cand_point - center_row)^2)
      })
      min(dist_sq_vec)
    })

    probs <- min_dists_sq
    if (sum(probs, na.rm = TRUE) == 0 || !any(is.finite(probs))) {
      next_idx <- sample.int(nrow(candidates), 1)
    } else {
      probs[!is.finite(probs)] <- 0
      next_idx <- sample.int(nrow(candidates), 1, prob = probs)
    }

    new_center <- candidates[next_idx, , drop = FALSE]
    newly_selected_centers <- rbind(newly_selected_centers, new_center)
    current_centers_oklab <- rbind(current_centers_oklab, new_center)
    candidates <- candidates[-next_idx, , drop = FALSE]
  }

  return(newly_selected_centers)
}

#' Initialize Using Color Harmony Principles
#' @noRd
initialize_harmony_based <- function(n_free, fixed_colors_oklab, hcl_bounds) {
  if (is.null(fixed_colors_oklab) || nrow(fixed_colors_oklab) == 0) {
    hcl_colors_init <- cbind(
      h = seq(0, 359, length.out = n_free + 1)[seq_len(n_free)],
      c = stats::runif(n_free, hcl_bounds$C[1], hcl_bounds$C[2]),
      l = stats::runif(n_free, hcl_bounds$L[1], hcl_bounds$L[2])
    )
    oklab_colors <- farver::convert_colour(
      hcl_colors_init,
      from = "hcl",
      to = "oklab"
    )
    colnames(oklab_colors) <- c("L", "a", "b")
    return(oklab_colors)
  }

  lab_fixed <- farver::convert_colour(
    fixed_colors_oklab,
    from = "oklab",
    to = "lab"
  )
  hcl_fixed <- farver::convert_colour(lab_fixed, from = "lab", to = "hcl")
  fixed_hues <- hcl_fixed[, "h"]

  new_hues <- numeric(n_free)
  current_hues_sorted <- sort(unique(fixed_hues %% 360))

  if (n_free == 1 && length(current_hues_sorted) == 1) {
    new_hues[1] <- (current_hues_sorted[1] + 180) %% 360
  } else if (n_free == 2 && length(current_hues_sorted) == 1) {
    new_hues[1] <- (current_hues_sorted[1] + 120) %% 360
    new_hues[2] <- (current_hues_sorted[1] + 240) %% 360
  } else {
    all_hues_for_gaps <- current_hues_sorted
    for (i in 1:n_free) {
      if (length(all_hues_for_gaps) == 0) {
        all_hues_for_gaps <- c(stats::runif(1, 0, 359))
      }

      num_hues_for_gaps <- length(all_hues_for_gaps)
      gaps_calc <- if (num_hues_for_gaps == 1) {
        c(360)
      } else {
        diff(c(all_hues_for_gaps, all_hues_for_gaps[1] + 360))
      }
      gap_starts_calc <- if (num_hues_for_gaps == 1) {
        all_hues_for_gaps[1]
      } else {
        all_hues_for_gaps
      }

      max_gap_idx <- which.max(gaps_calc)
      if (
        length(max_gap_idx) == 0 || max_gap_idx[1] > length(gap_starts_calc)
      ) {
        max_gap_idx <- 1
      }

      chosen_gap_start <- gap_starts_calc[max_gap_idx[1]]
      chosen_gap_size <- gaps_calc[max_gap_idx[1]]

      new_hue_val <- (chosen_gap_start + chosen_gap_size / 2) %% 360
      new_hues[i] <- new_hue_val
      all_hues_for_gaps <- sort(unique(c(all_hues_for_gaps, new_hue_val)))
    }
  }

  new_hcl <- cbind(
    h = new_hues,
    c = stats::runif(n_free, hcl_bounds$C[1], hcl_bounds$C[2]),
    l = stats::runif(n_free, hcl_bounds$L[1], hcl_bounds$L[2])
  )

  lab_colors_new <- farver::convert_colour(new_hcl, from = "hcl", to = "lab")
  oklab_colors_new <- farver::convert_colour(
    lab_colors_new,
    from = "lab",
    to = "oklab"
  )
  colnames(oklab_colors_new) <- c("L", "a", "b")
  return(oklab_colors_new)
}

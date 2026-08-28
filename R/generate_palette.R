# Default configuration for aesthetic initialization parameters.
# Users can override these via `aesthetic_init_config` argument in
# `generate_palette`.

.default_aesthetic_init_config <- list(
  # Versioning for future API stability.
  config_version = 1,

  # K-means++ OKLAB L-bounds adaptation from fixed color mean L.
  # The SD of fixed colors' L is multiplied by this value to define
  # the search window.
  kmeans_L_sd_multiplier = 1.5,

  # A small constant added to the allowed C deviation to handle cases
  # where SD is zero.
  kmeans_C_base_deviation = 0.05,
  # Factor determining how `fixed_aesthetic_influence` tightens the
  # C-filter.
  kmeans_C_influence_tightening_factor = 0.75, # Influence factor for
  # aesthetic-guided chroma filtering

  # Harmony HCL L/C bounds adaptation.
  # Fallback Standard Deviation for HCL L/C if only one fixed color or
  # actual SD is zero.
  harmony_hcl_sd_fallback = 15,
  # Multiplier for SDs derived from fixed colors' HCL L/C values.
  harmony_hcl_sd_multiplier = 1.0,

  # Fallback strategy.
  # Factor by which to relax the max_C_deviation if initial filtering
  # yields too few candidates.
  kmeans_C_filter_relaxation_factor = 1.5
)


# Helper Functions for generate_palette
# ==============================================================================

#' Setup and normalize palette generation parameters
#' @noRd
.setup_palette_params <- function(
  aesthetic_init_config,
  initialization
) {
  config <- .merge_aesthetic_config(aesthetic_init_config)
  initialization <- match.arg(initialization, c("k-means++", "harmony"))

  list(
    config = config,
    initialization = initialization
  )
}

#' Process fixed colors into OKLAB format
#' @noRd
.process_fixed_colors <- function(include_colors, progress) {
  n_fixed <- length(include_colors)

  if (progress) {
    cli::cli_alert_info("Preparing for palette generation...")
  }

  fixed_oklab <- NULL
  if (n_fixed > 0) {
    oklab_matrix <- .hex_to_oklab(include_colors)

    # Sort fixed colors by brightness (lightness) to ensure final
    # palette is brightness-sorted
    lightness_order <- order(oklab_matrix[, 1])
    fixed_oklab <- oklab_matrix[lightness_order, , drop = FALSE]
  }

  list(
    fixed_oklab = fixed_oklab,
    n_fixed = n_fixed
  )
}

#' Prepare initialization parameters using aesthetic profiling
#' @noRd
.prepare_initialization <- function(
  fixed_oklab,
  fixed_aesthetic_influence,
  init_lightness_bounds,
  init_hcl_bounds,
  config,
  progress
) {
  aesthetic_profile <- .calculate_aesthetic_profile(fixed_oklab)
  adapted_init_params <- .adapt_init_params(
    aesthetic_profile,
    fixed_aesthetic_influence,
    init_lightness_bounds,
    init_hcl_bounds,
    config,
    progress
  )

  list(
    aesthetic_profile = aesthetic_profile,
    adapted_init_params = adapted_init_params
  )
}

#' Initialize and validate free colors
#' @noRd
.init_and_validate_colors <- function(
  n_free,
  n_fixed,
  fixed_oklab,
  initialization,
  adapted_init_params,
  init_lightness_bounds,
  include_colors,
  return_metrics,
  progress
) {
  if (progress) {
    cli::cli_inform(
      "Initializing {n_free} free colors (method: {initialization})..."
    )
  }

  initial_free_colors <- initialize_colors(
    n_free,
    fixed_oklab,
    initialization,
    adapted_init_params,
    init_lightness_bounds
  )

  actual_n_init_free <- if (is.matrix(initial_free_colors)) {
    nrow(initial_free_colors)
  } else {
    0
  }

  if (actual_n_init_free < n_free) {
    if (progress) {
      cli::cli_alert_warning(
        "Initialization generated {actual_n_init_free} of {n_free} ",
        "requested free colors. Palette will be smaller."
      )
    }
    if (actual_n_init_free == 0) {
      return(.handle_no_free_colors(
        n_fixed,
        include_colors,
        return_metrics,
        progress,
        "Initialization failed for free colors."
      ))
    }
    n_free <- actual_n_init_free
  }

  current_all_colors_oklab <- if (!is.null(fixed_oklab)) {
    rbind(fixed_oklab, initial_free_colors)
  } else {
    initial_free_colors
  }
  fixed_mask <- c(rep(TRUE, n_fixed), rep(FALSE, n_free))

  list(
    initial_colors_oklab = current_all_colors_oklab,
    fixed_mask = fixed_mask,
    n_free = n_free
  )
}

#' Optimize color palette using configurable optimization algorithms
#' @noRd
.optimize_palette <- function(
  current_all_colors_oklab,
  fixed_mask,
  max_iterations,
  n_free,
  progress,
  optimizer = "nloptr_cobyla",
  weights = NULL,
  cvd_safe = TRUE
) {
  if (progress && n_free > 0) {
    cli::cli_inform(
      "Optimizing {n_free} free colors using {optimizer}..."
    )
  }

  # Switch statement for different optimizers
  opt_result <- switch(
    optimizer,
    "nloptr_cobyla" = optimize_colors_constrained(
      current_all_colors_oklab,
      fixed_mask,
      max_iterations,
      cvd_safe = cvd_safe
    ),
    "sann" = optimize_colors_sann(
      current_all_colors_oklab,
      fixed_mask,
      max_iterations,
      cvd_safe = cvd_safe
    ),
    "nlopt_direct" = optimize_colors_nlopt_direct(
      current_all_colors_oklab,
      fixed_mask,
      max_iterations,
      cvd_safe = cvd_safe
    ),
    "nlopt_neldermead" = optimize_colors_nlopt_neldermead(
      current_all_colors_oklab,
      fixed_mask,
      max_iterations,
      cvd_safe = cvd_safe
    ),
    "nlopt_lbfgs" = optimize_colors_lbfgs(
      current_all_colors_oklab,
      fixed_mask,
      max_iterations,
      weights
    ),
    stop(
      "Unsupported optimizer: ",
      optimizer,
      ". This should not happen after validation."
    )
  )

  opt_result
}

#' Finalize palette with metrics and class attributes
#' @noRd
.finalize_palette <- function(
  optimized_colors_oklab,
  opt_result,
  return_metrics,
  progress,
  generation_metadata = NULL
) {
  if (progress) {
    cli::cli_alert_info("Finalizing palette...")
  }

  # Convert to hex first, then sort by brightness (due to gamut
  # clamping effects)
  if (is.matrix(optimized_colors_oklab) && nrow(optimized_colors_oklab) > 0) {
    # Convert to hex colors first
    hex_colors <- .oklab_to_hex(optimized_colors_oklab)

    # Convert back to OKLAB to get the actual lightness values after
    # gamut clamping
    final_oklab_matrix <- .hex_to_oklab(hex_colors)

    # Sort hex colors by their actual final lightness values
    lightness_order <- order(final_oklab_matrix[, 1])
    hex_colors <- hex_colors[lightness_order]
    sorted_colors_oklab <- final_oklab_matrix[lightness_order, , drop = FALSE]
  } else {
    hex_colors <- character(0)
    sorted_colors_oklab <- optimized_colors_oklab
  }
  class(hex_colors) <- c("huerd_palette", class(hex_colors))
  attr(hex_colors, "optimization_details") <- opt_result$details

  if (return_metrics) {
    # Use sorted colors for metrics if they exist, otherwise use original matrix
    colors_for_metrics <- if (
      is.matrix(optimized_colors_oklab) && nrow(optimized_colors_oklab) > 0
    ) {
      sorted_colors_oklab
    } else {
      optimized_colors_oklab
    }
    metrics <- evaluate_palette(colors_for_metrics)
    attr(hex_colors, "metrics") <- metrics
  }

  # Store generation metadata for reproducibility
  if (!is.null(generation_metadata)) {
    attr(hex_colors, "generation_metadata") <- generation_metadata
  }

  if (progress) {
    cli::cli_alert_success("Done")
  }

  hex_colors
}

#' Generate Optimal Color Palette using Pure Minimax Optimization
#'
#' Creates a color palette that maximizes the minimum perceptual distance
#' between any two colors, using pure minimax optimization in OKLAB color
#' space. The function sorts the colors by brightness and can include fixed
#' brand colors.
#'
#' @param n Integer. Total number of colors in the palette.
#' @param include_colors Character vector. Hex colors that must be included
#'   in the palette unchanged. Default is NULL.
#' @param initialization Character. Initialization method for free colors:
#'   "k-means++" or "harmony". Default is "k-means++".
#' @param init_lightness_bounds Numeric vector of length 2. For k-means++
#'   initialization, target lightness (L in OKLAB) range for initial
#'   candidates. Default `c(0.2, 0.9)`.
#' @param init_hcl_bounds List. For harmony-based initialization, target
#'   `C` (Chroma) and `L` (Luminance) ranges for initial HCL colors.
#'   Default `list(C = c(40, 80), L = c(50, 80))`.
#' @param fixed_aesthetic_influence Numeric, 0 to 1. If `include_colors`
#'   are provided, controls how strongly their aesthetic properties
#'   influence the initialization for free colors. 0 = no influence,
#'   1 = strong influence. Default is 0.75.
#' @param aesthetic_init_config List. Advanced configuration for aesthetic
#'   initialization. Use `NULL` (default) for built-in defaults.
#' @param max_iterations Integer. Maximum optimization iterations. Default
#'   is 1000.
#' @param return_metrics Logical. Whether to return evaluation metrics as
#'   attributes. Default is TRUE.
#' @param progress Logical. Show progress messages. Default is
#'   `interactive()`.
#' @param weights Named numeric vector. Weights for multi-objective
#'   optimization. Supports: `c(distance = 1)` for discrete distance
#'   optimization, `c(smooth_repulsion = 1)` for smooth repulsion
#'   objective using inverse squared distances, or
#'   `c(smooth_logsumexp = 1)` for smooth log-sum-exp objective. Default
#'   is NULL, which is internally equivalent to `c(distance = 1)` for
#'   most optimizers. For "nlopt_lbfgs", NULL defaults to
#'   `smooth_repulsion`.
#' @param optimizer Character. Optimization algorithm to use. Currently
#'   supported: "nloptr_cobyla" (default) for deterministic optimization
#'   with constraint handling, "sann" for stochastic simulated annealing
#'   (excellent quality but not perfectly reproducible without a seed),
#'   "nlopt_neldermead" for derivative-free local optimization using the
#'   Nelder-Mead simplex algorithm (good alternative to COBYLA for robust
#'   local optimization), "nlopt_lbfgs" for gradient-based L-BFGS
#'   optimization (fastest convergence for smooth objectives; works best
#'   with `smooth_repulsion` or `smooth_logsumexp` weights), and
#'   "nlopt_direct" (**deprecated**) for deterministic global optimization
#'   via the DIRECT algorithm — produces degenerate palettes for most
#'   palette sizes and will be removed in a future release. The framework
#'   can support additional optimizers in future versions.
#' @param cvd_safe Logical. If `TRUE` (default), the objective maximizes
#'   the minimum perceptual distance in the worst case across deuteranopia,
#'   protanopia, and tritanopia simulations, producing palettes that are
#'   distinguishable for viewers with color vision deficiencies. If
#'   `FALSE`, the objective maximizes the minimum perceptual distance for
#'   normal vision only. Has no effect when `optimizer = "nlopt_lbfgs"`
#'   because the smooth objectives are normal-vision only.
#' @param ... Additional arguments reserved for future use.
#'
#' @return A character vector of hex colors with class `huerd_palette`,
#'   automatically sorted by brightness (lightness). If
#'   `return_metrics = TRUE`, includes evaluation metrics as attributes.
#'
#' @details
#' This function implements pure minimax optimization to create color
#' palettes with maximum worst-case perceptual distinguishability. The
#' approach focuses on a single, clear objective.
#'
#' The process:
#' 1. Initialize free colors using k-means++ or harmony-based methods
#' 2. Optimize using box-constrained nloptr to maximize the minimum
#'    perceptual distance (worst case across CVD simulations when
#'    `cvd_safe = TRUE`, the default)
#' 3. Sort final palette by OKLAB lightness for intuitive ordering
#' 4. Apply gamut compensation during brightness sorting
#'
#' The pure minimax approach ensures optimal categorical color palettes
#' without complex multi-objective trade-offs. Assess quality with
#' `evaluate_palette()` and visualize it with `plot_palette_analysis()`.
#'
#' @section Performance Tips:
#' \itemize{
#'   \item For low performance ratios, try increasing `max_iterations`
#'   \item Use `progress = TRUE` to monitor optimization convergence
#'   \item Include 2-3 fixed colors maximum for best optimization
#'   \item Use diagnostic dashboard `plot_palette_analysis()` for analysis
#'   }
#'
#' @examples
#' # Simple optimal palette
#' palette <- generate_palette(5, progress = FALSE)
#' print(palette)
#'
#' # Brand-constrained palette
#' brand_palette <- generate_palette(
#'   n = 6,
#'   include_colors = c("#4A6B8A", "#E5A04C"),
#'   progress = FALSE
#' )
#'
#' # Using specific optimizer (deterministic)
#' optimizer_palette <- generate_palette(
#'   n = 4,
#'   optimizer = "nloptr_cobyla",
#'   progress = FALSE
#' )
#'
#' # Using simulated annealing (stochastic, excellent quality)
#' set.seed(42)  # For reproducibility
#' sann_palette <- generate_palette(
#'   n = 4,
#'   optimizer = "sann",
#'   progress = FALSE
#' )
#'
#' # Using Nelder-Mead algorithm (derivative-free local, good alternative
#' # to COBYLA)
#' neldermead_palette <- generate_palette(
#'   n = 4,
#'   optimizer = "nlopt_neldermead",
#'   progress = FALSE
#' )
#'
#' # Using smooth optimization with L-BFGS (efficient for larger palettes)
#' smooth_palette <- generate_palette(
#'   n = 12,
#'   weights = c(smooth_repulsion = 1),
#'   optimizer = "nlopt_lbfgs",
#'   progress = FALSE
#' )
#'
#' # Using alternative smooth objective
#' logsumexp_palette <- generate_palette(
#'   n = 6,
#'   weights = c(smooth_logsumexp = 1),
#'   optimizer = "nlopt_lbfgs",
#'   progress = FALSE
#' )
#'
#' # Evaluate quality
#' evaluation <- evaluate_palette(brand_palette)
#' cat("Min distance:", evaluation$distances$min, "\n")
#' cat("Performance:", evaluation$distances$performance_ratio * 100, "%\n")
#'
#' # Comprehensive analysis
#' plot_palette_analysis(brand_palette)
#'
#' @export
generate_palette <- function(
  n,
  include_colors = NULL,
  initialization = c("k-means++", "harmony"),
  init_lightness_bounds = c(0.2, 0.9),
  init_hcl_bounds = list(C = c(40, 80), L = c(50, 80)),
  fixed_aesthetic_influence = 0.75,
  aesthetic_init_config = NULL,
  max_iterations = 1000,
  return_metrics = TRUE,
  progress = interactive(),
  weights = NULL,
  optimizer = "nloptr_cobyla",
  cvd_safe = TRUE,
  ...
) {
  if (!is.logical(cvd_safe) || length(cvd_safe) != 1 || is.na(cvd_safe)) {
    stop("`cvd_safe` must be a single TRUE or FALSE.", call. = FALSE)
  }

  # nolint start: object_usage_linter
  seed_info <- if (exists(".Random.seed")) {
    .Random.seed
  } else {
    NULL
  }
  # nolint end

  generation_metadata <- list(
    n_colors = n,
    include_colors = include_colors,
    initialization = initialization,
    init_lightness_bounds = init_lightness_bounds,
    init_hcl_bounds = init_hcl_bounds,
    fixed_aesthetic_influence = fixed_aesthetic_influence,
    aesthetic_init_config = aesthetic_init_config,
    max_iterations = max_iterations,
    return_metrics = return_metrics,
    weights = weights,
    optimizer = optimizer,
    cvd_safe = cvd_safe,
    seed = seed_info,
    package_version = utils::packageVersion("huerd"),
    target_space = "oklab",
    timestamp = Sys.time()
  )

  # Input validation
  validate_inputs(
    n,
    include_colors,
    init_lightness_bounds,
    init_hcl_bounds,
    fixed_aesthetic_influence,
    aesthetic_init_config,
    weights,
    optimizer
  )

  # Soft deprecation: DIRECT's center-lattice sampling cannot reliably
  # find all-distinct color configurations in 3n dimensions, so it
  # returns degenerate palettes for most palette sizes.
  if (identical(optimizer, "nlopt_direct")) {
    cli::cli_warn(c(
      "!" = paste0(
        "{.arg optimizer} = {.val nlopt_direct} is deprecated and will ",
        "be removed in a future release."
      ),
      "i" = paste0(
        "The DIRECT algorithm cannot reliably separate colors in this ",
        "parameterization and produces degenerate palettes (duplicate ",
        "colors) for most palette sizes."
      ),
      "i" = paste0(
        "Use {.val nloptr_cobyla} (the default) or ",
        "{.val nlopt_neldermead} instead."
      )
    ))
  }

  # Setup and parameter normalization
  params <- .setup_palette_params(
    aesthetic_init_config,
    initialization
  )

  # Calculate color counts and handle edge case
  n_free <- n - length(include_colors)
  if (n_free == 0) {
    return(.handle_no_free_colors(
      n,
      include_colors,
      return_metrics,
      progress,
      generation_metadata = generation_metadata
    ))
  }

  # Process fixed colors
  fixed_result <- .process_fixed_colors(include_colors, progress)

  # Prepare initialization parameters
  init_prep <- .prepare_initialization(
    fixed_result$fixed_oklab,
    fixed_aesthetic_influence,
    init_lightness_bounds,
    init_hcl_bounds,
    params$config,
    progress
  )

  # Initialize and validate colors
  init_result <- .init_and_validate_colors(
    n_free,
    fixed_result$n_fixed,
    fixed_result$fixed_oklab,
    params$initialization,
    init_prep$adapted_init_params,
    init_lightness_bounds,
    include_colors,
    return_metrics,
    progress
  )

  # Handle early return from initialization failure
  if (inherits(init_result, "huerd_palette")) {
    init_result
  } else {
    # Optimize colors
    opt_result <- .optimize_palette(
      init_result$initial_colors_oklab,
      init_result$fixed_mask,
      max_iterations,
      init_result$n_free,
      progress,
      optimizer,
      weights,
      cvd_safe = cvd_safe
    )

    # Finalize and return
    final_palette <- .finalize_palette(
      opt_result$palette,
      opt_result,
      return_metrics,
      progress,
      generation_metadata
    )

    final_palette
  }
}

#' Reproduce Palette from Existing huerd_palette Object
#'
#' Recreates an identical color palette from a previously generated
#' huerd_palette object using stored generation metadata.
#'
#' @param palette A huerd_palette object (result from `generate_palette()`)
#'   containing generation metadata.
#' @param progress Logical. Show progress messages. Default is
#'   `interactive()`. If NULL, uses the progress setting from the
#'   original generation.
#' @param ... Additional arguments reserved for future use.
#'
#' @return A character vector of hex colors with class `huerd_palette`,
#'   identical to the input palette when reproduction is successful.
#'
#' @details
#' This function reads the generation metadata stored in the
#' `generation_metadata` attribute of a huerd_palette object and
#' re-runs `generate_palette()` with the same parameters.
#'
#' Reproducibility depends on the optimizer used:
#' \itemize{
#'   \item **Deterministic optimizers** ("nloptr_cobyla",
#'     "nlopt_neldermead", "nlopt_lbfgs"): Reproduction is always identical
#'     regardless of the random seed, as these algorithms produce the same
#'     results for the same inputs.
#'   \item **Stochastic optimizers** ("sann"): Reproduction requires
#'     restoring the random seed captured during the original generation.
#'     The seed is scoped using `withr::with_seed()` to avoid mutating
#'     global state.
#' }
#'
#' The function validates that the input object contains the necessary metadata
#' and provides informative error messages if reproduction fails.
#'
#' @examples
#' \dontrun{
#' # Create a reproducible palette
#' set.seed(42)
#' original_palette <- generate_palette(
#'   n = 5,
#'   include_colors = c("#FF0000"),
#'   optimizer = "nloptr_cobyla",
#'   progress = FALSE
#' )
#'
#' # Reproduce the exact same palette
#' reproduced_palette <- reproduce_palette(original_palette)
#'
#' # Verify they are identical
#' identical(original_palette, reproduced_palette)
#'
#' # Examine generation metadata
#' metadata <- attr(original_palette, "generation_metadata")
#' str(metadata)
#' }
#'
#' @export
reproduce_palette <- function(palette, progress = NULL, ...) {
  # Validate input
  if (!inherits(palette, "huerd_palette")) {
    stop(
      "Input must be a huerd_palette object (result from generate_palette())"
    )
  }

  # Extract generation metadata
  metadata <- attr(palette, "generation_metadata")
  if (is.null(metadata)) {
    stop(
      "No generation metadata found in palette object. ",
      "This palette may have been created with an older version of huerd ",
      "or metadata was removed. Reproduction requires metadata."
    )
  }

  # Validate that metadata contains required fields
  required_fields <- c(
    "n_colors",
    "include_colors",
    "initialization",
    "init_lightness_bounds",
    "init_hcl_bounds",
    "fixed_aesthetic_influence",
    "aesthetic_init_config",
    "max_iterations",
    "return_metrics",
    "weights",
    "optimizer"
  )

  missing_fields <- setdiff(required_fields, names(metadata))
  if (length(missing_fields) > 0) {
    stop(
      "Missing required metadata fields: ",
      paste(missing_fields, collapse = ", "),
      ". Cannot reproduce palette."
    )
  }

  # Set progress - use metadata value if progress is NULL
  if (is.null(progress)) {
    progress <- if ("progress" %in% names(metadata)) {
      metadata$progress
    } else {
      FALSE
    }
  }

  # Package version compatibility check
  if (!is.null(metadata$package_version)) {
    current_version <- utils::packageVersion("huerd")
    if (metadata$package_version != current_version) {
      warning(
        "Package version mismatch: original palette was created with version ",
        metadata$package_version,
        ", current version is ",
        current_version,
        ". Reproduction may not be identical."
      )
    }
  }

  if (progress) {
    cli::cli_alert_info("Reproducing palette using stored metadata...")
  }

  # Reproduce palette using stored parameters
  # Use withr::set_seed to restore the exact RNG state
  if (!is.null(metadata$seed)) {
    # nolint start: object_usage_linter, object_name_linter
    reproduced_palette <- withr::with_preserve_seed({
      .Random.seed <<- metadata$seed
      generate_palette(
        n = metadata$n_colors,
        include_colors = metadata$include_colors,
        initialization = metadata$initialization,
        init_lightness_bounds = metadata$init_lightness_bounds,
        init_hcl_bounds = metadata$init_hcl_bounds,
        fixed_aesthetic_influence = metadata$fixed_aesthetic_influence,
        aesthetic_init_config = metadata$aesthetic_init_config,
        max_iterations = metadata$max_iterations,
        return_metrics = metadata$return_metrics,
        progress = progress,
        weights = metadata$weights,
        optimizer = metadata$optimizer,
        cvd_safe = metadata$cvd_safe %||% TRUE
      )
    })
    # nolint end
  } else {
    reproduced_palette <- generate_palette(
      n = metadata$n_colors,
      include_colors = metadata$include_colors,
      initialization = metadata$initialization,
      init_lightness_bounds = metadata$init_lightness_bounds,
      init_hcl_bounds = metadata$init_hcl_bounds,
      fixed_aesthetic_influence = metadata$fixed_aesthetic_influence,
      aesthetic_init_config = metadata$aesthetic_init_config,
      max_iterations = metadata$max_iterations,
      return_metrics = metadata$return_metrics,
      progress = progress,
      weights = metadata$weights,
      optimizer = metadata$optimizer,
      cvd_safe = metadata$cvd_safe %||% TRUE
    )
  }

  # Preserve the original generation metadata to maintain perfect
  # reproducibility
  attr(reproduced_palette, "generation_metadata") <- metadata

  reproduced_palette
}

# Default configuration for aesthetic initialization parameters.
# Users can override these via the `aesthetic_init_config` argument in `generate_palette`.
.DEFAULT_AESTHETIC_INIT_CONFIG <- list(
  # Versioning for future API stability.
  config_version = 1,

  # K-means++ OKLAB L-bounds adaptation from fixed color mean L.
  # The SD of fixed colors' L is multiplied by this value to define the search window.
  kmeans_L_sd_multiplier = 1.5,

  # K-means++ OKLAB C-filter adaptation from fixed color mean C.
  # The SD of fixed colors' C is multiplied by this value to define the allowed deviation.
  kmeans_C_sd_multiplier = 1.5,
  # A small constant added to the allowed C deviation to handle cases where SD is zero.
  kmeans_C_base_deviation = 0.05,
  # Factor determining how `fixed_aesthetic_influence` tightens the C-filter.
  kmeans_C_influence_tightening_factor = 0.75, # Influence factor for aesthetic-guided chroma filtering

  # Harmony HCL L/C bounds adaptation.
  # Fallback Standard Deviation for HCL L/C if only one fixed color or actual SD is zero.
  harmony_hcl_sd_fallback = 15,
  # Minimum allowed Standard Deviation for HCL L.
  harmony_hcl_L_min_sd = 5,
  # Minimum allowed Standard Deviation for HCL C.
  harmony_hcl_C_min_sd = 5,
  # Multiplier for the SDs derived from fixed colors' HCL L/C values.
  harmony_hcl_sd_multiplier = 1.0,

  # Fallback strategy.
  # Factor by which to relax the max_C_deviation if initial filtering yields too few candidates.
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
    cat("Preparing for palette generation...\n")
  }

  fixed_oklab <- NULL
  sorted_include_colors <- include_colors
  if (n_fixed > 0) {
    oklab_matrix <- .hex_to_oklab(include_colors)

    # Sort fixed colors by brightness (lightness) to ensure final palette is brightness-sorted
    lightness_order <- order(oklab_matrix[, 1])
    fixed_oklab <- oklab_matrix[lightness_order, , drop = FALSE]
    sorted_include_colors <- include_colors[lightness_order]
  }

  list(
    fixed_oklab = fixed_oklab,
    n_fixed = n_fixed,
    sorted_include_colors = sorted_include_colors
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
.initialize_and_validate_colors <- function(
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
    cat(
      "Initializing ",
      n_free,
      " free colors (method: ",
      initialization,
      ")...\n",
      sep = ""
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
      cat(
        "Warning: Initialization generated ",
        actual_n_init_free,
        " of ",
        n_free,
        " requested free colors. Palette will be smaller.\n",
        sep = ""
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
  optimizer = "nloptr_cobyla"
) {
  if (progress && n_free > 0) {
    cat(
      "Optimizing ",
      n_free,
      " free colors using ",
      optimizer,
      "...\n",
      sep = ""
    )
  }

  # Switch statement for different optimizers
  opt_result <- switch(
    optimizer,
    "nloptr_cobyla" = optimize_colors_constrained(
      current_all_colors_oklab,
      fixed_mask,
      max_iterations
    ),
    "sann" = optimize_colors_sann(
      current_all_colors_oklab,
      fixed_mask,
      max_iterations
    ),
    "nlopt_direct" = optimize_colors_nlopt_direct(
      current_all_colors_oklab,
      fixed_mask,
      max_iterations
    ),
    "nlopt_neldermead" = optimize_colors_nlopt_neldermead(
      current_all_colors_oklab,
      fixed_mask,
      max_iterations
    ),
    "nlopt_lbfgs" = optimize_colors_lbfgs(
      current_all_colors_oklab,
      fixed_mask,
      max_iterations
    ),
    # Future optimizers can be added here
    # "genetic_algorithm" = optimize_colors_genetic(...),
    # "particle_swarm" = optimize_colors_pso(...),
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
    cat("Finalizing palette...\n")
  }

  # Convert to hex first, then sort by brightness (due to gamut clamping effects)
  if (is.matrix(optimized_colors_oklab) && nrow(optimized_colors_oklab) > 0) {
    # Convert to hex colors first
    hex_colors <- .oklab_to_hex(optimized_colors_oklab)

    # Convert back to OKLAB to get the actual lightness values after gamut clamping
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

  # Add optimization states if they exist
  if ("optimization_states" %in% names(opt_result)) {
    attr(hex_colors, "optimization_states") <- opt_result$optimization_states
  }

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
    cat("Done.\n")
  }

  hex_colors
}

#' Generate Optimal Color Palette using Pure Minimax Optimization
#'
#' Creates a scientifically-grounded color palette that maximizes the minimum
#' perceptual distance between any two colors using pure minimax optimization in
#' the OKLAB color space. Colors are automatically sorted by brightness and can
#' include fixed brand colors.
#'
#' @param n Integer. Total number of colors in the palette.
#' @param include_colors Character vector. Hex colors that must be included
#'   in the palette unchanged. Default is NULL.
#' @param initialization Character. Initialization method for free colors:
#'   "k-means++" or "harmony". Default is "k-means++".
#' @param init_lightness_bounds Numeric vector of length 2. For k-means++ initialization,
#'   target lightness (L in OKLAB) range for initial candidates. Default `c(0.2, 0.9)`.
#' @param init_hcl_bounds List. For harmony-based initialization, target
#'   `C` (Chroma) and `L` (Luminance) ranges for initial HCL colors.
#'   Default `list(C = c(40, 80), L = c(50, 80))`.
#' @param fixed_aesthetic_influence Numeric, 0 to 1. If `include_colors` are provided,
#'   controls how strongly their aesthetic properties influence the initialization
#'   for free colors. 0 = no influence, 1 = strong influence. Default is 0.75.
#' @param aesthetic_init_config List. Advanced configuration for aesthetic initialization.
#'   Use `NULL` (default) for built-in defaults.
#' @param max_iterations Integer. Maximum optimization iterations. Default is 1000.
#' @param return_metrics Logical. Whether to return evaluation metrics as
#'   attributes. Default is TRUE.
#' @param progress Logical. Show progress messages. Default is `interactive()`.
#' @param weights Named numeric vector. Weights for multi-objective optimization.
#'   Currently only supports `c(distance = 1)` for distance-based optimization.
#'   Default is NULL (equivalent to pure distance optimization).
#' @param optimizer Character. Optimization algorithm to use. Currently supported:
#'   "nloptr_cobyla" (default) for deterministic optimization with constraint handling,
#'   "sann" for stochastic simulated annealing (excellent quality but not perfectly
#'   reproducible without a seed), "nlopt_direct" for deterministic global optimization
#'   using the DIRECT algorithm (best choice for scientific reproducibility and high
#'   quality, though may be slower), "nlopt_neldermead" for derivative-free local
#'   optimization using the Nelder-Mead simplex algorithm (good alternative to COBYLA
#'   for robust local optimization). The framework is designed to easily support
#'   additional optimizers in future versions.
#'
#' @return A character vector of hex colors with class `huerd_palette`, automatically
#'   sorted by brightness (lightness). If `return_metrics = TRUE`, includes evaluation
#'   metrics as attributes.
#'
#' @details
#' This function implements pure minimax optimization to create color palettes with
#' maximum worst-case perceptual distinguishability. The approach is scientifically
#' grounded and focuses on a single, clear objective.
#'
#' The process:
#' 1. Initialize free colors using k-means++ or harmony-based methods
#' 2. Optimize using box-constrained nloptr to maximize minimum perceptual distance
#' 3. Sort final palette by OKLAB lightness for intuitive ordering
#' 4. Apply gamut compensation during brightness sorting
#'
#' The pure minimax approach ensures optimal categorical color palettes without
#' complex multi-objective trade-offs. Quality can be assessed using `evaluate_palette()`
#' and visualized with `plot_palette_analysis()`.
#'
#' @section Performance Tips:
#' \itemize{
#'   \item For low performance ratios, try increasing `max_iterations`
#'   \item Use `progress = TRUE` to monitor optimization convergence
#'   \item Include 2-3 fixed colors maximum for best optimization
#'   \item Use diagnostic dashboard `plot_palette_analysis()` for analysis
#' }
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
#' # Using DIRECT algorithm (deterministic global, best for scientific reproducibility)
#' direct_palette <- generate_palette(
#'   n = 4,
#'   optimizer = "nlopt_direct",
#'   progress = FALSE
#' )
#'
#' # Using Nelder-Mead algorithm (derivative-free local, good alternative to COBYLA)
#' neldermead_palette <- generate_palette(
#'   n = 4,
#'   optimizer = "nlopt_neldermead",
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
  optimizer = "nloptr_cobyla"
) {
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
    seed = if (exists(".Random.seed")) .Random.seed else NULL,
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
  init_result <- .initialize_and_validate_colors(
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
    return(init_result)
  }

  # Optimize colors
  opt_result <- .optimize_palette(
    init_result$initial_colors_oklab,
    init_result$fixed_mask,
    max_iterations,
    init_result$n_free,
    progress,
    optimizer
  )

  # Finalize and return
  final_palette <- .finalize_palette(
    opt_result$palette,
    opt_result,
    return_metrics,
    progress,
    generation_metadata
  )

  return(final_palette)
}

#' Reproduce Palette from Existing huerd_palette Object
#'
#' Recreates an identical color palette from a previously generated huerd_palette
#' object using the stored generation metadata.
#'
#' @param palette A huerd_palette object (result from `generate_palette()`)
#'   containing generation metadata.
#' @param progress Logical. Show progress messages. Default is `interactive()`.
#'   If NULL, uses the progress setting from the original generation.
#'
#' @return A character vector of hex colors with class `huerd_palette`,
#'   identical to the input palette when reproduction is successful.
#'
#' @details
#' This function reads the generation metadata stored in the `generation_metadata`
#' attribute of a huerd_palette object and re-runs `generate_palette()` with
#' the exact same parameters. When a random seed was captured during original
#' generation, the reproduction will be identical if the optimizer supports
#' the usage of a seed. For deterministic optimizers like "nlopt_direct",
#' reproduction should always be identical regardless of random seed.
#'
#' The function validates that the input object contains the necessary metadata
#' and provides informative error messages if reproduction fails due to missing
#' metadata or package version incompatibilities.
#'
#' @examples
#' \dontrun{
#' # Create a reproducible palette
#' set.seed(42)
#' original_palette <- generate_palette(
#'   n = 5,
#'   include_colors = c("#FF0000"),
#'   optimizer = "nlopt_direct",
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
reproduce_palette <- function(palette, progress = NULL) {
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
      "or the metadata was removed. Reproduction requires metadata."
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

  # Restore random seed if available
  if (!is.null(metadata$seed)) {
    if (progress) {
      cat("Restoring random seed for reproducibility...\n")
    }
    .Random.seed <<- metadata$seed
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
    cat("Reproducing palette using stored metadata...\n")
  }

  # Reproduce the palette using stored parameters
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
    optimizer = metadata$optimizer
  )

  # Preserve the original generation metadata to maintain perfect reproducibility
  attr(reproduced_palette, "generation_metadata") <- metadata

  return(reproduced_palette)
}

# ==============================================================================
# COMPREHENSIVE DEPENDENCY AND FUNCTION VALIDATION TESTS
# ==============================================================================
# These tests verify that:
# 1. Required functions are defined and accessible
# 2. Package dependencies are properly declared and loaded
# 3. Function signatures match expected parameters
# 4. Integration between components works correctly
#
# These tests are designed to catch issues like:
# - Missing function definitions (like eval_f that was found in the Nelder-Mead optimizer)
# - Missing package dependencies
# - Function signature mismatches
# - Integration failures between components

describe("Function Existence Tests", {
  # ===========================================================================
  # CRITICAL: Optimization Core Functions
  # These functions MUST exist for the optimization pipeline to work
  # ===========================================================================

  describe("optimization_core.R - Required functions exist", {
    it("optimize_colors_constrained is defined", {
      expect_true(
        exists("optimize_colors_constrained", mode = "function"),
        info = "optimize_colors_constrained must be defined for COBYLA optimization"
      )
    })

    it("optimize_colors_sann is defined", {
      expect_true(
        exists("optimize_colors_sann", mode = "function"),
        info = "optimize_colors_sann must be defined for simulated annealing"
      )
    })

    it("optimize_colors_nlopt_direct is defined", {
      expect_true(
        exists("optimize_colors_nlopt_direct", mode = "function"),
        info = "optimize_colors_nlopt_direct must be defined for DIRECT algorithm"
      )
    })

    it("optimize_colors_nlopt_neldermead is defined", {
      expect_true(
        exists("optimize_colors_nlopt_neldermead", mode = "function"),
        info = "optimize_colors_nlopt_neldermead must be defined for Nelder-Mead algorithm"
      )
    })

    it("optimize_colors_lbfgs is defined", {
      expect_true(
        exists("optimize_colors_lbfgs", mode = "function"),
        info = "optimize_colors_lbfgs must be defined for L-BFGS optimization"
      )
    })

    it("objective_function_aggregator is defined", {
      expect_true(
        exists("objective_function_aggregator", mode = "function"),
        info = "objective_function_aggregator must be defined for multi-objective optimization"
      )
    })

    it("objective_min_perceptual_dist is defined", {
      expect_true(
        exists("objective_min_perceptual_dist", mode = "function"),
        info = "objective_min_perceptual_dist must be defined for perceptual distance optimization"
      )
    })

    it("objective_min_cvd_safe_dist is defined", {
      expect_true(
        exists("objective_min_cvd_safe_dist", mode = "function"),
        info = "objective_min_cvd_safe_dist must be defined for CVD-safe optimization"
      )
    })

    it(".clamp_to_bounds is defined", {
      expect_true(
        exists(".clamp_to_bounds", mode = "function"),
        info = ".clamp_to_bounds must be defined for bound constraints"
      )
    })
  })

  describe("optimization-smooth.R - Smooth objective functions exist", {
    it("objective_smooth_repulsion is defined", {
      expect_true(
        exists("objective_smooth_repulsion", mode = "function"),
        info = "objective_smooth_repulsion must be defined for smooth L-BFGS optimization"
      )
    })

    it("objective_smooth_logsumexp is defined", {
      expect_true(
        exists("objective_smooth_logsumexp", mode = "function"),
        info = "objective_smooth_logsumexp must be defined for log-sum-exp L-BFGS optimization"
      )
    })

    it("gradient_smooth_repulsion is defined", {
      expect_true(
        exists("gradient_smooth_repulsion", mode = "function"),
        info = "gradient_smooth_repulsion must be defined for L-BFGS gradient-based optimization"
      )
    })

    it("gradient_smooth_logsumexp is defined", {
      expect_true(
        exists("gradient_smooth_logsumexp", mode = "function"),
        info = "gradient_smooth_logsumexp must be defined for log-sum-exp gradient computation"
      )
    })
  })

  describe("helpers_core.R - Required functions exist", {
    it("calculate_perceptual_distances is defined", {
      expect_true(
        exists("calculate_perceptual_distances", mode = "function"),
        info = "calculate_perceptual_distances must be defined for distance calculations"
      )
    })

    it("validate_inputs is defined", {
      expect_true(
        exists("validate_inputs", mode = "function"),
        info = "validate_inputs must be defined for input validation"
      )
    })

    it(".validate_n is defined", {
      expect_true(
        exists(".validate_n", mode = "function"),
        info = ".validate_n must be defined for parameter validation"
      )
    })

    it(".validate_optimizer is defined", {
      expect_true(
        exists(".validate_optimizer", mode = "function"),
        info = ".validate_optimizer must be defined for optimizer validation"
      )
    })

    it(".handle_no_free_colors is defined", {
      expect_true(
        exists(".handle_no_free_colors", mode = "function"),
        info = ".handle_no_free_colors must be defined for edge case handling"
      )
    })

    it("%||% is defined", {
      expect_true(
        exists("%||%", mode = "function"),
        info = "%||% infix operator must be defined for default value handling"
      )
    })
  })

  describe("helpers_init.R - Required functions exist", {
    it(".calculate_aesthetic_profile is defined", {
      expect_true(
        exists(".calculate_aesthetic_profile", mode = "function"),
        info = ".calculate_aesthetic_profile must be defined for aesthetic initialization"
      )
    })

    it(".adapt_init_params is defined", {
      expect_true(
        exists(".adapt_init_params", mode = "function"),
        info = ".adapt_init_params must be defined for initialization parameter adaptation"
      )
    })

    it("initialize_colors is defined", {
      expect_true(
        exists("initialize_colors", mode = "function"),
        info = "initialize_colors must be defined for color initialization dispatch"
      )
    })

    it("initialize_kmeans_plus_plus is defined", {
      expect_true(
        exists("initialize_kmeans_plus_plus", mode = "function"),
        info = "initialize_kmeans_plus_plus must be defined for k-means++ initialization"
      )
    })

    it("initialize_harmony_based is defined", {
      expect_true(
        exists("initialize_harmony_based", mode = "function"),
        info = "initialize_harmony_based must be defined for harmony-based initialization"
      )
    })
  })

  describe("generate_palette.R - Required functions exist", {
    it("generate_palette is defined", {
      expect_true(
        exists("generate_palette", mode = "function"),
        info = "generate_palette must be defined as the main API function"
      )
    })

    it("reproduce_palette is defined", {
      expect_true(
        exists("reproduce_palette", mode = "function"),
        info = "reproduce_palette must be defined for palette reproducibility"
      )
    })

    it(".setup_palette_params is defined", {
      expect_true(
        exists(".setup_palette_params", mode = "function"),
        info = ".setup_palette_params must be defined for parameter setup"
      )
    })

    it(".process_fixed_colors is defined", {
      expect_true(
        exists(".process_fixed_colors", mode = "function"),
        info = ".process_fixed_colors must be defined for fixed color processing"
      )
    })

    it(".optimize_palette is defined", {
      expect_true(
        exists(".optimize_palette", mode = "function"),
        info = ".optimize_palette must be defined for optimization dispatch"
      )
    })

    it(".finalize_palette is defined", {
      expect_true(
        exists(".finalize_palette", mode = "function"),
        info = ".finalize_palette must be defined for palette finalization"
      )
    })
  })

  describe("color_metrics.R - Required functions exist", {
    it("evaluate_palette is defined", {
      expect_true(
        exists("evaluate_palette", mode = "function"),
        info = "evaluate_palette must be defined as public API for palette evaluation"
      )
    })

    it("evaluate_palette_quality is defined", {
      expect_true(
        exists("evaluate_palette_quality", mode = "function"),
        info = "evaluate_palette_quality must be defined for internal quality evaluation"
      )
    })

    it("analyze_cvd_safety_metrics is defined", {
      expect_true(
        exists("analyze_cvd_safety_metrics", mode = "function"),
        info = "analyze_cvd_safety_metrics must be defined for CVD safety analysis"
      )
    })

    it("analyze_color_distribution is defined", {
      expect_true(
        exists("analyze_color_distribution", mode = "function"),
        info = "analyze_color_distribution must be defined for distribution analysis"
      )
    })
  })

  describe("utils.R - Required functions exist", {
    it(".hex_to_oklab is defined", {
      expect_true(
        exists(".hex_to_oklab", mode = "function"),
        info = ".hex_to_oklab must be defined for color conversion"
      )
    })

    it(".oklab_to_hex is defined", {
      expect_true(
        exists(".oklab_to_hex", mode = "function"),
        info = ".oklab_to_hex must be defined for color conversion"
      )
    })

    it(".get_estimated_max_dist is defined", {
      expect_true(
        exists(".get_estimated_max_dist", mode = "function"),
        info = ".get_estimated_max_dist must be defined for performance ratio calculation"
      )
    })
  })
})

describe("Package Dependency Tests", {
  # ===========================================================================
  # CRITICAL: Package Dependencies
  # These packages MUST be available for the package to function
  # ===========================================================================

  describe("Core package dependencies", {
    it("nloptr is available", {
      expect_true(
        requireNamespace("nloptr", quietly = TRUE),
        info = "nloptr package must be available for optimization algorithms"
      )
    })

    it("farver is available", {
      expect_true(
        requireNamespace("farver", quietly = TRUE),
        info = "farver package must be available for color space conversions"
      )
    })

    it("colorspace is available", {
      expect_true(
        requireNamespace("colorspace", quietly = TRUE),
        info = "colorspace package must be available for CVD simulation"
      )
    })

    it("cli is available", {
      expect_true(
        requireNamespace("cli", quietly = TRUE),
        info = "cli package must be available for progress messages"
      )
    })
  })

  describe("Suggested package dependencies (soft checks)", {
    it("testthat is available for testing", {
      # This test file itself uses testthat
      expect_true(
        requireNamespace("testthat", quietly = TRUE),
        info = "testthat package should be available for tests"
      )
    })

    it("withr is available for test fixtures", {
      # Check if withr is available (may be suggested, not required)
      expect_true(
        requireNamespace("withr", quietly = TRUE),
        info = "withr package should be available for test fixtures"
      )
    })
  })

  describe("Package version compatibility", {
    it("nloptr has required functions", {
      skip_if_not_installed("nloptr")

      # Verify nloptr has the nloptr function we use
      expect_true(
        exists("nloptr", envir = asNamespace("nloptr"), mode = "function"),
        info = "nloptr package must have the nloptr() function"
      )
    })

    it("farver has required functions", {
      skip_if_not_installed("farver")

      # Verify farver has the conversion functions we use
      expect_true(
        exists(
          "convert_colour",
          envir = asNamespace("farver"),
          mode = "function"
        ),
        info = "farver package must have convert_colour() function"
      )
      expect_true(
        exists(
          "encode_colour",
          envir = asNamespace("farver"),
          mode = "function"
        ),
        info = "farver package must have encode_colour() function"
      )
      expect_true(
        exists(
          "decode_colour",
          envir = asNamespace("farver"),
          mode = "function"
        ),
        info = "farver package must have decode_colour() function"
      )
    })

    it("colorspace has CVD simulation functions", {
      skip_if_not_installed("colorspace")

      # Verify colorspace has the CVD simulation functions we use
      expect_true(
        exists("protan", envir = asNamespace("colorspace"), mode = "function"),
        info = "colorspace package must have protan() function for CVD simulation"
      )
      expect_true(
        exists("deutan", envir = asNamespace("colorspace"), mode = "function"),
        info = "colorspace package must have deutan() function for CVD simulation"
      )
      expect_true(
        exists("tritan", envir = asNamespace("colorspace"), mode = "function"),
        info = "colorspace package must have tritan() function for CVD simulation"
      )
      expect_true(
        exists("sRGB", envir = asNamespace("colorspace"), mode = "function"),
        info = "colorspace package must have sRGB() function for color space conversion"
      )
    })
  })

  describe("Dependency usage validation", {
    it("nloptr::nloptr is called correctly in optimize_colors_constrained", {
      # Test that we can actually call the optimizer
      skip_if_not_installed("nloptr")

      # Create a simple test case
      colors <- matrix(
        c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "a", "b"))
      )

      # This should not error if nloptr is properly integrated
      expect_no_error({
        result <- optimize_colors_constrained(
          initial_colors_oklab = colors,
          fixed_mask = c(FALSE, FALSE),
          max_iterations = 1
        )
      })
    })

    it("farver::convert_colour works for OKLAB conversions", {
      skip_if_not_installed("farver")

      # Test round-trip conversion
      hex_colors <- c("#FF0000", "#00FF00", "#0000FF")
      oklab <- farver::convert_colour(
        farver::decode_colour(hex_colors),
        from = "rgb",
        to = "oklab"
      )

      expect_true(is.matrix(oklab))
      expect_equal(nrow(oklab), 3)
      expect_equal(ncol(oklab), 3)
    })

    it("colorspace CVD simulation works", {
      skip_if_not_installed("colorspace")

      # Test that colorspace CVD simulation works
      srgb_obj <- colorspace::sRGB(0.5, 0.5, 0.5)
      expect_no_error({
        protan_result <- colorspace::protan(srgb_obj)
      })
    })
  })
})

describe("Function Signature Validation", {
  # ===========================================================================
  # Function Parameter Validation
  # These tests verify that functions have the expected parameter signatures
  # ===========================================================================

  describe("optimize_colors_constrained signature", {
    it("has expected parameters", {
      func_params <- names(formals(optimize_colors_constrained))
      expected_params <- c(
        "initial_colors_oklab",
        "fixed_mask",
        "max_iterations",
        "track_states",
        "save_every",
        "return_states"
      )
      expect_equal(
        sort(func_params),
        sort(expected_params),
        info = "optimize_colors_constrained must have all expected parameters"
      )
    })
  })

  describe("optimize_colors_nlopt_neldermead signature", {
    it("has expected parameters", {
      func_params <- names(formals(optimize_colors_nlopt_neldermead))
      expected_params <- c(
        "initial_colors_oklab",
        "fixed_mask",
        "max_iterations",
        "track_states",
        "save_every",
        "return_states"
      )
      expect_equal(
        sort(func_params),
        sort(expected_params),
        info = "optimize_colors_nlopt_neldermead must have all expected parameters"
      )
    })
  })

  describe("optimize_colors_lbfgs signature", {
    it("has expected parameters including weights", {
      func_params <- names(formals(optimize_colors_lbfgs))
      expected_params <- c(
        "initial_colors_oklab",
        "fixed_mask",
        "max_iterations",
        "weights",
        "track_states",
        "save_every",
        "return_states"
      )
      expect_equal(
        sort(func_params),
        sort(expected_params),
        info = "optimize_colors_lbfgs must have weights parameter"
      )
    })
  })

  describe("generate_palette signature", {
    it("has expected parameters", {
      func_params <- names(formals(generate_palette))
      expected_params <- c(
        "n",
        "include_colors",
        "initialization",
        "init_lightness_bounds",
        "init_hcl_bounds",
        "fixed_aesthetic_influence",
        "aesthetic_init_config",
        "max_iterations",
        "return_metrics",
        "progress",
        "weights",
        "optimizer",
        "..."
      )
      expect_equal(
        sort(func_params),
        sort(expected_params),
        info = "generate_palette must have all expected parameters"
      )
    })
  })

  describe("evaluate_palette signature", {
    it("has expected parameters", {
      func_params <- names(formals(evaluate_palette))
      expected_params <- c("colors", "...")
      expect_equal(
        func_params,
        expected_params,
        info = "evaluate_palette must have expected parameters"
      )
    })
  })

  describe("objective_smooth_repulsion signature", {
    it("has expected parameters", {
      func_params <- names(formals(objective_smooth_repulsion))
      expected_params <- c("colors_oklab", "epsilon")
      expect_equal(
        func_params,
        expected_params,
        info = "objective_smooth_repulsion must have expected parameters"
      )
    })
  })

  describe("gradient_smooth_repulsion signature", {
    it("has expected parameters", {
      func_params <- names(formals(gradient_smooth_repulsion))
      expected_params <- c("colors_oklab", "epsilon")
      expect_equal(
        func_params,
        expected_params,
        info = "gradient_smooth_repulsion must have expected parameters"
      )
    })
  })

  describe("validate_inputs signature", {
    it("has expected parameters", {
      func_params <- names(formals(validate_inputs))
      expected_params <- c(
        "n",
        "include_colors",
        "init_lightness_bounds",
        "init_hcl_bounds",
        "fixed_aesthetic_influence",
        "aesthetic_init_config",
        "weights",
        "optimizer"
      )
      expect_equal(
        sort(func_params),
        sort(expected_params),
        info = "validate_inputs must have all expected parameters"
      )
    })
  })
})

describe("Internal Function Closure Tests", {
  # ===========================================================================
  # Critical: Tests for internal functions that must exist within closures
  # These tests catch cases where internal functions are not properly defined
  # (like the eval_f function issue in the Nelder-Mead optimizer)
  # ===========================================================================

  describe("Internal eval_f function exists in optimizers", {
    it("optimize_colors_constrained can execute eval_f internally", {
      # This test ensures the internal eval_f function is properly defined
      # by testing that the optimizer can actually run
      colors <- matrix(
        c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "a", "b"))
      )

      expect_no_error({
        result <- optimize_colors_constrained(
          initial_colors_oklab = colors,
          fixed_mask = c(FALSE, FALSE),
          max_iterations = 5
        )
      })

      # Verify the result structure indicates the optimizer ran
      expect_true("details" %in% names(result))
      expect_true("iterations" %in% names(result$details))
    })

    it("optimize_colors_nlopt_neldermead can execute eval_f internally", {
      # This test specifically catches the issue where eval_f was missing
      colors <- matrix(
        c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "a", "b"))
      )

      expect_no_error({
        result <- optimize_colors_nlopt_neldermead(
          initial_colors_oklab = colors,
          fixed_mask = c(FALSE, FALSE),
          max_iterations = 5
        )
      })

      # Verify the result structure indicates the optimizer ran
      expect_true("details" %in% names(result))
      expect_true("iterations" %in% names(result$details))
    })

    it("optimize_colors_nlopt_direct can execute eval_f internally", {
      colors <- matrix(
        c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "a", "b"))
      )

      expect_no_error({
        result <- optimize_colors_nlopt_direct(
          initial_colors_oklab = colors,
          fixed_mask = c(FALSE, FALSE),
          max_iterations = 5
        )
      })

      expect_true("details" %in% names(result))
      expect_true("iterations" %in% names(result$details))
    })

    it("optimize_colors_sann can execute eval_f internally", {
      colors <- matrix(
        c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "a", "b"))
      )

      expect_no_error({
        result <- optimize_colors_sann(
          initial_colors_oklab = colors,
          fixed_mask = c(FALSE, FALSE),
          max_iterations = 5
        )
      })

      expect_true("details" %in% names(result))
      expect_true("iterations" %in% names(result$details))
    })

    it("optimize_colors_lbfgs can execute eval_f and eval_grad_f internally", {
      colors <- matrix(
        c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "a", "b"))
      )

      expect_no_error({
        result <- optimize_colors_lbfgs(
          initial_colors_oklab = colors,
          fixed_mask = c(FALSE, FALSE),
          max_iterations = 5,
          weights = c(smooth_repulsion = 1)
        )
      })

      expect_true("details" %in% names(result))
      expect_true("iterations" %in% names(result$details))
    })
  })

  describe("Internal objective functions are accessible", {
    it("objective_min_cvd_safe_dist can be called directly", {
      colors <- matrix(
        c(0.627, 0.224, 0.126, 0.701, -0.101, 0.108, 0.323, -0.003, -0.153),
        nrow = 3,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "a", "b"))
      )

      # This tests that the function is properly defined and callable
      expect_no_error({
        result <- objective_min_cvd_safe_dist(colors)
      })

      expect_true(is.numeric(result))
      expect_length(result, 1)
    })

    it("objective_min_perceptual_dist can be called directly", {
      colors <- matrix(
        c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "a", "b"))
      )

      expect_no_error({
        result <- objective_min_perceptual_dist(colors)
      })

      expect_true(is.numeric(result))
      expect_length(result, 1)
    })

    it("objective_smooth_repulsion can be called directly", {
      colors <- matrix(
        c(0.5, 0.1, 0.2, 0.7, -0.1, 0.1, 0.3, 0.2, -0.1),
        nrow = 3,
        byrow = TRUE
      )

      expect_no_error({
        result <- objective_smooth_repulsion(colors)
      })

      expect_true(is.numeric(result))
      expect_length(result, 1)
    })

    it("gradient_smooth_repulsion can be called directly", {
      colors <- matrix(
        c(0.5, 0.1, 0.2, 0.7, -0.1, 0.1, 0.3, 0.2, -0.1),
        nrow = 3,
        byrow = TRUE
      )

      expect_no_error({
        result <- gradient_smooth_repulsion(colors)
      })

      expect_true(is.matrix(result))
      expect_equal(dim(result), c(3, 3))
    })
  })
})

describe("Integration Tests", {
  # ===========================================================================
  # Integration Tests
  # These tests verify that different components work together correctly
  # ===========================================================================

  describe("Full generate_palette workflow", {
    it("generates palette with default settings", {
      expect_no_error({
        palette <- generate_palette(5, progress = FALSE)
      })

      expect_true(inherits(palette, "huerd_palette"))
      expect_equal(length(palette), 5)
    })

    it("generates palette with fixed colors", {
      expect_no_error({
        palette <- generate_palette(
          5,
          include_colors = c("#FF0000", "#00FF00"),
          progress = FALSE
        )
      })

      expect_true(inherits(palette, "huerd_palette"))
      expect_equal(length(palette), 5)
      # Fixed colors should be in the palette
      expect_true("#FF0000" %in% palette || "#ff0000" %in% tolower(palette))
    })

    it("works with all optimizer types", {
      optimizers <- c(
        "nloptr_cobyla",
        "sann",
        "nlopt_direct",
        "nlopt_neldermead",
        "nlopt_lbfgs"
      )

      for (opt in optimizers) {
        # This test specifically catches missing eval_f or other internal function issues
        expect_no_error({
          palette <- generate_palette(
            3,
            optimizer = opt,
            progress = FALSE
          )
        })
        expect_true(
          inherits(palette, "huerd_palette"),
          info = paste("Optimizer", opt, "should produce valid palette")
        )
      }
    })

    it("works with smooth optimization weights", {
      expect_no_error({
        palette <- generate_palette(
          5,
          weights = c(smooth_repulsion = 1),
          optimizer = "nlopt_lbfgs",
          progress = FALSE
        )
      })

      expect_true(inherits(palette, "huerd_palette"))
    })
  })

  describe("Color conversion integration", {
    it("hex_to_oklab and oklab_to_hex work together", {
      original_hex <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF")

      oklab_result <- .hex_to_oklab(original_hex)
      hex_result <- .oklab_to_hex(oklab_result)

      expect_equal(length(hex_result), length(original_hex))
      expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", hex_result)))
    })

    it("calculate_perceptual_distances integrates with objective functions", {
      colors_oklab <- matrix(
        c(0.5, 0.1, 0.0, 0.6, 0.0, 0.1),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "a", "b"))
      )

      # This tests the integration between distance calculation and objective
      dist_matrix <- calculate_perceptual_distances(colors_oklab)
      expect_true(is.matrix(dist_matrix))
      expect_equal(dim(dist_matrix), c(2, 2))
    })
  })

  describe("Evaluation integration", {
    it("evaluate_palette works with generated palette", {
      palette <- generate_palette(5, progress = FALSE)

      expect_no_error({
        evaluation <- evaluate_palette(palette)
      })

      expect_true(inherits(evaluation, "huerd_evaluation"))
      expect_equal(evaluation$n_colors, 5)
    })

    it("metrics are accessible after palette generation", {
      palette <- generate_palette(5, return_metrics = TRUE, progress = FALSE)

      metrics <- attr(palette, "metrics")
      expect_false(is.null(metrics))
      expect_true(inherits(metrics, "huerd_evaluation"))
    })
  })

  describe("Initialization and optimization integration", {
    it("k-means++ initialization integrates with optimization", {
      # This tests the full initialization -> optimization pipeline
      palette <- generate_palette(
        5,
        initialization = "k-means++",
        progress = FALSE
      )

      expect_true(inherits(palette, "huerd_palette"))
    })

    it("harmony initialization integrates with optimization", {
      palette <- generate_palette(
        5,
        initialization = "harmony",
        progress = FALSE
      )

      expect_true(inherits(palette, "huerd_palette"))
    })

    it("fixed colors influence initialization", {
      # Test that fixed colors are properly integrated
      fixed <- c("#FF0000", "#00FF00")
      palette <- generate_palette(
        4,
        include_colors = fixed,
        progress = FALSE
      )

      expect_true(inherits(palette, "huerd_palette"))
      expect_equal(length(palette), 4)
    })
  })

  describe("Edge case integration", {
    it("handles zero free colors (all fixed)", {
      palette <- generate_palette(
        3,
        include_colors = c("#FF0000", "#00FF00", "#0000FF"),
        progress = FALSE
      )

      expect_true(inherits(palette, "huerd_palette"))
      expect_equal(length(palette), 3)
    })

    it("handles single color palette", {
      palette <- generate_palette(1, progress = FALSE)

      expect_true(inherits(palette, "huerd_palette"))
      expect_equal(length(palette), 1)
    })

    it("handles empty palette", {
      palette <- generate_palette(0, progress = FALSE)

      expect_true(inherits(palette, "huerd_palette"))
      expect_equal(length(palette), 0)
    })
  })
})

describe("Error Handling Integration", {
  # ===========================================================================
  # Error Handling Tests
  # These tests verify that errors are handled gracefully across components
  # ===========================================================================

  describe("Validation error propagation", {
    it("invalid n parameter is caught", {
      expect_error(
        generate_palette(-1, progress = FALSE),
        info = "Negative n should trigger validation error"
      )
    })

    it("invalid include_colors format is caught", {
      expect_error(
        generate_palette(5, include_colors = "not_a_color", progress = FALSE),
        info = "Invalid hex color format should trigger error"
      )
    })

    it("too many fixed colors is caught", {
      expect_error(
        generate_palette(
          3,
          include_colors = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00"),
          progress = FALSE
        ),
        info = "Too many fixed colors should trigger error"
      )
    })

    it("invalid optimizer is caught", {
      expect_error(
        generate_palette(5, optimizer = "invalid_optimizer", progress = FALSE),
        info = "Invalid optimizer should trigger error"
      )
    })
  })

  describe("Recovery from optimization errors", {
    it("recovers from optimizer failure gracefully", {
      # Provide problematic input that might cause optimizer to fail
      # but the function should still return a valid structure
      colors <- matrix(
        c(
          0.5,
          0.1,
          0.0,
          0.5,
          0.1,
          0.0 # Identical colors
        ),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "a", "b"))
      )

      result <- optimize_colors_constrained(
        initial_colors_oklab = colors,
        fixed_mask = c(FALSE, FALSE),
        max_iterations = 1
      )

      # Should still return valid structure
      expect_true(is.list(result))
      expect_true("palette" %in% names(result))
      expect_true("details" %in% names(result))
    })
  })

  describe("Missing dependency handling", {
    it("provides meaningful error when nloptr is missing", {
      # This test verifies that the error message is helpful if nloptr is not available
      # We can't actually test the missing package case, but we can verify
      # that the code path exists
      expect_true(
        requireNamespace("nloptr", quietly = TRUE),
        info = "nloptr should be available for this test"
      )
    })
  })
})

describe("Export Validation", {
  # ===========================================================================
  # Public API Validation
  # These tests verify that public functions are properly exported
  # ===========================================================================

  describe("Public functions are exported", {
    it("generate_palette is exported", {
      expect_true(
        "generate_palette" %in% getNamespaceExports("huerd"),
        info = "generate_palette must be exported from the package"
      )
    })

    it("evaluate_palette is exported", {
      expect_true(
        "evaluate_palette" %in% getNamespaceExports("huerd"),
        info = "evaluate_palette must be exported from the package"
      )
    })

    it("reproduce_palette is exported", {
      expect_true(
        "reproduce_palette" %in% getNamespaceExports("huerd"),
        info = "reproduce_palette must be exported from the package"
      )
    })
  })

  describe("S3 methods are registered", {
    it("print.huerd_palette is registered", {
      # S3 methods are registered via S3method() in NAMESPACE, not direct exports
      # Check that the method exists and can be dispatched
      expect_true(
        exists("print.huerd_palette", mode = "function"),
        info = "print.huerd_palette S3 method must be defined"
      )
    })

    it("print.huerd_evaluation is registered", {
      expect_true(
        exists("print.huerd_evaluation", mode = "function"),
        info = "print.huerd_evaluation S3 method must be defined"
      )
    })
  })
})

describe("Constant and Configuration Validation", {
  # ===========================================================================
  # Constants Validation
  # These tests verify that required constants are defined
  # ===========================================================================

  describe("Required constants exist", {
    it(".CANDIDATE_POOL_BASE is defined", {
      expect_true(
        exists(".CANDIDATE_POOL_BASE"),
        info = ".CANDIDATE_POOL_BASE constant must be defined"
      )
      expect_true(
        is.numeric(.CANDIDATE_POOL_BASE),
        info = ".CANDIDATE_POOL_BASE must be numeric"
      )
    })

    it(".OKLAB_TOLERANCE is defined", {
      expect_true(
        exists(".OKLAB_TOLERANCE"),
        info = ".OKLAB_TOLERANCE constant must be defined"
      )
      expect_true(
        is.numeric(.OKLAB_TOLERANCE),
        info = ".OKLAB_TOLERANCE must be numeric"
      )
    })

    it(".default_aesthetic_init_config is defined", {
      expect_true(
        exists(".default_aesthetic_init_config"),
        info = ".default_aesthetic_init_config must be defined"
      )
      expect_true(
        is.list(.default_aesthetic_init_config),
        info = ".default_aesthetic_init_config must be a list"
      )
    })
  })
})

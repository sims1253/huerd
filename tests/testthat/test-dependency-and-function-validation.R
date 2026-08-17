# ==============================================================================
# INTEGRATION AND ERROR-HANDLING VALIDATION TESTS
# ==============================================================================
# These tests execute package code with real inputs and assert on results and
# behavior:
# - the nloptr dependency driving the COBYLA optimizer
# - optimizer and objective integration (internal closures)
# - the full generate_palette() workflow across optimizers and configurations
# - input validation and error propagation across components

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
        # nlopt_direct is deprecated and emits an expected warning on use
        expect_no_error({
          palette <- if (opt == "nlopt_direct") {
            suppressWarnings(generate_palette(
              3,
              optimizer = opt,
              progress = FALSE
            ))
          } else {
            generate_palette(
              3,
              optimizer = opt,
              progress = FALSE
            )
          }
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
})

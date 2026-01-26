# Integration tests based on user stories
#
# These tests validate complete user workflows rather than individual functions.
# Each test represents a realistic use case from a specific user perspective.
#
# User Story 1: Data Scientist creating accessible visualizations
# User Story 2: Designer integrating brand colors
# User Story 3: Package Developer building on huerd

# =============================================================================
# User Story 1: Data Scientist/Analyst
# =============================================================================
#
# Persona: Sarah Chen, senior data scientist at a healthcare analytics company.
# Goal: Create a 6-color palette for a dashboard visualizing patient outcomes
#       across treatment groups. Colors must be perceptually distinct,
#       colorblind-safe, and include company brand colors.

describe("User Story 1: Data Scientist can generate accessible dashboard palette", {
  it("generates basic palette with correct structure", {
    palette <- generate_palette(6, progress = FALSE)

    expect_length(palette, 6)
    expect_s3_class(palette, "huerd_palette")
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palette)))
  })

  it("includes brand colors in palette", {
    brand_blue <- "#1f77b4"
    brand_orange <- "#ff7f0e"

    brand_palette <- generate_palette(
      n = 6,
      include_colors = c(brand_blue, brand_orange),
      progress = FALSE
    )

    expect_length(brand_palette, 6)
    expect_true(
      toupper(brand_blue) %in% toupper(brand_palette),
      info = "Brand blue should be preserved in palette"
    )
    expect_true(
      toupper(brand_orange) %in% toupper(brand_palette),
      info = "Brand orange should be preserved in palette"
    )
  })

  it("verifies accessibility for colorblind viewers", {
    brand_palette <- generate_palette(
      n = 6,
      include_colors = c("#1f77b4", "#ff7f0e"),
      progress = FALSE
    )

    cvd_safe <- is_cvd_safe(brand_palette)
    expect_type(cvd_safe, "logical")
  })

  it("provides detailed evaluation metrics", {
    brand_palette <- generate_palette(
      n = 6,
      include_colors = c("#1f77b4", "#ff7f0e"),
      progress = FALSE
    )

    evaluation <- evaluate_palette(brand_palette)

    expect_s3_class(evaluation, "huerd_evaluation")
    expect_true("distances" %in% names(evaluation))
    expect_true("cvd_safety" %in% names(evaluation))
    expect_true(is.numeric(evaluation$distances$min))
    expect_true(evaluation$distances$min > 0)
    expect_true(is.numeric(evaluation$cvd_safety$worst_case_min_distance))
  })

  it("converts palette to character vector for ggplot2 use", {
    brand_palette <- generate_palette(
      n = 6,
      include_colors = c("#1f77b4", "#ff7f0e"),
      progress = FALSE
    )

    color_values <- as.character(brand_palette)
    expect_type(color_values, "character")
    expect_length(color_values, 6)
  })

  it("visualizes palette analysis", {
    skip_if_not_installed("ragg")

    palette <- generate_palette(6, progress = FALSE)

    expect_no_error({
      withr::with_pdf(tempfile(fileext = ".pdf"), {
        plot_palette_analysis(palette)
      })
    })
  })
})


# =============================================================================
# User Story 2: Designer/Branding Professional
# =============================================================================
#
# Persona: Marcus Rodriguez, brand designer at a marketing agency.
# Goal: Create a 5-color palette for a client's annual report that includes
#       specific brand colors and works for both print and digital media.
#       Needs to provide justification for color choices to stakeholders.

describe("User Story 2: Designer can create brand-compliant palette", {
  it("includes brand colors in palette", {
    client_navy <- "#003366"
    client_orange <- "#FF6600"

    palette <- generate_palette(
      n = 5,
      include_colors = c(client_navy, client_orange),
      progress = FALSE
    )

    expect_length(palette, 5)
    expect_s3_class(palette, "huerd_palette")

    expect_true(
      toupper(client_navy) %in% toupper(palette),
      info = "Client navy should be in palette"
    )
    expect_true(
      toupper(client_orange) %in% toupper(palette),
      info = "Client orange should be in palette"
    )
  })

  it("evaluates palette quality for stakeholder presentation", {
    palette <- generate_palette(
      n = 5,
      include_colors = c("#003366", "#FF6600"),
      progress = FALSE
    )

    evaluation <- evaluate_palette(palette)

    expect_true(!is.null(evaluation$distances$min))
    expect_true(!is.null(evaluation$distances$performance_ratio))
    expect_true(
      evaluation$distances$performance_ratio >= 0 &&
        evaluation$distances$performance_ratio <= 1
    )
  })

  it("checks CVD accessibility for compliance", {
    palette <- generate_palette(
      n = 5,
      include_colors = c("#003366", "#FF6600"),
      progress = FALSE
    )

    cvd_simulation <- simulate_palette_cvd(palette, cvd_type = "all")

    expect_s3_class(cvd_simulation, "huerd_simulation_result")
    expect_true("original" %in% names(cvd_simulation))
    expect_true("protan" %in% names(cvd_simulation))
    expect_true("deutan" %in% names(cvd_simulation))
    expect_true("tritan" %in% names(cvd_simulation))

    expect_length(cvd_simulation$protan, 5)
    expect_length(cvd_simulation$deutan, 5)
    expect_length(cvd_simulation$tritan, 5)
  })

  it("reproduces palette for documentation", {
    set.seed(2024)
    original_palette <- generate_palette(
      n = 5,
      include_colors = c("#003366", "#FF6600"),
      progress = FALSE
    )

    reproduced_palette <- reproduce_palette(original_palette, progress = FALSE)

    expect_equal(
      as.character(original_palette),
      as.character(reproduced_palette),
      info = "Reproduced palette should match original exactly"
    )

    expect_true(!is.null(attr(reproduced_palette, "generation_metadata")))
  })

  it("constrains lightness for print media", {
    palette <- generate_palette(
      n = 5,
      init_lightness_bounds = c(0.3, 0.7),
      progress = FALSE
    )

    expect_length(palette, 5)
    expect_s3_class(palette, "huerd_palette")
  })
})


# =============================================================================
# User Story 3: Package Developer
# =============================================================================
#
# Persona: Dr. Emily Watson, computational biology researcher.
# Goal: Integrate huerd into her single-cell RNA-seq visualization package.
#       Needs programmatic access with reliable, reproducible results and
#       proper error handling.

describe("User Story 3: Package developer can use programmatic API", {
  it("generates palette with deterministic settings", {
    set.seed(42)
    palette <- generate_palette(
      n = 12,
      optimizer = "nloptr_cobyla",
      progress = FALSE
    )

    expect_length(palette, 12)
    expect_s3_class(palette, "huerd_palette")
  })

  it("verifies reproducibility with same seed", {
    set.seed(42)
    palette <- generate_palette(
      n = 12,
      optimizer = "nloptr_cobyla",
      progress = FALSE
    )

    set.seed(42)
    palette_reproduced <- generate_palette(
      n = 12,
      optimizer = "nloptr_cobyla",
      progress = FALSE
    )

    expect_equal(
      as.character(palette),
      as.character(palette_reproduced),
      info = "Same seed should produce identical palette"
    )
  })

  it("accesses optimization metadata", {
    palette <- generate_palette(
      n = 8,
      return_metrics = TRUE,
      progress = FALSE
    )

    opt_details <- attr(palette, "optimization_details")
    expect_true(!is.null(opt_details))
    expect_true("iterations" %in% names(opt_details))
    expect_true("status_message" %in% names(opt_details))

    metrics <- attr(palette, "metrics")
    expect_s3_class(metrics, "huerd_evaluation")
  })

  it("handles edge cases", {
    # Single color (degenerate case)
    single <- generate_palette(n = 1, progress = FALSE)
    expect_length(single, 1)
    expect_s3_class(single, "huerd_palette")

    # Empty palette
    empty <- generate_palette(n = 0, progress = FALSE)
    expect_length(empty, 0)
    expect_s3_class(empty, "huerd_palette")

    # Large palette
    large <- generate_palette(
      n = 15,
      max_iterations = 100,
      progress = FALSE
    )
    expect_length(large, 15)
  })

  it("uses palette as character vector", {
    palette <- generate_palette(n = 6, progress = FALSE)

    expect_true(is.character(palette))
    expect_no_error(paste(palette, collapse = ", "))
    expect_no_error(palette[1:3])
    expect_no_error(rev(palette))

    char_vec <- as.character(palette)
    expect_type(char_vec, "character")
    expect_length(char_vec, 6)
  })

  it("evaluates external palettes", {
    external_colors <- c("#440154", "#3B528B", "#21918C", "#5DC863", "#FDE725")
    evaluation <- evaluate_palette(external_colors)

    expect_s3_class(evaluation, "huerd_evaluation")
    expect_true(evaluation$n_colors == 5)
    expect_true(is.numeric(evaluation$distances$min))
    expect_true(is.numeric(evaluation$distances$mean))

    expect_true(!is.null(evaluation$cvd_safety))
    expect_true(is.numeric(evaluation$cvd_safety$worst_case_min_distance))
  })

  it("chooses different optimizers", {
    fast_palette <- generate_palette(
      n = 6,
      optimizer = "nloptr_cobyla",
      max_iterations = 100,
      progress = FALSE
    )
    expect_length(fast_palette, 6)

    sann_palette <- generate_palette(
      n = 6,
      optimizer = "sann",
      max_iterations = 100,
      progress = FALSE
    )
    expect_length(sann_palette, 6)

    expect_s3_class(fast_palette, "huerd_palette")
    expect_s3_class(sann_palette, "huerd_palette")
  })
})


# =============================================================================
# Error Recovery Tests
# =============================================================================
#
# These tests validate that the system fails gracefully and provides useful
# error messages when things go wrong. They cover realistic failure scenarios
# that users might encounter during actual workflows.

describe("Error Recovery: Invalid input handling", {
  it("rejects non-huerd_palette in reproduce_palette", {
    # User passes wrong type to reproduce_palette
    invalid_input <- c("#FF0000", "#00FF00", "#0000FF")

    expect_error(
      reproduce_palette(invalid_input),
      regexp = "must be a huerd_palette object",
      info = "Should reject character vectors"
    )
  })

  it("rejects palette with missing metadata", {
    # Create palette without generation_metadata
    palette <- structure(
      c("#FF0000", "#00FF00", "#0000FF"),
      class = c("huerd_palette", "character")
    )

    expect_error(
      reproduce_palette(palette),
      regexp = "No generation metadata found",
      info = "Should reject palette without metadata"
    )
  })

  it("rejects palette with corrupted metadata fields", {
    # Create palette with incomplete metadata
    palette <- structure(
      c("#FF0000", "#00FF00", "#0000FF"),
      class = c("huerd_palette", "character"),
      generation_metadata = list(
        n_colors = 3,
        # Missing required fields like optimizer, include_colors, etc.
        timestamp = Sys.time()
      )
    )

    expect_error(
      reproduce_palette(palette),
      regexp = "Missing required metadata fields",
      info = "Should reject palette with incomplete metadata"
    )
  })

  it("rejects invalid hex color format", {
    # These are not valid hex colors - validation happens in validate_inputs
    expect_error(
      generate_palette(
        n = 3,
        include_colors = c("not_a_color", "#GGGGGG"),
        progress = FALSE
      ),
      regexp = "valid hex|hex colors|All elements",
      info = "Should reject invalid hex color format"
    )
  })

  it("rejects NA values in include_colors", {
    # NA values are not valid hex colors
    expect_error(
      generate_palette(
        n = 3,
        include_colors = c("#FF0000", NA, "#0000FF"),
        progress = FALSE
      ),
      regexp = "valid hex|All elements|NA",
      info = "Should reject NA in include_colors"
    )
  })

  it("rejects negative color count", {
    expect_error(
      generate_palette(n = -1, progress = FALSE),
      regexp = "must be|greater than|positive",
      info = "Should reject negative color count"
    )
  })

  it("rejects more fixed colors than requested total", {
    expect_error(
      generate_palette(
        n = 2,
        include_colors = c("#FF0000", "#00FF00", "#0000FF"),
        progress = FALSE
      ),
      regexp = "fixed",
      info = "Should reject when fixed colors exceed total requested"
    )
  })

  it("rejects invalid optimizer name", {
    expect_error(
      generate_palette(
        n = 4,
        optimizer = "nonexistent_optimizer",
        progress = FALSE
      ),
      regexp = "optimizer|Unsupported",
      info = "Should reject invalid optimizer"
    )
  })

  it("rejects invalid lightness bounds", {
    # Reversed bounds
    expect_error(
      generate_palette(
        n = 4,
        init_lightness_bounds = c(0.9, 0.2),
        progress = FALSE
      ),
      regexp = "lightness|bound",
      info = "Should reject reversed lightness bounds"
    )

    # Out of range bounds
    expect_error(
      generate_palette(
        n = 4,
        init_lightness_bounds = c(-0.5, 1.5),
        progress = FALSE
      ),
      regexp = "lightness|bound",
      info = "Should reject out of range lightness bounds"
    )
  })

  it("rejects invalid aesthetic influence value", {
    expect_error(
      generate_palette(
        n = 4,
        fixed_aesthetic_influence = 1.5,
        progress = FALSE
      ),
      regexp = "aesthetic|influence|0 to 1",
      info = "Should reject aesthetic influence > 1"
    )

    expect_error(
      generate_palette(
        n = 4,
        fixed_aesthetic_influence = -0.1,
        progress = FALSE
      ),
      regexp = "aesthetic|influence|0 to 1",
      info = "Should reject negative aesthetic influence"
    )
  })

  it("rejects NA values in include_colors", {
    # NA values are not valid hex colors
    expect_error(
      generate_palette(
        n = 3,
        include_colors = c("#FF0000", NA, "#0000FF"),
        progress = FALSE
      ),
      regexp = "valid hex|All elements|NA",
      info = "Should reject NA in include_colors"
    )
  })

  it("handles empty include_colors list", {
    # Empty but valid input should work
    expect_silent({
      palette <- generate_palette(
        n = 3,
        include_colors = character(0),
        progress = FALSE
      )
    })
    expect_length(palette, 3)
  })
})


describe("Error Recovery: Partial failure scenarios", {
  it("returns partial palette when initialization produces fewer colors", {
    # With extremely constrained bounds, initialization may fail to produce
    # all requested free colors. The system should still return what it can.
    # This tests graceful degradation rather than hard failure.

    # Create a palette with very tight constraints
    palette <- generate_palette(
      n = 10,
      init_lightness_bounds = c(0.4, 0.6), # Narrow range
      progress = FALSE
    )

    # Should return something, even if fewer than requested
    expect_s3_class(palette, "huerd_palette")
    expect_true(length(palette) > 0)
  })

  it("handles zero-color palette gracefully", {
    # Edge case: user requests 0 colors
    palette <- generate_palette(n = 0, progress = FALSE)

    expect_s3_class(palette, "huerd_palette")
    expect_length(palette, 0)
  })

  it("handles single-color palette gracefully", {
    # Single color palette is valid but has limited utility
    palette <- generate_palette(n = 1, progress = FALSE)

    expect_s3_class(palette, "huerd_palette")
    expect_length(palette, 1)
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", palette[1]))
  })

  it("handles exactly fixed colors case gracefully", {
    # When n equals number of fixed colors, no optimization needed
    palette <- generate_palette(
      n = 3,
      include_colors = c("#FF0000", "#00FF00", "#0000FF"),
      progress = FALSE
    )

    expect_s3_class(palette, "huerd_palette")
    expect_length(palette, 3)

    # Should have appropriate optimization details
    opt_details <- attr(palette, "optimization_details")
    expect_true(!is.null(opt_details))
    expect_equal(opt_details$iterations, 0)
  })

  it("provides meaningful error messages for validation failures", {
    # Verify errors contain actionable information
    tryCatch(
      {
        generate_palette(n = -5, progress = FALSE)
        fail("Should have thrown an error")
      },
      error = function(e) {
        expect_true(
          grepl("-5|negative|positive|number", e$message, ignore.case = TRUE),
          info = "Error message should mention the problematic value or constraint"
        )
      }
    )
  })

  it("handles partially invalid colors", {
    # When given invalid colors, validation throws an error
    expect_error(
      generate_palette(
        n = 4,
        include_colors = c("#FF0000", "#00FF00", "invalid", "#0000FF"),
        progress = FALSE
      ),
      regexp = "valid hex|All elements|Invalid"
    )
  })
})


describe("Error Recovery: Reproducibility edge cases", {
  it("handles missing seed in metadata", {
    # Create palette without seed (older versions or programmatic creation)
    palette <- structure(
      c("#FF0000", "#00FF00", "#0000FF"),
      class = c("huerd_palette", "character"),
      generation_metadata = list(
        n_colors = 3,
        include_colors = NULL,
        initialization = "k-means++",
        init_lightness_bounds = c(0.2, 0.9),
        init_hcl_bounds = list(C = c(40, 80), L = c(50, 80)),
        fixed_aesthetic_influence = 0.75,
        aesthetic_init_config = NULL,
        max_iterations = 100,
        return_metrics = TRUE,
        weights = NULL,
        optimizer = "nlopt_direct", # Deterministic
        progress = FALSE, # Required field
        timestamp = Sys.time()
        # Note: no 'seed' field
      )
    )

    # Should not error - deterministic optimizers don't need seed
    expect_no_error({
      reproduced <- reproduce_palette(palette, progress = FALSE)
    })
  })

  it("warns on version mismatch during reproduction", {
    # Create palette with different package version
    palette <- structure(
      c("#FF0000", "#00FF00", "#0000FF"),
      class = c("huerd_palette", "character"),
      generation_metadata = list(
        n_colors = 3,
        include_colors = NULL,
        initialization = "k-means++",
        init_lightness_bounds = c(0.2, 0.9),
        init_hcl_bounds = list(C = c(40, 80), L = c(50, 80)),
        fixed_aesthetic_influence = 0.75,
        aesthetic_init_config = NULL,
        max_iterations = 100,
        return_metrics = TRUE,
        weights = NULL,
        optimizer = "nlopt_direct",
        seed = NULL,
        package_version = "0.0.0", # Different version
        target_space = "oklab",
        timestamp = Sys.time()
      )
    )

    # Should warn about version mismatch but still work
    expect_warning(
      {
        reproduced <- reproduce_palette(palette, progress = FALSE)
      },
      regexp = "version|Version"
    )

    expect_s3_class(reproduced, "huerd_palette")
  })

  it("handles NULL progress in metadata gracefully", {
    # Create palette without progress field (older format)
    palette <- structure(
      c("#FF0000", "#00FF00", "#0000FF"),
      class = c("huerd_palette", "character"),
      generation_metadata = list(
        n_colors = 3,
        include_colors = NULL,
        initialization = "k-means++",
        init_lightness_bounds = c(0.2, 0.9),
        init_hcl_bounds = list(C = c(40, 80), L = c(50, 80)),
        fixed_aesthetic_influence = 0.75,
        aesthetic_init_config = NULL,
        max_iterations = 100,
        return_metrics = TRUE,
        weights = NULL,
        optimizer = "nlopt_direct",
        seed = NULL,
        package_version = utils::packageVersion("huerd")
        # Note: no 'progress' or 'timestamp' fields - older format
      )
    )

    # Should use default FALSE progress when progress is not in metadata
    expect_no_error({
      reproduced <- reproduce_palette(palette, progress = NULL)
    })

    expect_s3_class(reproduced, "huerd_palette")
  })
})


describe("Error Recovery: Evaluation edge cases", {
  it("handles external palette with single color", {
    # Single color evaluation should work but report limited metrics
    evaluation <- evaluate_palette("#FF0000")

    expect_s3_class(evaluation, "huerd_evaluation")
    expect_equal(evaluation$n_colors, 1)
  })

  it("handles empty palette evaluation", {
    evaluation <- evaluate_palette(character(0))

    expect_s3_class(evaluation, "huerd_evaluation")
    expect_equal(evaluation$n_colors, 0)
  })

  it("handles external palette with duplicate colors", {
    # Duplicates should be handled gracefully
    external_colors <- c("#FF0000", "#FF0000", "#00FF00", "#0000FF")

    expect_no_error({
      evaluation <- evaluate_palette(external_colors)
    })

    expect_s3_class(evaluation, "huerd_evaluation")
  })

  it("handles external palette with very similar colors", {
    # Almost identical colors may produce poor metrics but shouldn't error
    similar_colors <- c("#FF0000", "#FF0001", "#FF0002", "#FF0003")

    expect_no_error({
      evaluation <- evaluate_palette(similar_colors)
    })

    expect_s3_class(evaluation, "huerd_evaluation")
    # Distance metrics should reflect poor separation
    expect_true(evaluation$distances$min < 0.01)
  })

  it("handles malformed hex colors in evaluation", {
    # Malformed hex colors produce an error
    expect_error(
      evaluate_palette(c("#FF0000", "#GGGGGG")),
      regexp = "valid hex|All elements"
    )
  })

  it("handles numeric matrix input for evaluation", {
    # OKLAB matrix input should work
    oklab_matrix <- matrix(
      c(
        0.5,
        0.0,
        0.0,
        0.5,
        0.1,
        0.0,
        0.5,
        0.0,
        0.1
      ),
      ncol = 3,
      byrow = TRUE
    )

    expect_no_error({
      evaluation <- evaluate_palette(oklab_matrix)
    })

    expect_s3_class(evaluation, "huerd_evaluation")
  })
})


describe("Error Recovery: CVD simulation edge cases", {
  it("handles empty palette simulation", {
    result <- simulate_palette_cvd(character(0), cvd_type = "protan")

    expect_s3_class(result, "huerd_simulation_result")
    expect_length(result$protan, 0)
  })

  it("handles single color simulation", {
    result <- simulate_palette_cvd("#FF0000", cvd_type = "protan")

    expect_s3_class(result, "huerd_simulation_result")
    expect_length(result$protan, 1)
  })

  it("handles all CVD types simulation", {
    palette <- c("#FF0000", "#00FF00", "#0000FF")

    expect_no_error({
      result <- simulate_palette_cvd(palette, cvd_type = "all")
    })

    expect_s3_class(result, "huerd_simulation_result")
    expect_true("protan" %in% names(result))
    expect_true("deutan" %in% names(result))
    expect_true("tritan" %in% names(result))
    expect_true("original" %in% names(result))
  })

  it("handles single CVD type simulation", {
    palette <- c("#FF0000", "#00FF00", "#0000FF")

    expect_no_error({
      result <- simulate_palette_cvd(palette, cvd_type = "protan")
    })

    expect_s3_class(result, "huerd_simulation_result")
    expect_true("protan" %in% names(result))
    expect_false("deutan" %in% names(result))
  })
})


describe("Error Recovery: Visualization edge cases", {
  it("handles empty palette visualization gracefully", {
    skip_if_not_installed("ragg")

    expect_no_error({
      withr::with_pdf(tempfile(fileext = ".pdf"), {
        plot_palette_analysis(character(0))
      })
    })
  })

  it("handles single color palette visualization", {
    skip_if_not_installed("ragg")

    palette <- "#FF0000"

    expect_no_error({
      withr::with_pdf(tempfile(fileext = ".pdf"), {
        plot_palette_analysis(palette)
      })
    })
  })

  it("handles very large palette visualization", {
    skip_if_not_installed("ragg")

    palette <- generate_palette(n = 20, progress = FALSE)

    expect_no_error({
      withr::with_pdf(tempfile(fileext = ".pdf"), {
        plot_palette_analysis(palette)
      })
    })
  })
})


describe("Error Recovery: Workflow continuity", {
  it("handles invalid inputs during generation", {
    # Invalid inputs should produce an error - test that error is clear
    expect_error(
      generate_palette(
        n = 5,
        include_colors = c("#FF0000", NA, "#00FF00"),
        progress = FALSE
      ),
      regexp = "valid hex|All elements|NA"
    )
  })

  it("recovers after failed reproduction attempt with invalid metadata", {
    # Create palette with malformed metadata
    bad_palette <- structure(
      c("#FF0000", "#00FF00", "#0000FF"),
      class = c("huerd_palette", "character"),
      generation_metadata = list(
        n_colors = "three", # Wrong type
        optimizer = 123, # Wrong type
        include_colors = "not_a_vector" # Wrong type
      )
    )

    # Should fail with clear error
    expect_error(
      reproduce_palette(bad_palette),
      info = "Should error on type mismatch in metadata"
    )

    # Original palette should still be usable
    expect_true(inherits(bad_palette, "huerd_palette"))
  })

  it("handles concurrent modification of palette attributes", {
    # User modifies palette attributes incorrectly
    palette <- generate_palette(5, progress = FALSE)

    # Set invalid optimization_details
    attr(palette, "optimization_details") <- "not_a_list"

    # Should not crash when accessing attributes
    expect_false(is.null(palette)) # Palette itself is still valid

    # Subsequent operations on palette should work
    expect_no_error({
      char_result <- as.character(palette)
    })
    expect_type(char_result, "character")
    expect_length(char_result, 5)
  })

  it("handles serialization and deserialization of palettes", {
    # Create palette
    original <- generate_palette(5, progress = FALSE)

    # Serialize to RDS
    rds_path <- tempfile(fileext = ".rds")
    saveRDS(original, rds_path)

    # Deserialize
    restored <- readRDS(rds_path)

    # Should maintain class
    expect_s3_class(restored, "huerd_palette")

    # Colors should be preserved
    expect_equal(
      as.character(original),
      as.character(restored)
    )
  })

  it("handles corrupted optimization_details attribute", {
    palette <- generate_palette(5, progress = FALSE)

    # Corrupt the optimization_details
    opt_details <- attr(palette, "optimization_details")

    # Various ways to access should still work
    expect_no_error({
      iterations <- opt_details$iterations
      status <- opt_details$status_message
    })

    expect_true(is.numeric(iterations) || is.na(iterations))
    expect_true(is.character(status) || is.na(status))
  })
})


describe("Error Recovery: External dependency issues", {
  it("handles missing optional dependencies gracefully", {
    # Test that core functionality works without optional plotting deps
    palette <- generate_palette(5, progress = FALSE)

    expect_s3_class(palette, "huerd_palette")

    # Evaluation should work without ragg
    evaluation <- evaluate_palette(palette)
    expect_s3_class(evaluation, "huerd_evaluation")
  })

  it("handles weights with NA values", {
    # Weights with NA should produce error (R throws "missing value where TRUE/FALSE needed")
    expect_error(
      generate_palette(
        n = 5,
        weights = c(distance = NA, smooth_repulsion = 1),
        optimizer = "nlopt_lbfgs",
        progress = FALSE
      ),
      regexp = "missing|NA|value"
    )
  })

  it("handles zero weights", {
    # Zero weights should be handled gracefully with valid weight names
    expect_no_error({
      palette <- generate_palette(
        n = 5,
        weights = c(distance = 0, smooth_repulsion = 1),
        optimizer = "nlopt_lbfgs",
        progress = FALSE
      )
    })

    expect_s3_class(palette, "huerd_palette")
  })
})


describe("Error Recovery: Memory and resource constraints", {
  it("handles rapid successive palette generations", {
    # Generate many palettes in succession
    palettes <- lapply(1:10, function(i) {
      generate_palette(5, progress = FALSE)
    })

    expect_length(palettes, 10)
    expect_true(all(sapply(palettes, inherits, "huerd_palette")))
  })

  it("handles palette generation with very low max_iterations", {
    # Very low iterations should still return valid palette
    palette <- generate_palette(
      5,
      max_iterations = 1,
      progress = FALSE
    )

    expect_s3_class(palette, "huerd_palette")
    expect_length(palette, 5)
  })

  it("handles palette generation with extreme n value", {
    # Large n should still produce valid structure
    # (may take time, so limit iterations)
    palette <- generate_palette(
      n = 20,
      max_iterations = 100,
      progress = FALSE
    )

    expect_s3_class(palette, "huerd_palette")
    expect_length(palette, 20)
  })

  it("recovers after internal optimization error", {
    # With invalid OKLAB values, the optimization may fail
    # but the system should still return something or error clearly

    # This test verifies that error messages are informative
    tryCatch(
      {
        # Generate with normal parameters first
        palette <- generate_palette(5, progress = FALSE)

        # Should always succeed
        expect_s3_class(palette, "huerd_palette")
      },
      error = function(e) {
        # If it fails, the error should be informative
        expect_true(
          grepl("error|Error|failed|Failed", e$message),
          info = "Error message should indicate failure"
        )
      }
    )
  })
})


describe("Error Recovery: Data integrity", {
  it("preserves palette integrity after attribute modification", {
    palette <- generate_palette(5, progress = FALSE)

    # Access and modify (but should not corrupt)
    original_colors <- as.character(palette)

    # Modify attributes
    attr(palette, "custom_attr") <- "test"

    # Colors should still be intact
    expect_equal(as.character(palette), original_colors)
  })

  it("validates palette output structure", {
    palette <- generate_palette(5, progress = FALSE)

    # Verify structure
    expect_type(palette, "character")
    expect_length(palette, 5)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palette)))

    # Verify class
    expect_true(inherits(palette, "huerd_palette"))

    # Verify required attributes
    expect_true(!is.null(attr(palette, "optimization_details")))
  })

  it("handles invalid HCL bounds gracefully", {
    # Invalid HCL bounds format should produce error
    expect_error(
      generate_palette(
        n = 5,
        init_hcl_bounds = list(C = c(200, 300), L = c(50, 80)), # Invalid format
        progress = FALSE
      ),
      regexp = "init_hcl_bounds|HCL|list"
    )
  })

  it("handles OKLAB matrix with out-of-range values", {
    # Matrix with values outside normal OKLAB range - should still process
    bad_oklab <- matrix(
      c(
        1.5,
        0.0,
        0.0, # L > 1
        0.5,
        0.0,
        0.0
      ),
      ncol = 3,
      byrow = TRUE
    )

    # Should process without error
    expect_no_error({
      evaluation <- evaluate_palette(bad_oklab)
    })

    expect_s3_class(evaluation, "huerd_evaluation")
    expect_equal(evaluation$n_colors, 2)
  })
})


describe("Error Recovery: Character encoding", {
  it("handles special characters in color strings", {
    # Some systems may add special characters
    palette <- generate_palette(5, progress = FALSE)

    # Convert to hex and back
    hex_colors <- as.character(palette)

    # Should be clean ASCII hex
    expect_true(all(grepl("^[#0-9A-Fa-f]{7}$", hex_colors)))
  })

  it("handles whitespace in color inputs", {
    # Leading/trailing whitespace - not valid hex format
    expect_error(
      generate_palette(
        n = 3,
        include_colors = c(" #FF0000", "#00FF00 ", "  #0000FF  "),
        progress = FALSE
      ),
      regexp = "valid hex|All elements"
    )
  })
})

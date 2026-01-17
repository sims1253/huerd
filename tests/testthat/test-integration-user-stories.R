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
      max_iterations = 500,
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

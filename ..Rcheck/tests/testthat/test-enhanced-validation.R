describe("validate_color_input_smart()", {
  it("accepts valid hex colors", {
    valid_colors <- c("#FF0000", "#00FF00", "#0000FF")

    result <- validate_color_input_smart(valid_colors, context = "evaluation")

    expect_true(result$valid)
    expect_length(result$errors, 0)
  })

  it("warns about problematic but not invalid colors", {
    mixed_colors <- c("#FF0000", NA, "#0000FF")

    expect_warning(
      {
        result <- validate_color_input_smart(
          mixed_colors,
          context = "evaluation"
        )
      },
      "NA color values"
    )

    expect_true(result$valid)
  })

  it("rejects clearly invalid input", {
    expect_error(
      {
        validate_color_input_smart(123, context = "evaluation")
      },
      "no base colors provided"
    )

    expect_error(
      {
        validate_color_input_smart(
          c("not_a_color", "also_invalid"),
          context = "evaluation",
          strict_mode = TRUE
        )
      },
      "Invalid hex colors"
    )
  })

  it("provides context-appropriate feedback", {
    single_color <- "#FF0000"

    expect_warning(
      {
        result <- validate_color_input_smart(
          single_color,
          context = "evaluation"
        )
      },
      "fewer than 2 colors"
    )

    expect_no_warning({
      result <- validate_color_input_smart(single_color, context = "generation")
    })
  })
})

describe("validate_oklab_matrix()", {
  it("works correctly with valid OKLAB matrix", {
    valid_oklab <- matrix(c(0.5, 0, 0, 0.7, 0.1, -0.1), ncol = 3, byrow = TRUE)
    validation_result <- list(
      valid = TRUE,
      warnings = character(0),
      errors = character(0),
      processed_colors = NULL
    )

    result <- validate_oklab_matrix(
      valid_oklab,
      validation_result,
      strict_mode = FALSE
    )

    expect_true(result$valid)
    expect_length(result$errors, 0)
  })

  it("warns about unusual values", {
    extreme_oklab <- matrix(c(1.5, 0.8, 0.8), ncol = 3)
    validation_result <- list(
      valid = TRUE,
      warnings = character(0),
      errors = character(0),
      processed_colors = NULL
    )

    result <- validate_oklab_matrix(
      extreme_oklab,
      validation_result,
      strict_mode = FALSE
    )

    expect_true(result$valid)
    expect_true(length(result$warnings) > 0)
  })
})

describe("validation modes", {
  it("uses strict mode more restrictively than default", {
    problematic_colors <- c("#FF0000", "invalid", "#0000FF")

    expect_error({
      validate_color_input_smart(problematic_colors, strict_mode = TRUE)
    })

    expect_warning({
      result <- validate_color_input_smart(
        problematic_colors,
        strict_mode = FALSE
      )
      expect_true(result$valid)
    })
  })
})

test_that("smart validation accepts valid hex colors", {
  skip_if_not_available("validate_color_input_smart")

  valid_colors <- c("#FF0000", "#00FF00", "#0000FF")

  result <- validate_color_input_smart(valid_colors, context = "evaluation")

  # Should pass validation
  expect_true(result$valid)
  expect_length(result$errors, 0)
})

test_that("smart validation warns about problematic but not invalid colors", {
  skip_if_not_available("validate_color_input_smart")

  # Mix of valid and NA colors
  mixed_colors <- c("#FF0000", NA, "#0000FF")

  expect_warning(
    {
      result <- validate_color_input_smart(mixed_colors, context = "evaluation")
    },
    "NA color values"
  )

  # Should still be valid (warnings, not errors)
  expect_true(result$valid)
})

test_that("smart validation rejects clearly invalid input", {
  skip_if_not_available("validate_color_input_smart")

  # Completely wrong type
  expect_error(
    {
      validate_color_input_smart(123, context = "evaluation")
    },
    "Colors must be"
  )

  # Invalid hex format
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

test_that("context-aware validation provides appropriate feedback", {
  skip_if_not_available("validate_color_input_smart")

  single_color <- "#FF0000"

  # Should warn when evaluating single color
  expect_warning(
    {
      result <- validate_color_input_smart(single_color, context = "evaluation")
    },
    "fewer than 2 colors"
  )

  # Should not warn in generation context
  expect_no_warning({
    result <- validate_color_input_smart(single_color, context = "generation")
  })
})

test_that("OKLAB matrix validation works correctly", {
  skip_if_not_available("validate_oklab_matrix")

  # Valid OKLAB matrix
  valid_oklab <- matrix(c(0.5, 0, 0, 0.7, 0.1, -0.1), ncol = 3, byrow = TRUE)

  result <- list(valid = TRUE, warnings = character(0), errors = character(0))
  result <- validate_oklab_matrix(valid_oklab, result, strict_mode = FALSE)

  expect_true(result$valid)
  expect_length(result$errors, 0)
})

test_that("OKLAB validation warns about unusual values", {
  skip_if_not_available("validate_oklab_matrix")

  # OKLAB with extreme values
  extreme_oklab <- matrix(c(1.5, 0.8, 0.8), ncol = 3) # L > 1, extreme a/b

  result <- list(valid = TRUE, warnings = character(0), errors = character(0))
  result <- validate_oklab_matrix(extreme_oklab, result, strict_mode = FALSE)

  # Should have warnings but still be valid
  expect_true(result$valid)
  expect_true(length(result$warnings) > 0)
})

test_that("strict mode is more restrictive than default", {
  skip_if_not_available("validate_color_input_smart")

  problematic_colors <- c("#FF0000", "invalid", "#0000FF")

  # Strict mode should error
  expect_error({
    validate_color_input_smart(problematic_colors, strict_mode = TRUE)
  })

  # Default mode should warn but pass
  expect_warning({
    result <- validate_color_input_smart(
      problematic_colors,
      strict_mode = FALSE
    )
    expect_true(result$valid)
  })
})

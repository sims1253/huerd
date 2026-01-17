# Extracted from test-enhanced-validation.R:36

# test -------------------------------------------------------------------------
skip_if_not_available("validate_color_input_smart")
expect_error(
  {
    validate_color_input_smart(123, context = "evaluation")
  },
  "Colors must be"
)

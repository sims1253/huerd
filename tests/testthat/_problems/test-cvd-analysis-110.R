# Extracted from test-cvd-analysis.R:110

# test -------------------------------------------------------------------------
expect_warning(
  {
    result <- simulate_palette_cvd(character(0), cvd_type = "deutan")
  },
  "Input 'colors' contains no valid colors"
)
expect_true(is.character(result))
expect_equal(length(result), 0)

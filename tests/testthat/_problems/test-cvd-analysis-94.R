# Extracted from test-cvd-analysis.R:94

# test -------------------------------------------------------------------------
colors <- c("#FF0000")
mild_result <- simulate_palette_cvd(
  colors,
  cvd_type = "deutan",
  severity = 0.5
)
complete_result <- simulate_palette_cvd(
  colors,
  cvd_type = "deutan",
  severity = 1.0
)
expect_true(is.character(mild_result))
expect_true(is.character(complete_result))

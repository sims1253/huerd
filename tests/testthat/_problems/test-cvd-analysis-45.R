# Extracted from test-cvd-analysis.R:45

# test -------------------------------------------------------------------------
colors <- c("#FF0000", "#00FF00", "#0000FF")
result <- simulate_palette_cvd(colors, cvd_type = "deutan")
expect_true(is.character(result))

# Extracted from test-cvd-analysis.R:63

# test -------------------------------------------------------------------------
colors <- c("#FF0000", "#00FF00")
deutan_result <- simulate_palette_cvd(colors, cvd_type = "deutan")
protan_result <- simulate_palette_cvd(colors, cvd_type = "protan")
tritan_result <- simulate_palette_cvd(colors, cvd_type = "tritan")
expect_true(is.character(deutan_result))
expect_true(is.character(protan_result))
expect_true(is.character(tritan_result))
expect_equal(length(deutan_result), 2)
expect_equal(length(protan_result), 2)
expect_equal(length(tritan_result), 2)

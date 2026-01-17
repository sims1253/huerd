# Extracted from test-cvd-analysis.R:155

# test -------------------------------------------------------------------------
all_na_colors <- c(NA_character_, NA_character_, NA_character_)
expect_warning(
  {
    result1 <- simulate_palette_cvd(all_na_colors, cvd_type = "protan")
  },
  "Input 'colors' contains no valid colors"
)
expect_true(is.character(result1))
expect_equal(length(result1), 0)

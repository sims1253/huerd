# Extracted from test-convenience.R:177

# test -------------------------------------------------------------------------
pal <- generate_palette(5, progress = FALSE)
result <- interpret_palette_quality(pal)
expect_output(print(result), "Palette Quality Assessment")

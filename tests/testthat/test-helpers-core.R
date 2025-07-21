# Tests for %||% operator
test_that("%||% handles NULL values correctly", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(NULL %||% 42, 42)
  expect_equal(NULL %||% NULL, NULL)
})

test_that("%||% handles numeric NA and non-finite values", {
  expect_equal(NA_real_ %||% 42, 42)
  expect_equal(5.5 %||% 42, 5.5)
  expect_equal(Inf %||% 42, 42)
  expect_equal(-Inf %||% 42, 42)
  expect_equal(NaN %||% 42, 42)
  expect_equal(numeric(0) %||% 42, 42)
})

test_that("%||% handles character NA and other types", {
  expect_equal(NA_character_ %||% "default", "default")
  expect_equal("hello" %||% "default", "hello")
  expect_equal(TRUE %||% FALSE, TRUE)
  expect_equal(list(a = 1) %||% list(b = 2), list(a = 1))
})

test_that("%||% handles vector inputs correctly", {
  # The main bug: %||% should handle vectors properly
  expect_equal(c(1, 2) %||% 99, c(1, 2))
  expect_equal(c("a", "b") %||% "default", c("a", "b"))
  expect_equal(c(TRUE, FALSE) %||% TRUE, c(TRUE, FALSE))

  # Vector with some NA values should return the vector (not the fallback)
  expect_equal(c(1, NA, 3) %||% 99, c(1, NA, 3))
  expect_equal(c("a", NA, "c") %||% "default", c("a", NA, "c"))
})

test_that("%||% handles edge cases that caused the original bug", {
  # These specific cases were causing 'length = X' in coercion to 'logical(1)' errors
  range_vector <- c(0.1, 0.9) # Like lightness_oklab$range
  expect_equal(range_vector %||% c(0, 0), range_vector)

  color_vector <- c("#FF0000", "#00FF00")
  expect_equal(color_vector %||% character(0), color_vector)

  # Mixed vector that was problematic
  mixed_vector <- c(1, 2, 3, 4)
  expect_equal(mixed_vector %||% "fallback", mixed_vector)
})

# Tests for calculate_perceptual_distances
test_that("calculate_perceptual_distances handles empty input", {
  empty_matrix <- matrix(numeric(0), nrow = 0, ncol = 3)
  result <- calculate_perceptual_distances(empty_matrix)
  expect_equal(dim(result), c(0, 0))
  expect_true(is.matrix(result))
})

test_that("calculate_perceptual_distances handles single color", {
  single_color <- matrix(c(0.5, 0.1, -0.1), nrow = 1, ncol = 3)
  result <- calculate_perceptual_distances(single_color)
  expect_equal(dim(result), c(1, 1))
  expect_equal(result[1, 1], 0)
  expect_true(is.matrix(result))
})

test_that("calculate_perceptual_distances handles invalid input", {
  # NULL input
  result_null <- calculate_perceptual_distances(NULL)
  expect_true(is.matrix(result_null))
  expect_equal(dim(result_null), c(0, 0))
  expect_true(all(is.na(result_null) | length(result_null) == 0))

  # Non-matrix input (vector gets converted to column matrix)
  result_vector <- calculate_perceptual_distances(c(1, 2, 3))
  expect_true(is.matrix(result_vector))
  expect_equal(dim(result_vector), c(3, 3))
  expect_true(all(is.na(result_vector)))

  # Wrong number of columns
  wrong_cols <- matrix(c(1, 2), nrow = 1, ncol = 2)
  result_wrong <- calculate_perceptual_distances(wrong_cols)
  expect_true(is.matrix(result_wrong))
  expect_equal(dim(result_wrong), c(1, 1))
  expect_true(all(is.na(result_wrong)))
})

# ERROR CONDITION TESTS - Testing validation function error paths through generate_palette

test_that("generate_palette validation catches invalid aesthetic parameters", {
  # Test invalid aesthetic_init_config parameters (through generate_palette)

  # Invalid config type (not a list)
  expect_error(
    generate_palette(3, aesthetic_init_config = "invalid"),
    "'aesthetic_init_config' must be a list or NULL."
  )

  # Invalid config names
  expect_error(
    generate_palette(3, aesthetic_init_config = list(invalid_param = 0.5)),
    "Invalid name 'invalid_param' in aesthetic_init_config"
  )

  # Invalid config values (not numeric)
  expect_error(
    generate_palette(
      3,
      aesthetic_init_config = list(kmeans_L_sd_multiplier = "invalid")
    ),
    "Parameter 'kmeans_L_sd_multiplier' in aesthetic_init_config must be a single numeric value"
  )

  # Invalid config values (vector instead of single value)
  expect_error(
    generate_palette(
      3,
      aesthetic_init_config = list(kmeans_L_sd_multiplier = c(0.5, 0.7))
    ),
    "Parameter 'kmeans_L_sd_multiplier' in aesthetic_init_config must be a single numeric value"
  )
})


test_that("calculate_perceptual_distances handles single color with NA", {
  single_na <- matrix(c(0.5, NA, -0.1), nrow = 1, ncol = 3)
  result <- calculate_perceptual_distances(single_na)
  expect_equal(dim(result), c(1, 1))
  expect_true(is.na(result[1, 1]))
  expect_true(is.matrix(result))
})

test_that("calculate_perceptual_distances works with two colors", {
  two_colors <- matrix(
    c(
      0.5,
      0.1,
      -0.1,
      0.7,
      0.2,
      0.1
    ),
    nrow = 2,
    ncol = 3,
    byrow = TRUE
  )
  result <- calculate_perceptual_distances(two_colors)

  expect_equal(dim(result), c(2, 2))
  expect_equal(result[1, 1], 0)
  expect_equal(result[2, 2], 0)
  expect_equal(result[1, 2], result[2, 1]) # symmetric
  expect_true(result[1, 2] > 0) # positive distance
  expect_true(is.matrix(result))
})

test_that("calculate_perceptual_distances handles invalid input", {
  # Not a matrix
  result_vector <- calculate_perceptual_distances(c(1, 2, 3))
  expect_true(all(is.na(result_vector)))

  # Wrong number of columns
  wrong_cols <- matrix(c(1, 2), nrow = 1, ncol = 2)
  result_wrong <- calculate_perceptual_distances(wrong_cols)
  expect_true(all(is.na(result_wrong)))

  # NULL input
  result_null <- calculate_perceptual_distances(NULL)
  expect_true(all(is.na(result_null)))
})

# Tests for .merge_aesthetic_config
test_that(".merge_aesthetic_config merges with defaults correctly", {
  # Test with NULL user config
  result <- .merge_aesthetic_config(NULL)
  expect_true(is.list(result))
  expect_true("config_version" %in% names(result))
})

test_that(".merge_aesthetic_config merges user config with defaults", {
  # Need to check what's in the default config first
  default_config <- .DEFAULT_AESTHETIC_INIT_CONFIG
  user_config <- list(test_param = 999)

  result <- .merge_aesthetic_config(user_config)
  expect_true(is.list(result))
  expect_equal(result$test_param, 999)
  # Should still have default values for other parameters
  expect_true("config_version" %in% names(result))
})

test_that(".merge_aesthetic_config handles version mismatch warning", {
  # Test version mismatch warning path
  user_config <- list(config_version = "999.0", test_param = 123)

  expect_snapshot(.merge_aesthetic_config(user_config))
})

# Tests for .normalize_weights
test_that(".normalize_weights handles already normalized weights", {
  weights <- c(0.5, 0.5)
  result <- .normalize_weights(weights, "test_weights", progress = FALSE)
  expect_equal(result, weights)
  expect_equal(sum(result), 1.0)
})

test_that(".normalize_weights normalizes unnormalized weights", {
  weights <- c(2, 4, 6) # Sum = 12
  result <- .normalize_weights(weights, "test_weights", progress = FALSE)
  expect_equal(result, c(2 / 12, 4 / 12, 6 / 12))
  expect_equal(sum(result), 1.0)
})

test_that(".normalize_weights shows warning when normalizing with progress", {
  weights <- c(2, 4, 6) # Sum = 12
  expect_snapshot(.normalize_weights(weights, "test_weights", progress = TRUE))
})

# Tests for validate_inputs
test_that("validate_inputs validates n parameter", {
  # Basic valid parameters
  valid_params <- list(
    n = 3,
    include_colors = NULL,
    init_lightness_bounds = c(0.2, 0.8),
    init_hcl_bounds = list(L = c(20, 80), C = c(10, 60)),
    fixed_aesthetic_influence = 0.5,
    aesthetic_init_config = NULL
  )

  # Valid n
  expect_silent(do.call(validate_inputs, valid_params))

  # Invalid n: not numeric
  invalid_params_1 <- valid_params
  invalid_params_1$n <- "3"
  expect_error(
    do.call(validate_inputs, invalid_params_1),
    "'n' must be a single non-negative integer"
  )

  # Invalid n: negative
  invalid_params_2 <- valid_params
  invalid_params_2$n <- -1
  expect_error(
    do.call(validate_inputs, invalid_params_2),
    "'n' must be a single non-negative integer"
  )

  # Invalid n: not integer
  invalid_params_3 <- valid_params
  invalid_params_3$n <- 3.5
  expect_error(
    do.call(validate_inputs, invalid_params_3),
    "'n' must be a single non-negative integer"
  )
})

test_that("validate_inputs validates include_colors parameter", {
  valid_params <- list(
    n = 3,
    include_colors = NULL,
    init_lightness_bounds = c(0.2, 0.8),
    init_hcl_bounds = list(L = c(20, 80), C = c(10, 60)),
    fixed_aesthetic_influence = 0.5,
    aesthetic_init_config = NULL
  )

  # Valid include_colors: valid hex colors
  valid_params_1 <- valid_params
  valid_params_1$include_colors <- c("#FF0000", "#00FF00")
  expect_silent(do.call(validate_inputs, valid_params_1))

  # Invalid include_colors: not character
  invalid_params_1 <- valid_params
  invalid_params_1$include_colors <- c(1, 2, 3)
  expect_error(
    do.call(validate_inputs, invalid_params_1),
    "'include_colors' must be a character vector"
  )

  # Invalid include_colors: invalid hex format
  invalid_params_2 <- valid_params
  invalid_params_2$include_colors <- c("#XYZ123")
  expect_error(
    do.call(validate_inputs, invalid_params_2),
    "All elements in 'include_colors' must be valid hex colors"
  )

  # Invalid include_colors: too many colors
  invalid_params_3 <- valid_params
  invalid_params_3$n <- 2
  invalid_params_3$include_colors <- c("#FF0000", "#00FF00", "#0000FF")
  expect_error(
    do.call(validate_inputs, invalid_params_3),
    "Number of fixed colors.*cannot exceed total requested colors"
  )
})

# Tests for .handle_no_free_colors
test_that(".handle_no_free_colors works with fixed colors only", {
  result <- .handle_no_free_colors(
    n = 2,
    include_colors = c("#FF0000", "#00FF00"),
    return_metrics = FALSE,
    progress = FALSE
  )

  expect_true(is.character(result))
  expect_equal(length(result), 2)
  expect_true(inherits(result, "huerd_palette"))
  expect_equal(as.character(result), c("#FF0000", "#00FF00"))
  expect_true("optimization_details" %in% names(attributes(result)))
})

test_that(".handle_no_free_colors works with metrics", {
  result <- .handle_no_free_colors(
    n = 1,
    include_colors = c("#FF0000"),
    return_metrics = TRUE,
    progress = FALSE
  )

  expect_true(is.character(result))
  expect_equal(length(result), 1)
  expect_true(inherits(result, "huerd_palette"))
  expect_true("metrics" %in% names(attributes(result)))
  expect_true("optimization_details" %in% names(attributes(result)))
})

test_that("reproduce_palette works with valid metadata", {
  set.seed(42)
  original <- generate_palette(
    n = 3,
    optimizer = "nlopt_direct",
    progress = FALSE
  )

  reproduced <- reproduce_palette(original)

  expect_identical(original, reproduced)
})

test_that("reproduce_palette handles missing metadata", {
  # Create palette without metadata
  palette <- c("#FF0000", "#00FF00", "#0000FF")
  class(palette) <- "huerd_palette"

  expect_error(reproduce_palette(palette), "No generation metadata found")
})

test_that("reproduce_palette validates input", {
  expect_error(
    reproduce_palette("not_a_palette"),
    "must be a huerd_palette object"
  )
  expect_error(reproduce_palette(NULL), "must be a huerd_palette object")
})

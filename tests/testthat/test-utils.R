# Tests for print_color_vector
test_that("print_color_vector handles empty input", {
  expect_no_error({
    output <- capture.output(print_color_vector(character(0)))
  })
})

test_that("print_color_vector handles valid colors", {
  colors <- c("#FF0000", "#00FF00", "#0000FF")
  expect_no_error({
    output <- capture.output(print_color_vector(colors))
  })
})

# Tests for .get_estimated_max_dist
test_that(".get_estimated_max_dist handles invalid inputs", {
  # n < 2
  expect_true(is.na(.get_estimated_max_dist(1)))
  expect_true(is.na(.get_estimated_max_dist(0)))
  expect_true(is.na(.get_estimated_max_dist(-1)))

  # Wrong length
  expect_true(is.na(.get_estimated_max_dist(c(2, 3))))

  # Not numeric
  expect_true(is.na(.get_estimated_max_dist("2")))
})

test_that(".get_estimated_max_dist works with valid data", {
  result <- .get_estimated_max_dist(5)
  expect_true(is.numeric(result))
  expect_equal(length(result), 1)
  expect_false(is.na(result))
})

# Tests for print methods - S3 print methods defined in utils.R

test_that("print.huerd_palette runs without error for basic palette", {
  palette <- c("#FF0000", "#00FF00", "#0000FF")
  class(palette) <- c("huerd_palette", class(palette))

  expect_no_error({
    output <- capture.output(result <- print(palette))
    expect_identical(result, palette)
  })
})

test_that("print.huerd_palette handles empty palette", {
  empty_palette <- character(0)
  class(empty_palette) <- c("huerd_palette", class(empty_palette))

  expect_no_error({
    output <- capture.output(result <- print(empty_palette))
    expect_identical(result, empty_palette)
  })
})

test_that("print.huerd_evaluation works correctly", {
  evaluation <- list(
    n_colors = 3,
    distances = list(
      min = 0.567,
      mean = 0.789,
      median = 0.743,
      sd = 0.123,
      estimated_max = 0.645,
      performance_ratio = 0.88
    ),
    cvd_safety = list(
      worst_case_min_distance = 0.234,
      protan = list(min_distance = 0.345, preserved_ratio = 0.89),
      deutan = list(min_distance = 0.234, preserved_ratio = 0.76),
      tritan = list(min_distance = 0.456, preserved_ratio = 0.92)
    ),
    distribution = list(
      lightness_oklab = list(range = c(0.2, 0.8), mean = 0.5),
      chroma_oklab = list(range = c(0.05, 0.25), mean = 0.15),
      hue_oklab = list(circular_variance = 0.67)
    ),
    summary_heuristic_score = 85
  )
  class(evaluation) <- "huerd_evaluation"

  expect_no_error({
    output <- capture.output(result <- print(evaluation))
    expect_identical(result, evaluation)
  })
})

test_that("print.huerd_simulation_result works for single CVD type", {
  cvd_result <- c("#FF6B6B", "#4ECDC4", "#45B7D1")
  class(cvd_result) <- "huerd_simulation_result"
  attr(cvd_result, "cvd_type") <- "deutan"
  attr(cvd_result, "severity") <- 1.0

  expect_no_error({
    output <- capture.output(result <- print(cvd_result))
    expect_identical(result, cvd_result)
  })
})

test_that("print methods handle invalid colors gracefully", {
  invalid_palette <- c("#FF0000", "#INVALID", NA_character_)
  class(invalid_palette) <- c("huerd_palette", class(invalid_palette))

  expect_no_error({
    output <- capture.output(print(invalid_palette))
  })
})

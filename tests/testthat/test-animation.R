test_that("animate_repulsion handles basic inputs correctly", {
  # Temporarily redirect GIF output
  temp_gif <- tempfile(fileext = ".gif")

  # Test basic functionality with same seed should produce identical results
  set.seed(123)
  expect_no_error(
    animate_repulsion(
      n_colors = 3,
      filename = temp_gif
    )
  )

  # Check if file was created
  expect_true(file.exists(temp_gif))


  # Compare with reference file if it exists
  gif1_content <- readBin(temp_gif, "raw", n = file.info(temp_gif1)$size)
  if (file.exists("testdata/no_base.gif")) {
    ref_content <- readBin("testdata/no_base.gif", "raw",
      n = file.info("testdata/no_base.gif")$size
    )
    expect_identical(gif1_content, ref_content)
  }

  # Cleanup
  unlink(c(temp_gif))
})

test_that("animate_repulsion works with base colors", {
  temp_gif <- tempfile(fileext = ".gif")

  # Test with base colors using same seed
  set.seed(123)
  expect_no_error(
    animate_repulsion(
      n_colors = 5,
      base_colors = c("#FF0000", "#00FF00"),
      filename = temp_gif
    )
  )

  # Check file exists
  expect_true(file.exists(temp_gif))

  # Compare outputs
  # Compare with reference file if it exists
  gif1_content <- readBin(temp_gif, "raw", n = file.info(temp_gif1)$size)
  if (file.exists("testdata/with_base.gif")) {
    ref_content <- readBin("testdata/with_base.gif", "raw",
      n = file.info("testdata/with_base.gif")$size
    )
    expect_identical(gif1_content, ref_content)
  }

  unlink(c(temp_gif))
})

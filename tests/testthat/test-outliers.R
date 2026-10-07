test_that("MCOD detects stream outliers and returns their positions", {
  set.seed(17)

  stream <- DSD_Gaussians(k = 2, d = 2, noise = 0.1)
  detector <- DSOutlier_MCOD(r = 0.2, t = 2, w = 20)

  expect_s3_class(detector, "DSOutlier_MCOD")
  expect_identical(update(detector, stream, 20), detector)

  positions <- get_outlier_positions(detector)
  expect_true(is.data.frame(positions))
  expect_equal(ncol(positions), 2L)
})

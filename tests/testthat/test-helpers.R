test_that("MOA parameters are converted to CLI options", {
  expect_identical(
    streamMOA:::convert_params(list(e = 0.05, h = 100L, b = TRUE, c = FALSE)),
    "-e 0.05 -h 100 -b"
  )
  expect_error(streamMOA:::convert_params(list()), "invalid param list")
})

test_that("ellipsePoints returns translated and rotated ellipse coordinates", {
  points <- streamMOA:::ellipsePoints(2, 1, alpha = 90, loc = c(3, 4), n = 5)

  expect_equal(dim(points), c(5L, 2L))
  expect_equal(points[1, ], c(3, 6), tolerance = 1e-12)
  expect_equal(points[2, ], c(2, 4), tolerance = 1e-12)
  expect_equal(points[3, ], c(3, 2), tolerance = 1e-12)
})

test_that("abstract MOA base classes cannot be instantiated", {
  expect_error(DSC_MOA(), "abstract class")
  expect_error(DSD_MOA(), "abstract class")
})

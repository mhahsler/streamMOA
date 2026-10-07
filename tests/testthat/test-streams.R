test_that("MOA random RBF event streams generate numeric points", {
  stream <- DSD_RandomRBFGeneratorEvents(
    k = 2,
    d = 3,
    noiseLevel = 0,
    modelSeed = 17,
    instanceSeed = 23
  )

  expect_s3_class(stream, "DSD_RandomRBFGeneratorEvents")

  points <- get_points(stream, 5, info = FALSE)
  expect_equal(dim(points), c(5L, 3L))
  expect_true(all(vapply(points, is.numeric, logical(1))))
})

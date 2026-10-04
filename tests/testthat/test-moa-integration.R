test_that("MOA streams feed clusterers and expose cluster centers", {
  clusterer <- tryCatch(
    DSC_CluStream(m = 10, horizon = 100, t = 2),
    error = function(e) skip(paste("MOA Java classes unavailable:", conditionMessage(e)))
  )
  set.seed(17)
  stream <- stream::DSD_Gaussians(k = 2, d = 2, noise = 0)
  points <- stream::get_points(stream, 5, info = FALSE)

  expect_equal(dim(points), c(5L, 2L))
  expect_true(all(vapply(points, is.numeric, logical(1))))

  expect_identical(stream::update(clusterer, stream, 20), clusterer)
  centers <- stream::get_microclusters(clusterer)

  expect_true(is.data.frame(centers))
  expect_equal(ncol(centers), 2L)
  expect_true(nrow(centers) > 0L)
})

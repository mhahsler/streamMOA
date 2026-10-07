test_that("MOA clusterers accept stream data", {
  set.seed(17)

  stream <- DSD_Gaussians(k = 2, d = 2, noise = 0)

  # create the clusterers
  clusterers <- list(
    BICO = DSC_BICO_MOA(Cluster = 2, Dimensions = 2),
    CluStream = DSC_CluStream(m = 10, horizon = 100, t = 2),
    ClusTree = DSC_ClusTree(maxHeight = 3),
    DenStream = DSC_DenStream(epsilon = 0.1, initPoints = 5),
    DStream = DSC_DStream_MOA(),
    MCOD = DSC_MCOD(r = 0.2, t = 2, w = 20),
    StreamKM = DSC_StreamKM(sizeCoreset = 10, numClusters = 2, length = 100)
  )

  for (cl in clusterers) {
    if (interactive()) {
      cat("Updating:", cl$description, "\n")
    }
    update(cl, stream, 100)

    if (!(cl$description %in% c("DStream", "StreamKM")))
      centers <- get_centers(cl)
    else
      centers <- get_centers(cl, type = "macro")

    expect_true(is.data.frame(centers))
  }

})

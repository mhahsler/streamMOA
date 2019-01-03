
# streakm options:
# -s sizeCoreset 	10000 (Size of the coreset (m))
# -k numClusters 	5 (Number of clusters to compute)
# -l length 100000 (Length of the data stream (n), range: 0, Max_int)


#' streamKM++
#'
#' This is an interface to the MOA implementation of streamKM++.
#'
#' streamKM++ uses a tree-based sampling strategy to obtain a small weighted sample of the stream called coreset.
#' Upon reclustering, the algorithm applies the k-means++ algorithm to find a given number of centres in the coreset.
#'
#' @param sizeCoreset Size of the coreset
#' @param numClusters Number of clusters to compute
#' @param length Length of the data stream
#'
#' @author Matthias Carnein
#'
#' @references Marcel R. Ackermann, Christiane Lammersen, Marcus Maertens, Christoph Raupach, Christian Sohler, Kamil Swierkot. "StreamKM++: A Clustering Algorithm for Data Streams." In: Proceedings of the 12th Workshop on Algorithm Engineering and Experiments (ALENEX '10), 2010
#'
#' @examples
#' # data with 3 clusters
#' stream <- DSD_Gaussians(k=3, d=2)
#'
#' # cluster with streamKM++
#' streamkm <- DSC_StreamKM(sizeCoreset=10000, numClusters=3, length=10000)
#' update(streamkm, stream, 10000)
#' streamkm
#'
#' # plot macro-clusters
#' plot(streamkm, stream, type="macro")
#'
DSC_StreamKM <- function(sizeCoreset=10000, numClusters=5, length=100000L) {

  ### Java code does parameter checking
  paramList <- list(
    s=as.integer(sizeCoreset),
    k=as.integer(numClusters),
    l=as.integer(length)
  )

  ## MOA implementation does not return micro-clusters but only macro clusters
  clus <- DSC_MOA_Clusterer("moa/clusterers/streamkm/StreamKM", "StreamKM", paramList)

  clus
}

DSC_StreamKM_MOA <- DSC_StreamKM

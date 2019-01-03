
# BICO options:
# -k Cluster 	5 (Number of desired centers, range: 1 to Max_Int)
# -d Dimensions 	10 (Number of the dimensions of the input points, range: 1 to Max_Int)
# -n MaxClusterFeatures 1000 (Maximum size of the coreset, range: 1, Max_Int)
# -p Projections 10 (Number of random projections used for the nearest neighbour search, range: 1 to Max_Int)

#' BICO - Fast computation of k-means coresets in a data stream
#'
#' This is an interface to the MOA implementation of BICO.
#'
#' BICO maintains a tree which is inspired by the clustering tree of BIRCH,
#' a SIGMOD Test of Time award-winning clustering algorithm.
#' Each node in the tree represents a subset of these points. Instead of
#' storing all points as individual objects, only the number of points,
#' the sum and the squared sum of the subset's points are stored as key features
#' of each subset. Points are inserted into exactly one node.
#'
#' @param Cluster Number of desired centers
#' @param Dimensions Number of the dimensions of the input points
#' @param MaxClusterFeatures Maximum size of the coreset
#' @param Projections Number of random projections used for the nearest neighbour search
#'
#' @author Matthias Carnein
#'
#' @references Hendrik Fichtenberger, Marc Gille, Melanie Schmidt, Chris Schwiegelshohn, Christian Sohler: BICO: BIRCH Meets Coresets for k-Means Clustering. ESA 2013: 481-492
#'
#' @examples
#' # data with 3 clusters and 2 dimensions
#' stream <- DSD_Gaussians(k=3, d=2)
#'
#' # cluster with BICO
#' bico <- DSC_BICO_MOA(Cluster=3, Dimensions=2)
#' update(bico, stream, 10000)
#' bico
#'
#' # plot micro and macro-clusters
#' plot(bico, stream, type="both")
#'
DSC_BICO_MOA <- function(Cluster=5, Dimensions, MaxClusterFeatures=1000, Projections=10,
  k = NULL, space = NULL, p = NULL) {

  if(!is.null(k)) Cluster <- k
  if(!is.null(space)) MaxClusterFeatures <- space
  if(!is.null(p)) Projections <- p

  ### Java code does parameter checking
  paramList <- list(
    k=as.integer(Cluster),
    d=as.integer(Dimensions),
    n=as.integer(MaxClusterFeatures),
    p=as.integer(Projections)
  )

  clus <- DSC_MOA_Clusterer("moa/clusterers/kmeanspm/BICO", "BICO", paramList)

  clus
}

# dstream options:
# -d decayFactor 	0.998 (The decay factor, lambda, in (0,1), range: 0.001 to 0.999)
# -m Cm 	3.0 (Controls the threshold for dense grids, range: 1.001 to Double.MAX_VALUE)
# -l Cl		0.8 (Controls the threshold for sparse grids, in (0,1), range: 0.001 to 0.999)
# -b Beta 0.3 (Adjusts the window of protection for renaming previously deleted grids as sporadic, > 0, range: 0.001 to Double.MAX_VALUE)

#' D-Stream
#'
#' This is an interface to the MOA implementation of D-Stream.
#'
#' D-Stream creates an equally spaced grid and estimates the density in each grid cell using the count of points falling in the cells.
#' Grid cells are classified based on density into dense, transitional and sporadic cells.
#' The density is faded after every new point by a decay factor.
#'
#' @param decayFactor The decay factor
#' @param Cm Controls the threshold for dense grids
#' @param Cl Controls the threshold for sparse grids
#' @param Beta Adjusts the window of protection for renaming previously deleted grids as sporadic
#'
#' @author Matthias Carnein
#'
#' @references
#' Yixin Chen and Li Tu. 2007. Density-based clustering for real-time stream data. In Proceedings of the 13th ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (KDD '07). ACM, New York, NY, USA, 133-142.
#'
#' Li Tu and Yixin Chen. 2009. Stream data clustering based on grid density and attraction. ACM Transactions on Knowledge Discovery from Data, 3(3), Article 12 (July 2009), 27 pages.
#'
#' @examples
#' # data with 2 clusters in 2 dimensions
#' stream = DSD_Gaussians(2,2, mu=rbind(c(-10,-10), c(10,10)))
#'
#' # cluster with D-Stream
#' dstream <- DSC_DStream_MOA(decayFactor=0.998)
#' update(dstream, stream, 10000)
#' dstream
#'
#' # plot macro-clusters
#' plot(dstream, stream, type="macro")
#'
DSC_DStream_MOA <- function(decayFactor=0.998, Cm=3.0, Cl=0.8, Beta=0.3) {

  ### Java code does parameter checking
  paramList <- list(
    d=decayFactor,
    m=Cm,
    l=Cl,
    b=Beta
  )

  clus <- DSC_MOA_Clusterer("moa/clusterers/dstream/Dstream", "DStream", paramList)

  clus
}

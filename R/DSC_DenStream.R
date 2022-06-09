#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


#' DenStream Data Stream Clusterer
#'
#' Interface for the DenStream cluster algorithm for data streams implemented
#' in MOA.
#'
#' DenStream applies reachbility (from DBSCAN) between micro-clusters for
#' reclustering using `epsilon` x `offline` (defaults to 2) as the
#' reachability threshold.
#'
#' If `k` is specified it automatically chooses the reachability threshold
#' to find k clusters. This is achieved using single-link hierarchical
#' clustering.
#'
#' @family DSC_MOA
#'
#' @aliases DSC_DenStream DSC_DenStream_MOA denstream DenStream
#' @param epsilon defines the epsilon neighbourhood which is the maximal radius
#' of micro-clusters (r<=epsilon). Range: 0 to 1.
#' @param mu minpoints as the weight w a core-micro-clusters needs to be
#' created (w>=mu). Range: 0 to max(int).
#' @param beta multiplier for mu to detect outlier micro-clusters given their
#' weight w (w<beta x mu). Range: 0 to 1
#' @param lambda decay constant.
#' @param initPoints number of points to use for initialization via DBSCAN.
#' @param offline offline multiplier for epsilon. Range: between 2 and 20).
#' Used for reachability reclustering
#' @param processingSpeed Number of incoming points per time unit (important
#' for decay).  Range: between 1 and 1000.
#' @param recluster logical; should the offline DBSCAN-based (i.e.,
#' reachability at a distance of epsilon) be performed?
#' @param k integer; tries to automatically chooses offline to find k
#' macro-clusters.
#' @return An object of class `DSC_DenStream` (subclass of [DSC],
#' [DSC_MOA], [DSC_Micro]) or, for `recluster = TRUE`, an object
#' of class [DSC_TwoStage].
#' @author Michael Hahsler and John Forrest
#' @references
#' Cao F, Ester M, Qian W, Zhou A (2006). Density-Based Clustering
#' over an Evolving Data Stream with Noise. In Proceedings of the 2006 SIAM
#' International Conference on Data Mining, pp 326-337. SIAM.
#'
#' Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T
#' (2010).  MOA: Massive Online Analysis, a Framework for Stream Classification
#' and Clustering. In Journal of Machine Learning Research (JMLR).
#' @examples
#' # data with 3 clusters and 5% noise
#' set.seed(1000)
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
#'
#' # use Den-Stream with reachability reclustering
#' denstream <- DSC_DenStream(epsilon = .05)
#' update(denstream, stream, 500)
#' denstream
#'
#' # plot macro-clusters
#' plot(denstream, stream, type = "both")
#'
#' # plot micro-cluster
#' plot(denstream, stream, type = "micro")
#'
#' # show micro and macro-clusters
#' plot(denstream, stream, type = "both")
#'
#' # reclustering: Choose reclustering reachability threshold automatically to find 4 clusters
#' denstream2 <- DSC_DenStream(epsilon = .05, k = 4)
#' update(denstream2, stream, 500)
#' plot(denstream2, stream, type = "both")
#' @export
DSC_DenStream <- function(epsilon,
  mu = 1,
  beta = 0.2,
  lambda = 0.001,
  initPoints = 100,
  offline = 2,
  processingSpeed = 1,
  recluster = TRUE,
  k = NULL) {
  #, minPoints=10) {

  ### note:DenStream does not use horizon anymore!
  horizon <- 1000
  ### Java code does parameter checking
  # denstream options:
  # -e epsilon 	0.01 (defines the epsilon neighborhood, range: 0 to 1)
  # -p minPoints 	10 (min. num. points a cluster must have)
  # deprecated -b beta	0.001 (range: 0 to 1)
  # -m mu		1 (range: 0 to max(double)) -> this is actually minpoints and will get converted to int!
  # -l lambda (range: 0 to 1)
  # -i initPoints	10000 (number of points to use for initialization)

  paramList <- list(
    h = horizon,
    e = epsilon,
    #    p = minPoints,
    b = beta,
    m = mu,
    i = initPoints,
    l = lambda,
    o = offline,
    s = processingSpeed
  )

  clus <-
    DSC_MOA_Clusterer("moa/clusterers/denstream/WithDBSCAN",
      "DenStream",
      paramList)

  # note that reachability and single-link hc are equivalent
  if (recluster) {
    if (is.null(k))
      clus <- DSC_TwoStage(clus,
        DSC_Reachability(epsilon = (offline + 1e-9) * epsilon))
    else
      clus <- DSC_TwoStage(clus,
        DSC_Hierarchical(k = k, method = "single"))
  }

  clus
}

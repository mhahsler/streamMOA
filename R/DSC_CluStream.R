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


#' CluStream Data Stream Clusterer
#'
#' Class implements the CluStream cluster algorithm for data streams (Aggarwal et al, 2003).
#'
#' This is an interface to the MOA implementation of CluStream.
#'
#' If `k` is specified, then CluStream applies a weighted k-means
#' algorithm for reclustering (see Examples section below).
#'
#' @family DSC_MOA
#'
#' @aliases DSC_CluStream DSC_CluStream_MOA CluStream clustream
#' @param m Defines the maximum number of micro-clusters used in CluStream
#' @param horizon Defines the time window to be used in CluStream
#' @param t Maximal boundary factor (i.e., the kernel radius factor).  When deciding to
#' add a new data point to a micro-cluster, the maximum boundary is defined as
#' a factor of `t` of the RMS deviation of the data points in the
#' micro-cluster from the centroid.
#' @param k Number of macro-clusters to produce using weighted k-means.
#' @return An object of class `DSC_CluStream` (subclass of
#' [stream::DSC_Micro], [DSC_MOA] and [stream::DSC]).
#' @author Michael Hahsler and John Forrest
#' @references
#' Aggarwal CC, Han J, Wang J, Yu PS (2003). "A Framework for
#' Clustering Evolving Data Streams." In "Proceedings of the International
#' Conference on Very Large Data Bases (VLDB '03)," pp. 81-92.
#'
#' Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T
#' (2010). MOA: Massive Online Analysis, a Framework for Stream Classification
#' and Clustering. In Journal of Machine Learning Research (JMLR).
#' @examples
#' # data with 3 clusters and 5% noise
#' set.seed(1000)
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = .05)
#'
#' # cluster with CluStream
#' clustream <- DSC_CluStream(m = 50, horizon = 100, k = 3)
#' update(clustream, stream, 500)
#' clustream
#'
#' plot(clustream, stream, type = "both")
#' @export
DSC_CluStream <- function(m = 100,
  horizon = 1000,
  t = 2,
  k = 5) {

  paramList <- list(
    h = as.integer(horizon),
    m = as.integer(m),
    t = t,
    k = as.integer(k)
  )

  DSC_MOA_Clusterer("moa/clusterers/clustream/WithKmeans",
    "CluStream",
    paramList)
}

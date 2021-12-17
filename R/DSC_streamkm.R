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
#' @aliases DSC_StreamKM streamkm StreamKM
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
#' @export DSC_StreamKM
DSC_StreamKM <- function(sizeCoreset=10000, numClusters=5, length=100000L) {

  ### Java code does parameter checking
  # streakm options:
  # -s sizeCoreset 	10000 (Size of the coreset (m))
  # -k numClusters 	5 (Number of clusters to compute)
  # -l length 100000 (Length of the data stream (n), range: 0, Max_int)
  paramList <- list(
    s=as.integer(sizeCoreset),
    k=as.integer(numClusters),
    l=as.integer(length)
  )

  ## MOA implementation does not return micro-clusters but only macro clusters
  clus <- DSC_MOA_Clusterer("moa/clusterers/streamkm/StreamKM", "StreamKM", paramList)

  clus
}

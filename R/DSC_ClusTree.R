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

#Reclustering: suggests EM or k-means

#' ClusTree Data Stream Clusterer
#'
#' Interface for the MOA implementation of the ClusTree data stream clustering
#' algorithm (Kranen et al, 2009).
#'
#' ClusTree uses a compact and self-adaptive index structure for maintaining
#' stream summaries. Kranen et al (2009) suggest EM or k-means for reclustering.
#'
#' @family DSC_MOA
#'
#' @aliases DSC_ClusTree ClusTree clustree
#' @param horizon Range of the (time) window.
#' @param maxHeight The maximum height of the tree.
#' @param lambda number used to override computed lambda (decay).
#' @param k If specified, k-means with k clusters is used for reclustering.
#' @return An object of class `DSC_ClusTree` (subclass of [DSC],
#' [DSC_MOA], [DSC_Micro]).
#' @author Michael Hahsler and John Forrest
#' @references
#' Philipp Kranen, Ira Assent, Corinna Baldauf, and Thomas Seidl.
#' 2009. Self-Adaptive Anytime Stream Clustering. In Proceedings of the 2009
#' Ninth IEEE International Conference on Data Mining (ICDM '09). IEEE Computer
#' Society, Washington, DC, USA, 249-258. \doi{10.1109/ICDM.2009.47}
#'
#' Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T
#' (2010). MOA: Massive Online Analysis, a Framework for Stream Classification
#' and Clustering. In Journal of Machine Learning Research (JMLR).
#' @examples
#' # data with 3 clusters
#' set.seed(1000)
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
#'
#' clustree <- DSC_ClusTree(maxHeight = 3)
#' update(clustree, stream, 500)
#' clustree
#'
#' plot(clustree, stream)
#'
#' #' Use automatically the k-means reclusterer with k = 3 to create macro clusters
#' clustree <- DSC_ClusTree(maxHeight = 3, k = 3)
#' update(clustree, stream, 500)
#' clustree
#'
#' plot(clustree, stream, type = "both")
#' @export
DSC_ClusTree <-
  function(horizon = 1000,
    maxHeight = 8,
    lambda = NULL,
    k = NULL) {
    ### Java code does parameter checking
    # ClusTree (anytime clustering) options:
    # IntOption("horizon", 'h', "Range of the window.", 1000)
    # IntOption("maxHeight", 'H', "The maximal height of the tree", 8)
    paramList <- list(h = horizon,
      H = maxHeight)

    clus <-
      DSC_MOA_Clusterer("moa/clusterers/clustree/ClusTree", "ClusTree",
        paramList)

    # overwrite lambda
    if (!is.null(lambda))
      .jfield(clus$javaObj, "negLambda") <- -1 * lambda

    # note that reachability and single-link hc are equivalent
    if (!is.null(k))
      clus <- DSC_TwoStage(clus, DSC_Kmeans(k = k, nstart = 5))

    clus
  }

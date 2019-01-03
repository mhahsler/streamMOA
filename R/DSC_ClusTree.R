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


# ClusTree (anytime clustering) options:
# IntOption("horizon", 'h', "Range of the window.", 1000)
# IntOption("maxHeight", 'H', "The maximal height of the tree", 8)

#Reclustering: suggests EM or k-means

DSC_ClusTree <- function(horizon = 1000, maxHeight = 8, lambda = NULL,
  k = NULL) {

  ### Java code does parameter checking
  paramList <- list(
    h=horizon,
    H=maxHeight
  )

  clus <- DSC_MOA_Clusterer("moa/clusterers/clustree/ClusTree", "ClusTree",
    paramList)

  # overwrite lambda
  if(!is.null(lambda))
  	.jfield(clus$javaObj, "negLambda") <- -1*lambda

  # note that reachability and single-link hc are equivalent
  if(!is.null(k)) clus <- DSC_TwoStage(clus, DSC_Kmeans(k = k, nstart = 5))

  clus
}

DSC_ClusTree_MOA <- DSC_ClusTree

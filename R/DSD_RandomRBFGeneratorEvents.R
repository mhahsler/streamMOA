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


#' Random RBF Generator Events Data Stream Generator
#'
#' A class that generates random data based on RandomRBFGeneratorEvents
#' implemented in MOA.
#'
#' There are an assortment of parameters available for the underlying MOA data
#' structure, however, we have currently limited the available parameters to
#' the arguments above.  Currently the `modelSeed` and `instanceSeed` are set to
#' default values every time a [DSD_MOA] is created, therefore the
#' generated data will be the same. Because of this, it is important to set the
#' seed manually when different data is needed.
#'
#' The default behavior is to create a data stream with 3 clusters and concept
#' drift.  The locations of the clusters will change slightly, and they will
#' merge with one another as time progresses.
#'
#' @family DSD_MOA
#'
#' @param k The average number of centroids in the model.
#' @param d The dimensionality of the data.
#' @param numClusterRange Range for number of clusters.
#' @param kernelRadius The average radius of the micro-clusters.
#' @param kernelRadiusRange Deviation of the number of centroids in the model.
#' @param densityRange Density range.
#' @param speed Kernels move a predefined distance of 0.01 every X points.
#' @param speedRange Speed/Velocity point offset.
#' @param noiseLevel Noise level.
#' @param noiseInCluster Allow noise to be placed within a cluster.
#' @param eventFrequency Frequency of events.
#' @param eventMergeSplitOption Merge and split?
#' @param eventDeleteCreate Delete and create?
#' @param modelSeed Random seed for the model.
#' @param instanceSeed Random seed for the instances.
#' @return An object of class `DSD_RandomRBFGeneratorEvent` (subclass of
#' [DSD_MOA], [stream::DSD]).
#' @author Michael Hahsler and John Forrest
#' @references
#' Albert Bifet, Geoff Holmes, Bernhard
#' Pfahringer, Philipp Kranen, Hardy Kremer, Timm Jansen, Thomas Seidl.
#' MOA: Massive Online Analysis, a Framework for Stream
#' Classification and Clustering
#' _Journal of Machine Learning Research (JMLR)_, 2010.
#' @examples
#' stream <- DSD_RandomRBFGeneratorEvents()
#' get_points(stream, 10)
#'
#' if (interactive()) {
#' animate_data(stream, n = 5000, horizon = 100, xlim = c(0, 1), ylim = c(0, 1))
#' }
#' @export
DSD_RandomRBFGeneratorEvents <- function(k = 3,
  d = 2,
  numClusterRange = 3L,
  kernelRadius = 0.07,
  kernelRadiusRange = 0,
  densityRange = 0,
  speed = 100L,
  speedRange = 0L,
  noiseLevel = 0.1,
  noiseInCluster = FALSE,
  eventFrequency = 30000L,
  eventMergeSplitOption = FALSE,
  eventDeleteCreate = FALSE,
  modelSeed = NULL,
  instanceSeed = NULL) {
  #TODO: need error checking on the params

  # RandomRBFGeneratorEvents options:
  # -m modelRandomSeed
  # -i instanceRandomSeed
  # -K numCluster
  # -k numClusterRange
  # -R kernelRadius
  # -r kernelRadiusRange
  # -d densityRange
  # -V speed
  # -v speedRange
  # -N noiseLevel
  # -E eventFrequency
  # -M eventMergeWeight
  # -P eventSplitWeight
  # -a numAtts (dimensionality)
  # because there are so many parameters, let's only use a few key ones...

  if (is.null(modelSeed))
    modelSeed <- as.integer(runif(1L, 0, .Machine$integer.max))
  if (is.null(instanceSeed))
    instanceSeed <- as.integer(runif(1L, 0, .Machine$integer.max))

  paramList <- list(
    m = modelSeed,
    i = instanceSeed,
    K = k,
    k = as.integer(numClusterRange),
    R = kernelRadius,
    r = kernelRadiusRange,
    d = densityRange,
    V = speed,
    v = speedRange,
    N = noiseLevel,
    E = eventFrequency,
    n = noiseInCluster,
    M = eventMergeSplitOption,
    C = eventDeleteCreate,
    a = d
  )

  # converting the param list to a cli string to use in java
  cliParams <- convert_params(paramList)

  # initializing the clusterer
  strm <- .jnew("moa/streams/clustering/RandomRBFGeneratorEvents", class.loader = .rJava.class.loader)
  options <-
    .jcall(strm, "Lcom/github/javacliparser/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)
  .jcall(strm, "V", "prepareForUse")

  l <- list(
    description = "Random RBF Generator Events (MOA)",
    k = k,
    d = d,
    cliParams = cliParams,
    javaObj = strm
  )

  class(l) <- c("DSD_RandomRBFGeneratorEvents", "DSD_MOA", "DSD")
  l
}

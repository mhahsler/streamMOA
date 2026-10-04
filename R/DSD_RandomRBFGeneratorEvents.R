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
#' Generates random data using MOA's `RandomRBFGeneratorEvents` stream.
#'
#' Only a subset of the parameters supported by the underlying MOA generator is
#' exposed. If `modelSeed` or `instanceSeed` is `NULL`, a seed is sampled from
#' R's random-number generator. Set these arguments explicitly to reproduce a
#' stream; call `set.seed()` to make the generated default seeds reproducible.
#'
#' By default, the generator creates three clusters with concept drift. Cluster
#' locations move over time, and clusters may merge.
#'
#' @family DSD_MOA
#'
#' @param k Average number of centroids in the model.
#' @param d Number of dimensions in the generated stream.
#' @param numClusterRange Range for the number of clusters.
#' @param kernelRadius Average radius of the micro-clusters.
#' @param kernelRadiusRange Range of variation in micro-cluster radii.
#' @param densityRange Range of variation in cluster density.
#' @param speed Number of points between kernel movements.
#' @param speedRange Range of variation in kernel speed.
#' @param noiseLevel Proportion of noise points.
#' @param noiseInCluster If `TRUE`, allow noise points inside clusters.
#' @param eventFrequency Number of points between concept-drift events.
#' @param eventMergeSplitOption If `TRUE`, enable cluster merge and split events.
#' @param eventDeleteCreate If `TRUE`, enable cluster deletion and creation events.
#' @param modelSeed Random seed for the cluster model.
#' @param instanceSeed Random seed for generated instances.
#' @return An object of class `DSD_RandomRBFGeneratorEvents` (subclass of
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

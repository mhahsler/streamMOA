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


#' D-Stream Data Stream Clustering Algorithm
#'
#' This is an interface to the MOA implementation of D-Stream. A C++
#' implementation (including reclustering with attraction) is available as
#' [stream::DSC_DStream].
#'
#' D-Stream creates an equally spaced grid and estimates the density in each
#' grid cell using the count of points falling in the cells. Grid cells are
#' classified based on density into dense, transitional and sporadic cells. The
#' density is faded after every new point by a decay factor.
#'
#' **Notes:**
#'
#' - This implementation seems to use a 1 x 1 grid and therefore the range is increased in
#'   the example.
#' - The MOA implementation of D-Stream currently does not return micro clusters.
#'
#' @family DSC_MOA
#'
#' @param decayFactor The decay factor
#' @param Cm Controls the threshold for dense grids
#' @param Cl Controls the threshold for sparse grids
#' @param Beta Adjusts the window of protection for renaming previously deleted
#' grids as sporadic
#' @author Matthias Carnein
#' @references
#' Yixin Chen and Li Tu. 2007. Density-based clustering for
#' real-time stream data. In Proceedings of the 13th ACM SIGKDD International
#' Conference on Knowledge Discovery and Data Mining (KDD '07). ACM, New York,
#' NY, USA, 133-142.
#'
#' Li Tu and Yixin Chen. 2009. Stream data clustering based on grid density and
#' attraction. ACM Transactions on Knowledge Discovery from Data, 3(3), Article
#' 12 (July 2009), 27 pages.
#' @examples
#' set.seed(1000)
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05, space_limit = c(0, 10))
#'
#' # cluster with D-Stream
#' dstream <- DSC_DStream_MOA(Cm = 3)
#' update(dstream, stream, 1000)
#' dstream
#'
#' # plot macro-clusters
#' plot(dstream, stream, type= "macro")
#' @export
DSC_DStream_MOA <-
  function(decayFactor = 0.998,
    Cm = 3.0,
    Cl = 0.8,
    Beta = 0.3) {
    ### Java code does parameter checking
    # dstream options:
    # -d decayFactor 	0.998 (The decay factor, lambda, in (0,1), range: 0.001 to 0.999)
    # -m Cm 	4.0 (Controls the threshold for dense grids, range: 1.001 to Double.MAX_VALUE)
    # -l Cl		0.8 (Controls the threshold for sparse grids, in (0,1), range: 0.001 to 0.999)
    # -b Beta 0.3 (Adjusts the window of protection for renaming previously deleted grids as sporadic, > 0, range: 0.001 to Double.MAX_VALUE)


    paramList <- list(d = decayFactor,
      m = Cm,
      l = Cl,
      b = Beta)

    clus <-
      DSC_MOA_Clusterer("moa/clusterers/dstream/Dstream", "DStream", paramList)

    clus
  }

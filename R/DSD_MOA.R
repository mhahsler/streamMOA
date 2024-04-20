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

#' Base class for MOA-based Data Stream Generators
#'
#' Abstract base class for MOA-based data stream generators
#' directly inherits from [stream::DSD].
#'
#' @family DSD_MOA
#'
#' @param ... further arguments.
#' @return The abstract class cannot be instantiated and produces an error.
#'
#' @author Michael Hahsler
#' @references
#' MOA: Massive Online Analysis, a Framework for Stream
#' Classification and Clustering Albert Bifet, Geoff Holmes, Bernhard
#' Pfahringer, Philipp Kranen, Hardy Kremer, Timm Jansen, Thomas Seidl.
#' Journal of Machine Learning Research (JMLR).
#' @examples
#' DSD()
#' @export
DSD_MOA <-
  function(...)
    stop("DSD_MOA is an abstract class and cannot be instantiated!")

#' @export
get_points.DSD_MOA <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  info = TRUE,
  ...) {
  ### MOA streams cannot be out of points.

  if (n < 1)
    stop("n must be > 0")

  # pre-allocating the space for the matrix
  data <- matrix(NA, nrow = n, ncol = x$d)

  a <- integer(n)

  # packaging the java instances
  for (i in 1:n) {
    instance <-
      .jcall(x$javaObj, "Lmoa/core/InstanceExample;", "nextInstance")
    instance <-
      .jcall(instance,
        "Lcom/yahoo/labs/samoa/instances/Instance;",
        "getData")
    row <- .jcall(instance, "[D", "toDoubleArray")
    cl <- .jcall(instance, "D", "classValue")
    data[i, ] <- row[1:x$d]
    a[i] <- cl
  }

  data <- data.frame(data)
  a <- as.integer(a)

  ### MOA uses the largest index as noise
  a[a == x$k] <- NA_integer_
  a <- a + 1L

  if (info)
    data[['.class']] <- a

  data
}

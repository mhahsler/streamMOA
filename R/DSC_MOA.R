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

#' DSC_MOA Class
#'
#' An abstract class that inherits from the base class [stream::DSC] and provides the
#' common functions needed to interface MOA clusterers.
#'
#' `DSC_MOA` is a subclass of [stream::DSC] for MOA-based clusterers.
#' `DSC_MOA` classes operate in a different way in that the centers of the
#' micro-clusters have to be extracted from the underlying Java object. This is
#' done by using \pkg{rJava} to perform method calls directly in the JRI and
#' converting the multi-dimensional Java array into a local R data type.
#'
#' **Note:** The formula interface is currently not implemented for MOA-based clusterers.
#' Use [stream::DSF] to select features instead.
#'
#' @family DSC_MOA
#'
#' @author Michael Hahsler and John Forrest
#' @param ... further arguments.
#' @references
#' Albert Bifet, Geoff Holmes, Richard Kirkby, Bernhard Pfahringer
#' (2010). MOA: Massive Online Analysis, Journal of Machine Learning Research
#' 11: 1601-1604
#' @export
DSC_MOA <-
  function(...)
    stop("DSC_MOA is an abstract class and cannot be instantiated!")

## MOA specific stuff
#` @rdname DSC_MOA
#` @export
convert_params <- function(paramList = list()) {
  length <- length(paramList)
  if (length == 0)
    stop("invalid param list")

  cliParams <- ""

  for (i in 1:length) {
    if (is.logical(paramList[[i]])) {
      if (paramList[[i]])
        cliParams <- paste(cliParams, "-",
          names(paramList[i]), " ", sep = "")
    } else {
      cliParams <- paste(cliParams,
        "-",
        names(paramList[i]),
        " ",
        paramList[[i]],
        " ",
        sep = "")
    }
  }

  # removing the trailing space
  cliParams <- substr(cliParams, 1, nchar(cliParams) - 1)
}

DSC_MOA_Clusterer <- function(class,
  description = class,
  parameters = list()) {
  cliParameters <- convert_params(parameters)

  clusterer <-
    .jcast(.jnew(class, class.loader = .rJava.class.loader), "moa/clusterers/AbstractClusterer")
  options <-
    .jcall(clusterer,
      "Lcom/github/javacliparser/Options;",
      "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParameters)
  .jcall(clusterer, "V", "prepareForUse")

  # initializing the R object
  structure(
    list(
      description = description,
      class = class,
      parameters = parameters,
      cliParameters = cliParameters,
      javaObj = clusterer
    ),
    class = c(class, "DSC_MOA", "DSC_Micro", "DSC")
  )
}

#' @export
update.DSC_MOA <- function(object, dsd, n, verbose = FALSE, ...) {
  if (is.jnull(object$javaObj))
    stop("Java Object is not available.",
      call. = FALSE)

  if (n >= 1) {

    ## This loop is now done in Java to speed up clustering
    ## data has to be all doubles for MOA clusterers!
    #     for (i in 1:n) {
    #
    #       d <- get_points(dsd, 1)
    #       ## TODO: Check incoming data
    #
    #       x <- .jcast(
    #         .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(d))),
    #         "weka/core/Instance"
    #       )
    #
    #       .jcall(object$javaObj, "V", "trainOnInstanceImpl", x)
    #
    #       if(verbose && !i%%1000) cat("Processed", i, "points -",
    #         nclusters(object), "clusters\n")
    #
    #     }

    d <- get_points(dsd, n, info = FALSE)

    ## MOA needs a double array!
    d <- as.matrix(d)
    if (storage.mode(d) == "character")
      stop("DSC_MOA clusterers do not support characters/factors in streams.")
    storage.mode(d) <- "double"

    # jcall does not support a class loader for static classes methods
    #   so we use J which is slower
    #.jcall("StreamMOA",
    #  "V",
    #  "update",
    #  object$javaObj,
    #  .jarray(d, dispatch = TRUE))

    #.jcall(.jfindClass("StreamMOA", class.loader = .rJava.class.loader),
    #  "V",
    #  "update",
    #  object$javaObj,
    #  .jarray(d, dispatch = TRUE))

    J("StreamMOA", "update", object$javaObj,
      .jarray(d, dispatch = TRUE), class.loader = .rJava.class.loader)
  }



  # so cl <- cluster(cl, ...) also works
  invisible(object)
}


### accessors
#' @export
get_microclusters.DSC_MOA <- function(x, ...) {
  tryCatch(
    centers <- .get_centers_MOA(
      .jcall(
        x$javaObj,
        "Lmoa/cluster/Clustering;",
        "getMicroClusteringResult"
      )
    ),
    error = function(e)
      stop(
        paste0(
          "Micro-clusters not supported for ",
          x$description,
          " (Class:",
          x$class,
          ")"
        ),
        call. = FALSE
      )
  )

  centers
}

#' @export
get_macroclusters.DSC_MOA <- function(x, ...) {
  tryCatch(
    centers <- .get_centers_MOA(
      .jcall(
        x$javaObj,
        "Lmoa/cluster/Clustering;",
        "getClusteringResult"
      )
    ),
    error = function(e)
      stop(
        paste0(
          "Macro-clusters not supported for ",
          x$description,
          " (Class:",
          x$class,
          ")"
        ),
        call. = FALSE
      )
  )

  centers
}

.get_centers_MOA <- function(x) {
  # array of microclusters
  mClusters <- .jcall(x,
    "Lmoa/core/AutoExpandVector;", "getClustering")

  # length of array
  length <- .jcall(mClusters, "I", "size")
  #else length <- 0

  # empty clustering?
  if (length < 1)
    return(data.frame())


  m <- data.frame()

  # iterating over the array, extracting data to be plotted
  # the first point has already been used, so start from 2
  for (i in 1:length) {
    # will have to cast mCluster as moa/cluster/Cluster
    mCluster <- .jcall(mClusters, "Ljava/lang/Object;", "get", i - 1L)
    mCluster <- .jcast(mCluster, "Lmoa/cluster/Cluster")
    center <- .jcall(mCluster, "[D", "getCenter")
    #  weight <- .jcall(mCluster, "D", "getWeight")
    if (i == 1)
      m <- matrix(ncol = length(center), nrow = length)
    m[i, ] <- center
  }

  m <- as.data.frame(m)
  colnames(m) <- paste("X", 1:ncol(m), sep = "")


  # returning the matrix
  m
}

#' @export
get_microweights.DSC_MOA <- function(x, ...) {
  tryCatch(
    weights <- .get_weights_MOA(
      .jcall(
        x$javaObj,
        "Lmoa/cluster/Clustering;",
        "getMicroClusteringResult"
      )
    ),
    error = function(e)
      stop("Micro-clusters not supported")
  )

  weights
}

#' @export
get_macroweights.DSC_MOA <- function(x, ...) {
  tryCatch(
    weights <- .get_weights_MOA(
      .jcall(
        x$javaObj,
        "Lmoa/cluster/Clustering;",
        "getClusteringResult"
      )
    ),
    error = function(e)
      stop("Macro-clusters not supported")
  )

  weights
}

.get_weights_MOA <- function(x) {
  mClusters <- .jcall(x,
    "Lmoa/core/AutoExpandVector;", "getClustering")

  # length of array
  length <- .jcall(mClusters, "I", "size")

  # empty clustering?
  if (length < 1)
    return(numeric())

  m <- numeric(length)

  # iterating over the array, extracting data to be plotted
  # the first point has already been used, so start from 2
  for (i in 1:length) {
    # will have to cast mCluster as moa/cluster/Cluster
    mCluster <- .jcall(mClusters, "Ljava/lang/Object;", "get", i - 1L)
    mCluster <- .jcast(mCluster, "Lmoa/cluster/Cluster")
    weight <- .jcall(mCluster, "D", "getWeight")
    m[i] <- weight
  }

  m
}

### deep copy
#' @export
get_copy.DSC_MOA <- function(x) {
  #TODO
  stop("Copy not yet implemented for MOA")
}

### strict assignment
.get_radius_MOA <- function(x) {
  if (!.jcall(x$javaObj, "Z", "implementsMicroClusterer"))
    stop("Micro-clusters not supported.")

  x <-
    .jcall(x$javaObj,
      "Lmoa/cluster/Clustering;",
      "getMicroClusteringResult")
  mClusters <-
    .jcall(x, "Lmoa/core/AutoExpandVector;", "getClustering")

  # length of array
  length <- .jcall(mClusters, "I", "size")

  # empty clustering?
  if (length < 1)
    return(numeric())

  m <- numeric(length)

  # iterating over the array, extracting data to be plotted
  # the first point has already been used, so start from 2
  for (i in 1:length) {
    # will have to cast mCluster as moa/cluster/Cluster
    mCluster <- .jcall(mClusters, "Ljava/lang/Object;", "get", i - 1L)
    mCluster <- .jcast(mCluster, "Lmoa/cluster/Cluster")
    m[i] <- .jcall(mCluster, "D", "getRadius")
  }

  ### FIXME: increase radius for Clustream!!!
  ### the radius is the standard deviation. +- 2 standard deviations cover
  ### 95% of the data under the assumption of a Gaussian distribution
  m * 2
}

#' @export
get_assignment.DSC_MOA <-
  function(dsc,
    points,
    type = c("auto", "micro", "macro"),
    method = c("auto", "model", "nn"),
    ...) {
    type <- match.arg(type)
    method <- match.arg(method)

    if (method == "auto")
      method <- "model"
    ### We do not use MOA's macro clustering...
    if (method != "model" || type == "macro")
      return(NextMethod())

    c <- get_centers(dsc, type = type, ...)
    r <- .get_radius_MOA(dsc)

    if (nrow(c) > 0L) {
      dist <- dist(points, c)
      # Find the minimum distance and save the class
      assignment <- apply(dist, 1L, which.min)

      # dist>threshold means no assignment
      assignment[apply(dist, 1L, min) > r[assignment]] <- NA_integer_

    } else {
      #warning("There are no clusters!")
      assignment <- rep(NA_integer_, nrow(points))
    }
    assignment
  }

#' @export
plot.DSC_MOA <- function(x,
  dsd = NULL,
  n = 500,
  assignment = FALSE,
  ...) {
  NextMethod()

  if (assignment) {
    r <- .get_radius_MOA(x)
    p <- get_centers(x)

    ### add threshold circles
    if (!is.numeric(assignment))
      assignment <- 3L
    if (nrow(p) > 0) {
      points(p, col = "black", pch = 3L)
      for (i in 1:nrow(p)) {
        lines(
          ellipsePoints(r[i], r[i],
            loc = as.numeric(p[i, ]), n = 60),
          col = "black",
          lty = assignment
        )
      }
    }
  }
}

# check for NULL reference
#' @export
print.DSC_MOA <- function(x, ...) {
  if (is.jnull(x$javaObj))
    stop("Java Object is not available.", call. = FALSE)
  NextMethod()
}

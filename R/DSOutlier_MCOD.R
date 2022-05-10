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

DSC_MCOD <- function(r=0.1,t=50,w=1000,recheck_outliers=TRUE) {
  parameters <- list(r = as.double(r),t = as.integer(t),w = as.integer(w))
  cliParameters <- convert_params(parameters)

  clusterer <- .jcast(.jnew("StreamMOA_MCOD"), "moa/clusterers/AbstractClusterer")
  options <- .jcall(clusterer, "Lcom/github/javacliparser/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParameters)
  .jcall(clusterer, "V", "prepareForUse")

  # initializing the R object
  structure(
    list(
      description = "Micro-cluster outlier detector",
      class = "DSC_MCOD",
      parameters = parameters,
      recheck_outliers = recheck_outliers,
      cliParameters = cliParameters,
      javaObj = clusterer
    ),
    class = c("DSOutlier_MCOD", "DSOutlier","DSC_MCOD", "DSC_Micro","DSC_MOA","DSC")
  )
}


#' Micro-cluster Continuous Outlier Detector (MCOD)
#'
#' Class interfaces the MOA implementation of the MCOD algorithm for
#' distance-based data stream outlier detection.
#'
#' The algorithm detects density-based outliers. An object \eqn{x} is defined
#' to be an outlier if there are less than \eqn{t} objects lying at distance at
#' most \eqn{r} from \eqn{x}.
#'
#' @aliases DSC_MCOD DSC_MCOD_MOA DSOutlier_MCOD DSOutlier_MCOD_MOA MCOD
#' @param r Defines the micro-cluster radius
#' @param t Defines the number of neighbors (k in the article)
#' @param w Defines the window width in data points
#' @param recheck_outliers Defines that the MCOD algorithm allows re-checking
#' of detected outliers.
#' @return An object of class \code{DSC_MCOD} (subclass of \code{DSOutlier},
#' \code{DSC_Micro}, \code{DSC_MOA} and \code{DSC}).
#' @author Dalibor Krleža
#' @seealso \code{\link{DSOutlier}}, \code{\link{DSC}},
#' \code{\link{DSC_Micro}}, \code{\link{DSC_MOA}}
#' @references Kontaki M, Gounaris A, Papadopoulos AN, Tsichlas K, and
#' Manolopoulos Y (2016). Efficient and flexible algorithms for monitoring
#' distance-based outliers over data streams. Information systems, vol.
#' 55, pp. 37-53. \doi{10.1109/ICDE.2011.5767923}
#' @examples
#' set.seed(1234)
#'
#' # two-stage example
#' stream <- DSD_Gaussians(k = 3, d = 2,
#'             separation_type = "Mahalanobis", separation = 2,
#'             space_limit = c(0, 30), variance_limit = 0.8,
#'             outliers = 10,
#'             outlier_options = list(outlier_horizon = 200))
#'
#' mic_c <- DSC_MCOD(r = 2, t = 10, w = 200)
#' mac_c <- DSC_Kmeans(3)
#' c <- DSC_TwoStage(mic_c, mac_c)
#'
#' evaluate(c, stream, n = 200, type = "macro",
#'   measure = c("crand","outlierjaccard"))
#'
#' reset_stream(stream)
#' plot(c, stream, n = 200, type = "all")
#' @export DSOutlier_MCOD
DSOutlier_MCOD <- DSC_MCOD

get_outlier_positions.DSOutlier_MCOD <- function(x, ...) {
  tryCatch(
    centers <- .get_centers_MOA(.jcall(x$javaObj, "Lmoa/cluster/Clustering;", "getOutlierClusteringResult")),
    error=function(e) stop(paste0("Outliers retrieving error:", e, " (Class:", x$class,")"), call. = FALSE))

  centers
}

recheck_outlier.DSOutlier_MCOD <- function(x, outlier_correlated_id, ...) {
  if(!is.character(outlier_correlated_id)) stop("outlier correlated id must be a string")
  is_still_there <- FALSE
  s <- new(J("java.lang.String"), outlier_correlated_id)
  tryCatch(
    is_still_there <- .jcall(x$javaObj, "Z", "recheckOutlier", outlier_correlated_id),
    error=function(e) stop(paste0("Outlier rechecking error for ", x$description, " (Class:", x$class,")",e), call. = FALSE))

  return(is_still_there)
}

update.DSOutlier_MCOD <- function(object, dsd, n, verbose=FALSE, ...) {
  warning("Using update in single pass clusterers is not recommended. Use get_assignment instead.")
  if(is.jnull(object$javaObj)) stop("Java Object is not available.", call. = FALSE)

  if(n>=1) {

    if(!is(dsd, "DSD_data.frame"))
      stop("Cannot cluster stream (need a DSD_data.frame.)")

    d <- get_points(dsd, n)

    ## MOA needs a double array!
    d <- as.matrix(d)
    if(storage.mode(d) == "character") stop("DSC_MOA clusterers do not support characters/factors in streams.")
    storage.mode(d) <- "double"

    .jcall(object$javaObj, "Ljava/util/List;", "sm_update", .jarray(as.matrix(d), dispatch = TRUE))
  }

  invisible(object)
}

get_assignment.DSOutlier_MCOD <- function(dsc, points, type=c("auto", "micro", "macro"), method=c("auto", "nn", "model"), ...) {
  if(is.jnull(dsc$javaObj)) stop("Java Object is not available.", call. = FALSE)

  type <- match.arg(type)
  if(type=="macro")
    stop("MCOD does not allow macro-clustering")

  d <- as.matrix(points)
  if(storage.mode(d) == "character") stop("DSC_MOA clusterers do not support characters/factors in streams.")
  storage.mode(d) <- "double"

  predict_jlist <- .jcall(dsc$javaObj, "Ljava/util/List;", "sm_update", .jarray(as.matrix(d), dispatch = TRUE))
  predict_len <- .jcall(predict_jlist, "I", "size")
  assignment <- rep(NA_integer_,predict_len)
  outliers <- rep(FALSE,predict_len)
  outliers_corrid <- rep(NA,predict_len)
  for(i in 1:predict_len) {
    res_jobj <- .jcall(predict_jlist, "Ljava/lang/Object;", "get", i-1L)
    res_i <- .jcast(res_jobj, "LStreamMOA_MCODResult;")
    assignment[i] <- .jcall(res_i, "I", "getId")
    outliers[i] <- .jcall(res_i, "Z", "isOutlier")
    s <- .jcall(res_i, "Ljava/lang/String;", "getOutlierId")
    if(!is.null(s)) outliers_corrid[i] <- s
  }

  to_noise <- assignment==0
  assignment[to_noise] <- NA_integer_

  attr(assignment, "outliers") <- outliers
  attr(assignment, "outliers_corrid") <- outliers_corrid

  assignment
}

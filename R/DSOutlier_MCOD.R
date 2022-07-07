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


#' Micro-cluster Continuous Outlier Detector (MCOD)
#'
#' Class interfaces the MOA implementation of the MCOD algorithm for
#' distance-based data stream outlier detection.
#'
#' The algorithm detects density-based outliers. An object \eqn{x} is defined
#' to be an outlier if there are less than \eqn{t} objects lying at distance at
#' most \eqn{r} from \eqn{x}.
#'
#' Outliers are stored and can be retrieved using `get_outlier_position()` and
#' `recheck_outlier()`.
#'
#' **Note:** The implementation updates the clustering when [predict()] is called.
#'
#' @family DSC_MOA
#' @family DSOutlier_MOA
#'
#' @aliases DSC_MCOD DSC_MCOD_MOA DSOutlier_MCOD DSOutlier_MCOD_MOA MCOD
#'
#' @param r Defines the micro-cluster radius.
#' @param t Defines the number of neighbors (k in the article).
#' @param w Defines the window width in data points.
#' @param outlier_correlated_id ids of outliers.
#' @param recheck_outliers Defines that the MCOD algorithm allows re-checking
#'   of detected outliers.
#' @return An object of class `DSC_MCOD` (subclass of [DSOutlier],
#'   [DSC_Micro], [DSC_MOA] and [DSC]).
#' @author Dalibor Krleža
#' @references
#' Kontaki M, Gounaris A, Papadopoulos AN, Tsichlas K, and
#' Manolopoulos Y (2016). Efficient and flexible algorithms for monitoring
#' distance-based outliers over data streams. _Information Systems,_ Vol.
#' 55, pp. 37-53. \doi{10.1109/ICDE.2011.5767923}
#' @examples
#' # Example 1: Clustering with MCOD
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
#' mcod <- DSC_MCOD(r = .1, t = 3, w = 100)
#' update(mcod, stream, 100)
#' mcod
#'
#' plot(mcod, stream, n = 100)
#'
#' # Example 2: Predict outliers (have a class label of NA)
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
#' mcod <- DSOutlier_MCOD(r = .1, t = 3, w = 100)
#' update(mcod, stream, 100)
#'
#' plot(mcod, stream, n = 100)
#'
#' # MCOD can retried the outliers
#' get_outlier_positions(mcod)
#'
#' # Example 3: evaluate on a stream
#' evaluate_static(mcod, stream, n = 100, type = "micro",
#'   measure = c("crand", "noisePrecision", "outlierjaccard"))
#' @export
DSC_MCOD <- function(r = 0.1,
  t = 50,
  w = 1000,
  recheck_outliers = FALSE) {
  parameters <-
    list(r = as.double(r),
      t = as.integer(t),
      w = as.integer(w))
  cliParameters <- convert_params(parameters)

  clusterer <-
    .jcast(.jnew("StreamMOA_MCOD", class.loader = .rJava.class.loader),
      "moa/clusterers/AbstractClusterer")
  options <-
    .jcall(clusterer,
      "Lcom/github/javacliparser/Options;",
      "getOptions")
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
    class = c(
      "DSC_MCOD",
      "DSC_Micro",
      "DSC_MOA",
      "DSC"
    )
  )
}


#' @rdname DSC_MCOD
#' @export
DSOutlier_MCOD <- function(r = 0.1,
  t = 50,
  w = 1000,
  recheck_outliers = TRUE) {

  cl <- DSC_MCOD(r, t, w, recheck_outliers)
  class(cl) <- c("DSOutlier_MCOD", "DSOutlier", class(cl))
  cl
}

#' @export
update.DSC_MCOD <-
  function(object, dsd, n, verbose = FALSE, ...) {
    if (is.jnull(object$javaObj))
      stop("Java Object is not available.", call. = FALSE)

    if (n >= 1) {
      d <- get_points(dsd, n, info = FALSE)

      ## MOA needs a double array!
      d <- as.matrix(d)
      if (storage.mode(d) == "character")
        stop("DSC_MOA clusterers do not support characters/factors in streams.")
      storage.mode(d) <- "double"

      .jcall(object$javaObj,
        "Ljava/util/List;",
        "sm_update",
        .jarray(d, dispatch = TRUE))
    }

    invisible(object)
  }

#' @export
get_assignment.DSC_MCOD <-
  function(dsc,
    points,
    type = c("auto", "micro", "macro"),
    method = c("auto", "nn", "model"),
    ...) {
    if (is.jnull(dsc$javaObj))
      stop("Java Object is not available.", call. = FALSE)

    type <- match.arg(type)
    if (type == "macro")
      stop("MCOD does not implement macro-clustering")

    points <- remove_info(points)
    d <- as.matrix(points)
    if (storage.mode(d) == "character")
      stop("DSC_MOA clusterers do not support characters/factors in streams.")
    storage.mode(d) <- "double"

    predict_jlist <-
      .jcall(dsc$javaObj,
        "Ljava/util/List;",
        "sm_update",
        .jarray(as.matrix(d), dispatch = TRUE))
    predict_len <- .jcall(predict_jlist, "I", "size")
    assignment <- rep(NA_integer_, predict_len)
    outliers <- rep(FALSE, predict_len)
    outliers_corrid <- rep(NA, predict_len)
    for (i in 1:predict_len) {
      res_jobj <-
        .jcall(predict_jlist, "Ljava/lang/Object;", "get", i - 1L)
      res_i <- .jcast(res_jobj, "LStreamMOA_MCODResult;")
      assignment[i] <- .jcall(res_i, "I", "getId")
      outliers[i] <- .jcall(res_i, "Z", "isOutlier")
      s <- .jcall(res_i, "Ljava/lang/String;", "getOutlierId")
      if (!is.null(s))
        outliers_corrid[i] <- s
    }

    assignment[assignment == 0L] <- NA_integer_

    attr(assignment, "outliers") <- outliers
    attr(assignment, "outliers_corrid") <- outliers_corrid

    assignment
  }


### Additional functions for MCOD

#' @describeIn DSC_MCOD Returns spatial positions of all current outliers.
#' @param x a `DSC_MCOD` object.
#' @param ... further arguments are currently ignored.
#' @export
get_outlier_positions <- function(x, ...)
  UseMethod("get_outlier_positions")

#' @export
get_outlier_positions.DSOutlier_MCOD <- function(x, ...) {
  tryCatch(
    centers <-
      .get_centers_MOA(
        .jcall(
          x$javaObj,
          "Lmoa/cluster/Clustering;",
          "getOutlierClusteringResult"
        )
      ),
    error = function(e)
      stop(
        paste0("Outliers retrieving error:", e, " (Class:", x$class, ")"),
        call. = FALSE
      )
  )

  centers
}

#' @describeIn DSC_MCOD DSC_MCOD Re-checks the outlier having `outlier_correlated_id`.
#'   If this object is still an outlier, the method returns `TRUE`.
#' @export
recheck_outlier <- function(x, outlier_correlated_id, ...)
  UseMethod("recheck_outlier")

#' @export
recheck_outlier.DSOutlier_MCOD <-
  function(x, outlier_correlated_id, ...) {
    if (!is.character(outlier_correlated_id))
      stop("outlier correlated id must be a string")
    is_still_there <- FALSE
    s <- new(J("java.lang.String"), outlier_correlated_id)
    tryCatch(
      is_still_there <-
        .jcall(x$javaObj, "Z", "recheckOutlier", outlier_correlated_id),
      error = function(e)
        stop(
          paste0(
            "Outlier rechecking error for ",
            x$description,
            " (Class:",
            x$class,
            ")",
            e
          ),
          call. = FALSE
        )
    )

    return(is_still_there)
  }


#' @describeIn DSC_MCOD forget detected outliers from the outlier detector (currently not implemented).
#' @export
clean_outliers <- function(x, ...)
  UseMethod("clean_outliers")

.outlier_pch <- 4L
.outlier_col <- "#FF0000FF"

#' #' @export
#' plot.DSOutlier_MCOD <- function(x,
#'   dsd = NULL,
#'   n = 500,
#'   col_points = NULL,
#'   col_clusters = c("red", "blue", "green"),
#'   weights = TRUE,
#'   scale = c(1, 5),
#'   cex = 1,
#'   pch = NULL,
#'   method = c("pairs", "scatter", "pca"),
#'   dim = NULL,
#'   type = c("auto", "micro", "macro", "both"),
#'   # we keep 'both' for compatibility reasons
#'   assignment = FALSE,
#'   outliers = TRUE,
#'   ...) {
#'   NextMethod("plot", x, outliers = NULL)
#'
#'   if (outliers)
#'     points(get_outlier_positions(x), pch = .outlier_pch, col = .outlier_col)
#' }
#'

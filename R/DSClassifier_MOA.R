#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2022 Michael Hahsler
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


#' DSClassifier_MOA -- MOA-based Stream Classifiers
#'
#' Interface for MOA-based stream classification methods based on package \pkg{RMOA}.
#'
#' `DSClassifier_MOA` provides an interface to MOA-based stream classifiers using package
#' \pkg{RMOA}. RMOA provides access to MOAs stream classifiers in the following groups:
#'
#' * [RMOA::MOA_classification_trees]
#' * [RMOA::MOA_classification_bayes]
#' * [RMOA::MOA_classification_ensemblelearning]
#'
#' Subsequent calls to `update()` update the current model.
#'
#' @family DSClassifier_MOA
#'
#' @param formula a formula for the classification problem.
#' @param RMOA_classifier a `RMOA_classifier` object.
#'
#' @return An object of class `DSClassifier_MOA`
#' @author Michael Hahsler
#' @references
#' Wijffels, J. (2014) Connect R with MOA to perform streaming
#' classifications. https://github.com/jwijffels/RMOA
#'
#' Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T
#' (2010).  MOA: Massive Online Analysis, a Framework for Stream Classification
#' and Clustering. _Journal of Machine Learning Research (JMLR)_.
#' @examples
#' \dontrun{
#' library(streamMOA)
#' library(RMOA)
#'
#' # create a data stream for the iris dataset
#' data <- iris[sample(nrow(iris)), ]
#' stream <- DSD_Memory(data)
#' stream
#'
#' # define the stream classifier
#' cl <- DSClassifier_MOA(
#'   Species ~ Sepal.Length + Sepal.Width + Petal.Length,
#'   RMOA::HoeffdingTree()
#'   )
#'
#' cl
#'
#' # update the classifier with 100 points from the stream
#' update(cl, stream, 100)
#'
#' # predict the class for the next 50 points
#' newdata <- get_points(stream, n = 50)
#' pr <- predict(cl, newdata)
#' pr
#'
#' table(pr, newdata$Species)
#' }
#' @export
DSClassifier_MOA <- function(formula,
  RMOA_classifier)
  structure(
    list(
      description = paste0("MOA Classifier (", RMOA_classifier$type, ")"),
      formula = formula,
      RMOAObj = RMOA_classifier,
      RMOAObj_trained = list2env(list(trained = NULL))
    ),
    class = c("DSClassifier_MOA", "DSClassifier", "DST")
  )

#' @rdname DSClassifier_MOA
#' @param object a DSC object.
#' @param dsd a data stream object.
#' @param n number of data points taken from the stream.
#' @param verbose logical; show progress?
#' @param block process blocks of data to improve speed.
#' @param ... further arguments.
#' @export
update.DSClassifier_MOA <- function(object,
  dsd,
  n = 1,
  verbose = FALSE,
  block = 1000L,
  ...) {
  stream_RMOA <-
    RMOA::datastream_dataframe(data = get_points(dsd, n = n))

  ### RMOA uses match call to get the formula
  # object$RMOAObj$trainedMOA <- trainMOA(
  #   model = object$RMOAObj,
  #   formula = object$formula,
  #   data = stream_RMOA,
  #   chunksize = block,
  #   trace = verbose,
  #   reset = FALSE
  # )

  call <- paste0(
    "RMOA::trainMOA(
    model = object$RMOAObj,
    formula = ",
    deparse(object$formula),
    ",
    data = stream_RMOA,
    chunksize = block,
    trace = verbose,
    reset = FALSE
  )"
  )

  object$RMOAObj_trained$trained <- eval(parse(text = call))
}

#' @rdname DSClassifier_MOA
#' @param newdata dataframe with the new data.
#' @param type prediction type (see [RMOA::predict.MOA_trainedmodel()]).
#' @export
predict.DSClassifier_MOA <-
  function(object,
    newdata, type = "response", ...) {
    if (is.null(object$RMOAObj_trained$trained))
      stop("classifier is has not been trained!")

    predict(object$RMOAObj_trained$trained,
      newdata = newdata,
      type = type)
  }

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


#' DSRegressor_MOA -- MOA-based Stream Regressors
#'
#' Interface for MOA-based stream regression methods based on package \pkg{RMOA}.
#'
#' `DSRegressor_MOA` provides an interface to MOA-based stream regressors using package
#' \pkg{RMOA}. Available regressors can be found at [RMOA::MOA_regressors].
#'
#' Subsequent calls to `update()` update the current model.
#'
#' @family DSRegressor_MOA
#'
#' @param formula a formula for the regression problem.
#' @param RMOA_regressor a `RMOA_regressors` object.
#'
#' @return An object of class `DSRegressor_MOA`
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
#' # define a stream regression model.
#' cl <- DSRegressor_MOA(
#'   Sepal.Length ~ Species + Sepal.Width + Petal.Length,
#'   RMOA::Perceptron()
#'   )
#'
#' cl
#'
#' # update the model with 100 points from the stream
#' update(cl, stream, 100)
#'
#' # look at the RMOA model object
#' cl$RMOAObj
#'
#' # make predictions for the next 50 points
#' newdata <- get_points(stream, n = 50)
#' pr <- predict(cl, newdata)
#' pr
#'
#' plot(pr, newdata$Sepal.Length, xlim = c(0,10), ylim = c(0,10))
#' abline(a = 0, b = 1, col = "red")
#' }
#' @export
DSRegressor_MOA <- function(formula,
  RMOA_regressor)
  structure(
    list(
      description = paste0(
        "MOA Regressor (",
        RMOA_regressor$type,
        ")\nFormula: ",
        deparse(formula)
      ),
      formula = formula,
      RMOAObj = RMOA_regressor,
      RMOAObj_trained = list2env(list(trained = NULL))
    ),
    class = c("DSRegressor_MOA", "DSRegressor", "DST")
  )

#' @rdname DSRegressor_MOA
#' @param object a DSC object.
#' @param dsd a data stream object.
#' @param n number of data points taken from the stream.
#' @param verbose logical; show progress?
#' @param block process blocks of data to improve speed.
#' @param ... further arguments.
#' @export
update.DSRegressor_MOA <- function(object,
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

#' @rdname DSRegressor_MOA
#' @param newdata dataframe with the new data.
#' @param type prediction type (see [RMOA::predict.MOA_trainedmodel()]).
#' @export
predict.DSRegressor_MOA <-
  function(object,
    newdata, type = "response", ...) {
    if (is.null(object$RMOAObj_trained$trained))
      stop("classifier has not been trained!")

    predict(object$RMOAObj_trained$trained,
      newdata = newdata,
      type = type)
  }

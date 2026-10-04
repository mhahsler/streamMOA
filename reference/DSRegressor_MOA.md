# MOA-based stream regressor interface

Interface to MOA-based stream regression methods provided by the RMOA
package.

## Usage

``` r
DSRegressor_MOA(formula, RMOA_regressor)

# S3 method for class 'DSRegressor_MOA'
update(object, dsd, n = 1, verbose = FALSE, block = 1000L, ...)

# S3 method for class 'DSRegressor_MOA'
predict(object, newdata, type = "response", ...)
```

## Arguments

- formula:

  Formula describing the regression problem.

- RMOA_regressor:

  A regressor object from RMOA.

- object:

  A `DSRegressor_MOA` object.

- dsd:

  A data stream object.

- n:

  Number of data points to read from the stream.

- verbose:

  If `TRUE`, report progress.

- block:

  Number of points processed per block.

- ...:

  Further arguments passed to the underlying method.

- newdata:

  Data frame containing new observations.

- type:

  prediction type (see
  [`RMOA::predict.MOA_trainedmodel()`](https://rdrr.io/pkg/RMOA/man/predict.MOA_trainedmodel.html)).

## Value

An object of class `DSRegressor_MOA`

## Details

`DSRegressor_MOA` provides an interface to MOA-based stream regressors
through RMOA. See
[RMOA::MOA_regressors](https://rdrr.io/pkg/RMOA/man/MOA_regressors.html)
for available regressors.

Calls to [`update()`](https://rdrr.io/r/stats/update.html) train the
current model incrementally.

## References

Wijffels, J. (2014) Connect R with MOA to perform streaming
classifications. https://github.com/jwijffels/RMOA

Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T
(2010). MOA: Massive Online Analysis, a Framework for Stream
Classification and Clustering. *Journal of Machine Learning Research
(JMLR)*.

## Author

Michael Hahsler

## Examples

``` r
if (FALSE) { # \dontrun{
library(streamMOA)
library(RMOA)

# create a data stream for the iris dataset
data <- iris[sample(nrow(iris)), ]
stream <- DSD_Memory(data)
stream

# define a stream regression model.
cl <- DSRegressor_MOA(
  Sepal.Length ~ Species + Sepal.Width + Petal.Length,
  RMOA::Perceptron()
  )

cl

# update the model with 100 points from the stream
update(cl, stream, 100)

# look at the RMOA model object
cl$RMOAObj

# make predictions for the next 50 points
newdata <- get_points(stream, n = 50)
pr <- predict(cl, newdata)
pr

plot(pr, newdata$Sepal.Length, xlim = c(0,10), ylim = c(0,10))
abline(a = 0, b = 1, col = "red")
} # }
```

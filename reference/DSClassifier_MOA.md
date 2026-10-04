# MOA-based stream classifier interface

Interface to MOA-based stream classification methods provided by the
RMOA package.

## Usage

``` r
DSClassifier_MOA(formula, RMOA_classifier)

# S3 method for class 'DSClassifier_MOA'
update(object, dsd, n = 1, verbose = FALSE, block = 1000L, ...)

# S3 method for class 'DSClassifier_MOA'
predict(object, newdata, type = "response", ...)
```

## Arguments

- formula:

  Formula describing the classification problem.

- RMOA_classifier:

  A classifier object from RMOA.

- object:

  A `DSClassifier_MOA` object.

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

An object of class `DSClassifier_MOA`

## Details

`DSClassifier_MOA` provides an interface to MOA-based stream classifiers
through RMOA. The package provides classifiers in these groups:

- [RMOA::MOA_classification_trees](https://rdrr.io/pkg/RMOA/man/MOA_classification_trees.html)

- [RMOA::MOA_classification_bayes](https://rdrr.io/pkg/RMOA/man/MOA_classification_bayes.html)

- [RMOA::MOA_classification_ensemblelearning](https://rdrr.io/pkg/RMOA/man/MOA_classification_ensemblelearning.html)

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

# define the stream classifier. MOAmodelOptions can be passed on as a control parameter
#   to the call RMOA::HoeffdingTree(). See ? RMOA::MOAoptions
cl <- DSClassifier_MOA(
  Species ~ Sepal.Length + Sepal.Width + Petal.Length,
  RMOA::HoeffdingTree()
  )

cl

# update the classifier with 100 points from the stream
update(cl, stream, 100)

# look at the classifier RMOA object
cl$RMOAObj

# predict the class for the next 50 points
newdata <- get_points(stream, n = 50)
pr <- predict(cl, newdata)
pr

table(pr, newdata$Species)
} # }
```

\name{DSD_RandomRBFGeneratorEvents}
\alias{DSD_RandomRBFGeneratorEvents}
\title{Random RBF Generator Events Data Stream Generator}

\description{
A class that generates random data based on
RandomRBFGeneratorEvents implemented in MOA.
}

\usage{
DSD_RandomRBFGeneratorEvents(k = 3, d = 2, numClusterRange = 3L, 
    kernelRadius = 0.07, kernelRadiusRange = 0, densityRange = 0, 
    speed =100L, speedRange = 0L, noiseLevel = 0.1,
    noiseInCluster = FALSE, eventFrequency = 30000L,
    eventMergeSplitOption = FALSE, eventDeleteCreate = FALSE, 
    modelSeed = NULL, instanceSeed = NULL)
}

\arguments{
	\item{k}{The average number of centroids in the model.}
	\item{d}{The dimensionality of the data.}
	\item{numClusterRange}{Range for numner of clusters.}
	\item{kernelRadius}{The average radius of the micro-clusters.}
	\item{kernelRadiusRange}{Deviation of the number of centroids in the model.}
	\item{densityRange}{Density range.}
	\item{speed}{Kernels move a predefined distance of 0.01 every X points.}
	\item{speedRange}{Speed/Velocity point offset.}
	\item{noiseLevel}{Noise level.}
	\item{noiseInCluster}{Allow noise to be placed within a cluster.}
	\item{eventFrequency}{Frequency of events.}
	\item{eventMergeSplitOption}{Merge and split?}
	\item{eventDeleteCreate}{Delete and create?}
	\item{modelSeed}{Random seed for the model.}
	\item{instanceSeed}{Random seed for the instances.}
}
\details{
There are an assortment of parameters available for the underlying MOA data
structure, however, we have currently limited the available parameters to
the arguments above.  Currently the modelSeed and instanceSeed are set to
default values every time a \code{DSD_MOA} is created, therefore the
generated data will be the same. Because of this, it is important to set
the seed manually when different data is needed.

The default behavior is to create a data stream with 3
clusters and concept drift.  The locations of the clusters will change
slightly, and they will merge with one another as time progresses.
}

\value{
    An object of class \code{DSD_RandomRBFGeneratorEvent}
    (subclass of \code{DSD_MOA}, \code{DSD}).
}

\references{
MOA: Massive Online Analysis, a Framework for Stream Classification and
Clustering Albert Bifet, Geoff Holmes, Bernhard Pfahringer, Philipp Kranen,
Hardy Kremer, Timm Jansen, Thomas Seidl.  Journal of Machine Learning
Research (JMLR).
}
\seealso{
	\code{\link{DSD}}
}

\author{
    Michael Hahsler and John Forrest
}


\examples{
stream <- DSD_RandomRBFGeneratorEvents()
get_points(stream, 10, class=TRUE)

\dontrun{
animate_data(stream, n=5000, pointInterval=100, xlim=c(0,1), ylim=c(0,1))
}
}

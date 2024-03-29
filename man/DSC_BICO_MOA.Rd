% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DSC_BICO_MOA.R
\name{DSC_BICO_MOA}
\alias{DSC_BICO_MOA}
\title{BICO - Fast computation of k-means coresets in a data stream}
\usage{
DSC_BICO_MOA(
  Cluster = 5,
  Dimensions,
  MaxClusterFeatures = 1000,
  Projections = 10,
  k = NULL,
  space = NULL,
  p = NULL
)
}
\arguments{
\item{Cluster, k}{Number of desired centers}

\item{Dimensions}{The number of the dimensions of the input points (stream)
need to be specified in advance}

\item{MaxClusterFeatures, space}{Maximum size of the coreset}

\item{Projections, p}{Number of random projections used for the nearest
neighbor search}
}
\description{
This is an interface to the MOA implementation of BICO. The original BICO
implementation by Fichtenberger et al is also available as
\link[stream:DSC_BICO]{stream::DSC_BICO}.
}
\details{
BICO maintains a tree which is inspired by the clustering tree of BIRCH, a
SIGMOD Test of Time award-winning clustering algorithm. Each node in the
tree represents a subset of these points. Instead of storing all points as
individual objects, only the number of points, the sum and the squared sum
of the subset's points are stored as key features of each subset. Points are
inserted into exactly one node.
}
\examples{
# data with 3 clusters and 2 dimensions
set.seed(1000)
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)

# cluster with BICO
bico <- DSC_BICO_MOA(Cluster = 3, Dimensions = 2)
update(bico, stream, 100)
bico

# plot micro and macro-clusters
plot(bico, stream, type = "both")
}
\references{
Hendrik Fichtenberger, Marc Gille, Melanie Schmidt, Chris
Schwiegelshohn, Christian Sohler: BICO: BIRCH Meets Coresets for k-Means
Clustering. ESA 2013: 481-492
}
\seealso{
Other DSC_MOA: 
\code{\link{DSC_CluStream}()},
\code{\link{DSC_ClusTree}()},
\code{\link{DSC_DStream_MOA}()},
\code{\link{DSC_DenStream}()},
\code{\link{DSC_MCOD}()},
\code{\link{DSC_MOA}()},
\code{\link{DSC_StreamKM}()}
}
\author{
Matthias Carnein
}
\concept{DSC_MOA}

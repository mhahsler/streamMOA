\name{DSC_CluStream}
\alias{DSC_CluStream}
\alias{CluStream}
\alias{clustream}
\title{CluStream Data Stream Clusterer}
\description{
 Class implements the CluStream cluster algorithm for data streams.    
    }

\usage{
DSC_CluStream(m = 100, horizon = 1000, t = 2, k=NULL)
}

\arguments{
  \item{m}{Defines the maximum number of micro-clusters used in CluStream}
  \item{horizon}{Defines the time window to be used in CluStream}
  \item{t}{Maximal boundary factor (=Kernel radius factor). 
  When deciding to add a new data point to a
  micro-cluster, the maximum boundary is defined as a factor of \code{t} of the
  RMS deviation of the data points in the micro-cluster from the centroid. 
  }
  \item{k}{Number of macro-clusters to produce using weighted k-means. \code{NULL}
  disables automatic reclustering.}
}

\details{
This is an interface to the MOA implementation of CluStream.

CluStream applies a weighted k-means algorithm for reclustering (see 
Examples section below).
}

\value{
An object of class \code{DSC_CluStream} (subclass of 
\code{DSC_Micro}, \code{DSC_MOA} and \code{DSC}), or,
  if \code{k} is not \code{NULL} then an object of \code{DSC_TwoStage}. 
}

\references{
Aggarwal CC, Han J, Wang J, Yu PS (2003). "A Framework for Clustering 
Evolving Data Streams." In "Proceedings of the International 
Conference on Very Large Data Bases (VLDB '03)," pp. 81-92.

Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T (2010). 
MOA: Massive Online Analysis, a Framework for Stream Classification and 
Clustering. In Journal of Machine Learning Research (JMLR).
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}},
	\code{\link{DSC_MOA}}
}

\author{
    Michael Hahsler and John Forrest
}

\examples{
# 3 clusters with 5% noise
stream <- DSD_Gaussians(k=3, d=2, noise=.05)

# cluster with CluStream  
clustream <- DSC_CluStream(m=50)
update(clustream, stream, 500)
clustream

# plot micro-clusters
plot(clustream, stream)

# plot assignment area (micro-cluster radius)
plot(clustream, stream, assignment=TRUE, weights=FALSE)
  
# reclustering. Use weighted k-means for CluStream
kmeans <- DSC_Kmeans(k=3, weighted=TRUE)
recluster(kmeans, clustream)
plot(kmeans, stream, type="both")
  
# use k-means automatically
clustream <- DSC_CluStream(m=50, k=3)
update(clustream, stream, 500)
clustream

plot(clustream, stream, type="both")
}

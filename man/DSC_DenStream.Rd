\name{DSC_DenStream}
\alias{DSC_DenStream}
\alias{denstream}
\alias{DenStream}
\title{DenStream Data Stream Clusterer}
\description{
Class implements the DenStream cluster algorithm for data streams.
}
\usage{
DSC_DenStream(epsilon, mu = 1, beta = 0.2, lambda = 0.001,
    initPoints = 100, offline = 2, processingSpeed=1, recluster = TRUE, k=NULL)
}

\arguments{
  \item{epsilon}{defines the epsilon neighbourhood which is the 
    maximal radius of micro-clusters (r<=epsilon). Range: 0 to 1.}
  \item{mu}{minpoints as the weight w a core-micro-clusters needs to be created 
    (w>=mu). Range: 0 to max(int).}
  \item{beta}{multiplier for mu to detect outlier micro-clusters given their 
    weight w (w<beta x mu). Range: 0 to 1} 
  \item{lambda}{decay constant.}
  \item{initPoints}{number of points to use for initialization via DBSCAN.}
  \item{offline}{offline multiplier for epsilon. Range: between 2 and 20). Used for
    reachability reclustering }
  \item{processingSpeed}{Number of incoming points per time unit 
    (important for decay). 
    Range: between 1 and 1000.}
  \item{recluster}{logical; should the offline DBSCAN-based 
    (i.e., reachability at a distance of epsilon) be performed? }
  \item{k}{integer; tries to automatically chooses offline to find 
    k macro-clusters.} 
}

\details{
Interface to the DenStream implementation in MOA.
  
DenStream applies weighted DBSCAN for reclustering (see Examples section below).
}
\value{
An object of class \code{DSC_DenStream} (subclass of
\code{DSC}, \code{DSC_MOA}, \code{DSC_Micro}) or,
  for \code{recluster=TRUE}, an object of class \code{DSC_TwoStage}.
}

\references{
Cao F, Ester M, Qian W, Zhou A (2006). Density-Based Clustering over an
Evolving Data Stream with Noise. In Proceedings of the 2006 SIAM
International Conference on Data Mining, pp 326-337. SIAM. 

Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T
(2010).  MOA: Massive Online Analysis, a Framework for Stream
Classification and Clustering. In Journal of Machine Learning Research
(JMLR).
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
stream <- DSD_Gaussians(k=3, d=2, noise=0.05)

denstream <- DSC_DenStream(epsilon=.05)
update(denstream, stream, 500)
denstream

# plot micro-clusters
plot(denstream, stream)
  
# plot the micro-cluster assignment area
plot(denstream, stream, assignment=TRUE, weights=FALSE)

# show macro-clusters (using density reachability with epsilon x offline)
plot(denstream, stream, type="both")

# reclustering. DenStream micro-clusters with k-means instead
km <- DSC_Kmeans(k=3, weighted=TRUE)
recluster(km, denstream)
plot(km, stream, type="both")
}

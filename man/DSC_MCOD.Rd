\name{DSC_MCOD_MOA}
\alias{DSC_MCOD}
\title{Micro-cluster Continuous Outlier Detector}
\description{
 Class implements the MCOD algorithm for data streams.
}

\usage{
DSC_MCOD(r = 0.1, t = 50, w = 1000, recheck_outliers = TRUE)
}

\arguments{
  \item{r}{Defines the micro-cluster radius}
  \item{t}{Defines the number of neighbors (k in the article)}
  \item{w}{Defines the window width in data points}
  \item{recheck_outliers}{Defines that the MCOD algorithm allows re-checking of detected outliers.}
}

\details{
This is an interface to the MOA implementation of MCOD.
}

\value{
An object of class \code{DSC_MCOD} (subclass of
\code{DSC_SinglePass}, \code{DSC_Outlier}, \code{DSC_Micro}, \code{DSC_MOA} and \code{DSC}).
}

\references{
Kontaki M, Gounaris A, Papadopoulos AN, Tsichlas K, and Manolopoulos Y (2016). "Efficient and flexible algorithms for monitoring distance-based outliers over data streams." In "Information systems", vol. 55, pp. 37-53.
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}},
	\code{\link{DSC_MOA}},
	\code{\link{DSC_SinglePass}},
	\code{\link{DSC_Outlier}}
}

\author{
Dalibor Krleža
}

\examples{
# two-stage example
stream <- DSD_Gaussians(k = 3, d = 2, o = 10, outlier_horizon = 1000,
            separation_type = "Mahalanobis", lim = c(0, 15))
mic_c <- DSC_MCOD(r = 1, t = 10, w = 1000)
mac_c <- DSC_Kmeans(3)
c <- DSC_TwoStage(mic_c, mac_c)
evaluate(c, stream, n = 1000, type = "macro", measure = c("crand","outlierjaccard"))
reset_stream(stream)
plot(c, stream, n = 1000, type = "all")
}

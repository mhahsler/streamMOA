\name{DSC_MCOD_MOA}
\alias{DSC_MCOD}
\title{Micro-cluster Continuous Outlier Detector (MCOD)}
\description{
 Class interfaces the MOA implementation of the MCOD algorithm for distance-based data stream outlier detection.
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
The algorithm detects density-based outliers. An object \eqn{x} is defined to be an outlier if there are less than \eqn{t} objects lying at distance at most \eqn{r} from \eqn{x}.
}

\value{
An object of class \code{DSC_MCOD} (subclass of
\code{DSC_SinglePass}, \code{DSC_Outlier}, \code{DSC_Micro}, \code{DSC_MOA} and \code{DSC}).
}

\references{
Kontaki M, Gounaris A, Papadopoulos AN, Tsichlas K, and Manolopoulos Y (2016). "Efficient and flexible algorithms for monitoring distance-based outliers over data streams." In "Information systems", vol. 55, pp. 37-53.\doi{10.1109/ICDE.2011.5767923}
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
stream <- DSD_Gaussians(k = 3, d = 2,
            separation_type = "Mahalanobis", separation = 4,
            space_limit = c(0, 30), variance_limit = 0.8,
            outliers = 10,
            outlier_options = list(outlier_horizon = 1000))

mic_c <- DSC_MCOD(r = 1, t = 10, w = 1000)
mac_c <- DSC_Kmeans(3)
c <- DSC_TwoStage(mic_c, mac_c)

evaluate(c, stream, n = 1000, type = "macro",
  measure = c("crand","outlierjaccard"))

reset_stream(stream)
plot(c, stream, n = 1000, type = "all")
}

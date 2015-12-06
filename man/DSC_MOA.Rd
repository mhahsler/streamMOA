\name{DSC_MOA}
\alias{DSC_MOA}
\title{DSC_MOA Class}

\description{
An abstract class that inherits from the base class DSC 
and provides the common functions needed to interface MOA clusterers.
}

\details{
\code{DSC_MOA} classes operate in a different way in that the centers of the
micro-clusters have to be extracted from the underlying Java object. This is
done by using rJava to perform method calls directly in the JRI and converting the 
multi-dimensional Java array into a local R data type.
}

\references{
MOA: Massive Online Analysis, a Framework for Stream Classification and
Clustering Albert Bifet, Geoff Holmes, Bernhard Pfahringer, Philipp Kranen,
Hardy Kremer, Timm Jansen, Thomas Seidl.  Journal of Machine Learning Research
(JMLR).
}

\seealso{
	\code{\link{DSC}}
}

\author{
    Michael Hahsler and John Forrest
}


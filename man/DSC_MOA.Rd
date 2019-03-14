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
Albert Bifet, Geoff Holmes, Richard Kirkby, Bernhard Pfahringer (2010). MOA: Massive Online Analysis, Journal of Machine Learning Research 11: 1601-1604
}

\seealso{
	\code{\link{DSC}}
}

\author{
    Michael Hahsler and John Forrest
}


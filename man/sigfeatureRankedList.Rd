\name{sigfeatureRankedList}
\alias{sigfeatureRankedList}
\docType{data}
\title{
Processed output data after using the function named "sigFeature()".
}
\description{
The variable "sigfeatureRankedList" contains the output of the
function named "sigFeature()".
}
\usage{data("sigfeatureRankedList")}
\format{
\preformatted{
    The format is:
    int [1:2204] 2064 370 2032 2035 1519 1573 1446 2105 997 611 ...
}
}
\details{
The dataset contains the ranked feature address which can indicate
the expression values inside the expression dataset.
}

\value{
\item{sigfeatureRankedList }{returns the feature list.}
}

\source{
https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE2280
}
\references{
Guyon, I., et al. (2002) Gene selection for cancer classification
using support vector machines, Machine learning, 46, 389-422.
}
\examples{
data(sigfeatureRankedList)
## maybe str(sigfeatureRankedList) ; plot(sigfeatureRankedList) ...
}
\keyword{datasets}

\name{featureRankedList}
\alias{featureRankedList}
\docType{data}
\title{
Processed output data after using the function named "svmrfeFeatureRanking()".
}
\description{
The variable "featureRankedList" contains the output of
the function named "svmrfeFeatureRanking()".
}
\usage{data("featureRankedList")}
\format{
\preformatted{
    The format is:
    int [1:2204] 1073 1404 1152 5 1253 1557 105 1207 792 57 ...
}
}
\details{
To solve the classification problem with the help of ranking the
features an algorithm was proposed by Guyon named SVM-RFE. In this
algorithm the dataset has been trained with SVM linear kernel model
and removed the feature with smallest ranking criterion. This criterion
is the w value of the decision hyperplane given by the SVM.
}

\value{
\item{featureRankedList }{returns the feature list.}
}

\source{
http://www.uccor.edu.ar/paginas/seminarios/Software/SVM-RFE.zip
}
\references{
Guyon, I., et al. (2002) Gene selection for cancer classification
using support vector machines, Machine learning, 46, 389-422.
}
\examples{
data(featureRankedList)
## maybe str(featureRankedList) ; plot(featureRankedList) ...
}
\keyword{datasets}

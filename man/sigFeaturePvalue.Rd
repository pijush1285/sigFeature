\name{sigFeaturePvalue}
\alias{sigFeaturePvalue}

\title{
Find the p-value of those ranked features by using t-statistic
}
\description{
This function will compute the p-value of those ranked features
between the two classes by using t-statistic.
}
\usage{
sigFeaturePvalue(x, y, NumberOfSignificantGene=0, SignificantGeneLilt=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{
    n-by-d data matrix to train (n chips/patients, d clones/genes)
    }
\item{y}{
    vector of class labels -1 or 1\'s (for n chips/patients )
    }
\item{NumberOfSignificantGene}{
    Number of the selected features.
    }
\item{SignificantGeneLilt}{
    Selected feature list.
    }
}
\details{
This function will calculate the p-value.
}
\value{
returns p-value list.
}
\references{
Peng CH, Liao CT, Peng SC, Chen YJ et al. A novel molecular
signature identified by systems genetics approach predicts prognosis
in oral squamous cell carcinoma. PLoS One 2011;6(8):e23452. PMID: 21853135

}
\author{
Pijush Das <topijush@gmail.com>, et al.
}




\seealso{
svm, svm.fs
}
\examples{
#Example for sigFeaturePvalue() function
#Data set taken from GSE2280
library(SummarizedExperiment)
data(ExampleRawData, package="sigFeature")

x  <- t(assays(ExampleRawData)$counts)
y  <- colData(ExampleRawData)$sampleLabels


#Claculating the p-value.
pvalues <- sigFeaturePvalue(x,y)
#Histogram plot of those p-values.
hist(unlist(pvalues),breaks=seq(0,0.08,0.0015),
xlab = "p-value", ylab = "Frequency", main = "")


#Load the process "sigfeatureRankedList" data.
data("sigfeatureRankedList")
#Claculating the p-value.
pvalues <- sigFeaturePvalue(x, y, 50, sigfeatureRankedList)
#Histogram plot of those p value.
hist(unlist(pvalues),breaks=seq(0,0.08,0.0015), xlab =
"p-value", ylab = "Frequency", main = "")


#Load the process "featureRankedList" data.
data("featureRankedList")
#Claculating the p-value.
pvalues <- sigFeaturePvalue(x, y, 50, featureRankedList)
#Histogram plot of those p value.
hist(unlist(pvalues),breaks=seq(0,0.08,0.0015), xlab =
"p-value", ylab = "Frequency", main = "")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


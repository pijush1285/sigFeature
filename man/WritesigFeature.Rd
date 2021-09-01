\name{WritesigFeature}
\alias{WritesigFeature}

\title{
Write the features and sample IDs.
}
\description{
This function will write the output data produce from the
function sigFeatureRanking.enfold.
}
\usage{
WritesigFeature(results, x,  fileName="Result")
}

\arguments{
    \item{results}{
    the object produce by the function named sigFeatureRanking.enfold
    }
    \item{x}{
    n-by-d data matrix to train (n chips/patients, d clones/genes).
}
    \item{fileName}{
    name of the output file.
    }
}
\details{
This function will write the variables.
}
\value{
results output file.
}
\references{
Becker, N., Werft, W., Toedt, G., Lichter, P. and Benner,
A.(2009) PenalizedSVM: a R-package for feature selection SVM
classification, Bioinformatics, 25(13),p 1711-1712
}
\author{
Pijush Das <topijush@gmail.comn>, et al.
}



\seealso{
predict.penSVM, svm (in package e1071)
}
\examples{
#Example for WritesigFeature()
#Data set taken from GSE2280
library(SummarizedExperiment)
data(ExampleRawData, package="sigFeature")

x  <- t(assays(ExampleRawData)$counts)
y  <- colData(ExampleRawData)$sampleLabels


#For 10 fold cross validation.
#results = sigFeature.enfold(x,y,"kfold",10)


#to write the output
#data(results)
#WritesigFeature(results, x)

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


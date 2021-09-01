\name{svmrfeFeatureRanking}
\alias{svmrfeFeatureRanking}
\title{
R implementation of the SVM-RFE algorithm for binary classification problems
}
\description{
To solve the classification problem with the help of ranking the
features an algorithm was proposed by Guyon, Isabelle, et al. named
SVM-RFE. In this algorithm the dataset has been trained with SVM linear
kernel model and the feature containing the smallest ranking is removed.
This criterion is the w value of the decision hyperplane given by the SVM.
}
\usage{
    svmrfeFeatureRanking(x,y)
}


\arguments{
    \item{x}{
    x n-by-d data matrix to train (n samples/patients, d clones/genes)
    }
    \item{y}{
    y vector of class labels -1 or 1\'s (for n chips/patients )
    }
}

\details{
Adopted from R code: http://www.uccor.edu.ar/busquedas/?txt_palabra=seminarios
}
\value{
returns the feature list.
}
\references{
Guyon, Isabelle, et al. "Gene selection for cancer classification using
support vector machines." Machine learning 46.1-3 (2002): 389-422.

Zhang, H. H., Ahn, J., Lin, X. and Park, C. (2006). Gene selection using
support vector machines with nonconvex penalty. Bioinformatics, 22, pp. 88-95.
}

\author{
Guyon, Isabelle, et al.
}

\note{
This function also rank the feature.
}



\seealso{
scadsvc, predict.penSVM, sim.data
}
\examples{
#Example for svmrfeFeatureRanking()
#Data set taken from GSE2280
library(SummarizedExperiment)
data(ExampleRawData, package="sigFeature")

x  <- t(assays(ExampleRawData)$counts)
y  <- colData(ExampleRawData)$sampleLabels

x <- x[ ,1:500]

#featureRankedList = svmrfeFeatureRanking(x,y)
print(featureRankedList[1:10])

#Train the data with ranked frature
#library(e1071)
#svmmodel = svm(x[ , featureRankedList[1:50]], y, cost = 10, kernel="linear")
#summary(svmmodel)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

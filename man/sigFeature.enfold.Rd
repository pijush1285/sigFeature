\name{sigFeature.enfold}
\alias{sigFeature.enfold}
\title{
Significant feature selection with k-fold data.
}
\description{
After converting the dataset into k-folds the function named
"sigFeature.enfold()" is used to select significant features from
the classes. The randomization process is used to sub-sample the dataset.
}
\usage{
sigFeature.enfold(x, y, CV, CVnumber=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{
n-by-d data matrix to train (n chips/patients, d clones/genes)
}
\item{y}{
vector of class labels -1 or 1 s (for n chips/patiens )
}
\item{CV}{
the number of folds in case of k-fold cross validation.
}
\item{CVnumber}{
the number of folds in case of n fold cross validation.
}

}
\details{
The "sigFeature()" function is further enhanced by incorporating
one cross validation methods such as k-fold external cross validation.
In this k-fold cross validation procedure k-1 fold are used for
selecting the feature and one fold remain untouched which will latter
used as test sample set.
}
\value{

\item{feature.ids }{selected significant features.}
\item{train.data.ids }{training chips/patients ids.}
\item{test.data.ids }{testng chips/patients ids.}
\item{train.data.level}{vector of class labels -1 or 1s
(for n chips/patiens ) for train da.}
\item{test.data.level}{vector of class labels -1 or 1s
(for n chips/patiens ) for test da.}

}
\references{
Zhang, H. H., Ahn, J., Lin, X. and Park, C. (2006). Gene selection
using support vector machines with nonconvex penalty. Bioinformatics,
22, pp. 88-95.
}
\author{
Pijush Das <topijush@gmail.com>, et al.
}
\note{
This function will compute the feature with cross checking.
}


\seealso{
findgacv.scad, predict.penSVM, sim.data
}
\examples{
#Example for sigFeature.enfold()
#Data set taken from GSE2280
#library(SummarizedExperiment)
#data(ExampleRawData, package="sigFeature")

#x  <- t(assays(ExampleRawData)$counts)
#y  <- colData(ExampleRawData)$sampleLabels

#For ten fold external cross validation.
#results = sigFeature.enfold(x,y,"kfold",10)


#Compactly display the internal structure of an R object named "results"
data(results)
str(results)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


\name{sigCVError}
\alias{sigCVError}

\title{
Mean external cross validation (k-fold) error calculation.
}
\description{
The function "sigCVError()" computes the mean external cross validation
(k-fold) errors and the standered deviation of the errors.
}
\usage{
sigCVError(i, results, input)
}

\arguments{
    \item{i}{
number of features/genes from top of the ranked list.
}
    \item{results}{
the results is produced from sigFeatureRanking.enfold() function.
}
    \item{input}{
vector of class labels -1 or 1's (for n samples/patients).
n-by-d data matrix to train (n chips/patients, d features/genes).
}
}
\details{
The features which are produced on the basis of frequency are used here to
enumerate mean external cross validation (k-fold) errors and the standard
deviation of the errors. Training and test samples are same which are
initially produced after splitting the main sample set in to k-fold.
In each iteration k-1 folds are considered as training samples and
remaining one fold is considered as test samples.In this external cross
validation procedure, feature numbers are increased one by one by using
the expression values from training dataset as well as test dataset.
After that, traning samples are trained to test the tesing samples
dynamically. The number of un classified samples are averaged and are
called as external cross validation error rate.
}
\value{

\item{error }{Return the error list.}
}
\references{
Zhang, H. H., Ahn, J., Lin, X. and Park, C. (2006). Gene
selection using support vector machines with nonconvex penalty.
Bioinformatics, 22, pp. 88-95.
}
\author{
Pijush Das <topijush@gmail.com>, et al.
}




\seealso{
findgacv.scad, predict.penSVM, sim.data
}
\examples{
#Example for sigCVError()
#Data set taken from GSE2280
#library(SummarizedExperiment)
#data(ExampleRawData, package="sigFeature")

#x  <- t(assays(ExampleRawData)$counts)
#y  <- colData(ExampleRawData)$sampleLabels

#inputdata <- data.frame(y=as.factor(y) ,x=x)

#For 10 fold cross validation.
#results = sigFeature.enfold(x, y, "kfold",10)

#Find out the frequency of the top 400 features selected by every iteration.
#FeatureBasedonFrequency <- sigFeatureFrequency(results, 400, 400, pf=FALSE)
#str(FeatureBasedonFrequency[1])


#To run the code given bellow will take huge time.
#featsweepSigFe = lapply(1:400, sigCVError, FeatureBasedonFrequency, inputdata)

#Thus the process data is given below.
data("featsweepSigFe")
str(featsweepSigFe[1])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

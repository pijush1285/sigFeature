\name{predict}
\alias{predict.sigFeature}

\title{
Predict Method for Feature Selection SVM
}
\description{
This function predicts values based upon a model trained by svm. If class assignment is provided, confusion table, misclassification table, sensitivity and specificity are calculated.
}
\usage{
\method{predict}{sigFeature}(object, newdata, decision.values = FALSE,
probability = FALSE, ..., na.action = na.omit)
}
\arguments{
\item{object}{
  Object class sigFeatureRanking, created by sigFeatureRanking.
  }
  \item{newdata}{
  A matrix containing the new input data, samples in rows, features in columns.
  }
  \item{decision.values}{
  Logical controlling whether the decision values of all binary classifiers computed in multiclass shall be computed and returned.
  }
  \item{probability}{
  Logical indicating whether class probabilities should be computed and returned.
  }
  \item{...}{
  Currently not used.
  }
  \item{na.action}{
  A function to specify the action to be taken if 'NA's are found.
  }
}

\details{
http://ugrad.stat.ubc.ca/R/library/e1071/html/predict.svm.html
}
\value{
A vector of predicted values (for classification: a vector of labels, for density estimation: a logical vector). If decision.value is TRUE, the vector gets a "decision.values" attribute containing a n x c matrix (n number of predicted values, c number of classifiers) of all c binary classifiers decision values. There are k * (k - 1) / 2 classifiers (k number of classes). The colnames of the matrix indicate the labels of the two classes. If probability is TRUE, the vector gets a "probabilities" attribute containing a n x k matrix (n number of predicted values, k number of classes) of the class probabilities.
}


\references{
Chang, Chih-Chung and Lin, Chih-Jen: LIBSVM 2.0: Solving Different Support Vector Formulations.http://www.csie.ntu.edu.tw/~cjlin/papers/libsvm2.ps.gz

Chang, Chih-Chung and Lin, Chih-Jen:Libsvm: Introduction and Benchmarks, http://www.csie.ntu.edu.tw/~cjlin/papers/q2.ps.gz
}

\author{
David Meyer
}

\note{
This function will predict the train set.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
svm, svm.fs
}
\examples{
# Example for predict() function
data("ExampleRawData")
dim(ExampleRawData)

x <- ExampleRawData[ ,-2205]
y <- ExampleRawData[ ,2205]


#Train the data on the basis of top 1000 ranked fratures
#which were computed by sigFeature() function.
require(e1071)
data("sigfeatureRankedList")
sigFeature.model = svm(x[ , sigfeatureRankedList[1:1000]], y,
type="C-classification", kernel="linear")
summary(sigFeature.model)


#Testing or predicting the same dataset.
pred <- predict(sigFeature.model, x[ , sigfeatureRankedList[1:1000]])
table(pred,y)


#Train the data on the basis of top 1000 ranked fratures
#which were computed by svmrfeFeatureRanking() function.
data("featureRankedList")
RFE.model = svm(x[ , featureRankedList[1:1000]], y,
type="C-classification", kernel="linear")
summary(RFE.model)

#Testing or predicting the same dataset.
pred <- predict(RFE.model, x[ , featureRankedList[1:1000]])
table(pred,y)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

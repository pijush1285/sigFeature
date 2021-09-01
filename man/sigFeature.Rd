\name{sigFeature}
\alias{sigFeature}
\title{
Significant Feature Selection by using SVM-RFE & t-statistic.
}
\description{
Significant Feature selected by using SVM-RFE and t-statistic.
}
\usage{
sigFeature(X,Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{X }{ n-by-d data matrix to train (n samples/patients, d features/genes)}
\item{Y }{ vector of class labels -1 or 1's (for n samples/patients)}
}


\details{
The idea of "sigFeature"  (Significant Feature Selection) begins from
the lack of support vector machine recursive feature (SVM-RFE) method.
The feature ranked by the SVM-RFE algorithm may or may not be
differentially significant among the classes (in case of binary
classification). Significant features which have good classification
accuracy and which are differentially significant have an impotent
role in biological aspect.

In data mining and optimisation, the feature selection is a very active
field of research where the SVM-RFE is distinguished as one of the most
effective methods. This is a greedy method that only hopes to find the
best possible combination for classification. In this algorithm, greedy
method similar to SVM-RFE is used. The prime intention of this algorithm
is to enumerate the ranking weights for all the features and sort the
features according to weight vectors as the basis for classification.
The coefficient and the expression mean differences between two compared
groups are used to calculate the weight value of that particular feature.
The iteration process is followed by backward removal of the feature.
The iteration process is continued until there is only one feature remaining
in the dataset. The smallest ranking weight will be removed by the algorithm
while the feature variables with significant impact remains. Finally, the
feature variables will be listed in the descending order of descriptive
difference degree.

}


\value{
returns the feature list.
}


\references{
1. Karatzoglou, Alexandros, David Meyer, and Kurt Hornik.
"Support vector machines in R." (2005).

2. O'Donnell RK, Kupferman M, Wei SJ, Singhal S et al.
Gene expression signature predicts lymphatic metastasis in
squamous cell carcinoma of the oral cavity. Oncogene 2005
Feb 10;24(7):1244-51.
}
\author{
Pijush Das <topijush@gmail.com>, et al.
}


\seealso{
SVM, predict.penSVM
}
\examples{
#Data set taken from GSE2280
library(SummarizedExperiment)
data(ExampleRawData, package="sigFeature")

x  <- t(assays(ExampleRawData)$counts)
y  <- colData(ExampleRawData)$sampleLabels


#Number of features are reduced to minimized the build time.
x <- x[ , 1:100]

#Feature selection with sigFeature functio.
system.time(sigfeatureRankedList <- sigFeature(x,y))
str(sigfeatureRankedList)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


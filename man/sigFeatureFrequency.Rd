\name{sigFeatureFrequency}
\alias{sigFeatureFrequency}
\title{
Arrange the features on the basis of frequency.
}
\description{
Arrange the features on the basis of frequency.
}
\usage{
sigFeatureFrequency(x, results, n, m, pf=FALSE)
}

\arguments{
\item{x}{
n-by-d data matrix to train (n samples/patients, d features/genes.
}
\item{results}{
The "results" variable contains the output produced by the function
"sigFeature.enfold()".
\preformatted{
List of 1
$ :List of 5
    ..$ feature.ids     : int [1:2204] 2064 2031 2035 1573 370  ...
    ..$ train.data.ids  : chr [1:24] "GSM42246" "GSM42248" ...
    ..$ test.data.ids   : chr [1:3] "GSM42263" "GSM42251" "GSM42260"
    ..$ train.data.level: Named num [1:24] 1 1 1 1 1 -1 -1 -1 -1 -1 ...
    .. ..- attr(*, "names")= chr [1:24] "GSM42246" "GSM42248"  ...
    ..$ test.data.level : Named num [1:3] -1 1 1
    .. ..- attr(*, "names")= chr [1:3] "GSM42263" "GSM42251" "GSM42260"
}

}
\item{n}{
n number of top features which will be taken from each feature lists.
}
\item{m}{
m number of top features which will be selected on the basis of frequency.
}
\item{pf}{
this variable used to print the all the output data into a .csv file.
}
}




\details{
In this example a new function is introduced named as "sigFeatureFrequency()".
The main purpose of this function is to arrange the features on the basis
of its frequency. In the previous session the dataset is divided into k-folds.
Out of which k-1 folds are used for training purpose and one fold is kept for
test purpose. Thus each time the algorithm will produce a set of feature lists
which finally end up with k number of feature lists. "sigFeatureFrequency()"
function is used which will rank all the feature according to its frequency.
Details description of this function is given in the help file.
}

\value{
\preformatted{
List of 1
$ :List of 5
    ..$ feature.ids     : num [1:400] 187 225 246 303 313 370 394 469  ...
    ..$ train.data.ids  : chr [1:24] "GSM42246" "GSM42248" "GSM42250" ...
    ..$ test.data.ids   : chr [1:3] "GSM42263" "GSM42251" "GSM42260"
    ..$ train.data.level: Named num [1:24] 1 1 1 1 1 -1 -1 -1 -1 -1 ...
    .. ..- attr(*, "names")= chr [1:24] "GSM42246" "GSM42248" "GSM42250" ...
    ..$ test.data.level : Named num [1:3] -1 1 1
    .. ..- attr(*, "names")= chr [1:3] "GSM42263" "GSM42251" "GSM42260"
}
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
#Example for sigFeatureFrequency()
#Data set taken from GSE2280
library(SummarizedExperiment)
data(ExampleRawData, package="sigFeature")

x  <- t(assays(ExampleRawData)$counts)
y  <- colData(ExampleRawData)$sampleLabels


#For ten fold external cross validation.
#results = sigFeature.enfold(x,y,"kfold",10)


data(results)
FeatureBasedonFrequency <- sigFeatureFrequency(x, results, 400, 400, pf=FALSE)
str(FeatureBasedonFrequency[1])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


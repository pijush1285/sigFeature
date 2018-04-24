\name{featsweepSigFe}
\alias{featsweepSigFe}
\docType{data}
\title{
Processed output data after using the function named "sigCVError()".
}
\description{
The variable "featsweepSigFe" contains the output of the function named
"sigCVError()". The features which are produced on the basis of frequency
are used here to enumerate mean external cross validation (k-fold) errors
and the standard deviation of the errors. Training and test samples are
same which are initially produced after splitting the main sample set in
to k-folds. In each iteration, k-1 folds are considered as training
samples and remaining one fold is considered as testing samples. In this
external cross validation procedure, feature numbers are increased one
by one by using the expression values from training dataset as well as
test dataset. After that, traning samples are trained to test the tesing
samples dynamically. The number of un classified samples are averaged
and are called as external cross validation error rate.
}
\usage{data("featsweepSigFe")}
\format{
\preformatted{
The format is:
    List of 400
    $ :List of 3
        ..$ svm.list:List of 10
        .. ..$ :'data.frame':  1 obs. of  4 variables:
        .. .. ..$ gamma     : num 0.000244
        .. .. ..$ cost      : num 0.001
        .. .. ..$ error     : num 0.333
        .. .. ..$ dispersion: num NA
        .. ..$ :'data.frame':  1 obs. of  4 variables:
        .. .. ..$ gamma     : num 0.000244
        .. .. ..$ cost      : num 0.001
        .. .. ..$ error     : num 0.333
        .. .. ..$ dispersion: num NA
        .. ..$ :'data.frame':  1 obs. of  4 variables:
        .. .. ..$ gamma     : num 0.000244
        .. .. ..$ cost      : num 0.001
        .. .. ..$ error     : num 0.667
        .. .. ..$ dispersion: num NA
        .. ..$ :'data.frame':  1 obs. of  4 variables:
        .. .. ..$ gamma     : num 0.000244
        .. .. ..$ cost      : num 0.001
        .. .. ..$ error     : num 0.333
        .. .. ..$ dispersion: num NA
        .. ..$ :'data.frame':  1 obs. of  4 variables:
        .. .. ..$ gamma     : num 0.000244
        .. .. ..$ cost      : num 0.001
        .. .. ..$ error     : num 0.333
        .. .. ..$ dispersion: num NA
        .. ..$ :'data.frame':  1 obs. of  4 variables:
        .. .. ..$ gamma     : num 1
        .. .. ..$ cost      : num 1.78
        .. .. ..$ error     : num 0.667
        .. .. ..$ dispersion: num NA
        .. ..$ :'data.frame':  1 obs. of  4 variables:
        .. .. ..$ gamma     : num 0.000244
        .. .. ..$ cost      : num 0.001
        .. .. ..$ error     : num 0
        .. .. ..$ dispersion: num NA
        .. ..$ :'data.frame':  1 obs. of  4 variables:
        .. .. ..$ gamma     : num 0.000244
        .. .. ..$ cost      : num 0.001
        .. .. ..$ error     : num 0
        .. .. ..$ dispersion: num NA
        .. ..$ :'data.frame':  1 obs. of  4 variables:
        .. .. ..$ gamma     : num 0.000244
        .. .. ..$ cost      : num 0.001
        .. .. ..$ error     : num 0.5
        .. .. ..$ dispersion: num NA
        .. ..$ :'data.frame':  1 obs. of  4 variables:
        .. .. ..$ gamma     : num 0.000244
        .. .. ..$ cost      : num 0.001
        .. .. ..$ error     : num 0.5
        .. .. ..$ dispersion: num NA
        ..$ error   : num 0.367
        ..$ errorSD : num 0.233
        ...
    }
}

\value{
\item{featsweepSigFe }{Return the values stored in the variable.}
}

\source{
https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE2280
}
\references{
O'Donnell RK, Kupferman M, Wei SJ, Singhal S et al. Gene
expression signature predicts lymphatic metastasis in squamous
cell carcinoma of the oral cavity. Oncogene 2005 Feb 10;
24(7):1244-51. PMID: 15558013
}
\examples{
data(featsweepSigFe)
## maybe str(featsweepSigFe) ; plot(featsweepSigFe) ...
}
\keyword{datasets}

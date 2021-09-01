\name{PlotErrors}
\alias{PlotErrors}

\title{
Plot the mean CV errors.
}
\description{
A useful function for plotting the errors which is
enumerated by using the function sigCVError().
}
\usage{
PlotErrors(featsweepSigFe, ylim.min=0, ylim.max=0)
}

\arguments{
\item{featsweepSigFe}{
a list variable containing the gamma, cost, error, dispersion values.
\preformatted{
The format is:
    List of 1
    ...
    $ :List of 3
        ..$ svm.list:List of 1
        .. ..$ :data.frame: 1 obs. of  4 variables:
        .. .. ..$ gamma     : num 0.000244
        .. .. ..$ cost      : num 0.001
        .. .. ..$ error     : num 0.5
        .. .. ..$ dispersion: num NA
        ..$ error   : num 0.367
        ..$ errorSD : num 0.233
    }
}



\item{ylim.min}{
minimum y label value in the graph.
}
\item{ylim.max}{
maximum y label value in the graph.
}

}
\details{
This plot function will show the errors.
}
\value{
returns plot.
}
\references{
Chang, Chih-Chung and Lin, Chih-Jen: LIBSVM 2.0: Solving Different
Support Vector Formulations.
http://www.csie.ntu.edu.tw/~cjlin/papers/libsvm2.ps.gz

Chang, Chih-Chung and Lin, Chih-Jen:Libsvm: Introduction and Benchmarks,
http://www.csie.ntu.edu.tw/~cjlin/papers/q2.ps.gz
}
\author{
Pijush Das <topijush@gmail.com>, et al.
}



\seealso{
svm, svm.fs
}
\examples{
# Example for PlotErrors()
# Data set taken from GSE2280

data(featsweepSigFe)
dim(featsweepSigFe)

#For plotting the mean external cross validation
#error and the standard deviation of the of the
#miss classifications of top 400 features.
PlotErrors(featsweepSigFe, 0, 0.4)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


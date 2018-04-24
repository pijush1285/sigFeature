\name{ExampleRawData}
\alias{ExampleRawData}
\docType{data}
\title{
Example dataset to test the performance of the sigFeature package.
}
\description{
For this significant feature selection procedure, microarray data (GSE2280)
from patients with squamous cell carcinoma of the oral cavity (OSCC) has
been used( O'Donnell RK et al. (2005)). Affymetrix Human Genome  Array,
U133A was selected for genome-wide transcription analysis for this data
set. In this paper, the gene expression profiles obtained from primary
squamous cell carcinoma of the oral cavity (OSCC) that were metastatic
to lymph nodes (N+) compared to those that were not metastatic (N-).
A total of 18 OSCCs were analyzed for gene expression. In their analysis
a predictive rule was built using a support vector machine, and the
accuracy of the rule was evaluated using cross-validation the original
data set and prediction of an independent set of four patients. A signature
gene set is produced which is able to predict  the four independent patients
correctly as well as associating five lymph node metastases from the original
patient set with the metastatic primary tumour group.
}
\usage{data("ExampleRawData")}
\format{
\preformatted{
    The format is:
    num [1:27, 1:2205] 72.5 177.2 75.7 128.9 142 ...
    - attr(*, "dimnames")=List of 2
    ..$ : chr [1:27] "GSM42246" "GSM42248" "GSM42250" "GSM42252" ...
    ..$ : chr [1:2205] "1494_f_at" "179_at" "200014_s_at" "200059_s_at" ...
}
}
\details{
For "sigFeature" package evaluation, the microarray dataset has been
classified into two classes such as lymph node metastatic (N+) and
No lymph node metastatic (N-) (according to the TNM staging), provided
in the dataset. After downloading the data set from GEO database firstly,
it was normalized using the  "quantile"  normalization method using the
Bioconductor package "limma". To reduce the runtime of this sigFeature
function, a subset of the total dataset is taken by the ratio between the
difference between two groups with cut off value (p-value 0.07). Now the
expression value of the sub-dataset is considered here as "x". The patients
without lymph node metastasis are represented -1, and the patients with
lymph node metastasis are represented as 1. Those -1 and 1 value is
incorporated with "y" as sample labels.

}

\value{
\item{ExampleRawData }{Return the values stored in the variable.}
}

\source{
    https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE2280
}
\references{
O'Donnell RK, Kupferman M, Wei SJ, Singhal S et al. Gene expression
signature predicts lymphatic metastasis in squamous cell carcinoma
of the oral cavity. Oncogene 2005 Feb 10;24(7):1244-51. PMID: 15558013
}
\examples{
data("ExampleRawData")
}
\keyword{datasets}



library(SummarizedExperiment)
data(ExampleRawData, package="sigFeature")

x  <- t(assays(ExampleRawData)$counts)
y  <- colData(ExampleRawData)$sampleLabels
x <- x[ , 1:100]


test_sigFeature <- function(){
    sigFeature(x, y)
}

#########################################################################
## Name:        sigFeaturePvalue.R
## Author:      Pijush Das, Dr. Susanta Roychudhury, Dr. Sucheta Tripathy
##
##
## Change Log:
##   * April 4, 2018
##       - split sigFeature.R into several files for easier readability
##         and maintenance
#########################################################################

#########################################################################
## sigFeaturePvalue(): This function will compute the p-value of those
## ranked features between the two classes by using t-statistic.
##
##
#########################################################################


sigFeaturePvalue <- function(x, y, NumberOfSignificantGene=0, SignificantGeneLilt=0){
  if(NumberOfSignificantGene!=0){
    #if(SignificantGeneLilt==0){stop("Gene list is not available.")}
    SelectedGene <- SignificantGeneLilt[1:NumberOfSignificantGene]
  }else{
    SelectedGene=1:dim(x)[2]
  }
  m = 1:dim(x)[1]
  clsA  <- x[m[which(y== names(table(y))[1])], ]
  clsB  <- x[m[which(y== names(table(y))[2])], ]
  clsA1 <- clsA[ ,SelectedGene]
  clsB1 <- clsB[ ,SelectedGene]
  pvals <- lapply(1:dim(clsA1)[2], function(i) t.test(clsA1[ ,i],clsB1[ ,i])$p.value)
  return(pvals)
}

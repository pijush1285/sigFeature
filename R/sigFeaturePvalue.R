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


sigFeaturePvalue <- function(x, y, NumberOfSignificantGene=0,
                            SignificantGeneLilt=0){

    #Checking for the variables
    stopifnot(!is.null(x) == TRUE, !is.null(y) == TRUE)
    stopifnot(!is.null(NumberOfSignificantGene) == TRUE)
    stopifnot(!is.null(SignificantGeneLilt) == TRUE)

    if(NumberOfSignificantGene!=0){
    #if(SignificantGeneLilt==0){stop("Gene list is not available.")}
    SelectedGene <- SignificantGeneLilt[seq_len(NumberOfSignificantGene)]
    }else{
    SelectedGene=seq_len(dim(x)[2])
    }
    m = seq_len(dim(x)[1])
    clsA  <- x[m[which(y== names(table(y))[1])], ]
    clsB  <- x[m[which(y== names(table(y))[2])], ]
    clsA1 <- clsA[ ,SelectedGene]
    clsB1 <- clsB[ ,SelectedGene]
    pvals <- lapply(seq_len(dim(clsA1)[2]),
                function(i) t.test(clsA1[ ,i],clsB1[ ,i])$p.value)
    return(pvals)
}

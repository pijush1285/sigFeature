#########################################################################
## Name:        sigFeatureFrequency.R
## Author:      Pijush Das, Dr. Susanta Roychudhury, Dr. Sucheta Tripathy
##
##
## Change Log:
##   * April 4, 2018
##       - split sigFeature.R into several files for easier readability
##         and maintenance
#########################################################################

#########################################################################
## sigFeatureFrequency(): The function makes a single list of features
## from the feature lists produced by the function sigFeature.enfold()
## based on the frequency.
##
##
#########################################################################



sigFeatureFrequency <- function(x, results, n, m, pf=FALSE){
    #Checking for positive integer parameter
    stopifnot(!is.null(x) == TRUE)
    stopifnot(is.list(results) == TRUE, length(results) > 0)
    try(if(n <= 0) stop("n must be a positive integer"))
    try(if(m <= 0) stop("m must be a positive integer"))

    #Select n number of feature produce in each fold change
    ListOfAllFeature = NULL
    for(i in seq_len(length(results))){
        listOfFeature_1 <- results[[i]]$feature.ids[seq_len(n)]
        ListOfAllFeature <- cbind(ListOfAllFeature,listOfFeature_1)
    }

    #Finding out the frequency of each feature.
    FeatureFreq <- table(ListOfAllFeature)
    FeatureFreq1 <- data.frame(FeatureFreq)
    FeatureFreq2 <- FeatureFreq1[order(FeatureFreq1$Freq, decreasing = TRUE), ]


    #Create a matrix to collect "FeatureAddress","GeneName", "Frequency"
    FeatureNamesWithFreq <- matrix( nrow = dim(FeatureFreq2)[1], ncol = 3)
    colnames(FeatureNamesWithFreq) <-
    c("FeatureAddress","GeneName", "Frequency" )
    col <- colnames(x)

    count = 1
    for(i in seq_len(dim(FeatureFreq2)[1])){
    FeatureNamesWithFreq[count, 1] <-
    as.numeric(as.character(FeatureFreq2$ListOfAllFeature))[i]
    v <- as.numeric(as.character(FeatureFreq2$ListOfAllFeature))[i]
    FeatureNamesWithFreq[count, 2] <- col[v]
    FeatureNamesWithFreq[count, 3] <- FeatureFreq2$Freq[i]
    count = count + 1
    }


    #Print the sort listed features.
    if(pf == TRUE) {
    write.csv(FeatureNamesWithFreq, "SigFeat_FeatureNamesWithFreq.csv")
    }

    #Now cross validation with the finally short listed feature (m = 400)
    FeatureFreq3 <- as.numeric(as.matrix(FeatureFreq2[seq_len(m), 1]))

    listFunction <- function(i, FeatureFreq3, results){
    #x1 <- results
    lst <- results[[i]]
    return(list(feature.ids=FeatureFreq3, train.data.ids=lst$train.data.ids,
                test.data.ids=lst$test.data.ids,
                train.data.level=lst$train.data.level,
                test.data.level=lst$test.data.level))
    }
    #Finally data set is produce which will be used as
    #input of cross validation function.
    NewResults = lapply(seq_len(length(results)),
                listFunction, FeatureFreq3, results)

    return(NewResults)

}













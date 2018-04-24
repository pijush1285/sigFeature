#########################################################################
## Name:        sigFeature.R
## Author:      Pijush Das, Dr. Susanta Roychudhury, Dr. Sucheta Tripathy
##
##
## Change Log:
##   * April 4, 2018
##       - split sigFeature.R into several files for easier readability
##         and maintenance
#########################################################################

#########################################################################
## sigFeature(): The prime intention of this algorithm is to enumerate
## the ranking weights for all features and sort the features according
## to weight vectors as the classification basis.
##
##
#########################################################################


sigFeature = function(X, Y){

    #Checking for the variables
    stopifnot(!is.null(X) == TRUE, !is.null(Y) == TRUE)

    m = seq_len(dim(X)[1])
    clsA  <- X[m[which(Y == names(table(Y))[1])], ]
    clsB  <- X[m[which(Y == names(table(Y))[2])], ]
    pvals <- lapply(seq_len(dim(clsA)[2]),
                function(i)t.test(clsA[ ,i],clsB[ ,i])$p.value)
    md    <- unlist(pvals)
    y = c(rep(-1, as.vector(table(Y))[1]),rep(1, as.vector(table(Y))[2]))
    x <- X[c(m[which(Y == names(table(Y))[1])],
            m[which(Y == names(table(Y))[2])]), ]
    n = ncol(x)
    breatheFeaturesIndexes = seq_len(n)
    featureCodifiedList = vector(length=n)
    codifiedFeatureIndex = n
    while(length(breatheFeaturesIndexes) > 0){
    svmModel = svm(x[, breatheFeaturesIndexes], y, cost = 10,
    cachesize=500,scale=FALSE, type="C-classification", kernel="linear")
    rankingCriteria <- t(svmModel$coefs*y[svmModel$index]) %*% svmModel$SV * md
    ranking = sort(rankingCriteria, index.return = TRUE)$ix
    featureCodifiedList[codifiedFeatureIndex] =
            breatheFeaturesIndexes[ranking[1]]
    codifiedFeatureIndex = codifiedFeatureIndex - 1
    (breatheFeaturesIndexes = breatheFeaturesIndexes[-ranking[1]])
    md = md[-ranking[1]]
    }
    return(featureCodifiedList)
}

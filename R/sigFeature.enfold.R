#########################################################################
## Name:        sigFeature.enfold.R
## Author:      Pijush Das, Dr. Susanta Roychudhury, Dr. Sucheta Tripathy
##
##
## Change Log:
##   * April 4, 2018
##       - split sigFeature.R into several files for easier readability
##         and maintenance
#########################################################################

#########################################################################
## sigFeature.enfold(): The function convert a dataset into k fold and
## removes one fold from the total set then selects the significant
## features from the remaining dataset. The sample selection in each fold
## is random. This process of removing one fold and select feature from
## remaining set continues until all folds have been removed once. This
## process is used for removing the sample biasness.
##
##
#########################################################################


.sigFeature.enfold <- function(test.fold, X, Y) {
    # Wrapper to run svmRFE function while omitting a given test fold
    train.data = X[-test.fold, ]
    train.data.level = Y[-test.fold]
    test.data  = setdiff(row.names(X), row.names(train.data))
    #print(test.data)
    test.data.level = Y[test.fold]
    # Rank the features
    features.ranked = sigFeature(train.data,train.data.level)
    return(list(feature.ids=features.ranked,
                train.data.ids=row.names(train.data),
                test.data.ids=test.data, train.data.level=train.data.level,
                test.data.level=test.data.level))
}



sigFeature.enfold <- function(x, y, CV, CVnumber=0){

    #Checking for the variables
    stopifnot(!is.null(x) == TRUE, !is.null(y) == TRUE)
    stopifnot(is.character(CV) == TRUE, !is.null(CVnumber) == TRUE)

    if(CV == "kfold"){
        print("n fold cross validation selected")
        nfold = CVnumber
        if(CVnumber == 0){stop("CVnumber was not selected.")}
        nrows = nrow(x)
        folds = rep(seq_len(CVnumber), len=nrows)[sample(nrows)]
        folds = lapply(seq_len(CVnumber), function(x) which(folds == x))
        results = lapply(folds, .sigFeature.enfold, x, y)
    }else{
        print("LOO cross validation selected")
        nrows = nrow(x)
        results = lapply(seq_len(nrows), .sigFeature.enfold, x, y)
    }
    return(results)
}

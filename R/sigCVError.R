#########################################################################
## Name:        sigCVError.R
## Author:      Pijush Das, Dr. Susanta Roychudhury, Dr. Sucheta Tripathy
##
##
## Change Log:
##   * April 4, 2018
##       - split sigFeature.R into several files for easier readability
##         and maintenance
#########################################################################

#########################################################################
## sigCVError(): The features which are produced on the basis of frequency
## are used here to enumerate mean external cross-validation (k-fold)
## errors and the standard deviation of the errors. Training and testing
## samples are same which are initially produced after splitting the main
## sample set into k-fold. In each iteration k-1, fold samples are
## considered as training samples and remaining one fold samples are
## chosen as testing samples. In this operation, the feature numbers are
## increased one by one. For a particular incrimination of a feature
## miss-classification error rate is calculated for k-fold cross-validation.
##
##
#########################################################################


sigCVError <- function(i, results, input) {

    #Checking for the user variable.
    stopifnot(!is.null(input) == TRUE)
    stopifnot(is.list(results) == TRUE, length(results) > 0)

    svm.list = lapply(results, function(x) tune(svm,
        train.x      = input[x$train.data.ids, 1+x$feature.ids[seq_len(i)]],
        train.y      = input[x$train.data.ids, 1],
        validation.x = input[x$test.data.ids, 1+x$feature.ids[seq_len(i)]],
        validation.y = input[x$test.data.ids, 1],
        ranges     = tune(svm,
        train.x = input[x$train.data.ids, 1+x$feature.ids[seq_len(i)]],
        train.y = input[x$train.data.ids, 1],
    ranges  = list(gamma=2^(-12:0), cost = 10^seq(-3,1, by = 0.25)))$best.par,
        tunecontrol = tune.control(sampling='fix'))$perf)


        #error = mean(unlist(apply(svm.list, function(x) x$error)))
        #errorSD = sd(unlist(apply(svm.list, function(x) x$error)))
        error = mean(sapply(svm.list,function(x) x$error))
        errorSD = sd(sapply(svm.list,function(x) x$error))
                    
        return(list(svm.list=svm.list, error=error, errorSD=errorSD))
}

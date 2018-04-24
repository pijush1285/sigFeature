#########################################################################
## Name:        PlotErrors.R
## Author:      Pijush Das, Dr. Susanta Roychudhury, Dr. Sucheta Tripathy
##
##
## Change Log:
##   * April 4, 2018
##       - split sigFeature.R into several files for easier readability
##         and maintenance
#########################################################################

#########################################################################
## PlotErrors(): A useful function for plotting the errors which is
## enumerated by using the function sigCVError().
##
##
#########################################################################


    PlotErrors <- function(featsweepSigFe, ylim.min=0, ylim.max = 0){

        #Checking for the user variable.
        stopifnot(!is.null(ylim.min) == TRUE, !is.null(ylim.max) == TRUE)
        stopifnot(is.list(featsweepSigFe) == TRUE, length(featsweepSigFe) > 0)
        #For plotting the error
        SigFe=unlist(lapply(featsweepSigFe, function(x)
        ifelse(is.null(x), NA, x$error)))
        SigFe_1<-sort(SigFe, decreasing = TRUE)

        #For plotting the SD
        sigFeSD=unlist(lapply(featsweepSigFe, function(x)
        ifelse(is.null(x), NA, x$errorSD)))
        sigFe_2<-sort(sigFeSD, decreasing = TRUE)

        #Plot easily.
        ym=seq_len(length(featsweepSigFe))


    par(mfrow=c(1,2))
    plot(ym, SigFe_1, type="l", col="green", lwd=2, xlab="Number of Features",
        ylab="Mean external CV error", ylim=c(ylim.min, ylim.max))
    legend("topright", legend=c("sigFeature" ), col=c("green"), lwd=1)
    grid(nx=NULL, ny=NULL, col="gray", lty="dotted")

    plot(ym, sigFe_2, type="l", col="blue", lwd=2, xlab="Number of Features",
        ylab="Standard Deviation", ylim=c(ylim.min, ylim.max))
    legend("topright", legend=c("sigFeature"), col=c("blue"), lwd=1)
    grid(nx=NULL, ny=NULL, col="gray", lty="dotted")

}




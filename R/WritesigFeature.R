#########################################################################
## Name:        WritesigFeature.R
## Author:      Pijush Das, Dr. Susanta Roychudhury, Dr. Sucheta Tripathy
##
##
## Change Log:
##   * April 4, 2018
##       - split sigFeature.R into several files for easier readability
##         and maintenance
#########################################################################

#########################################################################
## WritesigFeature(): This function will write the output data produced
## from the function sigFeatureRanking.enfold().
##
##
#########################################################################


WritesigFeature <-  function(results, x,  fileName="Result"){
  wb = createWorkbook()
  for(i in 1:length(results)){
    label = paste("Iteration_",i)
    SelectedGene <- results[[i]]$feature.ids
    FeatureName <- colnames(x[,SelectedGene])
    l <- list(FeatureID=results[[i]]$feature.ids,
              FeatureName=FeatureName,
              TrainSampleID=results[[i]]$train.data.ids,
              TrainSampleLabels=results[[i]]$train.data.level,
              TestSampleID=results[[i]]$test.data.ids,
              TestSampleLabels=results[[i]]$test.data.level )
    n <- max(sapply(l, length))
    ll <- lapply(l, function(X) {c(as.character(X), rep("", times = n - length(X)))})
    out <- do.call(cbind, ll)
    out = data.frame(out)
    addWorksheet(wb, sheetName=label)
    writeDataTable(wb, i, x = out)
  }
  saveWorkbook(wb, paste(fileName,".xlsx", sep = ""))
}

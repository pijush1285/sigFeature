#########################################################################
## Name:        predict.R
## Author:      David Meyer et. al
##
##
## Change Log:
##   * April 4, 2018
##       - split sigFeature.R into several files for easier readability
##         and maintenance
#########################################################################

#########################################################################
## predict(): This function is taken from the R package "e1071".
## This function predicts values based upon a model trained by svm()
## function.
##
##
#########################################################################


predict.sigFeature<-function (object, newdata, decision.values=FALSE,probability=FALSE, ... , na.action=na.omit)
{
  if (missing(newdata))
    return(fitted(object))
  if (object$tot.nSV < 1)
    stop("Model is empty!")
  if (inherits(newdata, "Matrix")) {
    loadNamespace("SparseM")
    loadNamespace("Matrix")
    newdata <- as(newdata, "matrix.csr")
  }
  if (inherits(newdata, "simple_triplet_matrix")) {
    loadNamespace("SparseM")
    ind<-order(newdata$i, newdata$j)
    newdata<-new("matrix.csr", ra=newdata$v[ind], ja=newdata$j[ind], ia=as.integer(cumsum(c(1, tabulate(newdata$i[ind])))),
                   dimension=c(newdata$nrow, newdata$ncol))
  }
  sparse <- inherits(newdata, "matrix.csr")
  if (object$sparse || sparse)
    loadNamespace("SparseM")
  act <- NULL
  if ((is.vector(newdata) && is.atomic(newdata)))
    newdata <- t(t(newdata))
  if (sparse)
    newdata <- SparseM::t(SparseM::t(newdata))
  preprocessed <- !is.null(attr(newdata, "na.action"))
  rowns <- if (!is.null(rownames(newdata)))
    rownames(newdata)
  else 1:nrow(newdata)
  if (!object$sparse) {
    if (inherits(object, "svm.formula")) {
      if (is.null(colnames(newdata)))
        colnames(newdata)<-colnames(object$SV)
      newdata<-model.matrix(delete.response(terms(object)),as.data.frame(newdata))
      newdata<-na.action(newdata)
      act<-attr(newdata, "na.action")
    }
    else {
      newdata <- na.action(as.matrix(newdata))
      act <- attr(newdata, "na.action")
    }
  }
  if (!is.null(act) && !preprocessed)
    rowns <- rowns[-act]
  if (any(object$scaled))
    newdata[, object$scaled] <- scale(newdata[, object$scaled, drop = FALSE], center = object$x.scale$"scaled:center",
                                      scale = object$x.scale$"scaled:scale")
  if (ncol(object$SV) != ncol(newdata))
    stop("test data does not match model !")
  ret <- .C("svmpredict", as.integer(decision.values), as.integer(probability),
            as.double(if (object$sparse) object$SV@ra else t(object$SV)),
            as.integer(nrow(object$SV)), as.integer(ncol(object$SV)),
            as.integer(if (object$sparse) object$SV@ia else 0), as.integer(if (object$sparse) object$SV@ja else 0),
            as.double(as.vector(object$coefs)), as.double(object$rho),
            as.integer(object$compprob), as.double(if (object$compprob) object$probA else 0),
            as.double(if (object$compprob) object$probB else 0),
            as.integer(object$nclasses), as.integer(object$tot.nSV),
            as.integer(object$labels), as.integer(object$nSV), as.integer(object$sparse),
            as.integer(object$type), as.integer(object$kernel), as.integer(object$degree),
            as.double(object$gamma), as.double(object$coef0), as.double(if (sparse) newdata@ra else t(newdata)),
            as.integer(nrow(newdata)), as.integer(if (sparse) newdata@ia else 0),
            as.integer(if (sparse) newdata@ja else 0), as.integer(sparse),
            ret = double(nrow(newdata)), dec = double(nrow(newdata) *
                                                        object$nclasses * (object$nclasses - 1)/2), prob = double(nrow(newdata) * object$nclasses), PACKAGE = "e1071")
  ret2 <- if (is.character(object$levels))
    factor(object$levels[ret$ret], levels = object$levels)
  else if (object$type == 2)
    ret$ret == 1
  else if (any(object$scaled) && !is.null(object$y.scale))
    ret$ret * object$y.scale$"scaled:scale" + object$y.scale$"scaled:center"
  else ret$ret
  names(ret2) <- rowns
  ret2 <- napredict(act, ret2)
  if (decision.values) {
    colns = c()
    for (i in 1:(object$nclasses - 1)) for (j in (i + 1):object$nclasses) colns <- c(colns,
                                                                                     paste(object$levels[object$labels[i]], "/", object$levels[object$labels[j]],sep = ""))
    attr(ret2, "decision.values") <- napredict(act, matrix(ret$dec,
                                                           nrow = nrow(newdata), byrow = TRUE, dimnames = list(rowns,colns)))
  }
  if (probability && object$type < 2) {
    if (!object$compprob)
      warning("SVM has not been trained using `probability = TRUE`, probabilities not available for predictions.")
    else attr(ret2, "probabilities") <- napredict(act, matrix(ret$prob,
                                                              nrow = nrow(newdata), byrow = TRUE, dimnames = list(rowns, object$levels[object$labels])))
  }
  ret2
}





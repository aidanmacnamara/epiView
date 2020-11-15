#'  prints a variance explained table from a prcomp PCA output
#'
#' @param pca.obj a prcomp PCA object
#' @param k the number of PC's to be included. If NA, all PCs are included
#'
#' @return a data frame of percentages and cumulatived percentages explained
#' @export
#'
#' @examples
#' pca.obj=prcomp(USArrests)
#' summary(pca.obj)
#'
#' varianceExplainedSummary(pca.obj)
#' varianceExplainedSummary(pca.obj,3)

varianceExplainedSummary=function(pca.obj,k=NA){

  # note that this makes sense as the $sdev component of the PCA object
  # is actually the square root of the eigenvalues. Thus, here SD^2 are
  # just the eignevalues as expected.

  SD=pca.obj$sdev
  percent=percent=100*SD^2/sum(SD^2)

  if (is.na(k)){
    k=length(SD)
  }

  out=rbind(percent,
            cumulative=cumsum(percent))[,1:k]

  colnames(out)=paste0("PC",1:ncol(out))
  out

}


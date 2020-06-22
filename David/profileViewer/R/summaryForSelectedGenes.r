#' Title
#'
#' @param genes
#' @param rv
#' @param contrasts
#'
#' @return
#' @export


summaryForSelectedGenes=function(genes,rv,contrasts=c("primary","PMA")){

  first=T
  result=NULL

  # reverses the order to get the descriptions after the genes
  for (contrast in contrasts){

    M=subset(rv$contrasts[[contrast]],
             gene %in% genes)[,c("gene","log2FoldChange","pvalue","description")]

    M$dir = ifelse(M$log2FoldChange>0,"up","down")
    print(colnames(M))

    colnames(M)[c(2,3,5)]=paste(contrast,c("l2fc","pvalue","dir"),sep=".")

    if (first){
      result=M
      first=F
    } else {
      result=merge(M,result)
    }
  }

  result
}



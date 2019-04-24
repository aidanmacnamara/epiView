#' constructs an input data source object
#'
#' Constructs an input object for the GoViewer tool.
#'
#' @param x a data frame or similar of gene or genomic data. Alternatively, supplying a
#'     DEseqResults object here will automatically populate this object
#' @param name a character string to be used as a name to identify the classfication
#' @param annotation a string defining the annotation type. Current the tool only supports SYMBOL, the default.
#' @param na.rm if true, all removes missing values from x before returning
#'
#' @return an object of the class GoViewer.input
#' @export

goViewerInputSource=function(x,
                             name="source data input",
                             annotation="SYMBOL",
                             na.rm=F){

  # automatic conversion from DESeq
  if (class(x) == "DESeqResults"){
    require(DESeq2)
    x=data.frame(
      name=rownames(x),
      l2fc=x$log2FoldChange,
      l2fc.se=x$lfcSE,
      p=x$pvalue,
      padj=x$padj)
  }

  # remove NA's if required
  if (na.rm){
    x=na.omit(x)
  }

  obj=list(name=name,
       date=date(),
       annotation=annotation,
       data=x)

  class(obj)="GoViewer.source"
  obj

}


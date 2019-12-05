#' constructs an input data object
#'
#' Constructs an input object for the GoViewer tool.
#'
#' @param x a named vector of gene classifications. This can either be binary to split the list into two groups or
#'     numeric containing values which can be ordered for a later split by the tool. See type below. Gene names are
#'     provided as the vectors names attribute using the convention defined by annotation. Alternatively, supplying a
#'     DEseqResults object here will automatically populate x as an 'ordered' object
#' @param name a character string to be used as a name to identify the classfication
#' @param annotation a string defining the annotation type. Current the tool only supports SYMBOL, the default.
#' @param type either 'ordered' or 'split' corresponding to the numeric types described for x above.
#' @param na.rm if true, all removes missing values from x before returning
#'
#' @return an object of the class GoViewer.input
#' @export
#'
#' @examples
#'
#' genes=runif(10)
#' names(genes)=paste("gene",LETTERS[1:10])
#'
#' goViewerInput(genes,
#'               name="test",
#'               annotation="test only")

goViewerInput=function(x,
                       name="input vector",
                       annotation="SYMBOL",
                       type="ordered",
                       na.rm=F){

  # automatic conversion from DESeq
  if (class(x) == "DESeqResults"){
    require(DESeq2)
    x2=x$pvalue
    names(x2)=rownames(x)
    type="ordered"
    x=x2
  }

  # remove NA's if required
  if (na.rm){
    x=x[!is.na(x)]
  }

  # store object after basic checks
  if (type %in% c("ordered","split")){

    if (is.null(names(x))){
      stop("names attribute for 'x' not defined.")
    }


    if (type == "split"){

      if (!is.logical(x)){
        stop("x must be logical if type is 'split'")
      }

    }

    obj=list(data=x,
             annotation=annotation,
             name=name,
             type=type,
             date=date())

    class(obj)="GoViewer.input"
    obj


  } else {
    stop("type must be one of 'ordered' or 'split'")
  }
}


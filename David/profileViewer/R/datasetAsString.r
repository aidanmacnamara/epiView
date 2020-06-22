#' makes a text copy of a dataset
#'
#' Creates a text representation of a dataset in standard column format
#' separating columns by tabs and rows by new lines (R's \n). Note that
#' printed copies of the output are a text only representation of the string;
#' use cat() to correctly 'display' the tabs and new lines.
#'
#' @param x a data frame
#'
#' @return a string, a text representation of the above
#' @export
#'
#' @examples
#'
#' a=data.frame(a=1:4,b=letters[1:4])
#' print(a)
#' cat(datasetAsString(a))

datasetAsString=function(x){

  if (is.data.frame(x)){

 #   x = apply(x,1:2,as.character)
 #   print(x)

    out=paste0(paste(colnames(x),collapse="\t"),
               "\n")

    for (i in 1:nrow(x)){
      out=paste0(out,
                 paste(x[i,],collapse="\t"),
                 "\n")
    }

    out

  } else {
    stop("x should be a data.frame")
  }

}


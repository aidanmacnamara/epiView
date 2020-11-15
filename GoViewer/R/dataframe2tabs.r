#' converts a dataframe to a tab-deliminated charactor string
#'
#' Converts a dataframe in to a character string which prints as a tab-deliminated file. Columns are
#' separated by tabs, rows by new-lines. The last character is a new line.
#'
#' @param x a data frame
#'
#' @return a charactor string
#' @export
#'
#' @examples
#' dataframe2tabs(data.frame(a=c("one","two","three"),b=1:3,c=11:13))

dataframe2tabs=function(x){

  paste(paste(
    c(paste(colnames(x),collapse="\t"),
      apply(x,1,
            paste,collapse="\t")),
    collapse="\n"),
    "\n",sep="")
}


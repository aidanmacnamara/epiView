#' lists the number of marks by line
#'
#' Data integration methods require a full data cube. This function
#' returns a table of the number of entries by cell line and marks.
#' All values should be equal for any follow on data integration
#' methods to work.
#'
#' @param x data object, a copy of the contents of Aidan's
#'   global_choice()$tmp
#'
#' @return data frame of cell line x marks giving the number of
#'    data points for each combination
#' @export
#'
#' @import plyr
#' @import reshape

listMarksByLine=function(x){

  # note that only the first mark=slot has a fully up-to-date
  # version of the annotation information

  cellLines=rownames(x[[1]]$res)

  a=rename(ldply(x,
        function(z){
          data.frame(
            line=cellLines,
            n=apply(z$res,1,function(x){sum(!is.na(x))})
          )
        }),
        c(".id"="mark"))

  as.data.frame(cast(a,line~mark,value="n"))
}


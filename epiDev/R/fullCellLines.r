#' lists the cell lines containing a given number of entries
#' for all marks
#'
#' For each cell line, checks that at least n entries are present
#' for each mark type. Normally requiring at least one (the default)
#' this allows you to rule out samples which are missing for at least
#' one of the mark types.
#'
#' @param x a list of data types in Aidan's global_choice()$tmp format.
#' @param n the minimum of required data points. Normally the
#'   default
#'
#' @return a list of cell line names statisfying the required
#'   condition.
#' @export

fullCellLines=function(x,n=1){
  n.table=listMarksByLine(x)
  n.table[,1][apply(n.table[,-1],1,min)>=n]
}

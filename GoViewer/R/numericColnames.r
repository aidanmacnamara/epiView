#' returns the names of numeric dataframes columns
#'
#' Works like \link[base]{colnames} but only returns those which are numeric
#'
#' @param x a data frame
#'
#' @return the names of numeric columns
#' @export
#'
#' @examples
#' x=1:10
#' z=data.frame(a=x,
#'              b=as.factor(x),
#'              c=as.character(x),
#'              d=as.integer(x),
#'              e=as.Date(x,origin="2000-01-01"),
#'              f=x^2)
#' str(z)
#'
#' numericColnames(z)

numericColnames=function(x){
  result=unlist(lapply(x,is.numeric))
  names(result[result])
}



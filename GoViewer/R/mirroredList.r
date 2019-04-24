#' returns a list with names equal to content
#'
#' @param x a vector
#' @param list if false, a named vector is returned
#'
#' @return a named list
#' @export
#'
#' @examples
#' mirroredList(1:3)
#' mirroredList(c("Monday","Tuesday","Wednesday"))
#' mirroredList(c("Monday","Tuesday","Wednesday"),list=F)
#'
#' x=1:5
#' names(x)=LETTERS[1:5]
#' x
#' mirroredList(x,list=F)

mirroredList=function(x,list=T){

  if (!is.null(names(x))){
    warning("any existing names attributes will be overwritten")
  }

  x=as.character(x)
  names(x)=x

  if (list){
    as.list(x)
  } else {
    x
  }
}



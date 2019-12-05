#' returns a named list of sequential numbers
#'
#' Returns a list with elements 1 to the input's length labeled by a
#' character representation of the input.
#' Possible applications are selection lists in Shiny.
#' Optionally will also return a named vector.
#'
#' @param x a vector of numbers or characters
#' @param list if false, a named vector is returned instead
#'
#' @return a named list or vector
#' @export
#'
#' @examples
#' inverseNumberedList(c("Monday","Tuesday","Wednesday","Thursday","Friday"))
#' inverseNumberedList(c("Monday","Tuesday","Wednesday","Thursday","Friday"),list=F)
#' inverseNumberedList(1000:1010)
#' inverseNumberedList(1/(1:5))

inverseNumberedList=function(x,list=T){
  out=1:length(x)
  names(out)=as.character(x)

  if (list){
    as.list(out)
  } else {
    out
  }

}



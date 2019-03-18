#' returns number strings like "100%"
#' 
#' A simple wrapper for paste0 for rescalling sizes etc. 
#'
#' @param x an input number
#'
#' @return a string of the form "x%" where x is the given number
#' @export
#'
#' @examples
#' numberToStringPercent(50)
numberToStringPercent=function(x){
  paste0(x,"%")
}
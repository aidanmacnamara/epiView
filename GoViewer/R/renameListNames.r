#' assigns generic names to list slots
#'
#' Forces a generic naming convention on a list of named or unnamed entries
#'
#' @param x a input list
#'
#' @return a copy of x with top-level slots renamed slot-1, slot-2, etc.
#' @export
#'
#' @examples
#' renameListNames(list(1:2,1:4))
renameListNames=function(x){
  names(x)=sprintf("slot-%d",1:length(x))
  x
}




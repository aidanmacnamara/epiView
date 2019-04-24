#' sets the names of a character vector to have the same values as the vector itself
#'
#' @param x a character vector
#'
#' @return a character vector but with names equal to itself
#' @export
#'
#' @examples
#' a=letters[1:5]
#' a
#'
#' echoNames(a)

echoNames=function(x){
  names(x)=x
  x
}


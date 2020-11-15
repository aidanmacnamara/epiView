#' prints a single vector as a string
#'
#' Prints a vector as a string with comma deliminators. See examples for details
#'
#' @param x a vector
#'
#' @return a string
#' @export
#'
#' @examples
#' vector2string(1:4)
#' vector2string(signif(1/1:4,3))
#' vector2string(letters[1:5])

vector2string=function(x){
  paste0("(",paste(x,collapse=", "),")")
}

#' a multiple pattern grepl
#'
#' Applies multiple patterns to a string returning a TRUE if any of them
#' match
#'
#' @param patterns a string vector of patterns
#' @param x a string vector to be matched
#'
#' @return a logical vector of the same length as x
#' @export
#'
#' @examples
#'
#' a=c("Aaa","bbC","ee","ff")
#' multiGrepl(c("aa","ee"),a)

multiGrepl=function(patterns,x){

  result=rep(F,length(x))

  for (pattern in patterns){
    result[grep(pattern,x)]=T
  }

  result
}

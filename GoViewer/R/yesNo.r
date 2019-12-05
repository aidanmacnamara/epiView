#' boolean yes-no formatter
#'
#' Prints yes or no for true or false. Supports vectors and alternative
#' choices of words.
#'
#' @param x a boolean variable (or 0's and 1's)
#' @param yesno an optional length two vector containing a translation of true and false
#'
#' @return
#' @export
#'
#' @examples
#' yesNo(c(T,T,F))
#' yesNo(c(T,T,F),yesno=c("ja","nein"))

yesNo=function(x,yesno=c("yes","no")){

  yesno[2-x]
}


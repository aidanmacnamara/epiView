#' lists the elements of a vector as a comma delimited list with final 'and' or 'or'
#'
#' Generates user readable text output in plain text format of the form "Monday, Tuesday and Wednesday".
#'
#' @param x the vector to be listed. If necessary, this will be coerced to character strings.
#' @param and normally "and" but can be set to any other word between the final two words.
#'
#' @return the required string
#' @export
#'
#' @examples
#' andList(c("red","black","blue"))
#' andList(1:10)
#' andList(1:20,"or")
andList=function(x,and="and"){

  n=length(x)

  if (n>1){
    paste(paste(x[-n],collapse=", "),
          and,x[n])
  } else {
    x
  }
}

#' a formatted version of paste
#'
#' Use for printing reports to the screen etc.
#'
#' @param fmt a \link{sprintf} type format, if a vector is concatonnated to a scalar with new
#'     lines
#' @param ... the parameters to be printed
#'
#' @return a character string
#' @export
#'
#' @examples
#' fpaste(c("today is the %s",
#'          "I am %s",
#'          "my score is %d"),
#'          date(),"happy",8)
#'
#' cat(fpaste(c("today is the %s",
#'              "I am %s",
#'              "my score is %d"),
#'            date(),"happy",8))

fpaste=function(fmt,...){
  paste(sprintf(paste(fmt,collapse="\n"),
              ...),sep="")
}

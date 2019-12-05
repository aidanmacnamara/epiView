#' formated cat
#'
#' A version of \link[base]{sprintf} enclosed by a cat to write directly to the screen
#'
#' @param format a string format
#' @param ... parameters passed after the format (e.g. values)
#'
#' @return a copy of the sctring printed to output
#' @export
#'
#' @examples
#' fcat("%s = %f","x",1.2)
#' fcat("%s = (%d,%d)","a",34,54)


fcat=function(format,...){
  out=sprintf(format,...)
  cat(out,"\n",sep="")
  out
}



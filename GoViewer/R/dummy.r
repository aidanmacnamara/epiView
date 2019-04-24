#'  dummy function
#'
#'  Accepts an arbitary number of arguments but returns a NULL printing details of its call to the screen
#'
#' @param ... any number of arguments, possibly zero
#'
#' @return NULL
#' @export
#'
#' @examples
#' dummy()
#' dummy(1,2,3,4)
dummy=function(...){
  cat(">>> call to dummy(): [development program tracing]\n")
  print(match.call())
  NULL
}



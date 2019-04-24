#' tests if an expression is invalidated
#'
#' See https://stackoverflow.com/questions/45478521/listen-to-reactive-invalidation-in-shiny for details
#'
#' @param obj a shiny reactive object
#'
#' @imports shiny
#' @return a logical scalar
#' @export
invalidated=function(obj){

  if (!is.reactive(obj)){
     stop("object to be tested is not a reactive object")
  }

  attr(obj,"observable")$.invalidated
}

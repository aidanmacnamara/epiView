#' normalises a vector by its L2-norm
#'
#' Missing values are ignored.
#'
#' @param x a vector
#'
#' @return a normalised vector
#' @export
#'
#' @examples
#' normaliseL2(c(1,1))
#' normaliseL2(c(1,1,1))
#' normaliseL2(c(1,1,NA))

normaliseL2=function(x){
  x/sqrt(sum(x^2,na.rm=T))
}




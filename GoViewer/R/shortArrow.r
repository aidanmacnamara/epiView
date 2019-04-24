#' a shorten arrow
#'
#' Not used.Need something for ggplot2, but this is base graphics
#'
#' @param x0
#' @param y0
#' @param x1
#' @param y1
#' @param alpha
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' plot(c(1,2),c(2,4),xlim=c(0,3),ylim=c(1,5))
#' shortArrow(1,2,2,4,0.80,col="red")

shortArrow=function(x0,y0,x1,y1,alpha=1,...){

  dx=x1-x0
  dy=y1-y0

  mx=(x0+x1)/2
  my=(y0+y1)/2

  a2=alpha/2

  x0s=mx-a2*dx
  x1s=mx+a2*dx

  y0s=my-a2*dy
  y1s=my+a2*dy

  arrows(x0s,y0s,x1s,y1s,...)

}

plot(c(1,2),c(2,4),xlim=c(0,3),ylim=c(1,5))
shortArrow(1,2,2,4,0.80,col="red")

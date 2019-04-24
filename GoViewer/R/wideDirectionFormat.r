#' prepares projected direction observations to reconstruct a direction vector
#'
#' In an attempt to guess the direction in a higher dimensional PCA space, the
#' user selects the direction in one or combinations of PCA components. This
#' code takes the output captured from the GUI and coverts it in to a simpler form
#' suitable for checks and calculations. For more details, see the example
#'
#' @param n the number of dimensions or PCs to be considered
#' @param x a data frame of paired PC coordinates and x and y displacements
#'
#' @return a data frame of the type produced by \link{makeW}
#' @export
#'
#' @examples
#'
#' # example of use. A vector in 6-space with two observed planes
#'
#' n=6
#'
#' dirDefined=data.frame(i=c(1,1),j=c(2,4),x=c(8,10),y=c(-7,2))
#' dirDefined
#'
#' w=wideDirectionFormat(n,dirDefined)
#' w
#'
#' a=checkPcaViewConnectivity(w)
#' a
#'
#' d1=findDirection(w[a$selected,])
#' d1
#'
#' d2=rep(0,n)
#' d2[a$selected]=d1
#' d2

wideDirectionFormat=function(n,x){

  w=array(NA,dim=c(n,nrow(x)))

  for (i in 1:nrow(x)){
    w[x[i,"i"],i]=x[i,"x"]
    w[x[i,"j"],i]=x[i,"y"]
  }

  w
}


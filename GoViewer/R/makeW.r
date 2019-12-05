#' creates test projections from all possible views
#'
#' For a given vector, returns approximate views from all possible 2D axis combination views. Any required views can then be selected
#' by standard column subsetting. Non-observed co-ordinates are returned as NAs.
#' This routine is provided for testing and demonstration purposes.
#'
#' @param z a vector of any length
#' @param sigma the SD of the noise to be added to get observation
#'
#' @return a data frame. If z has length, this is a data frame with n rows and n(n-1)/2 columns. Each column
#'    contains two non-missing entries: the respective observations.
#' @export
#'
#' @examples
#' makeW(1:5)
makeW=function(z,sigma=0.01){

  n=length(z)
  w=array(NA,dim=c(n,n*(n-1)/2))

  k=0
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      k=k+1
      w[i,k]=z[i]+rnorm(1,0,sd=sigma)
      w[j,k]=z[j]+rnorm(1,0,sd=sigma)
    }
  }
  w
}


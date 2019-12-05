#' computes a matrix of all length-p binary vectors
#'
#' @param p integer, the vector length
#'
#' @return 2^p x p  matrix
#' @export
#'
#' @examples
#' M=matrix2p(4)
matrix2p=function(p){

  M=matrix(0,nrow=2^p,ncol=p)

  for (i in 1:p){
    M[,i]=rep(c(0,1),each=2^(p-i))
  }

  M
}

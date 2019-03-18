#' Title
#'
#' @param x
#' @param k
#'
#' @return
#' @export
#'
#' @examples
cpFactor=function(x,k){
    cp.obj=cp(x,k)

    # reorder by lambda
    iorder=order(-cp.obj$lambdas)

    cp.obj$U[[1]]=cp.obj$U[[1]][,iorder]
    cp.obj$U[[2]]=cp.obj$U[[2]][,iorder]
    cp.obj$U[[3]]=cp.obj$U[[3]][,iorder]
    cp.obj$lambdas=cp.obj$lambdas[iorder]

    cp.obj
}

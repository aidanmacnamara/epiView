#' computes a tabular two-way Venn diagram from a list of contrast objects
#'
#' @param Q a list of goViewerInput objects all of type 'split'
#'
#' @return a matrix of cross tabulations of frequency counts.
#' @export
#'
#' @examples
#' data(P2)
#' Q2=P2[1:3]
#'
#' demo=function(x,a=500,b=1000){
#'   x$type="split"
#'   x$data=0*x$data
#'   x$data[a:b]=x$data[a:b]+1
#'   x
#' }
#'
#' Q2=list(demo(P2[[1]],100,500),
#'         demo(P2[[2]],100,1000),
#'         demo(P2[[3]],5000,6000))
#'
#'
#' computeTwoWayVennDiagram(Q2)
#' computeTwoWayVennDiagram(Q2[1]) # also works for one selection
#' computeTwoWayVennDiagram(list()) # but not an empty list
#'
#' Q2[[1]]$type="ordered"
#' computeTwoWayVennDiagram(Q2)

computeTwoWayVennDiagram=function(Q){

  if (any(unlist(lapply(Q,"[[","type")) != "split")){
    stop("at least one input object is not of type 'split'")
  }

  if (length(Q) == 0){
    stop("require at least input object in list")
  }


  n=length(Q)
  M=matrix(NA,nrow=n,ncol=n)

  for (i in 1:n){
    for (j in 1:n){
      if (j >= i){
        M[i,j]=sum(Q[[i]]$data * Q[[j]]$data)
      }
    }
  }

  if (n > 26){

    # should never need - included for robustness only

    rownames(M)=sprintf("[%d] %s",
                        1:n,
                        unlist(lapply(Q,"[[","name")))

    colnames(M)=sprintf("[%d]",
                        1:n)
  } else {

    rownames(M)=sprintf("[%s] %s",
                        LETTERS[1:n],
                        unlist(lapply(Q,"[[","name")))

    colnames(M)=sprintf("[%s]",
                        LETTERS[1:n])
  }

  M
}

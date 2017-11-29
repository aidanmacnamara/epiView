#' folds a list of marks into a tensor format
#'
#' Assumes that each mark has identical dimensions and dimnames.
#'
#' @param x a list of marks similar to
#'
#' @return a rTensor object of the form input-rows x input-columns x marks
#' @export
#'
#' @examples
#' m=matrix(1:4,nrow=2,dimnames=list(c("a","b"),c("x","y")))
#'
#' x=list(one=list(res=m+100),
#'        two=list(res=m+200),
#'        three=list(res=m+300))
#'
#' a=foldToThreeTensor(x)
#' a
#' a@data

foldToThreeTensor=function(x){

  x1=x[[1]]$res
  out=array(NA,dim=c(dim(x1),length(x)))

  for (i in 1:length(x)){
    out[,,i]=as.matrix(x[[i]]$res)
  }

  dimnames(out)=list(rownames(x1),
                     colnames(x1),
                     names(x))

  as.tensor(out)
}


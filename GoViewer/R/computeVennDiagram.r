#' computes a full Venn diagram
#'
#' Note , to test, draw out a diagram first, populate with some random [unique] symbols, and then
#' note down the original sets.
#'
#' @param Q  a named list of sets. If no names are given, default names are added
#'
#' @return a matrix with as many columns as logical combinations and as many rows as there are input sets. The column names give the sums
#'   the corresponding combinations and the row names, the names of the sets.
#' @export
#'
#' @examples
#'
#' computeVennDiagram(list(A=c(4,5,6,7),
#'                         B=c(5,6,7,8,9,10),
#'                         C=c(7,10)))
#'
#' computeVennDiagram(list(c(4,1,2,3,5,6,7),
#'                         c(1,2,3,5,6,7,9),
#'                         c(5,6,7,9,8)))

computeVennDiagram=function(Q){

  p=length(Q)

  if (is.null(names(Q))){
    names(Q)=paste("set",1:p)
  }

  M=t(matrix2p(p))[,-1]

  colnames(M)=letters[1:ncol(M)]
  rownames(M)=names(Q)

  all.terms=setListHelper(Q,union)

  all.terms

  dual=list(present=Q,
            absent=lapply(Q,function(x){
              setdiff(all.terms,x)
            }))


  nSubset=NULL

  for (i in 1:ncol(M)){

    test=list()

    for (j in 1:p){
      if (M[j,i] == 1){

        test[[j]]=dual[["present"]][[j]]
      } else {

        test[[j]]=dual[["absent"]][[j]]
      }
    }

    matches=setListHelper(test,intersect)
    nSubset=c(nSubset,length(matches))

  }

  colnames(M)=nSubset
  M
}





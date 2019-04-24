#' checks to see if PCA views are compariable to triangulate a single direction
#'
#' Consider a six dimensional space, but we only have views for directions 1, 2 and 3, and 5 and 6.
#' In this case we (i) only have information about dimensions 1, 2, 3, 5 and 6 and (ii) have a suggested direction for TWO vectors,
#' one in dimensions 1-3 and a second in 4-6. This function (a) takes a collection of projected directions, returns the dimensions
#' the hidden vector lies in [i.e. those that define its hyperplane or subspace] and (c) if multiple vectors in different subspaces are
#' implied, as list of two sets of accross which projections should be returned to define a single vector.
#'
#' @param w table of input projections of the form simulated by \link{makeW}
#'
#' @return a list with two slots, selected and missing: selected gives a vector of the dimensions in which the vector is assumed
#'    to lie. If a single direction is described, missing has length zero - list(). If more than one vector is implied, two vectors -
#'    group1 and group2 are returned given the dimensions of the subspaces what have to be linked to define a single direction.
#' @export
#'
#' @examples
#'
#' w=makeW(1:6)
#' checkPcaViewConnectivity(w)
#'
#' w[,1:2]
#' checkPcaViewConnectivity(w[,1:2])
#'
#' w[,c(1:2,15)]
#' checkPcaViewConnectivity(w[,c(1:2,15)])

checkPcaViewConnectivity=function(w){

  # makes a adjacency matrix

  makeAdjMatrix=function(w){

    # set up space for the and adjacency matrix
    n=nrow(w)
    A=diag(1,n)
    dimnames(A)=list(paste(1:n),paste(1:n))

    # use of row and colnames allows us to first subset out those
    # directions that are never represented
    select=apply(!is.na(w),1,any)
    A=A[select,select]

    # and allow us to create a matrix of the dimensions we are interested in
    for (i in 1:ncol(w)){
      axes=as.character((1:n)[!is.na(w[,i])])
      A[axes[1],axes[2]]=1
      A[axes[2],axes[1]]=1
    }
    A
  }

  # computes the k-th matrix power

  matrixPower=function(A,k=2){
    if (k>1){
      B=A
      for (i in 2:k){
        B = B %*% A
      }
      B
    } else {
      A
    }
  }

  # the binary image (B) of the k-th power of the adjacency matrix gives the
  # closure of the network
  A=makeAdjMatrix(w)
  A.k=matrixPower(A,k=nrow(w))

  zeros = A.k == 0

  # only need to look at one direction (matrix is symmetric) so blank off the
  # upper diagonal to FALSE
  zeros = zeros & !upper.tri(zeros)

  selected=(1:nrow(w))[apply(!is.na(w),1,any)] # repeating


  zeroRows=rownames(zeros)[apply(zeros,1,any)]
  if (length(zeroRows) > 0){
    zeroCols=colnames(zeros)[apply(zeros,2,any)]
    list(selected=selected,
         missing=list(group1=as.numeric(zeroRows),group2=as.numeric(zeroCols)))
  } else {
    list(selected=selected,missing=list())
  }

}



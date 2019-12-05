#' estimates a n-dimensional direction from  at least n-1 views
#'
#' Estimates the direction in a PCA subspace from looking at k-1 or more axis combinations. Used to triangulate
#' a direction corresponding to a given set of differences through multiple 2D PCA views.
#'
#' Note that to ensure that the problem is uniquely defined we also constrain the normalisation to give a L2-norm of 1.
#' This is ensured via the regularisation parameter p. This optimisation task is however trivial. As soon as the direction
#' is found, the optimisation is simply a quadratic along a radius.
#'
#' @param w a data frame of columns each corresponding to observed views. Non-observed coordinates should be coded as NA
#' @param p a penality parameter for the length of the vector returned by the optimisation. This is an internal parameter,
#'    and the output is forced normalised if scale1 is not selected
#' @param scale1 If true, the output is rescaled so the the first component is one. This is useful for testing and demonstration
#'    purposes
#' @param trace if true, the number of iterations is also returned
#' @param warn if true (default), the code checks to see if enougth columns are specified to triangulate a vector of the correction
#'    dimensionality. No checks are however made for redunancy (linear dependence etc.)
#'
#' @return a vector of length nrow(w)
#' @export
#'
#' @examples
#' w=makeW(1:5)
#' findDirection(w,scale1=T,trace=T)
#' findDirection(w[,1:5],scale1=T,trace=T)
#' findDirection(w[,1:4],scale1=T,trace=T)
#' findDirection(w[,1:3],scale1=T,trace=T)
#' findDirection(w[,1:3],scale1=T,trace=T,warn=F)

findDirection=function(w,p=1,scale1=F,trace=F,warn=T){

  # do we have enough dimensions?
  # Just a basic count here - does not check if the input views are linearily independent
  if (warn){
    if (ncol(w) < nrow(w)-1){
      stop("at least k-1 views needed to estimate the direction of a k-dimensional vector")
    }
  }

  # the objective function to be maximised
  f0=function(z,lambda,w,p=100){
    value=0
    for (j in 1:ncol(w)){
      for (i in 1:nrow(w)){
        if (!is.na(w[i,j])){
          value=value+(z[i]-lambda[j]^2*w[i,j])^2
        }
      }
    }
    value+p*(sum(z^2)-1)^2
  }

  # a wrapper to bind the data to objective function by lexical scoping
  makeF=function(w,p=100){
     function(x){
      f0(x[1:nrow(w)],x[-(1:nrow(w))],w,p=p)
    }
  }

  # the active code

  # ... normalise columns because of convergence issues
  w=apply(w,2,normaliseL2)

  f=makeF(w,p=p)
  z.start=c(rep(0,nrow(w)),
            rep(1,ncol(w))) # need to scale directions o/w nlm() stops too early
  result=nlm(f,z.start)
  if (trace) cat(result$iterations,"iterations\n")
  if (result$code > 2){
    warning(sprintf("nlm returns with code %d: code may have failed",result$code))
  }


  z=result$estimate[1:nrow(w)]
  if (scale1){
    z/z[1]
  } else {
    normaliseL2(z)
  }

}




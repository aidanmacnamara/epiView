#' helper function to extend binary set functions to lists of sets
#'
#' @param z a list of sets, each represented as a vector
#' @param op a bnary function, default is intersect()
#'
#' @return a vector representation of a set
#' @export
#'
#' @examples
#' setListHelper(list(1:10,2:4,1:3),intersect)
#' setListHelper(list(1:10,2:4,1:3),union)

setListHelper=function(z,op=intersect){

  k=length(z)

  if (k > 1){
    result = op(z[[1]],z[[2]])
    if (k > 2){
      for (i in 2:k)
        result=op(result,z[[k]])
    }
    result

  } else {
    z[[1]]
  }

}



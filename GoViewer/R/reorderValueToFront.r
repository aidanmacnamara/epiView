#' moves a given value to the front of a vector
#'
#' @param x a vector of values
#' @param value the value or vector of values to be moved forward
#' @param order if true, returns the indices of the permuted vector labelled with the contents of the original array
#'
#' @return a vector of values potentially reordered
#' @export
#'
#' @examples
#' reorderValueToFront(letters[1:6],"d")
#' reorderValueToFront(letters[1:6],c("b","d"))
#'
#' reorderValueToFront(letters[1:6],"z")
#' reorderValueToFront(letters[1:6],c("z","c"))
#'
#' reorderValueToFront(letters[1:6],"d",order=T)
#' reorderValueToFront(letters[1:6],c("b","d"),order=T)

reorderValueToFront=function(x,value,order=F){

  if (any(duplicated(x))){
    stop("'x' assumed not to have duplicates")
  }

  result=c(intersect(value,x),setdiff(x,value))

  if (order){
    index=1:length(result)
    names(index)=x
    index[result]
  } else {
    result
  }

}





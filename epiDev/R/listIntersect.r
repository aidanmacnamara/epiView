#' returns the intersect of a list of sets
#'
#' Extends the base intersect() function to more than two sets
#' by using a list notation. Note that the input argument must be
#' flat list; multiple arguments currently not supported.
#'
#' @param x a flat list containing the sets to be compared
#'
#' @return
#' @export
#'
#' @examples
#' listIntersect(list(1:6,2:5,4:10))

listIntersect=function(x){

  if (is.list(x)){

    n=length(x)
    z=x[[1]]

    if (n > 1){
      for (i in 2:n){
        z=intersect(z,x[[i]])
      }
    }
    z

  } else {
    stop("expecting a list")
  }
}


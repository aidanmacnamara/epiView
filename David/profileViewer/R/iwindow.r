#' returns a right-handed section from a vector 1:n
#'
#' Returns a right-handed section from the vector 1:n if necessary wrapping round. If the section
#' width equals or is greater than the vector length, n, the entire vector is returned
#'
#' @param i the index from with the section is to be made
#' @param w the width of (number of elements in) the section
#' @param n the value of n
#'
#' @return an integer vector
#' @export
#'
#' @examples
#' iwindow(5,3,10)
#' iwindow(5,7,10)
#' iwindow(5,20,10)
#' iwindow(5,0,10)

iwindow=function(i,w,n){

  if( w > 0){
    if (w > n){
      1:n
    } else {
      sort((i:(i+w-1)-1) %% n + 1)
    }
  } else {
    stop("w must be greater or equal to one")
  }

}




#' DEFUNCT lists availablity of markers by project
#' 
#' Think this no longer works - just gives a list of marks ignorning projects
#'
#' @param x a data object returned by Aidan's global_choice()
#'
#' @return a charactor vector with the names of the contained marks
#' @export
#'
#' @examples
listMarksByProject=function(x){
  names(x$tmp)
}

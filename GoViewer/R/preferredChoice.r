#' gives position of first match in a preference list
#'
#' Gives the position or value of the first match in a list of possibilities.
#' Default values can be provided for the case where there is not hit.
#'
#' @param preference a value to be matched
#' @param possibilities a vector of possible matches
#' @param default the index of the default value in x returned if there is no match
#' @param value if true, the value rather than index of the result is returned
#'
#' @return a index or entry from possibilities
#' @export
#'
#' @examples
#' preferredChoice("b",letters[1:4],99)
#' preferredChoice("x",letters[1:4],99)
#'
#' preferredChoice("b",letters[1:4],1,value=T)
#' preferredChoice("x",letters[1:4],1,value=T)

preferredChoice=function(preference,possibilities,default=1,value=F){

  hits = possibilities %in% preference

  if (sum(hits) > 0){
    out=(1:length(hits))[hits][1]
  } else {
    out=default
  }

  if (value){
    possibilities[out]
  } else {
    out
  }
}



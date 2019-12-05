#' swaps values for names in a named vector or flat atomic list
#'
#' @param x a single depth list of atomic values. If nanes() is NULL, numbers are used
#'
#' @return an inverst copy of x with names and values transponsed
#' @export
#'
#' @examples
#' #'
#' x=list(a="alpha",b="beta",c="gamma")
#'
#' x
#' invertList(x)
#'
#' # but
#'
#' y=list(a=1,b=2,c=3:4)
#' invertList(y)

invertList=function(x){

  if (is.null(names(x))){
    names(x)=as.character(1:length(x))
  }

  result=as.list(names(x))
  result.names=unlist(x)
  if (length(result.names) != length(result)){
    stop("assumes `x` is a single depth list of atomic elements")
  } else {
    names(result)=result.names
    result
  }
}


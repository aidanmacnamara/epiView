#' transforms data prior to analysis
#'
#' Supports a selection of simple data transformations to be
#' applied for prior to analysis. Note that this can still be
#' used after quantile normalisation: quantile normalisation
#' standardises samples to a common distribution but does not
#' its final shape. This is the function of a standard data
#' transformation.
#'
#'
#' @param x a data object of the form of Aidan's global_choice()$tmp
#' @param selection one of 'none', 'sqrt', 'log10' [for log10(x+1)] or 'softLog10' [currently not
#'   implemented].
#' @param lambda a non-zero parameter for 'softLog10'  if selected. A
#'   value of zero (default) will reduce to log10.
#'
#' @return a transformed copy of x
#' @export

transformData=function(x,selection="none",lambda=0){

  if (selection == "none"){
    x
  } else {

    # if a transformation selected, first set up a helper
    # function ...

    if (selection == "log10"){
      f=function(w){log10(w+1)}
    } else if (selection == "sqrt"){
      f=sqrt
    } else if (selection == "softLog10"){
      f=log10
    } else {
      stop("selection",selection,"not recognised")
    }

    # ... and then apply to the data

    for (i in 1:length(x)){
      x[[i]]$res=f(x[[i]]$res)
    }

    x
  }
}

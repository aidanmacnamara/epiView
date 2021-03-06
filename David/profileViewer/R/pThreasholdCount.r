#' sets a count limit via p-values with or without adjustment
#'
#' Retruns the number of p-values at or below a given threahold. FDR or other
#' corrections can be selected by setting the method parameter. For FDR use "fdr"
#' or its alias "BH". See examples for some properties of this filter. Missing values
#' are removed before calculation.
#'
#' @param p a vector p values
#' @param method an adjustment method. The default "none" is for no action
#' @param limit a threashold
#'
#' @return the number of p-values below limit after adjustment by method
#' @export
#'
#' @examples
#' p=seq(0.01,1,0.01)
#' p
#'
#' sum(p<=0.3)
#' pThreasholdCount(p,limit=0.3)
#'
#' # just gives one - the above p is just uniform. p/a returns constant adjusted
#' # values of 1/a
#' sum(p.adjust(p,method="BH")<=0.3)
#' pThreasholdCount(p,method="BH",limit=0.3)
#' p.adjust(p,method="BH")
#'
#' # gives 30 because the FDR corresponds to a straight line in a log-log QQ-plot?
#' p=10^(-(1:30))
#' sum(p.adjust(p,method="BH")<=0.3)
#' pThreasholdCount(p,method="BH",limit=0.3)

pThreasholdCount=function(p,method="none",limit=0.3){
  sum(p.adjust(na.omit(p),method=method)<=limit)
}

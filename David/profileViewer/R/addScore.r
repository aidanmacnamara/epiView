#' equivalence based score to test proximity to zero
#'
#' Returns the absolute value of a fold change estimate plus a penalty for the its uncertainly
#' based on a TOST equivalence framework.
#'
#' @param w  a table of DESeq2 like contrast results
#' @param alpha a TOST alpha level, the TOST will use a (1-2*alpha)% CI
#'
#' @return a copy of w with an additional columns zero.score
#' @export
#'
#' @examples
#' data("profiles")
#' head(addScore(profiles$contrasts$c.prime,0.05))

addScore=function(w,alpha=0.10){

  z.alpha=qnorm(1-alpha)
  w$zero.score=abs(w$log2FoldChange)+2*z.alpha*w$lfcSE
  w
}



